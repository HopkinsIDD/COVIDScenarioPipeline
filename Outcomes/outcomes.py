import itertools
import time
import warnings

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from SEIR.utils import config 
import pyarrow.parquet as pq
import pyarrow as pa
import pandas as pd

def create_delay_frame(parameters,data_src, places, dates,compartment):
    # Read the config for this compartement
    probability = parameters['probability']
    delay =       parameters['delay']

    # Create new compartement
    data = np.empty_like(data_src)
    # Draw with from source compartement
    data = np.random.binomial(data_src, probability * np.ones_like(data_src))  
    # Shift to account for the delay
    data = shift(data, delay, fill_value=0)

    rc = pd.DataFrame(data, columns = places, index = dates)
    rc.reset_index(inplace=True)
    rc = pd.melt(rc, id_vars='time', value_name = compartment, var_name='geoid')

    # Make duration
    if 'duration' in parameters:
         duration = parameters['duration']
         data  = np.cumsum(data, axis = 0) - shift(np.cumsum(data, axis=0), duration)
                
         df = pd.DataFrame(data, columns=places, index=dates)
         df.reset_index(inplace=True)
         df = pd.melt(df, id_vars='time', value_name = parameters['duration_name'], var_name='geoid')
         rc = pd.merge(rc,df)
        
    return(rc)

def run_delayframe_outcomes(config, setup_name, outdir, scenario_seir, scenario_outcomes, nsim = 1, index=1, n_jobs=1):
    start = time.monotonic()
    sim_ids = np.arange(index, index + nsim)

    # Prepare the probability table:
    # Either mean of probabilities given or from the file... This speeds up a bit the process.
    # However needs an ordered dict, here we're abusing a bit the spec.
    config_outcomes = config["outcomes"]["settings"][scenario_outcomes]
    if (config["outcomes"]["param_from_file"].get()):
        # load a file from the seir model, to know how to filter the provided csv file
        sim_id_str = str(sim_ids[0]).zfill(9)
        diffI = pd.read_parquet(f'model_output/{setup_name}/{sim_id_str}.seir.parquet')
        diffI = diffI[diffI['comp'] == 'diffI']
        dates = diffI.time
        diffI.drop(['comp'], inplace = True, axis = 1)

        # Load the actual csv file
        branching_file = config["outcomes"]["param_place_file"].as_str()
        branching_data = pd.read_csv(branching_file, converters={"geoid": str})
        branching_data = branching_data[branching_data['geoid'].isin(diffI.drop('time', axis=1).columns)]
        if (branching_data.shape[0] != diffI.drop('time', axis=1).columns.shape[0]):
            raise ValueError(f"Places in seir input files does not correspond to places in outcome probability file {branching_file}")

    parameters = {}
    for new_comp in config_outcomes:
        parameters[new_comp] = {}
        if config_outcomes[new_comp]['source'].exists():
            # Read the config for this compartement
            parameters[new_comp]['source'] = config_outcomes[new_comp]['source'].as_str()
            parameters[new_comp]['probability'] = np.mean(
                config_outcomes[new_comp]['probability']['value'].as_random_distribution()(size = 10000))
            
            parameters[new_comp]['delay'] = int(np.round(np.mean(
                config_outcomes[new_comp]['delay']['value'].as_random_distribution()(size = 10000))))
            
            if config_outcomes[new_comp]['duration'].exists():
                parameters[new_comp]['duration'] = int(np.round(np.mean(
                    config_outcomes[new_comp]['duration']['value'].as_random_distribution()(size = 10000))))
                if config_outcomes[new_comp]['duration']['name'].exists():
                    parameters[new_comp]['duration_name'] = config_outcomes[new_comp]['duration']['name'].as_str()
                else:
                    parameters[new_comp]['duration_name'] = new_comp+'_curr'
            
            if (config["outcomes"]["param_from_file"].get()):
                colname = 'R'+new_comp+'|'+parameters[new_comp]['source']
                if colname in branching_data.columns:
                    print(f"Using 'param_from_file' for probability {colname}")
                    parameters[new_comp]['probability'] = parameters[new_comp]['probability'] * \
                        branching_data[colname].to_numpy()
                else:
                    print(f"NOT using 'param_from_file' for probability {colname}")

        elif config_outcomes[new_comp]['sum'].exists():
            parameters[new_comp]['sum'] = config_outcomes[new_comp]['sum']
        else:
            raise ValueError(f"No 'source' or 'sum' specified for comp {new_comp}")

    if n_jobs == 1:          # run single process for debugging/profiling purposes
        for sim_id in tqdm.tqdm(sim_ids):
            onerun_delayframe_outcomes(sim_id, parameters, setup_name, outdir, scenario_seir, scenario_outcomes)
    else:
        tqdm.contrib.concurrent.process_map(onerun_delayframe_outcomes, sim_ids, 
                                                                    itertools.repeat(parameters), 
                                                                    itertools.repeat(setup_name), 
                                                                    itertools.repeat(outdir), 
                                                                    itertools.repeat(scenario_seir), 
                                                                    itertools.repeat(scenario_outcomes),
                                            max_workers=n_jobs)

    print(f"""
>> {nsim} outcomes simulations completed in {time.monotonic()-start:.1f} seconds
""")

def onerun_delayframe_outcomes(sim_id, parameters, setup_name, outdir, scenario_seir, scenario_outcomes):
    sim_id_str = str(sim_id).zfill(9)
    diffI = pd.read_parquet(f'model_output/{setup_name}/{sim_id_str}.seir.parquet')
    diffI = diffI[diffI['comp'] == 'diffI']
    dates = diffI.time
    diffI.drop(['comp'], inplace = True, axis = 1)
    places = diffI.drop(['time'], axis=1).columns
    all_data = {}
    # We store them as numpy matrices. Dimensions is dates X places
    all_data['incidI'] = diffI.drop(['time'], axis=1).to_numpy().astype(np.int32)
    shape = all_data['incidI'].shape

    outcomes = pd.melt(diffI, id_vars='time', value_name = 'incidI', var_name='geoid')
    for new_comp in parameters:
        if 'source' in parameters[new_comp]:
            # Produce a dataframe
            source = parameters[new_comp]['source']
            df = create_delay_frame(parameters[new_comp], all_data[source], places, dates, new_comp)

            # And merge it
            outcomes = pd.merge(outcomes, df)

        elif 'sum' in parameters[new_comp]:
            outcomes[new_comp] = outcomes[parameters[new_comp]['sum']].sum(axis=1)

    out_df = pa.Table.from_pandas(outcomes, preserve_index = False)
    #pa.parquet.write_table(out_df, f"{outdir}{scenario_outcomes}-{sim_id_str}.outcomes.parquet")
    pa.parquet.write_table(out_df, f"{outdir}{scenario_outcomes}_death_death-{sim_id_str}.hosp.parquet")


    return outcomes

""" Quite fast shift implementation, along the first axis, 
    which is date. num is an integer not negative nor zero """
def shift(arr, num, fill_value=0):
    result = np.empty_like(arr)
    #if num > 0:
    result[:num] = fill_value
    result[num:] = arr[:-num]
    #elif num < 0:
    #    result[num:] = fill_value
    #    result[:num] = arr[-num:]
    #else:
    #    result[:] = arr
    return result

