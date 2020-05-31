import itertools
import time
import warnings

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from utils import config
import pyarrow.parquet as pq
import pyarrow as pa
import pandas as pd


def run_delayframe_outcomes(config, setup_name, outdir, scenario_seir, scenario_outcomes, nsim = 1, index=1, n_jobs=1):
    start = time.monotonic()
    sim_ids = np.arange(index, index + nsim)

    # Prepare the probability table:
    # Either mean of probabilities given or from the file... This speeds up a bit the process.
    # However needs an ordered dict, here we're abusing a bit the spec.
    config_outcomes = config["outcomes"]["settings"][scenario_outcomes]
    if (config["outcomes"]["param_from_file"].get()):
        print('ok file')
        # load a file from the seir model, to know how to filter the provided csv file
        sim_id_str = str(sim_ids[0]).zfill(9)
        diffI = pd.read_parquet(f'model_output/{setup_name}/{sim_id_str}.seir.parquet')
        diffI = diffI[diffI['comp'] == 'diffI']
        dates = diffI.time
        diffI.drop(['comp'], inplace = True, axis = 1)
        print('ok input')

        # Load the actual csv file
        print()
        branching_file = config["outcomes"]["param_place_file"].as_str()
        branching_data = pd.read_csv(branching_file, converters={"place": str})
        branching_data = branching_data[branching_data['place'].isin(diffI.drop('time', axis=1).columns)]
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
    all_data['incidence'] = diffI.drop(['time'], axis=1).to_numpy().astype(np.int32)
    shape = all_data['incidence'].shape

    outcomes = pd.melt(diffI, id_vars='time', value_name = 'incidence', var_name='place')
    for new_comp in parameters:
        if 'source' in parameters[new_comp]:
            # Read the config for this compartement
            source =      parameters[new_comp]['source']
            probability = parameters[new_comp]['probability']
            delay =       parameters[new_comp]['delay']
    
            # Create new compartement
            all_data[new_comp] = np.empty_like(all_data['incidence'])
            # Draw with from source compartement
            all_data[new_comp] = np.random.binomial(all_data[source], probability * np.ones_like(all_data[source]))  
                                       # Check dimension for from file

            import matplotlib.pyplot as plt
            plt.imshow(probability * np.ones_like(all_data[source]))
            plt.title(np.mean(probability))
            plt.savefig('P'+new_comp + '|' + source)
            
            # Shift to account for the delay
            all_data[new_comp] = shift(all_data[new_comp], delay, fill_value=0)
            
            # Produce a dataframe an merge it
            df = pd.DataFrame(all_data[new_comp], columns=places, index=dates)
            df.reset_index(inplace=True)
            df = pd.melt(df, id_vars='time', value_name = new_comp, var_name='place')
            outcomes = pd.merge(outcomes, df)
            
            # Make duration
            if 'duration' in parameters[new_comp]:
                duration = parameters[new_comp]['duration']
                all_data[new_comp+'_curr'] = np.cumsum(all_data[new_comp], axis = 0) - shift(np.cumsum(all_data[new_comp], axis=0), duration)
                
                df = pd.DataFrame(all_data[new_comp+'_curr'], columns=places, index=dates)
                df.reset_index(inplace=True)
                df = pd.melt(df, id_vars='time', value_name = new_comp+'_curr', var_name='place')
                outcomes = pd.merge(outcomes, df)

            elif 'sum' in parameters[new_comp]:
                outcomes[new_comp] = outcomes[parameters[new_comp]['sum']].sum(axis=1)

    out_df = pa.Table.from_pandas(outcomes, preserve_index = False)
    pa.parquet.write_table(out_df, f"{outdir}{scenario_outcomes}-{sim_id_str}.outcomes.parquet")

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

