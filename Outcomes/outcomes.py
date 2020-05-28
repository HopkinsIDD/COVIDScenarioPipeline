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

    parameter_table = None
    if (config["outcomes"]["param_from_file"].get()):
        # stuff to put it in parameter table
        raise('Loading not supported')

    if n_jobs == 1:          # run single process for debugging/profiling purposes
        for sim_id in tqdm.tqdm(sim_ids):
            onerun_delayframe_outcomes(sim_id, config, setup_name, outdir, scenario_seir, scenario_outcomes, parameter_table)
    else:
        tqdm.contrib.concurrent.process_map(onerun_delayframe_outcomes, sim_ids, itertools.repeat(config), 
                                                                    itertools.repeat(setup_name), 
                                                                    itertools.repeat(outdir), 
                                                                    itertools.repeat(scenario_seir), 
                                                                    itertools.repeat(scenario_outcomes),
                                                                    itertools.repeat(parameter_table),
                                            max_workers=n_jobs)

    print(f"""
>> {nsim} outcomes simulations completed in {time.monotonic()-start:.1f} seconds
""")

def onerun_delayframe_outcomes(sim_id, config, setup_name, outdir, scenario_seir, scenario_outcomes, parameter_table):
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
    config_outcomes = config["outcomes"]["settings"][scenario_outcomes]
    for new_comp in config_outcomes:
        if config_outcomes[new_comp]['source'].exists():
            # Read the config for this compartement
            source = config_outcomes[new_comp]['source'].as_str()
            probability = config_outcomes[new_comp]['probability']['value'].as_random_distribution()
            delay = config_outcomes[new_comp]['delay']['value'].as_random_distribution()

            # Overwrite parameters with place specific one:
            if (parameter_table):
                if f'D{new_comp}' in parameter_table.keys:
                    print('changing delay')
                if f'L{new_comp}|{source}' in parameter_table.keys:
                    print('Changing probablities')
    
            # Create new compartement
            all_data[new_comp] = np.empty_like(all_data['incidence'])
            # Draw with from source compartement
            all_data[new_comp] = np.random.binomial(all_data[source], 
                                                    probability(size = shape))
            
            # Shift to account for the delay
            all_data[new_comp] = shift(all_data[new_comp], int(delay(size=1)), fill_value=0)
            
            # Produce a dataframe an merge it
            df = pd.DataFrame(all_data[new_comp], columns=places, index=dates)
            df.reset_index(inplace=True)
            df = pd.melt(df, id_vars='time', value_name = new_comp, var_name='place')
            outcomes = pd.merge(outcomes, df)
            
            # Make duration
            if config_outcomes[new_comp]['duration'].exists():
                duration = config_outcomes[new_comp]['duration']['value'].as_random_distribution()
                if (parameter_table):
                    if f'L{new_comp}' in parameter_table.keys:
                        print('changing durations')
                all_data[new_comp+'_curr'] = np.cumsum(all_data[new_comp], axis = 0) - shift(np.cumsum(all_data[new_comp], axis=0), int(duration(size=1)))
                
                df = pd.DataFrame(all_data[new_comp+'_curr'], columns=places, index=dates)
                df.reset_index(inplace=True)
                df = pd.melt(df, id_vars='time', value_name = new_comp+'_curr', var_name='place')
                outcomes = pd.merge(outcomes, df)

            elif config_outcomes[new_comp]['sum'].exists():
                outcomes[new_comp] = outcomes[config_outcomes[new_comp]['sum'].as_str_seq()].sum(axis=1)

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

