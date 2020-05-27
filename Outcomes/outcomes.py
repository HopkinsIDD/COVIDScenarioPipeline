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


def run_parallel(config, setup_name, outdir, scenario_seir, scenario_outcomes, nsim = 1, index=1, n_jobs=1):
    start = time.monotonic()
    sim_ids = np.arange(index, index + nsim)

    if n_jobs == 1:          # run single process for debugging/profiling purposes
        for sim_id in tqdm.tqdm(sim_ids):
            onerun_Outcomes(sim_id, config, setup_name, outdir, scenario_seir, scenario_outcomes)
    else:
        tqdm.contrib.concurrent.process_map(onerun_Outcomes, sim_ids, itertools.repeat(config), 
                                                                    itertools.repeat(setup_name), 
                                                                    itertools.repeat(outdir), 
                                                                    itertools.repeat(scenario_seir), 
                                                                    itertools.repeat(scenario_outcomes),
                                            max_workers=n_jobs)

    print(f"""
>> {s.nsim} outcomes simulations completed in {time.monotonic()-start:.1f} seconds
""")

def onerun_Outcomes(sim_id, config, setup_name, outdir, scenario_seir, scenario_outcomes):
    diffI = pd.read_parquet('model_output/east-coast_ImmediateCT_noSD/000000001.seir.parquet')
    diffI = diffI[diffI['comp'] == 'diffI']
    dates = diffI.time
    diffI.drop(['comp'], inplace = True, axis = 1)
    places = diffI.drop(['time'], axis=1).columns
    all_data = {}
    # We store them as numpy matrices. Dimensions is dates X places
    all_data['incidence'] = diffI.drop(['time'], axis=1).to_numpy().astype(np.int32)
    shape = all_data['incidence'].shape

    outcomes = pd.melt(diffI, id_vars='time', value_name = 'incidence', var_name='place')
    config_outcomes = config["outcomes"]["settings"][scenario]
    for new_comp in config_outcomes:
        # Read the config for this compartement
        source = config_outcomes[new_comp]['source'].as_str()
        probability = config_outcomes[new_comp]['probability']['value'].as_random_distribution()
        delay = config_outcomes[new_comp]['delay']['value'].as_random_distribution()
        
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
            all_data[new_comp+'_curr'] = np.cumsum(all_data[new_comp], axis = 0) - shift(np.cumsum(all_data[new_comp], axis=0), int(duration(size=1)))
            
            df = pd.DataFrame(all_data[new_comp+'_curr'], columns=places, index=dates)
            df.reset_index(inplace=True)
            df = pd.melt(df, id_vars='time', value_name = new_comp+'_curr', var_name='place')
            outcomes = pd.merge(outcomes, df)

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

