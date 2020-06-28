import os
import copy
import boto3
import json
import datetime
import pandas as pd
import pyarrow.parquet as pq

from constants import severities, parameters  
from parse import get_parquet
# pytest
# from preprocessor.constants import severities, parameters  
# from preprocessor.parse import get_parquet

def get_configs(bucket: str, folder: str) -> list:
    # return list of configurations, including config_names, scenarios, sevs, 
    # run_ids, dates, and number of scenes that appear in base folder dir

    s3 = boto3.resource('s3')
    s3_bucket = s3.Bucket(bucket)

    # get configs from s3 bucket key filenames
    configs, scenarios, sevs, runs = [], [], [], []
    prefix = folder + '/model_output/hosp/USA'
    for file_obj in s3_bucket.objects.filter(Prefix=prefix):
        key_string = file_obj.key.split('/')

        configs.append(key_string[3])       # idx of "config_name" 
        scenarios.append(key_string[4])     # idx of "npi_scenario" 
        sevs.append(key_string[5])          # idx of "scenario_severity" 
        runs.append(key_string[6])          # idx of "run_id" 

    # validation TODO: uncomment
    # validate(key_string, configs, scenarios, sevs, runs)

    # TODO: probably a better way to do this, decide with more test data how
    num_of_sims = int(len(sevs) / len(list(set(sevs))))

    # get configs from first ParquetDataset
    first_file, r0 = get_parquet(
        bucket, folder, configs[0], scenarios[0], sevs[0], runs[0], '1')

    # get dates
    timestamps = []
    seen = set() # optimized to loop over minimum number of timestamps
    for ts in first_file.read(columns=['time'])[0]:
        if ts in seen:
            break
        timestamps.append(ts)
        seen.add(ts)
    dates = [ts.as_py().strftime('%Y-%m-%d') for ts in timestamps]

    # get geoids and row index to geoid mapping for parsing function
    geoids = [] #, geoid_map = [], {}
    geoids_chunk = first_file.read(columns=['geoid'])[0]
    # i = 0
    for idx in range(0, len(geoids_chunk), len(dates)): 
        geoids.append(geoids_chunk[idx].as_py())    # build geoids list
        # geoid_map[i] = geoids_chunk[idx]            # build idx-to-geoid mapping
        # i+=1
        
    print('Configurations returned')
    return list(set(configs)), list(set(scenarios)), list(set(sevs)), \
           list(set(runs)), geoids, dates, num_of_sims

def validate(key_string: str, configs: list, scenarios: list, sevs: list, runs: list):
    # raise errors if input files are faulty
    path = 'config_name/npi_scenario/severity/run_id/global/final/index.run_id.file_type.parquet'

    if len(key_string) != 10 or \
        key_string[1] != 'model_output' or \
        key_string[8] != 'final':
        print('Filename key must follow the format:', path)

    if key_string[9].split('.')[-1] != 'parquet':
        print('Simulation files must be .parquet')

    if not configs or not scenarios or not sevs or not runs:
        print('Error in file structure. Missing configs, scenarios, sevs, or runs.')

    print('File structure validation complete')

def aggregate_by_state(final: dict, state_dict: dict, states: list):
    # final: dict with county-level geoids
    # state_dict: dict with only state-level geoids to be added to final dict
    geoids = list(final.keys())

    for state in states:
        state_geoids = [g for g in geoids if g[0:2] == state]

        for geoid in state_geoids:
            scenarios = list(final[geoid].keys())

            for scen in scenarios:
                severities = list(final[geoid][scen].keys())
                severities.remove('dates')

                for sev in severities:
                    parameters = list(final[geoid][scen][sev].keys())

                    for param in parameters:
                        # sims is dict obj of all sim num and values to agg
                        sims_to_agg = final[geoid][scen][sev][param]['sims']
                        dates = final[geoid][scen]['dates']
                        
                        if len(state_dict[state][scen][sev][param]['sims']) == 0:
                            state_dict[state][scen][sev][param]['sims'] = copy.deepcopy(sims_to_agg)
                        else:
                            for sim in sims_to_agg:
                                for i, date in enumerate(dates):
                                    val = sims_to_agg[sim][i]
                                    state_dict[state][scen][sev][param]['sims'][sim][i] += val
    
        final[state] = state_dict[state]

    return

def write_to_file(final: dict, geoids_to_save: list):
    # save each geoid as an individual json
    # TODO: where is this getting written to?
    path = 'batch/dashboard_preprocessor/results/geo'

    for geoid in geoids_to_save:
        with open(path + geoid + '.json', 'w') as f:
            json.dump(final[geoid], f)

