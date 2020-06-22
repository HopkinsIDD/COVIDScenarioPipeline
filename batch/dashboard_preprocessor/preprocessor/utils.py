import os
import copy
import boto3
import json
import logging
import datetime
import pandas as pd
import pyarrow.parquet as pq

# from constants import severities, parameters  
# from parse import get_parquet
# pytest
from preprocessor.constants import severities, parameters  
from preprocessor.parse import get_parquet

def get_configs(bucket: str, base_path: str) -> list:
    # return list of configurations, including config_names, scenarios, sevs, 
    # run_ids, dates, and number of scenes that appear in base_path dir

    s3 = boto3.resource('s3')
    s3_bucket = s3.Bucket(bucket)

    configs, scenarios, sevs, runs = [], [], [], []
    prefix = base_path + '/model_output/hosp/USA'

    for file_obj in s3_bucket.objects.filter(Prefix=prefix):
        key_string = file_obj.key.split('/')

        configs.append(key_string[3])       # idx of "config_name" 
        scenarios.append(key_string[4])     # idx of "npi_scenario" 
        sevs.append(key_string[5])          # idx of "scenario_severity" 
        runs.append(key_string[6])          # idx of "run_id" 

    # validation
    validate(key_string, configs, scenarios, sevs, runs)

    # TODO: probably a better way to do this
    num_of_sims = int(len(sevs) / len(list(set(sevs))))

    # get dates from first file
    first_file = get_parquet(
        bucket, base_path, configs[0], scenarios[0], sevs[0], runs[0], '1')
    datetimes = pd.to_datetime(first_file.time.unique())
    dates = [date.strftime('%Y-%m-%d') for date in datetimes]

    logging.info('Configurations returned')

    return list(set(configs)), list(set(scenarios)), list(set(sevs)), \
           list(set(runs)), dates, num_of_sims

def validate(key_string: str, configs: list, scenarios: list, sevs: list, runs: list):
    # raise errors if input files are faulty
    format = 'config_name/npi_scenario/severity/run_id/global/final/index.run_id.file_type.parquet'

    if len(key_string) != 10 or \
        key_string[1] != 'model_output' or \
        key_string[8] != 'final':
        logging.error('Filename key must follow the format:', format)

    if key_string[9].split('.')[-1] != 'parquet':
        logging.error('Simulation files must be .parquet')

    if not configs or not scenarios or not sevs or not runs:
        logging.error('Error in file structure. Missing configs, scenarios, sevs, or runs.')

    logging.info('File structure validation complete')

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
    
    for geoid in geoids_to_save:
        with open('geo' + geoid + '.json', 'w') as f:
            json.dump(final[geoid], f)

