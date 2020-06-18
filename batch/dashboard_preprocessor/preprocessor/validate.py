import os
import logging
import pandas as pd
import pyarrow.parquet as pq

def validate_headers(path: str):
    # raise error if headers of files don't include all parameters required 
    headers = pd.read_parquet(path).columns

    required_params = [
        'geoid', 'time', 'hosp_curr', 'icu_curr', 'vent_curr',
        'incidD', 'incidH', 'incidI', 'incidICU', 'incidVent'
    ]

    for param in required_params:
        if param not in headers:
            logging.error('Required parameter ' + param + ' does not exist in file header')

    return
    
def validate_files(dir: str):
    # raise errors if input files are faulty

    scenarios =  [sim for sim in os.listdir(dir) if sim != '.DS_Store']

    if scenarios == []:
        logging.error('No scenario directories in ', dir)

    for scenario in scenarios:
        scenario_dir = dir + scenario + '/'
        files = [f for f in os.listdir(scenario_dir) if f != '.DS_Store']

        if files == []:
            logging.error('No simulation files in ' + scenario + ' directory')

        file_path = scenario_dir + files[0]
        name, ext = os.path.splitext(file_path)

        # TODO: validate that file_name is expected structure
        # name.split('_') should return x, y, z
        if ext != '.parquet':
            logging.error('Simulation files must be .parquet')

        validate_headers(file_path)

    return

