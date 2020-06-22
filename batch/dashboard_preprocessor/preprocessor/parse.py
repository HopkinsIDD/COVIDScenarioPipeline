import os
import s3fs
import logging
import datetime
import pandas as pd
import pyarrow.parquet as pq

# from constants import severities, parameters 
# pytest
from preprocessor.constants import severities, parameters

def init_obj(geoids: list, scenarios: list, severities: list, 
             parameters: list, dates: list) -> dict:
    # build structure of final Dict obj
    final = {}

    for geoid in geoids:
        final[geoid] = {}

        for scenario in scenarios:
            final[geoid][scenario] = {'dates': dates}

            for sev in severities:
                final[geoid][scenario][sev] = {}

                for param in parameters:
                    final[geoid][scenario][sev][param] = {
                        'peak': 0,
                        'sims': {},
                        'conf': {}
                    }
    return final

def get_parquet(bucket: str, base_path: str, config: str, scenario: str, 
    severity: str, run: str, sim: str):
    # returns parquet sim file pd.DataFrame and associated r0 float 
    # file path: {prefix}{index}.{run_id}.{file_type}.parquet
    # prefix: {config_name}/{npi_scenario}/{severity_scenario}/{run_id}/global/final/
    # index: sim number + leading zeros
    # key: USA-20200618T024241/model_output/hosp/USA/pld_inf/high/2020.06.18.02:53:08./
    #      global/final/000000245.2020.06.18.02:53:08..hosp.parquet

    # build parquet filename path key
    head = base_path + '/model_output/hosp/'
    prefix = '/'.join([config, scenario, severity, run, 'global/final/'])
    index = '0' * (9 - len(sim)) + sim
    key = head + prefix + index + '.' + run + '.hosp.parquet'

    # path of sim file and spar file where associated r0 lives
    s3 = s3fs.S3FileSystem()
    path = 's3://' + bucket + '/' + key
    r0_path = path.replace('hosp', 'spar')

    try:
        df = pq.ParquetDataset(path, filesystem=s3).read_pandas().to_pandas()
        spar_df = pq.ParquetDataset(r0_path, filesystem=s3).read_pandas().to_pandas()
        r0 = round(float(spar_df[spar_df['parameter'] == 'R0']['value']), 2)
        return (df, r0)

    except OSError:
        non_path = '/'.join([config, scenario, severity, run, str(sim)])
        logging.warning('Obj does not exist in bucket %s', non_path)
        return (None, None)

def parse_sim(df, final: dict, geoids: list, scenario: str, 
              severity: str, parameters: list, sim: str):
    # reads sim pd.DataFrame and populates data into final Dict Obj 

    # include all geoids if user did not designate specific geoids
    geoids = df.geoid.unique().tolist() if geoids == [] else geoids
    cols = ['time', 'geoid'] + parameters

    for geoid in geoids:
        for param in parameters:
            vals = [int(val) for val in df[df.geoid == geoid][param].tolist()]
            final[geoid][scenario][severity][param]['sims'][sim] = vals

def d3_transform(final: dict, r0_map: dict):
    # transforms each nested simulation dict object into d3-friendly format

    geoids = list(final.keys())
    for geoid in geoids:
        scenarios = list(final[geoid].keys())

        for scenario in scenarios:
            severities = list(final[geoid][scenario].keys())
            severities.remove('dates')

            for sev in severities:
                parameters = list(final[geoid][scenario][sev].keys())

                for param in parameters:
                    obj_to_transform = final[geoid][scenario][sev][param]['sims']
                    sims = list(obj_to_transform.keys())
                    d3_all_sims = []

                    for sim in sims:
                        d3_sim = {}
                        d3_sim['name'] = int(sim)
                        d3_sim['vals'] = obj_to_transform[sim]
                        d3_sim['over'] = False
                        d3_sim['max'] = max(d3_sim['vals'])
                        d3_sim['r0'] = return_r0(r0_map, scenario, sev, sim)
                        d3_all_sims.append(d3_sim)

                    # sort dict objs by ascending sim num
                    d3_all_sims = sorted(d3_all_sims, key=lambda k: k['name'])
                    final[geoid][scenario][sev][param]['sims'] = d3_all_sims

def return_r0(r0_map: dict, scenario: str, severity: str, sim: str) -> float:
    # returns r0 float from r0_map dict based on provided arguments

    key = '/'.join([scenario, severity, str(sim)])
    return r0_map[key]