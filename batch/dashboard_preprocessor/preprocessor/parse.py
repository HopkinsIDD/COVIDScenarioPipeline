import os
import s3fs
import logging
import datetime
import pandas as pd
import pyarrow.parquet as pq

from constants import severities, parameters 

 #pytest
# from preprocessor.constants import severities, parameters
def init_obj(geoids: list, scenarios: list, severities: list, parameters: list, dates: list) -> dict:
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
    severity: str, run_id: str, sim: str):
    # file path: {prefix}{index}.{run_id}.{file_type}.parquet
    # prefix: {config_name}/{npi_scenario}/{severity_scenario}/{run_id}/global/final/
    # index: sim number + leading zeros
    # key: USA-20200618T024241/model_output/hosp/USA/pld_inf/high/2020.06.18.02:53:08./
    #      global/final/000000245.2020.06.18.02:53:08..hosp.parquet

    # build parquet filename path key
    head = base_path + '/model_output/hosp/'
    prefix = '/'.join([config, scenario, severity, run_id, 'global/final/'])
    index = '0' * (9 - len(sim)) + sim
    key = head + prefix + index + '.' + run_id + '.hosp.parquet'

    # read into pandas df
    s3 = s3fs.S3FileSystem()
    path = 's3://' + bucket + '/' + key
    df = pq.ParquetDataset(path, filesystem=s3).read_pandas().to_pandas()

    return df

def parse_sim(df, final: dict, geoids: list, scenario: list,
              severity: list, parameters: list, sim: str):
    # reads file at path and populates final Dict Obj via mutation
    # returns None

    cols = ['time', 'geoid'] + parameters
    # df = pq.read_table(path, columns=cols).to_pandas()

    # include all geoids if user did not designate specific geoids
    geoids = df.geoid.unique().tolist() if geoids == [] else geoids
    for geoid in geoids:
        for param in parameters:
            vals = [int(val) for val in df[df.geoid == geoid][param].tolist()]
            final[geoid][scenario][severity][param]['sims'][sim] = vals

    return

def parse_dirs(dir: str, geoids: list, scenarios: list, dates: list) -> dict:
    # May be able to eliminate this step with final model structure

    logging.info('start:', datetime.datetime.now())
    final = init_obj(geoids, scenarios, severities, parameters, dates)

    for scenario in scenarios:
        logging.info('-----> parsing scenario...', scenario)

        scenario_dir = dir + scenario + '/'
        files =  [f for f in os.listdir(scenario_dir) if f != '.DS_Store']

        # parse by simulation file
        for sim_file in files:
            # TODO: clean up with final structure
            # scenario, severity, sim = file.split('_')
            config, scenario, severity, simstr = sim_file.split('_')
            # severity = sim_file.split('_')[1].split('-')[0]
            sim = simstr.split('.')[0].lstrip('0')
            file_path = scenario_dir + sim_file
            logging.info(sim_file)

            parse_sim(file_path, final, geoids, scenario, severity, parameters, sim)

    return final

# dates = ['2020-05-05', '2020-05-06', '2020-05-07', '2020-05-08', '2020-05-09']
# final = parse_dirs('store/', ['06085', '06019'], ['Inference', 'Lockdown'], dates)

def d3_transform(final: dict):
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
                        d3_sim['r0'] = 0  # TODO: add separate function to populate r0
                        d3_all_sims.append(d3_sim)

                    # sort dict objs by ascending sim num
                    d3_all_sims = sorted(d3_all_sims, key=lambda k: k['name'])
                    final[geoid][scenario][sev][param]['sims'] = d3_all_sims
