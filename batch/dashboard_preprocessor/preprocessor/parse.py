import os
import s3fs
import datetime
import pandas as pd
import pyarrow.parquet as pq

from constants import severities, parameters 
# pytest
# from preprocessor.constants import severities, parameters

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

def get_parquet(bucket: str, folder: str, config: str, scenario: str, 
    severity: str, run: str, sim: str):
    # returns sim pyarrow.parquet.ParquetDataset and associated r0 float 
    # file path: {prefix}{index}.{run_id}.{file_type}.parquet
    # prefix: {config_name}/{npi_scenario}/{severity_scenario}/{run_id}/global/final/
    # index: sim number + leading zeros
    # key: USA-20200618T024241/model_output/hosp/USA/pld_inf/high/2020.06.18.02:53:08./
    #      global/final/000000245.2020.06.18.02:53:08..hosp.parquet

    # build parquet filename path key
    head = folder + '/model_output/hosp/'
    prefix = '/'.join([config, scenario, severity, run, 'global/final/'])
    index = '0' * (9 - len(sim)) + sim
    key = head + prefix + index + '.' + run + '.hosp.parquet'

    # path of sim file and spar file where associated r0 lives
    s3 = s3fs.S3FileSystem()
    path = 's3://' + bucket + '/' + key
    r0_path = path.replace('hosp', 'spar')

    try:
        pq_dataset = pq.ParquetDataset(path, filesystem=s3) #.read_pandas().to_pandas()
        r0 = pq.ParquetDataset(r0_path, filesystem=s3).read(columns=['value'])[0][1].as_py() #.read_pandas().to_pandas()
        return (pq_dataset, round(r0, 2))

    except OSError:
        non_path = '/'.join([config, scenario, severity, run, str(sim)])
        print('Object does not exist in bucket:', non_path)
        return (None, None)

def parse_sim(pq_dataset, final: dict, geoids: list, scenario: str, 
              severity: str, parameters: list, sim: str, date_len: int):
    # parameter dataset is pyarrow.parquet.ParquetDataset 
    # function populates data into final Dict Obj 

    # TODO: geoid_map can be a tuple for further optimization ('3505','6023','2022')
    arrow_table = pq_dataset.read(columns=parameters)
    # geoid_idx = 0
    # geoid = geoid_map[geoid_idx]

    for p_idx, parameter in enumerate(parameters):
        param_chunk = arrow_table[p_idx]
        
        for g_idx, geoid in enumerate(geoids):
            final[geoid][scenario][severity][parameter]['sims'][sim] = \
                param_chunk[g_idx * date_len: (g_idx + 1) * date_len].to_pylist()
                # to_pylist() adds 3 seconds to each sim file

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


            # take chunk associated with it and sim_obj = chunk

        # for idx, val in enumerate(arrow_table[p_idx]):
        #     sim_obj = final[geoid][scenario][severity][parameter]['sims']
        #     # if sim not in sim_obj: # TODO: default dict?
        #     # instantiate [] * date_len
        #     # idx into and apply  list.idx[0] = val
        #     #     sim_obj[sim] = [val]
        #     # else:
        #     #     sim_obj[sim].append(val)

        #     if idx >= geoid_idx * date_len: # TODO >= or >
        #         geoid_idx += 1
        #         geoid = geoid_map[geoid_idx]

    # for row in df.itertuples():
    #     for parameter in parameters:
    #         val = getattr(row, parameter)
    #         sim_obj = final[row.geoid][scenario][severity][parameter]['sims']
    #         if sim not in sim_obj: # default dict?
    #             sim_obj[sim] = [val]
    #         else:
    #             sim_obj[sim].append(val)

    # for geoid in geoids:
    #     for param in parameters:
    #         vals = [int(val) for val in df[df.geoid == geoid][param].tolist()]
    #         final[geoid][scenario][severity][param]['sims'][sim] = vals
