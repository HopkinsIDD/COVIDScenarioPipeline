import os
import copy
import json
import logging
import datetime
import pandas as pd
import pyarrow.parquet as pq

# from constants import severities, parameters  
# pytest
from preprocessor.constants import severities, parameters 

def init_final_obj(geoids: list, scenarios: list, severities: list, parameters: list, dates: list) -> dict:
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

def get_dates(dir: str, scenarios: list) -> list:
    # returns list of dates from first file in scenario dir

    files =  [f for f in os.listdir(dir + scenarios[0] + '/') if f != '.DS_Store']
    file_path = dir + scenarios[0] + '/' + files[0]

    df = pq.read_table(file_path, columns=['time']).to_pandas()
    dates = sorted(df.time.unique().tolist())
    
    return [date.strftime('%Y-%m-%d') for date in dates]

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

def join_r0(final: dict):

    # TODO
    return
    
def write_to_file(final: dict, geoids_to_save: list):
    # save each geoid as an individual json
    # TODO: where is this getting written to?
    
    for geoid in geoids_to_save:
        with open('geo' + geoid + '.json', 'w') as f:
            json.dump(final[geoid], f)

