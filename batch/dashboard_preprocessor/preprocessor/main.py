#!/usr/bin/env python

import os
import sys
import s3fs
import json
import click
import datetime
import pyarrow.parquet as pq

from constants import geoidsCA, geoidsNY, parameters, dates_from_constants
from geoids import all_geoids
from utils import get_configs, aggregate_by_state, write_to_file
from parse import init_obj, get_parquet, parse_sim, d3_transform
from geo import build_geo_obj, stats_for_county_boundaries
from actuals import get_actuals

MEDIAN_CSV_PATH = '/Users/lxu213/Documents/COVIDScenarioPipeline/batch/dashboard_preprocessor/store/median.csv'

@click.command()
@click.option('--bucket', type=str, default='idd-dashboard-runs', 
    help='s3 bucket where model parquets to be processed are located')
@click.option('--folder', type=str, default='USA-20200618T024241', # negotiate this
    help='Top-level model parquet directory folder')
# @click.option('--scenarios', type=str, default=['scenarioA', 'scenarioB', 'scenarioA', 'scenarioB'], 
#     help='Scenarios to process into json for visualizing on dashboard')
def main(bucket: str, folder: str): # , scenarios: list

    # CONFIGURATION ---------------
    # print('starting preprocessing for dashboard...', datetime.datetime.now()) 
    # configs, \
    # scenarios, \
    # severities, \
    # runs, \
    # geoids, \
    # dates, \
    # sims = get_configs(bucket, folder)

    # TODO: CONFIGS FOR TESTING 
    config = 'USA'
    scenarios = ['pld_inf']
    severities = ['high', 'low']
    runs = ['2020.06.18.02:53:08.', '2020.06.18.02:42:40.']
    geoids = all_geoids
    dates = dates_from_constants
    sims = 5
    parameters = ["incidD","incidH","incidI","incidICU","incidVent"]

    # PARSE SIMS ---------------
    r0_map = {}     # track associated r0 to join later
    final = init_obj(geoids, scenarios, severities, parameters, dates)

    # for config in configs:
    for scenario in scenarios:
        for severity in severities:
            for run in runs:
                for sim in range(1, sims + 1):
                    sim_pq, r0 = get_parquet( \
                        bucket, folder, config, scenario, severity, run, str(sim)) 
                        
                    if sim_pq is not None:
                        start = datetime.datetime.now()
                        current = '/'.join([scenario, severity, str(sim)])
                        
                        r0_map[current] = r0
                        parse_sim(sim_pq, final, geoids, scenario, \
                            severity, parameters, sim, len(dates))
                            
                        print('...', current, '--', datetime.datetime.now() - start)
                print('bytes', sys.getsizeof(final))

    # sys.getsizeof(final['06085']['pld_inf']['high']['incidI']['sims'][1] = 2176
    # 2176 bytes * 3150 geoids * 3 sevs * 8 params * 300 sims = up to 49 GB!

    # AGGREGATE BY STATE ---------------
    print('aggregate by state')
    start = datetime.datetime.now()
    # add state-level sims to final, pass in init obj that just contains states
    states = list(set([geoid[0:2] for geoid in geoids]))
    state_dict = init_obj(states, scenarios, severities, parameters, dates)
    aggregate_by_state(final, state_dict, states)
    print('end---', datetime.datetime.now() - start)
    print('bytes', sys.getsizeof(final))

    # STATS FOR MAP ---------------
    # build stats for GeoMap Boundaries 
    # 3:44 min for all geoids on 3 parameters
    print('stats for county boundaries start')
    start = datetime.datetime.now()

    init_geo_obj = build_geo_obj(final)
    print('built init_geo_obj')
    parameters = ['incidI','incidH','incidD']
    geo_obj = stats_for_county_boundaries(
        init_geo_obj, MEDIAN_CSV_PATH, geoids, scenarios, parameters)
    # with open('statsForMap.json', 'w') as f:
    #     json.dump(geo_obj, f)
    print('end ---', datetime.datetime.now() - start)
    print('bytes', sys.getsizeof(final))

    # TRANSFORM ---------------
    print('transform')
    start = datetime.datetime.now() 
    # transform each sim dict obj into D3-friendly format 
    d3_transform(final, r0_map)
    print('end---', datetime.datetime.now() - start)
    print('bytes', sys.getsizeof(final))

    # CALC ACTUALS DATA ---------------
    # get_actuals()

    # JSON DUMP ---------------  
    print('write')
    start = datetime.datetime.now()
    write_to_file(final, geoidsCA)
    print('end---', datetime.datetime.now() - start)

    print('dashboard pre-processing complete!', datetime.datetime.now())


if __name__ == "__main__":
    main()

# TODO: test get_parquet or get_configs?? may require mocking s3 call
# TODO: get_configs.. num_of_sims = int(len(sevs) / len(list(set(sevs))))
# TODO: test_parse_sims and test_d3_transform failing due to inability to 
# coax vscode into state where it will leg me run debug tests
# TODO: drop columns you don't need 
# TODO: add test or assumption stating what data format is required
# TODO: can delete parse_sim_out.json if still unused
