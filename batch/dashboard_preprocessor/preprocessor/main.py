#!/usr/bin/env python

import os
import s3fs
import click
import datetime
import pyarrow.parquet as pq

from constants import geoidsCA, geoidsNY, parameters
from utils import get_configs, aggregate_by_state, write_to_file
from parse import init_obj, get_parquet, parse_sim, d3_transform
from quantiles import calc_quantiles, transform_quantiles
from geo import stats_for_county_boundaries
from actuals import get_actuals

@click.command()
@click.option('--bucket', type=str, default='idd-dashboard-runs', 
    help='s3 bucket where model parquets to be processed are located')
@click.option('--folder', type=str, default='USA-20200618T024241', # negotiate this
    help='Top-level model parquet directory folder')
# @click.option('--scenarios', type=str, default=['scenarioA', 'scenarioB', 'scenarioA', 'scenarioB'], 
#     help='Scenarios to process into json for visualizing on dashboard')
def main(bucket: str, folder: str): # , scenarios: list

    # CONFIGURATION ---------------
    print('starting preprocessing for dashboard...', datetime.datetime.now()) 
    configs, \
    scenarios, \
    severities, \
    runs, \
    geoids, \
    dates, \
    sims = get_configs(bucket, folder)

    # sims = 4 # TODO: FOR TESTING 
    geoids = geoidsCA

    # PARSE SIMS ---------------
    r0_map = {}     # track associated r0 to join later
    final = init_obj(geoids, scenarios, severities, parameters, dates)

    for config in configs:
        for scenario in scenarios:
            for severity in severities:
                for run in runs:
                    for sim in range(1, sims + 1):
                        sim_pq, r0 = get_parquet(bucket, folder, config, \
                                                 scenario, severity, run, str(sim)) 
                        if sim_pq is not None:
                            current = '/'.join([scenario, severity, str(sim)])
                            start = datetime.datetime.now()
                            print('...', current)
                            
                            r0_map[current] = r0
                            parse_sim(sim_pq, final, geoids, scenario, \
                                severity, parameters, sim, len(dates))
                            print('end', datetime.datetime.now() - start)
                            print('--------------')

    # print('aggregate by state')
    # start = datetime.datetime.now()
    # # AGGREGATE BY STATE ---------------
    # # add state-level sims to final, pass in init obj that just contains states
    # states = list(set([geoid[0:2] for geoid in geoids]))
    # state_dict = init_obj(states, scenarios, severities, parameters, dates)
    # aggregate_by_state(final, state_dict, states)
    # print('end---', datetime.datetime.now() - start)

    print('transform')
    start = datetime.datetime.now()
    # TRANSFORM ---------------
    # transform each sim dict obj into D3-friendly format 
    d3_transform(final, r0_map)
    print('end---', datetime.datetime.now() - start)

    # CALC ACTUALS DATA ---------------
    # get_actuals()

    # JSON DUMP ---------------  # only save a few for testing...
    # geoids_to_save = [
    #     '06', '06037', '06075', '06085', '06019', 
    #     '36', '36005', '36061', '36081'
    # ]
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

    # print('calc quantiles')
    # start = datetime.datetime.now()
    # # CALC QUANTILES ---------------
    # calc_quantiles(final)
    # print('end---', datetime.datetime.now() - start)

    # print('stats for map')
    # start = datetime.datetime.now()
    # # STATS FOR MAP ---------------
    # # build stats for GeoMap Boundaries before quantiles are transformed
    # geo_obj = stats_for_county_boundaries(final)
    # # with open('results/statsForMap.json', 'w') as f:
    # #     json.dump(geo_obj, f)
    # print('end---', datetime.datetime.now() - start)

    # print('transform quantiles')
    # start = datetime.datetime.now()
    # # TRANSFORM QUANTILES ---------------
    # transform_quantiles(final)
    # print('end---', datetime.datetime.now() - start)
