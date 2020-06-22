import os
import s3fs
import logging
import datetime
import pyarrow.parquet as pq

from constants import geoidsCA, geoidsNY, parameters
from utils import get_configs, aggregate_by_state, write_to_file
from parse import init_obj, get_parquet, parse_sim, d3_transform
from quantiles import calc_quantiles, transform_quantiles
from geo import stats_for_county_boundaries

def main(bucket: str, base_path: str, geoids: list = []):
    # bucket: s3 bucket name, e.g., 'idd-dashboard-runs'
    # base_path: top-level model file directory, e.g., 'USA-20200618T024241'
    # geoids [optional]: list of geoids to save, default is all geoids

    # CONFIGURATION ---------------
    logging.info('starting preprocessing for dashboard...') 
    configs, \
    scenarios, \
    severities, \
    runs, \
    dates, \
    sims = get_configs(bucket, base_path)

    # PARSE SIMS ---------------
    parameters = ['incidI', 'incidD']
    r0_map = {}
    final = init_obj(geoids, scenarios, severities, parameters, dates)

    for config in configs:
        for scenario in scenarios:
            for severity in severities:
                for run in runs:
                    for sim in range(1, sims + 1):
                        sim_df, r0 = get_parquet(bucket, base_path, config, \
                                             scenario, severity, run, str(sim)) 

                        if sim_df is not None:
                            current = '/'.join([scenario, severity, str(sim)])
                            print('...', current)

                            # track associated r0 to join later
                            r0_map[current] = r0
                            parse_sim(sim_df, final, geoids, scenario, \
                                      severity, parameters, sim)

    # AGGREGATE BY STATE ---------------
    # add state-level sims to final, pass in init obj that just contains states
    states = list(set([geoid[0:2] for geoid in geoids]))
    state_dict = init_obj(states, scenarios, severities, parameters, dates)
    aggregate_by_state(final, state_dict, states)

    # TRANSFORM ---------------
    # transform each sim dict obj into D3-friendly format to make room for quantiles
    d3_transform(final, r0_map)

    # CALC QUANTILES ---------------
    calc_quantiles(final)

    # STATS FOR MAP ---------------
    # build stats for GeoMap Boundaries before quantiles are transformed
    geo_obj = stats_for_county_boundaries(final)
    with open('statsForMap.json', 'w') as f:
        json.dump(geo_obj, f)

    # TRANSFORM QUANTILES ---------------
    transform_quantiles(final)

    # JSON DUMP ---------------
    geoids_to_save = [
        '06', '06037', '06075', '06085', '06019', 
        '36', '36005', '36061', '36081'
    ]
    write_to_file(final, geoids_to_save)

    logging.info('pre-processing for frontend dashboard complete!')


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)

    # geoids = geoidsCA + geoidsNY
    geoids = ['06085', '06019', '36005']
    bucket = 'idd-dashboard-runs'
    base_path = 'USA-20200618T024241'

    main(bucket, base_path, geoids)

# FOR TESTING # list out configs to avoid hitting s3 so much
# configs = ['USA']
# scenarios = ['pld_inf']
# severities = ['low', 'high']
# runs = ['2020.06.18.02:42:40.', '2020.06.18.02:53:08.']
# dates = dates_from_constants # constants
# sims = 3 # 300

# TODO: test get_parquet or get_configs?? may require mocking s3 call
# TODO: get_configs.. num_of_sims = int(len(sevs) / len(list(set(sevs))))
# TODO: write file ... where to?
# TODO: test_parse_sims and test_d3_transform failing due to inability to 
# coax vscode into state where it will leg me run debug tests