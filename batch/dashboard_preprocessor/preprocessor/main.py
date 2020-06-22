import os
import s3fs
import logging
import datetime
import pyarrow.parquet as pq

from constants import parameters, geoidsCA, geoidsNY   # severities
from utils import get_configs, aggregate_by_state, join_r0, write_to_file
from parse import init_obj, get_parquet, parse_sim, d3_transform
from quantiles import calc_quantiles, transform_quantiles
from geo import stats_for_county_boundaries


def main(bucket: str, base_path: str, geoids = []):
    # bucket: s3 bucket name, e.g., 'idd-dashboard-runs'
    # base_path: top-level model file directory, e.g., 'USA-20200618T024241'
    # geoids [optional]: list of geoids to save, default is all geoids

    configs, \
    scenarios, \
    severities, \
    runs, \
    dates, \
    sims = get_configs(bucket, base_path)

    # ---------------
    logging.info('start:', datetime.datetime.now())
    final = init_obj(geoids, scenarios, severities, parameters, dates)

    for config in configs:
        for scenario in scenarios:
            for severity in severities:
                for run in runs:
                    for sim in range(1, sims + 1):
                        df = get_parquet(
                            bucket, \
                            base_path, \
                            config, \
                            scenario, \
                            severity, \
                            run, \
                            str(sim)) 

                        parse_sim(df, final, geoids, parameters, sim)

    # parse all sim files in scenario directories
    # final = parse_dirs(path, geoids, scenarios, dates)
 
    # add state-level sims, init obj that just contains states to pass in
    states = list(set([geoid[0:2] for geoid in geoids]))
    state_dict = init_obj(states, scenarios, severities, parameters, dates)
    aggregate_by_state(final, state_dict, states)

    # join r0 values from model parameters
    join_r0(final)

    # transform each sim dict obj into D3-friendly format
    d3_transform(final)

    # calculate quantiles based on all sims
    calc_quantiles(final)

    # build stats for GeoMap Boundaries before quantiles are transformed
    # TODO: write to file
    geo_obj = stats_for_county_boundaries(final)

    # transform quantiles
    transform_quantiles(final)

    # write to individual files
    geoids_to_save = [
        '06', '06037', '06075', '06085', '06019', 
        '36', '36005', '36061', '36081'
    ]
    write_to_file(final, geoids_to_save)

    logging.info('end:', datetime.datetime.now())
    logging.info('pre-processing for frontend dashboard complete!')


if __name__ == "__main__":
    # path location to s3 bucket or to local file directory
    # file_location = '/Users/lxu213/Documents/COVIDScenarioPipeline/batch/dashboard_preprocessor/inference_runs'

    geoids = geoidsCA + geoidsNY
    bucket = 'idd-dashboard-runs'
    base_path = 'USA-20200618T024241'
    # runs_to_process = ['2020.06.18.02:53:08.', '2020.06.18.02:42:40.']
    
    # list all objects with certain directory 'USA-20200618T024241/' and runs_to_process...?

    # get_configs(bucket, base_path)
    main(bucket, base_path, geoids)
    # get_parquet('s3://idd-dashboard-runs/', 'USA-20200618T024241', 'USA', 'pld_inf', 'high', '2020.06.18.02:53:08.', '245')

