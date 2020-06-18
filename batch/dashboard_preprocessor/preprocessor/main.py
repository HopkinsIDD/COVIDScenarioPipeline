import os
import logging
import datetime
from validate import validate_files
from constants import parameters, severities, geoidsCA, geoidsNY
from utils import get_dates, init_final_obj, aggregate_by_state, join_r0, write_to_file
from parse import parse_dirs, d3_transform
from quantiles import calc_quantiles, transform_quantiles
from geo import stats_for_county_boundaries

def main(dir: str, geoids = []):
    # geoids default to all geoids unless designated
    # TODO: once file structure is set, update which functions need path, update dir

    scenarios = [sim for sim in os.listdir(dir) if sim != '.DS_Store']

    # validate input files
    validate_files(dir)

    # grab dates for populating init_final_obj function
    dates = get_dates(dir, scenarios)

    # parse all sim files in scenario directories
    final = parse_dirs(dir, geoids, scenarios, dates)
 
    # add state-level sims, init obj that just contains states to pass in
    states = list(set([geoid[0:2] for geoid in geoids]))
    state_dict = init_final_obj(states, scenarios, severities, parameters, dates)
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
    dir = 'store/'
    geoids = geoidsCA + geoidsNY

    main(dir, geoids)

