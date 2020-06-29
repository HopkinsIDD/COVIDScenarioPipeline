import pandas as pd
from constants import severities, parameters 
# from preprocessor.constants import severities, parameters 

def build_geo_obj(final: dict) -> dict:
    # build structure of Dict Obj for stats to populate county boundaries
    # differs from init_obj as it includes state and geoid key but not severity

    if not final.keys():
        print('No geoids in data, cannot build stats for county boundaries')
    geoids = list(final.keys())
    scenarios = list(final[geoids[0]].keys())

    if not scenarios:
        print('No scenarios in data, cannot build stats for county boundaries')
    parameters = list(final[geoids[0]][scenarios[0]]['high'].keys()) # Map shows high only

    obj = {}
    states = list(set([geoid[0:2] for geoid in geoids]))
    for state in states:
        obj[state] = {}

        for geoid in geoids:
            if geoid[0:2] == state and len(geoid) == 5:
                obj[state][geoid] = {}

                for scenario in scenarios:
                    obj[state][geoid][scenario] = {}

                    for param in parameters:
                        obj[state][geoid][scenario][param] = []
    return obj

def stats_for_county_boundaries(geo_obj: dict, median_csv_path: str, geoids: list, 
    scenarios: list, parameters: list) -> dict:
    # builds dict of stats to join into the map-view GeoJSON county boundaries 
                
    # TODO: discuss with research team where csv will live
    median_df = pd.read_csv(median_csv_path, dtype={'geoid': str})

    count = 0
    for geoid in geoids:
        # for tracking time elapsed
        if count % 100 == 0: 
            print(count)
        median_by_geoid = median_df[median_df.geoid == geoid]

        for scenario in scenarios:

            for param in parameters:
                # TODO: designate scenario of median_df, may be in filename
                median_list = median_by_geoid[param].to_list()
                geo_obj[geoid[0:2]][geoid][scenario][param] = median_list
        count+=1

    return geo_obj
