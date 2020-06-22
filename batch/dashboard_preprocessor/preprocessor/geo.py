import logging
# from constants import severities, parameters 
from preprocessor.constants import severities, parameters 

def init_geo_obj(geoids: list, scenarios: list, parameters: list) -> dict:
    # build structure of Dict Obj for stats to populate county boundaries
    # differs from init_obj as it includes state and geoid key but not severity

    obj = {}
    states = list(set([geoid[0:2] for geoid in geoids]))
    for state in states:
        obj[state] = {}

        for geoid in geoids:
            if geoid[0:2] == state:
                obj[state][geoid] = {}

                for scenario in scenarios:
                    obj[state][geoid][scenario] = {}

                    for param in parameters:
                        obj[state][geoid][scenario][param] = []

    return obj

def stats_for_county_boundaries(final: dict) -> dict:
    # builds dict of stats to join into the map-view GeoJSON county boundaries 

    if not final.keys():
        logging.error('No geoids in data, cannot build stats for county boundaries')

    # initialize geo object to be populated
    geoids = list(final.keys())
    scenarios = list(final[geoids[0]].keys())

    if not scenarios:
        logging.error('No scenarios in data, cannot build stats for county boundaries')
        
    sev = 'high' # only high severity will be shown in map-view
    parameters = list(final[geoids[0]][scenarios[0]][sev].keys())
    geo_obj = init_geo_obj(geoids, scenarios, parameters)

    for geoid in geoids:

        for scenario in scenarios:

            for param in parameters:
                median = final[geoid][scenario][sev][param]['conf']['p50']
                geo_obj[geoid[0:2]][geoid][scenario][param] = median

    return geo_obj

    