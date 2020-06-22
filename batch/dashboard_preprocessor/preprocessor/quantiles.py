import logging
import datetime
import pandas as pd

def calc_quantiles(final: dict):
    # calculate and add the p10, p50, and p90 into final dict
    geoids = list(final.keys())

    for geoid in geoids:
        scenarios = list(final[geoid].keys())

        for scenario in scenarios:
            severities = list(final[geoid][scenario].keys())
            severities.remove('dates')

            for sev in severities:
                parameters = list(final[geoid][scenario][sev].keys())

                for param in parameters:
                    conf_obj = {'p10': [], 'p50': [], 'p90': []}
                    dates = final[geoid][scenario]['dates']

                    for d, date in enumerate(dates):
                        sim_obj = final[geoid][scenario][sev][param]['sims']
                        list_by_day = []

                        for s, sim in enumerate(sim_obj):
                            list_by_day.append(sim_obj[s]['vals'][d])

                        intervals = [0.1, 0.5, 0.9]
                        quantiles = pd.Series(list_by_day).quantile(intervals)

                        for i in intervals:
                            conf_obj['p' + str(int(i * 100))].append(quantiles[i])

                    final[geoid][scenario][sev][param]['conf'] = conf_obj

    logging.info('Quantiles added' )

def transform_quantiles(final: dict):
    # transform confidence bounds of final dict to D3-friendly format
    geoids = list(final.keys())

    for geoid in geoids:
        scenarios = list(final[geoid].keys())

        for scenario in scenarios:
            severities = list(final[geoid][scenario].keys())
            severities.remove('dates')

            for sev in severities:
                parameters = list(final[geoid][scenario][sev].keys())

                for param in parameters:
                    conf_list = []
                    dates = final[geoid][scenario]['dates']
                    intervals = list(final[geoid][scenario][sev][param]['conf'].keys())

                    for d, date in enumerate(dates):
                        conf_obj = {}

                        for interval in intervals:
                            conf_obj[interval] = final[geoid][scenario][sev][param]['conf'][interval][d]

                        conf_list.append(conf_obj)

                    final[geoid][scenario][sev][param]['conf'] = conf_list
                        