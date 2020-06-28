import pandas as pd

USAFACTS_DEATH_URL = 'https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv'
USAFACTS_CASE_URL = 'https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv'

def init_actuals(parameters: list) -> dict:
    # initialize actuals object 

    obj = {}
    for parameter in parameters:
        obj[parameter] = {}
        for geoid in geoids:
            obj[parameter][geoid] = []

    return obj

def format(df, tag: str):
    # unpivots usafacts dataframe and formats date

    df = df.drop(columns=['stateFIPS', 'County Name', 'State'])
    df = df.rename(columns={'countyFIPS': 'geoid'})
    df = df.drop([0])

    def format_date(date):
        # formats date from 1/27/20 to 2020-01-27
        date_list = date.split('/')
        for i, item in enumerate(date_list): 
            if len(item) == 1:
                item = '0' + item
            date_list[i] = item
        new_date = '2020-' + date_list[0] + '-' + date_list[1]
        return new_date

    df = df.set_index('geoid').stack().reset_index()
    df = df.rename(columns={'level_1': 'date', 0: tag})
    df.date = df.date.map(lambda x: format_date(x))
    df.geoid = df.geoid.map(lambda x: '0' + x if len(x) == 4 else x)

    return df

def get_actuals() -> dict:
    # get actuals for incidD and incidC

    parameters = ['incidD', 'incidC']
    #TODO: put in loop if there are more than 2 parameters
    actuals = init_actuals(parameters)

    death = pd.read_csv(USAFACTS_DEATH_URL, dtype={'countyFIPS': str})
    confirmed_case = pd.read_csv(USAFACTS_DEATH_URL, dtype={'countyFIPS': str})

    # for parameter in parameters:

    death_df = format(death, 'deaths')
    case_df = format(confirmed_case, 'confirmed_cases')

    for geoid in geoids:
        dates = death_df[death_df['geoid'] == geoid]['date']
        deaths = list(death_df[death_df['geoid'] == geoid]['deaths'])
        cases = list(case_df[death_df['geoid'] == geoid]['confirmed_cases'])

        # calculate daily incident values from cumsum
        deaths_series = deaths.transform(lambda s: s.sub(s.shift().fillna(0)).abs())
        cases_series = cases.transform(lambda s: s.sub(s.shift().fillna(0)).abs())
        incidD = list(deaths_series.apply(lambda x: int(x)))
        incidC = list(cases_series.apply(lambda x: int(x)))

        for i in range(len(dates)):
            actuals[geoid]['incidD'].append({dates[i], incidD[i]})
            actuals[geoid]['incidC'].append({dates[i], incidC[i]})


    return