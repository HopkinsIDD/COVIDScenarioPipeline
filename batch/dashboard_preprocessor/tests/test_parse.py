import json
import pandas as pd
from preprocessor.parse import init_obj, parse_sim, d3_transform

class TestParse:

    def test_init_obj(self):
        # returns expected final initialized object

        geoids = ['06085', '06019']
        scenarios = ['ScenarioA']
        severities = ['high']
        parameters = ['incidD',  'incidH',  'incidI']
        dates = ['2020-05-05', '2020-05-06', '2020-05-07', '2020-05-08', '2020-05-09']

        final = init_obj(geoids, scenarios, severities, parameters, dates)

        with open('tests/resources/init_obj.json') as f:
            expected = json.load(f)

        assert final == expected

    # def test_parse_sim(self):
    #     # returns expected populated final dict object after parsing sim file

    #     df = pd.read_csv('tests/fixtures/ScenarioA/Config_ScenarioA_high_1.csv')
    #     scenario = 'ScenarioA'
    #     geoids = ['06085', '06019']
    #     dates = ['2020-05-05', '2020-05-06', '2020-05-07', '2020-05-08', '2020-05-09']
    #     parameters = ['incidD',  'incidH',  'incidI']
    #     severity = 'high'
    #     sim = '1'

    #     with open('tests/resources/parse_sim_in.json') as f:
    #         final = json.load(f)

    #     parse_sim(df, final, geoids, scenario, severity, parameters, sim)

    #     with open('tests/resources/parse_sim_out.json') as f:
    #         expected = json.load(f)
            
    #     assert final == expected

    # def test_d3_transform(self):
    #     # returns expected transformed d3 format

    #     with open('tests/resources/transform_in.json') as f:
    #         final = json.load(f)
    #     with open('tests/resources/transform_out.json') as f:
    #         expected = json.load(f)

    #     d3_transform(final)

    #     assert final == expected