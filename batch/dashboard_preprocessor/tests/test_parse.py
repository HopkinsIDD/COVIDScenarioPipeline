import json
from preprocessor.parse import parse_sim, d3_transform
from preprocessor.utils import init_final_obj

class TestParse:
    def test_parse_sim(self):
        # returns expected populated final dict object after parsing sim file

        file_path = 'tests/fixtures/ScenarioA/Config_ScenarioA_high_1.parquet'
        scenario = 'ScenarioA'
        geoids = ['06085', '06019']
        dates = ['2020-05-05', '2020-05-06', '2020-05-07', '2020-05-08', '2020-05-09']
        parameters = ['incidD',  'incidH',  'incidI']
        severity = 'high'
        sim = '1'

        with open('tests/resources/parse_sim_in.json') as f:
            final = json.load(f)
        # final = init_final_obj(geoids, [scenario], [severity], parameters, dates)
        parse_sim(file_path, final, geoids, scenario, severity, parameters, sim)

        with open('tests/resources/parse_sim_out.json') as f:
            expected = json.load(f)
            
        assert final == expected

    def test_d3_transform(self):
        # returns expected transformed d3 format

        with open('tests/resources/transform_in.json') as f:
            final = json.load(f)
        with open('tests/resources/transform_out.json') as f:
            expected = json.load(f)

        d3_transform(final)

        assert final == expected