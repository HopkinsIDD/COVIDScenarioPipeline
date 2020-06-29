import json
import pyarrow.parquet as pq

from preprocessor.parse import init_obj, parse_sim, d3_transform
from preprocessor.geoids import all_geoids

class TestParse:

    def test_init_obj(self):
        # returns expected final initialized object

        geoids = all_geoids
        scenarios = ['ScenarioA']
        severities = ['high']
        parameters = ['incidH', 'incidI']
        dates = ['2020-01-01', '2020-01-02', '2020-01-03']

        final = init_obj(geoids, scenarios, severities, parameters, dates)

        with open('tests/resources/init_obj.json') as f:
            expected = json.load(f)

        assert final == expected

    def test_parse_sim(self):
        # returns expected populated final dict object after parsing sim file

        path = 'tests/fixtures/pld_inf/000000001.2020.06.18.02:53:08..hosp.parquet'
        pq_dataset = pq.ParquetDataset(path) 
        scenario = 'ScenarioA'
        parameters = ['incidH', 'incidI']
        severity = 'high'
        sim = '1'
        date_len = 230

        # test must include all geoids ordered based on input parquet as the
        # chunk indexing optimization is contingent on all ordered geoids
        geoids = all_geoids

        with open('tests/resources/init_obj.json') as f:
            final = json.load(f)

        parse_sim(pq_dataset, final, geoids, scenario, severity, parameters, sim, date_len)

        with open('tests/resources/parse_sim_06085.json') as f:
            expected_06085 = json.load(f)
        with open('tests/resources/parse_sim_06019.json') as f:
            expected_06019 = json.load(f)

        assert final['06085']['ScenarioA']['high']['incidI']['sims']['1'] == expected_06085
        assert final['06019']['ScenarioA']['high']['incidI']['sims']['1'] == expected_06019

    def test_d3_transform(self):
        # returns expected transformed d3 format

        r0_map = {
            "ScenarioA/low/1": 2.2, 
            "ScenarioA/low/10": 2.8, 
            "ScenarioA/high/1": 2.76, 
            "ScenarioA/high/10": 2.42, 
            }

        with open('tests/resources/transform_in.json') as f:
            final = json.load(f)
        with open('tests/resources/transform_out.json') as f:
            expected = json.load(f)

        d3_transform(final, r0_map)

        assert final == expected