import json
from preprocessor.utils import get_dates, init_final_obj, aggregate_by_state

class TestUtils:
    def test_get_dates(self):
        # returns expected dates array

        scenarios = ['ScenarioA']
        expected = [
            '2020-05-05', '2020-05-06', '2020-05-07', '2020-05-08', '2020-05-09'
        ]

        assert get_dates('tests/fixtures/', scenarios) == expected

    def test_init_final_obj(self):
        # returns expected final initialized object

        geoids = ['06085', '06019']
        scenarios = ['ScenarioA']
        severities = ['high']
        parameters = ['incidD',  'incidH',  'incidI']
        dates = ['2020-05-05', '2020-05-06', '2020-05-07', '2020-05-08', '2020-05-09']

        final = init_final_obj(geoids, scenarios, severities, parameters, dates)

        with open('tests/resources/init_obj.json') as f:
            expected = json.load(f)

        assert final == expected

    def test_aggregate_by_state(self):
        # returns expected final dict with aggregated vals of state

        states = ['06', '36']

        with open('tests/resources/agg_state_in.json') as f:
            state_dict = json.load(f)
        with open('tests/resources/agg_final_in.json') as f:
            final = json.load(f)

        aggregate_by_state(final, state_dict, states)

        with open('tests/resources/agg_final_out.json') as f:
            expected = json.load(f) 

        assert final == expected

