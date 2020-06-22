import json
from preprocessor.utils import aggregate_by_state

class TestUtils:

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

