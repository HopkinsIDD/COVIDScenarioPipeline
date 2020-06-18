import json
from preprocessor.geo import stats_for_county_boundaries

class TestGeo:
    def test_stats_for_county_boundaries(self):
        # returns expected stat json for populating into county_boundaries

        with open('tests/resources/geo_in.json') as f:
            initial = json.load(f)
        with open('tests/resources/geo_out.json') as f:
            expected = json.load(f)

        final = stats_for_county_boundaries(initial)

        assert final == expected
