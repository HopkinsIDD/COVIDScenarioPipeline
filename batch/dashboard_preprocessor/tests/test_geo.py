import json
from preprocessor.geo import build_geo_obj, stats_for_county_boundaries

class TestGeo:
    def test_build_geo_obj(self):
        # returns expected initialized geo obj for stats_for_county_boundaries
        with open('tests/resources/geo_init_obj.json') as f:
            expected = json.load(f)
        with open('tests/resources/geo_init_in.json') as f:
            final_parsed_obj = json.load(f)

        init_geo_obj = build_geo_obj(final_parsed_obj)

        assert init_geo_obj == expected

    def test_stats_for_county_boundaries(self):
        # returns expected stat json for populating into county_boundaries
        
        median_csv_path = 'tests/resources/median_in.csv'
        geoids = ['06085', '06019', '36005']
        scenarios = ['ScenarioA']
        parameters = ['incidD', 'incidH', 'incidI']

        with open('tests/resources/geo_init_obj.json') as f:
            geo_obj = json.load(f)
        with open('tests/resources/geo_out.json') as f:
            expected = json.load(f)

        final = stats_for_county_boundaries(
            geo_obj, median_csv_path, geoids, scenarios, parameters)

        assert final == expected
