import json
from preprocessor.quantiles import calc_quantiles, transform_quantiles

class TestQuantiles:
    def test_calc_quantiles(self):
        # returns expected transformed d3 format

        with open('tests/resources/quantiles_in.json') as f:
            final = json.load(f)
        with open('tests/resources/quantiles_out.json') as f:
            expected = json.load(f)

        calc_quantiles(final)

        assert final == expected

    def test_transform_quantiles(self):
        # returns expected transformed d3 format

        with open('tests/resources/transform_quant_in.json') as f:
            final = json.load(f)
        with open('tests/resources/transform_quant_out.json') as f:
            expected = json.load(f)

        transform_quantiles(final)

        assert final == expected

