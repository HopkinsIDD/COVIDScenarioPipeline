import numpy as np
import pandas as pd
import datetime

import pytest

from Outcomes import outcomes

TEST_SETUP_NAME = "Testing test"

test_config = {
                "probability": {
                    "value": {
                        "distribution": "fixed", 
                        "value": "0.6"
                    }
                },
                "delay": {
                    "value": {
                        "distribution": "fixed",
                        "value": "5"
                    }
                },
                "duration": {
                    "value": {
                        "distribution": "fixed",
                        "value": "5"
                    }
                }
            }

class TestSpatialSetup:
    def test_shift_works(self):
        arr = np.reshape(np.arange(100),[10,10])
    def test_create_delay_frame_works(self):
        # Write something sensible here...
        input_data = np.arange(1000).reshape([100,10])
        places = ['a','b','c','d','e','f','g','h','i','j']
        dates = pd.Series(pd.date_range("2020-01-01","2020-04-09"),name='time')
        rc = outcomes.create_delay_frame(
            {"source": "other_test", "probability": .5, "delay": 1, "duration": 2, "duration_name": "fish"},
            input_data,
            places,
            dates,
            "test"
        )
        df = pd.DataFrame(outcomes.shift(input_data,1),columns = places, index = dates)
        df.reset_index(inplace=True)
        df = pd.melt(df,id_vars='time', value_name = 'fish', var_name = 'geoid')
        assert(np.abs((rc['test'] - df['fish'] * .5).mean()) <= .75)

## For testing:
a = TestSpatialSetup()

