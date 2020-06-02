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
        # outcomes.create_delay_frame(
        #     {"probability": .5, "delay": 5, "duration": 7},
        #     pd.DataFrame(
        #         list(zip(
        #           pd.date_range("2020-01-01","2020-01-31"),
        #           np.arange(31,dtype='float64'),
        #           np.arange(31,dtype='float64')
        #         )),
        #         columns = ['geoid','time','amount']
        #     ),
        #     ['a','b','c','d','e','f','g','h','i','j'],
        #     pd.date_range("2020-01-01","2020-01-10"),
        #     "test"
        # )
        assert(1==1)
