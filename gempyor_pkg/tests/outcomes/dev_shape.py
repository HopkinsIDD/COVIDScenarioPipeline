# This file will become tests at some point.

import gempyor
import numpy as np
import pandas as pd
import datetime
import pytest

from gempyor.utils import config

import pandas as pd
import numpy as np
import datetime
import matplotlib.pyplot as plt
import glob, os, sys
from pathlib import Path

# import seaborn as sns
import pyarrow.parquet as pq
import pyarrow as pa
from gempyor import file_paths, setup, outcomes
from gempyor.utils import config

# os.chdir(os.path.dirname(__file__))
config_path_prefix = ""  #'tests/outcomes/'


config.clear()
config.read(user=False)
config.set_file(f"config_shape_full.yml")

outcomes_shapes = config["outcomes_shapes"]

# check that all convolution kernel from shape have mass 1, and are 1d array
for outcomes_shape in outcomes_shapes:
    ck = outcomes_shapes[outcomes_shape].as_convolution_kernel()
    assert len(ck.shape) == 1
    assert 1 - 1e-6 < ck.sum() < 1 + 1e-6

# specific checks:
delay_hosp = outcomes_shapes["delay_hosp"].as_convolution_kernel()
assert len(delay_hosp) == 11

duration_hosp = outcomes_shapes["duration_hosp"].as_convolution_kernel()
assert len(duration_hosp) == 8

duration_icu = outcomes_shapes["duration_icu"].as_convolution_kernel()
assert len(duration_icu) == 19

test_automatic_support = outcomes_shapes["test_automatic_support"].as_convolution_kernel()
# the automatic support has a cutoff at .99 of the mass of the distrubtion:
# get the distribution: 
auto_dist = outcomes_shapes["test_automatic_support"].as_random_distribution(return_dist= True)
assert auto_dist.cdf(len(test_automatic_support)) > .99
assert auto_dist.cdf(len(test_automatic_support)-2) < .99







#duration_hosp = outcomes_shapes["duration_hosp"].as_convolution_kernel()



inference_simulator = gempyor.InferenceSimulator(
    config_path=f"{config_path_prefix}config_shape_full.yml",
    run_id=1,
    prefix="",
    first_sim_index=1,
    deathrate="high_death_rate",
    stoch_traj_flag=False,
)

outcomes.onerun_delayframe_outcomes(
    sim_id2write=1, s=inference_simulator.s, load_ID=False
)

### test shape generation

#breakpoint()
