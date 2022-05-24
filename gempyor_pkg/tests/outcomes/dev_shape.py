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
config.clear()
config.read(user=False)
config.set_file(f"config_shape.yml")

outcomes_shapes = config["outcomes_shapes"]

duration_hosp = outcomes_shapes["duration_hosp"].as_convolution_kernel()
print(duration_hosp, np.sum(duration_hosp))

delay_hosp = outcomes_shapes["delay_hosp"].as_convolution_kernel()
print(delay_hosp)
breakpoint()
