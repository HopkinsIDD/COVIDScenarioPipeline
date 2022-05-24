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
geoid = ["15005", "15007", "15009", "15001", "15003"]
diffI = np.arange(5) * 2


config.clear()
config.read(user=False)
config.set_file(f"config_shape_full.yml")


inference_simulator = gempyor.InferenceSimulator(
    config_path=f"{config_path_prefix}config_shape_full.yml",
    run_id=1,
    prefix="",
    first_sim_index=1,
    deathrate="high_death_rate",
    stoch_traj_flag=False,
    out_run_id=550,
)

outcomes.onerun_delayframe_outcomes(
    sim_id2write=1, s=inference_simulator.s, load_ID=False
)

hosp_read = pq.read_table(
    f"{config_path_prefix}model_output/hosp/000000001.550.hosp.parquet"
).to_pandas()

incidH = hosp_read[["incidH", "geoid", "date"]].pivot(
    columns="geoid", index="date", values="incidH"
)

delay_hosp = config["outcomes_shapes"]["delay_hosp"].as_convolution_kernel()

# check that the sum is conserved
for i, place in enumerate(geoid):
    assert 0.2 * diffI[i] - 1e-6 < incidH[place].sum() < 0.2 * diffI[i] + 1e-6
    assert len(incidH[place].to_numpy().nonzero()[0]) == 11
    assert (
        incidH[place].iloc[incidH[place].to_numpy().nonzero()[0]]
        == delay_hosp * diffI[i] * 0.2
    ).all()

hosp_curr = hosp_read[["duration_with_a_twist", "geoid", "date"]].pivot(
    columns="geoid", index="date", values="duration_with_a_twist"
)

incidICU = hosp_read[["incidICU", "geoid", "date"]].pivot(
    columns="geoid", index="date", values="incidICU"
)
icu_curr = hosp_read[["incidICU_curr", "geoid", "date"]].pivot(
    columns="geoid", index="date", values="incidICU_curr"
)


# the incidI has values
#               15001 15003 15005 15007 15009
# 2020-04-15 --> 6.0   8.0   0.0   2.0   4.0
# and the rest is all zeros
