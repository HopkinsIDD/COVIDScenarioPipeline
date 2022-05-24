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

# the incidI has values
#               15001 15003 15005 15007 15009
# 2020-04-15 --> 6.0   8.0   0.0   2.0   4.0
# and the rest is all zeros


delay_hosp = config["outcomes_shapes"]["delay_hosp"].as_convolution_kernel()
delay_hosp = delay_hosp[len(delay_hosp) // 2 :]  # only the future part as defined.

# the convolution doesn't bring anything in the past.
assert (incidH.loc[:"2020-04-14"] == 0).all().all()

# check that the sum is conserved
for i, place in enumerate(geoid):
    assert 0.2 * diffI[i] - 1e-6 < incidH[place].sum() < 0.2 * diffI[i] + 1e-6
    if diffI[i] > 0.0001:
        assert len(incidH[place].to_numpy().nonzero()[0]) == 11
        assert (
            delay_hosp * diffI[i] * 0.2 - 1e-6
            < incidH[place].iloc[incidH[place].to_numpy().nonzero()[0]]
        ).all()
        assert (
            delay_hosp * diffI[i] * 0.2 + 1e-6
            > incidH[place].iloc[incidH[place].to_numpy().nonzero()[0]]
        ).all()
    else:
        assert (incidH[place] == 0).all()

hosp_curr = hosp_read[["duration_with_a_twist", "geoid", "date"]].pivot(
    columns="geoid", index="date", values="duration_with_a_twist"
)

incidICU = hosp_read[["incidICU", "geoid", "date"]].pivot(
    columns="geoid", index="date", values="incidICU"
)
icu_curr = hosp_read[["incidICU_curr", "geoid", "date"]].pivot(
    columns="geoid", index="date", values="incidICU_curr"
)

for i, place in enumerate(geoid):
    icu_curr_pl = icu_curr[place]
    incidICU_pl = incidICU[place]  # date is 2020-04-18

    pploutofICU = pd.DataFrame(
        np.convolve(
            incidICU_pl,
            config["outcomes_shapes"]["duration_icu_mod"].as_convolution_kernel(),
            mode="same",
        ),
        index=icu_curr_pl.index,
    )
    # the duration is:
    #  array: [.5, .25, .25] with  shift: 3 and we have e.g incidI at 2020-04-18    0.8
    # so we should havel, in pploutofICU:
    # 2020-04-18  0.0  # no one is out the same day
    # 2020-04-19  0.0  # nor the next
    # 2020-04-20  0.0  # nor the next
    # 2020-04-21  0.4  # half the first day
    # 2020-04-22  0.2  # a quarter
    # 2020-04-23  0.2  # a quarter
    # so ppl in icu_curr are:
    # 2020-04-18    0.8  # everyone
    # 2020-04-19    0.8  # everyone
    # 2020-04-20    0.8  # everyone
    # 2020-04-21    0.4  # half at the end of the day !
    # 2020-04-22    0.2  # a quarter now, and then zeros.
    if diffI[i] != 0:
        assert (
            len(icu_curr_pl[icu_curr_pl == incidICU_pl.max()]) == 3
        )  # 3 days with everyone
        assert icu_curr_pl.loc["2020-04-17"] == 0  # no one in the past
        assert (
            icu_curr_pl.loc["2020-04-18"] == diffI[i] * 0.1
        )  # everyone in the first day
        assert (
            icu_curr_pl.loc["2020-04-19"] == diffI[i] * 0.1
        )  # everyone in the first day
        assert (
            icu_curr_pl.loc["2020-04-20"] == diffI[i] * 0.1
        )  # everyone in the first day
        assert (
            icu_curr_pl.loc["2020-04-21"] == diffI[i] * 0.1 / 2
        )  # half at the end of the day
        assert (
            diffI[i] * 0.1 / 4 - 1e-6
            < icu_curr_pl.loc["2020-04-22"]
            < diffI[i] * 0.1 / 4 + 1e-6
        )  # a quarter now, and then zeros.
        assert icu_curr_pl.loc["2020-04-23"] == 0  # no one thereafter


gamma_rate = config["outcomes_shapes"]["gamma_rate"].as_random_distribution(
    return_dist=True
)
gamma_scale = config["outcomes_shapes"]["gamma_scale"].as_random_distribution(
    return_dist=True
)

# check that the two parametrizations are the same
assert (
    gamma_rate.pdf(np.arange(30, step=0.1)) == gamma_scale.pdf(np.arange(30, step=0.1))
).all()

assert gamma_rate.stats() == (np.array(30.0), np.array(300.0))  # mean, variance
