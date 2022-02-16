import numpy as np
import os
import pytest
import warnings
import shutil

import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
import filecmp
import pandas as pd
import matplotlib.pyplot as plt

import gempyor.seir
from gempyor import compartments, seir, NPI, file_paths, setup

from gempyor.utils import config

DATA_DIR = "data"

config.clear()
config.read(user=False)
config.set_file(f"{DATA_DIR}/config.yml")

ss = setup.SpatialSetup(
    setup_name="test_seir",
    geodata_file=f"{DATA_DIR}/geodata.csv",
    mobility_file=f"{DATA_DIR}/mobility.txt",
    popnodes_key="population",
    nodenames_key="geoid",
)

index = 1
run_id = "test_SeedOneNode"
prefix = ""
s = setup.Setup(
    setup_name="test_seir",
    spatial_setup=ss,
    nsim=1,
    npi_scenario="None",
    npi_config=config["interventions"]["settings"]["None"],
    parameters_config=config["seir"]["parameters"],
    seeding_config=config["seeding"],
    ti=config["start_date"].as_date(),
    tf=config["end_date"].as_date(),
    interactive=True,
    write_csv=False,
    first_sim_index=index,
    in_run_id=run_id,
    in_prefix=prefix,
    out_run_id=run_id,
    out_prefix=prefix,
    dt=0.25,
)

seeding_data = s.seedingAndIC.draw_seeding(sim_id=100, setup=s)
initial_conditions = s.seedingAndIC.draw_ic(sim_id=100, setup=s)

mobility_geoid_indices = s.mobility.indices
mobility_data_indices = s.mobility.indptr
mobility_data = s.mobility.data

npi = NPI.NPIBase.execute(
    npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames
)

params = s.parameters.parameters_quick_draw(s.n_days, s.nnodes)
params = s.parameters.parameters_reduce(params, npi)

(
    parsed_parameters,
    unique_strings,
    transition_array,
    proportion_array,
    proportion_info,
) = s.compartments.get_transition_array(params, s.parameters.pnames)


states = seir.steps_SEIR_nb(
    s.compartments.compartments.shape[0],
    s.nnodes,
    s.n_days,
    parsed_parameters,
    s.dt,
    transition_array,
    proportion_info,
    proportion_array,
    initial_conditions,
    seeding_data,
    mobility_data,
    mobility_geoid_indices,
    mobility_data_indices,
    s.popnodes,
    True,
)
df = seir.states2Df(s, states)
assert (
    df[(df["mc_value_type"] == "prevalence") & (df["mc_infection_stage"] == "R")].loc[
        str(s.tf), "20002"
    ]
    > 1
)
print(df)
ts = df
cp = "R"
ts = ts[(ts["mc_infection_stage"] == cp) & (ts["mc_value_type"] == "prevalence")]
ts = ts.drop(["mc_value_type", "mc_infection_stage", "mc_name"], axis=1)
ts = ts.pivot(columns="mc_vaccination_stage").sum(axis=1, level=1)
ts["unvaccinated"].plot()

out_df = df
out_df["date"] = out_df.index
pa_df = pa.Table.from_pandas(out_df, preserve_index=False)
pa.parquet.write_table(pa_df, "testlol.parquet")

df2 = SEIR.seir.onerun_SEIR(100, s)
