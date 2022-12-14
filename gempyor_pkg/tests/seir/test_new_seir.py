import numpy as np
import os
import pytest
import warnings
import shutil
import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
from functools import reduce

from gempyor import setup, seir, NPI, file_paths, compartments

from gempyor.utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"
os.chdir(os.path.dirname(__file__))


def test_constant_population():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(
        setup_name="test_seir",
        geodata_file=f"{DATA_DIR}/geodata.csv",
        mobility_file=f"{DATA_DIR}/mobility.txt",
        popnodes_key="population",
        nodenames_key="geoid",
    )

    s = setup.Setup(
        setup_name="test_seir",
        spatial_setup=ss,
        nsim=1,
        npi_scenario="None",
        npi_config_seir=config["interventions"]["settings"]["None"],
        parameters_config=config["seir"]["parameters"],
        seeding_config={},
        initial_conditions_config=config["initial_conditions"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=True,
        write_csv=False,
        dt=0.25,
        stoch_traj_flag=False,
    )

    initial_conditions = s.seedingAndIC.draw_ic(sim_id=0, setup=s)
    seeding_data, seeding_amounts = s.seedingAndIC.load_seeding(sim_id=100, setup=s)

    npi = NPI.NPIBase.execute(npi_config=s.npi_config_seir, global_config=config, geoids=s.spatset.nodenames)

    parameters = s.parameters.parameters_quick_draw(n_days=s.n_days, nnodes=s.nnodes)
    parameter_names = [x for x in s.parameters.pnames]

    print("RUN_FUN_START")
    (
        unique_strings,
        transition_array,
        proportion_array,
        proportion_info,
    ) = s.compartments.get_transition_array()
    parsed_parameters = s.compartments.parse_parameters(parameters, s.parameters.pnames, unique_strings)
    print("RUN_FUN_END")
    print(proportion_array)

    states = seir.steps_SEIR(
        s,
        parsed_parameters,
        transition_array,
        proportion_array,
        proportion_info,
        initial_conditions,
        seeding_data,
        seeding_amounts,
    )
