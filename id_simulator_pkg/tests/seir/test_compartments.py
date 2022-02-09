import numpy as np
import os
import pytest
import warnings
import shutil


import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
import filecmp

from id_simulator import compartments, seir, NPI, file_paths, setup

from id_simulator.utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"
os.chdir(os.path.dirname(__file__))


def test_check_transitions_parquet_creation():
    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")
    original_compartments_file = f"{DATA_DIR}/parsed_compartment_compartments.parquet"
    original_transitions_file = f"{DATA_DIR}/parsed_compartment_transitions.parquet"
    lhs = compartments.Compartments(seir_config=config["seir"])
    rhs = compartments.Compartments(
        seir_config=config["seir"],
        compartments_file=original_compartments_file,
        transitions_file=original_transitions_file,
    )

    assert lhs.times_set == 1
    assert rhs.times_set == 1
    # assert(lhs.parameters == rhs.parameters) ## parameters objects do not have an == operator
    assert (lhs.compartments == rhs.compartments).all().all()
    assert (lhs.transitions == rhs.transitions).all().all()
    assert lhs == rhs


def test_check_transitions_parquet_writing_and_loading():
    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")
    lhs = compartments.Compartments(seir_config=config["seir"])
    temp_compartments_file = f"{DATA_DIR}/parsed_compartment_compartments.test.parquet"
    temp_transitions_file = f"{DATA_DIR}/parsed_compartment_transitions.test.parquet"
    lhs.toFile(
        compartments_file=temp_compartments_file, transitions_file=temp_transitions_file
    )
    rhs = compartments.Compartments(
        seir_config=config["seir"],
        compartments_file=temp_compartments_file,
        transitions_file=temp_transitions_file,
    )

    assert lhs.times_set == 1
    assert rhs.times_set == 1
    assert (lhs.compartments == rhs.compartments).all().all()
    assert (lhs.transitions == rhs.transitions).all().all()
    assert lhs == rhs


def test_Setup_has_compartments_component():
    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(
        setup_name="test_values",
        geodata_file=f"{DATA_DIR}/geodata.csv",
        mobility_file=f"{DATA_DIR}/mobility.txt",
        popnodes_key="population",
        nodenames_key="geoid",
    )

    s = setup.Setup(
        setup_name="test_values",
        spatial_setup=ss,
        nsim=1,
        npi_scenario="None",
        config_version="old",
        npi_config=config["interventions"]["settings"]["None"],
        parameters_config=config["seir"]["parameters"],
        seir_config=config["seir"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=True,
        write_csv=False,
        dt=0.25,
    )
    assert type(s.compartments) == compartments.Compartments
    assert type(s.compartments) == compartments.Compartments

    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/config_compartmental_model_full.yml")

    s = setup.Setup(
        setup_name="test_values",
        spatial_setup=ss,
        nsim=1,
        npi_scenario="None",
        config_version="v2",
        npi_config=config["interventions"]["settings"]["None"],
        parameters_config=config["seir"]["parameters"],
        seir_config=config["seir"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=True,
        write_csv=False,
        dt=0.25,
    )
    assert type(s.compartments) == compartments.Compartments
    assert type(s.compartments) == compartments.Compartments
