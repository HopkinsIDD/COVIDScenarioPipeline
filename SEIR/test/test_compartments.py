import numpy as np
import os
import pytest
import warnings
import shutil


import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
import filecmp

from SEIR import setup, seir, NPI, file_paths

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"



def test_check_transitions_parquet_creation():
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")
    original_compartments_file = f"{DATA_DIR}/parsed_compartment_compartments.parquet"
    original_transitions_file = f"{DATA_DIR}/parsed_compartment_transitions.parquet"
    lhs = setup.Compartments(seir_config = config["seir"])
    rhs = setup.Compartments(
        seir_config = config["seir"],
        compartments_file = original_compartments_file,
        transitions_file = original_transitions_file
    )

    # assert(lhs.parameters == rhs.parameters) ## parameters objects do not have an == operator
    assert((lhs.compartments == rhs.compartments).all().all())
    # print(lhs.transitions.source)
    # print(rhs.transitions.source)
    # assert((lhs.transitions == rhs.transitions).all().all())
    # assert(lhs == rhs)

    temp_compartments_file = f"{DATA_DIR}/parsed_compartment_compartments.test.parquet"
    temp_transitions_file = f"{DATA_DIR}/parsed_compartment_transitions.test.parquet"
    lhs.toFile(compartments_file = temp_compartments_file, transitions_file = temp_transitions_file)
    # assert(filecmp.cmp(temp_compartments_file, original_compartments_file))
    # assert(filecmp.cmp(temp_transitions_file, original_transitions_file))

def test_check_transitions_parquet_loading():
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")
    lhs = setup.Compartments(seir_config = config["seir"])
    temp_compartments_file = f"{DATA_DIR}/parsed_compartment_compartments.test.parquet"
    temp_transitions_file = f"{DATA_DIR}/parsed_compartment_transitions.test.parquet"
    lhs.toFile(compartments_file = temp_compartments_file, transitions_file = temp_transitions_file)
    rhs = setup.Compartments(
        seir_config = config["seir"],
        compartments_file = temp_compartments_file,
        transitions_file = temp_transitions_file
    )

    # assert(lhs.parameters == rhs.parameters) ## parameters objects do not have an == operator
    assert((lhs.compartments == rhs.compartments).all().all())
    assert(lhs.transitions.shape == rhs.transitions.shape)
    assert((lhs.transitions["source"] == rhs.transitions["source"]).all())
    assert((lhs.transitions["destination"] == rhs.transitions["destination"]).all())
    print(lhs.transitions["rate"])
    print(rhs.transitions["rate"])
    print(lhs.transitions["rate"] == rhs.transitions["rate"])
    print(lhs.transitions["rate"][6])
    print(rhs.transitions["rate"][6])
    assert((lhs.transitions["rate"] == rhs.transitions["rate"]).all())
    # assert((lhs.transitions["proportional_to"] == rhs.transitions["proportional_to"]).all())
    # print(lhs.transitions.source)
    # print(rhs.transitions.source)
    # assert((lhs.transitions == rhs.transitions).all().all())
    # assert(lhs == rhs)
