import numpy as np
import os
import pytest
import warnings
import shutil


import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
import filecmp

from SEIR import setup, seir, NPI, file_paths, Parameters

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"


def test_parameters_from_config_plus_read_write():
    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")
    lhs = Parameters.Parameters(parameter_config=config["seir"])
    nt_inter = 10
    nnodes = 5
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")
    p = Parameters.Parameters(parameter_config=config["seir"])
    p_draw = p.parameters_quick_draw(nt_inter=10, nnodes=5)
    # test shape
    assert (p_draw.shape == (len(config["seir"]["parameters"].keys()), nt_inter, nnodes))

    p.parameters_write(p_draw=p_draw, fname='test_pwrite')

    rhs = Parameters.Parameters(parameter_config=config["seir"])
    p_load = rhs.parameters_load(fname='test_pwrite', nt_inter=nt_inter, nnodes=nnodes)

    assert ((p_draw == p_load).all())
