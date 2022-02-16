import numpy as np
import pandas as pd
import os
import pytest
import warnings
import shutil

import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
import filecmp

from gempyor import setup, seir, NPI, file_paths, parameters

from gempyor.utils import config, write_df, read_df

DATA_DIR = os.path.dirname(__file__) + "/data"
os.chdir(os.path.dirname(__file__))


def test_parameters_from_config_plus_read_write():
    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")

    lhs = parameters.Parameters(
        parameter_config=config["seir"]["parameters"], config_version="v2"
    )
    nt_inter = 10
    nnodes = 5

    p = parameters.Parameters(
        parameter_config=config["seir"]["parameters"], config_version="v2"
    )
    p_draw = p.parameters_quick_draw(nt_inter=10, nnodes=5)
    # test shape
    assert p_draw.shape == (len(config["seir"]["parameters"].keys()), nt_inter, nnodes)

    write_df(fname="test_pwrite.parquet", df=p.getParameterDF(p_draw=p_draw))

    rhs = parameters.Parameters(
        parameter_config=config["seir"]["parameters"], config_version="v2"
    )
    p_load = rhs.parameters_load(
        param_df=read_df("test_pwrite.parquet"), nt_inter=nt_inter, nnodes=nnodes
    )

    assert (p_draw == p_load).all()


def test_parameters_quick_draw_old():
    config.set_file(f"{DATA_DIR}/parameters_only.yml")

    date_range = pd.date_range("2020-01-30", "2020-02-01")
    dt = 0.25
    nt_inter = int((len(date_range) - 1) * (1 / dt)) + 1
    nnodes = 200
    npi = pd.DataFrame(0.0, index=date_range, columns=range(nnodes))

    params = parameters.Parameters(parameter_config=config, config_version="old")

    ### Check that the object is well constructed:
    print(params.pnames)
    assert params.pnames == ["alpha", "sigma", "gamma", "R0"]
    assert params.npar == 4
    assert params.intervention_overlap_operation["sum"] == []
    assert params.intervention_overlap_operation["prod"] == [
        pn.lower() for pn in params.pnames
    ]

    p_array = params.parameters_quick_draw(nt_inter, nnodes)
    print(p_array.shape)

    alpha = p_array[params.pnames2pindex["alpha"]]
    R0s = p_array[params.pnames2pindex["R0"]]
    sigma = p_array[params.pnames2pindex["sigma"]]
    gamma = p_array[params.pnames2pindex["gamma"]]
    # susceptibility_reduction = p_array[parameters.pnames2pindex['']]
    # transmissibility_reduction = p_array[parameters.pnames2pindex['alpha']]

    assert alpha.shape == (nt_inter, nnodes)
    assert (alpha == 0.5).all()

    assert R0s.shape == (nt_inter, nnodes)
    assert len(np.unique(R0s)) == 1
    assert ((2 <= R0s) & (R0s <= 3)).all()

    assert sigma.shape == (nt_inter, nnodes)
    assert (sigma == config["sigma"].as_evaled_expression()).all()

    assert gamma.shape == (nt_inter, nnodes)
    assert len(np.unique(gamma)) == 1
