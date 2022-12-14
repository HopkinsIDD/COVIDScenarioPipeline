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
    # Would be better to build a setup
    ss = setup.SpatialSetup(
        setup_name="test_seir",
        geodata_file=f"{DATA_DIR}/geodata.csv",
        mobility_file=f"{DATA_DIR}/mobility.txt",
        popnodes_key="population",
        nodenames_key="geoid",
    )

    index = 1
    run_id = "test_parameter"
    prefix = ""
    s = setup.Setup(
        setup_name="test_seir",
        spatial_setup=ss,
        nsim=1,
        npi_scenario="None",
        config_version="v2",
        npi_config_seir=config["interventions"]["settings"]["None"],
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

    lhs = parameters.Parameters(
        parameter_config=config["seir"]["parameters"],
        ti=s.ti,
        tf=s.tf,
        nodenames=s.spatset.nodenames,
        config_version="v2",
    )
    n_days = 10
    nnodes = 5

    p = parameters.Parameters(
        parameter_config=config["seir"]["parameters"],
        ti=s.ti,
        tf=s.tf,
        nodenames=s.spatset.nodenames,
        config_version="v2",
    )
    p_draw = p.parameters_quick_draw(n_days=10, nnodes=5)
    # test shape
    assert p_draw.shape == (len(config["seir"]["parameters"].keys()), n_days, nnodes)

    write_df(fname="test_pwrite.parquet", df=p.getParameterDF(p_draw=p_draw))

    rhs = parameters.Parameters(
        parameter_config=config["seir"]["parameters"],
        ti=s.ti,
        tf=s.tf,
        nodenames=s.spatset.nodenames,
        config_version="v2",
    )
    p_load = rhs.parameters_load(param_df=read_df("test_pwrite.parquet"), n_days=n_days, nnodes=nnodes)

    assert (p_draw == p_load).all()


def test_parameters_quick_draw_old():
    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/parameters_only.yml")

    ss = setup.SpatialSetup(
        setup_name="test_seir",
        geodata_file=f"{DATA_DIR}/geodata.csv",
        mobility_file=f"{DATA_DIR}/mobility.txt",
        popnodes_key="population",
        nodenames_key="geoid",
    )
    index = 1
    run_id = "test_parameter"
    prefix = ""
    s = setup.Setup(
        setup_name="test_seir",
        spatial_setup=ss,
        nsim=1,
        npi_scenario="None",
        npi_config_seir=config["interventions"]["settings"]["None"],
        parameters_config=config["seir"]["parameters"],
        seeding_config=config["seeding"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        config_version="old",
        interactive=True,
        write_csv=False,
        first_sim_index=index,
        in_run_id=run_id,
        in_prefix=prefix,
        out_run_id=run_id,
        out_prefix=prefix,
        dt=0.25,
    )

    params = parameters.Parameters(
        parameter_config=config["seir"]["parameters"],
        ti=s.ti,
        tf=s.tf,
        nodenames=s.spatset.nodenames,
        config_version="old",
    )

    ### Check that the object is well constructed:
    print(params.pnames)
    assert params.pnames == ["alpha", "sigma", "gamma", "R0"]
    assert params.npar == 4
    assert params.intervention_overlap_operation["sum"] == []
    assert params.intervention_overlap_operation["prod"] == [pn.lower() for pn in params.pnames]

    p_array = params.parameters_quick_draw(n_days=s.n_days, nnodes=s.nnodes)
    print(p_array.shape)

    alpha = p_array[params.pnames2pindex["alpha"]]
    R0s = p_array[params.pnames2pindex["R0"]]
    sigma = p_array[params.pnames2pindex["sigma"]]
    gamma = p_array[params.pnames2pindex["gamma"]]
    # susceptibility_reduction = p_array[parameters.pnames2pindex['']]
    # transmissibility_reduction = p_array[parameters.pnames2pindex['alpha']]

    assert alpha.shape == (s.n_days, s.nnodes)
    assert (alpha == 0.5).all()

    assert R0s.shape == (s.n_days, s.nnodes)
    assert len(np.unique(R0s)) == 1
    assert ((2 <= R0s) & (R0s <= 3)).all()

    assert sigma.shape == (s.n_days, s.nnodes)
    assert (sigma == config["seir"]["parameters"]["sigma"].as_evaled_expression()).all()

    assert gamma.shape == (s.n_days, s.nnodes)
    assert len(np.unique(gamma)) == 1


def test_parameters_from_timeserie_file():
    config.clear()
    config.read(user=False)
    config.set_file(f"{DATA_DIR}/config_compartmental_model_format.yml")
    ss = setup.SpatialSetup(
        setup_name="test_seir",
        geodata_file=f"{DATA_DIR}/geodata.csv",
        mobility_file=f"{DATA_DIR}/mobility.txt",
        popnodes_key="population",
        nodenames_key="geoid",
    )
    index = 1
    run_id = "test_parameter"
    prefix = ""
    s = setup.Setup(
        setup_name="test_seir",
        spatial_setup=ss,
        nsim=1,
        npi_scenario="None",
        config_version="v2",
        npi_config_seir=config["interventions"]["settings"]["None"],
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

    lhs = parameters.Parameters(
        parameter_config=config["seir"]["parameters"],
        ti=s.ti,
        tf=s.tf,
        nodenames=s.spatset.nodenames,
        config_version="v2",
    )
    n_days = 10
    nnodes = 5

    p = parameters.Parameters(
        parameter_config=config["seir"]["parameters"],
        ti=s.ti,
        tf=s.tf,
        nodenames=s.spatset.nodenames,
        config_version="v2",
    )
    p_draw = p.parameters_quick_draw(n_days=10, nnodes=5)
    # test shape
    assert p_draw.shape == (len(config["seir"]["parameters"].keys()), n_days, nnodes)

    write_df(fname="test_pwrite.parquet", df=p.getParameterDF(p_draw=p_draw))

    rhs = parameters.Parameters(
        parameter_config=config["seir"]["parameters"],
        ti=s.ti,
        tf=s.tf,
        nodenames=s.spatset.nodenames,
        config_version="v2",
    )
    p_load = rhs.parameters_load(param_df=read_df("test_pwrite.parquet"), n_days=n_days, nnodes=nnodes)

    assert (p_draw == p_load).all()
