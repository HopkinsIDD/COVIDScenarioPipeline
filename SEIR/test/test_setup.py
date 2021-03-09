import datetime
import numpy as np
import os
import pandas as pd
import pytest
import confuse

from SEIR import setup

from ..utils import config

TEST_SETUP_NAME = "minimal_test"

DATA_DIR = os.path.dirname(__file__) + "/data"

class TestSpatialSetup:
    def test_SpatialSetup_success(self):
        ss = setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                                geodata_file=f"{DATA_DIR}/geodata.csv",
                                mobility_file=f"{DATA_DIR}/mobility.txt",
                                popnodes_key="population",
                                nodenames_key="geoid")

    def test_bad_popnodes_key_fail(self):
        # Bad popnodes_key error
        with pytest.raises(ValueError, match=r'.*popnodes_key.*'):
            setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                                geodata_file=f"{DATA_DIR}/geodata.csv",
                                mobility_file=f"{DATA_DIR}/mobility_small.txt",
                                popnodes_key="wrong",
                                nodenames_key="geoid")

    def test_bad_nodenames_key_fail(self):
        with pytest.raises(ValueError, match=r'.*nodenames_key.*'):
            setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                                geodata_file=f"{DATA_DIR}/geodata.csv",
                                mobility_file=f"{DATA_DIR}/mobility.txt",
                                popnodes_key="population",
                                nodenames_key="wrong")

    def test_mobility_dimensions_fail(self):
        with pytest.raises(ValueError, match=r".*mobility.*dimensions.*"):
            setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                                geodata_file=f"{DATA_DIR}/geodata.csv",
                                mobility_file=f"{DATA_DIR}/mobility_small.txt",
                                popnodes_key="population",
                                nodenames_key="geoid")

    def test_mobility_too_big_fail(self):
        with pytest.raises(ValueError, match=r".*mobility.*population.*"):
            setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                                geodata_file=f"{DATA_DIR}/geodata.csv",
                                mobility_file=f"{DATA_DIR}/mobility_big.txt",
                                popnodes_key="population",
                                nodenames_key="geoid")

def test_parameters_quick_draw():
    config.set_file(f"{DATA_DIR}/parameters_only.yml")

    date_range = pd.date_range("2020-01-30", "2020-02-01")
    dt = 0.25
    nt_inter = int((len(date_range) - 1) * (1/dt)) + 1
    nnodes = 200
    npi = pd.DataFrame(0.0, index=date_range,
                            columns=range(nnodes))

    alpha,\
    beta,\
    sigma,\
    gamma,\
    n_parallel_compartments,\
    susceptibility_reduction,\
    transmissibility_reduction,\
    n_parallel_transitions,\
    transition_rate,\
    transition_from,\
    transition_to = \
        setup.parameters_quick_draw(config, nt_inter, nnodes)

    assert alpha.shape == (nt_inter, nnodes)
    assert (alpha == 0.5).all()

    assert beta.shape == (nt_inter, nnodes)
    assert (len(np.unique(beta)) == 1)
    assert (((1/6. * 2) <= beta)  & (beta <= (1./2.6 * 3))).all()

    assert sigma.shape == (nt_inter, nnodes)
    assert (sigma == config["sigma"].as_evaled_expression()).all()

    assert gamma.shape == (nt_inter, nnodes)
    assert (len(np.unique(gamma)) == 1)
    assert (((setup.n_Icomp * (1./6)) <= gamma) & (gamma <= (setup.n_Icomp * (1/2.6)))).all()
