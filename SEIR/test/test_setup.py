import datetime
import numpy as np
import os
import pytest
import confuse

from SEIR import setup

from ..utils import config

TEST_SETUP_NAME = "minimal_test"

DATA_DIR = os.path.dirname(__file__) + "/data"

def test_SpatialSetup():
    # good one
    ss = setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    # mobility_file is not the correct dimensions
    with pytest.raises(ValueError):
        setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility_small.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    # Bad popnodes_key error
    with pytest.raises(ValueError):
        setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility_small.txt",
                            popnodes_key="wrong",
                            nodenames_key="geoid")

    # Bad nodenames_key
    with pytest.raises(ValueError):
        setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility_small.txt",
                            popnodes_key="population",
                            nodenames_key="wrong")

def test_Setup_set_filter():
    ss = setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_set_filter_name",
                        spatial_setup=ss,
                        nsim=1,
                        npi_scenario="test_set_filter_scenario",
                        npi_config=confuse.Configuration("test"),
                        ti=datetime.date(2020, 1, 31),
                        tf=datetime.date(2020, 12, 31),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    # filter has bad dimensions
    with pytest.raises(ValueError):
        s.set_filter(np.zeros((1,1)))

