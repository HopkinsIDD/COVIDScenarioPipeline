import pytest
from SEIR import setup

from ..utils import config

TEST_SETUP_NAME = "minimal_test"

def test_SpatialSetup():
    ss = setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file="test/data/geodata.csv",
                            mobility_file="test/data/mobility.txt",
                            popnodes_key="population")

    # mobility_file is not the correct dimensions
    with pytest.raises(ValueError):
        setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file="test/data/geodata.csv",
                            mobility_file="test/data/mobility_small.txt",
                            popnodes_key="population")

    # Bad popnodes_key error
    with pytest.raises(KeyError):
        setup.SpatialSetup(setup_name=TEST_SETUP_NAME,
                            geodata_file="test/data/geodata.csv",
                            mobility_file="test/data/mobility_small.txt",
                            popnodes_key="pops")

def test_Setup_buildICfromfilter():

    config.set_file("test/data/config.yml")

    s = setup.Setup(setup_name= "test_None",
                        spatial_setup=setup.SpatialSetup(
                            setup_name=TEST_SETUP_NAME,
                            geodata_file="test/data/geodata.csv",
                            mobility_file="test/data/mobility.txt",
                            popnodes_key="population",
                        ),
                        nsim=5,
                        npi_scenario="None",
                        npi_config=config["interventions"]["settings"]["None"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=True,
                        dt=config["dt"].as_number())
    y0 = s.test_Setup_buildICfromfilter()
    # y0[S, :] = self.popnodes - draw
    # y0[E, :] = (draw / 4).astype(np.int)
    # y0[I1, :] = (draw / 4).astype(np.int)
    # y0[I2, :] = (draw / 4).astype(np.int)
    # y0[I3, :] = (draw / 4).astype(np.int)
    #TODO write test to make sure they sum to y0


def test_parameters_quick_draw():
    pass
