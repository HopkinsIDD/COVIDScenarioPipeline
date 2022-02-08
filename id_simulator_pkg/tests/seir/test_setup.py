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
        ss = setup.SpatialSetup(
            setup_name=TEST_SETUP_NAME,
            geodata_file=f"{DATA_DIR}/geodata.csv",
            mobility_file=f"{DATA_DIR}/mobility.txt",
            popnodes_key="population",
            nodenames_key="geoid",
        )

    def test_bad_popnodes_key_fail(self):
        # Bad popnodes_key error
        with pytest.raises(ValueError, match=r".*popnodes_key.*"):
            setup.SpatialSetup(
                setup_name=TEST_SETUP_NAME,
                geodata_file=f"{DATA_DIR}/geodata.csv",
                mobility_file=f"{DATA_DIR}/mobility_small.txt",
                popnodes_key="wrong",
                nodenames_key="geoid",
            )

    def test_bad_nodenames_key_fail(self):
        with pytest.raises(ValueError, match=r".*nodenames_key.*"):
            setup.SpatialSetup(
                setup_name=TEST_SETUP_NAME,
                geodata_file=f"{DATA_DIR}/geodata.csv",
                mobility_file=f"{DATA_DIR}/mobility.txt",
                popnodes_key="population",
                nodenames_key="wrong",
            )

    def test_mobility_dimensions_fail(self):
        with pytest.raises(ValueError, match=r".*mobility.*dimensions.*"):
            setup.SpatialSetup(
                setup_name=TEST_SETUP_NAME,
                geodata_file=f"{DATA_DIR}/geodata.csv",
                mobility_file=f"{DATA_DIR}/mobility_small.txt",
                popnodes_key="population",
                nodenames_key="geoid",
            )

    def test_mobility_too_big_fail(self):
        with pytest.raises(ValueError, match=r".*mobility.*population.*"):
            setup.SpatialSetup(
                setup_name=TEST_SETUP_NAME,
                geodata_file=f"{DATA_DIR}/geodata.csv",
                mobility_file=f"{DATA_DIR}/mobility_big.txt",
                popnodes_key="population",
                nodenames_key="geoid",
            )
