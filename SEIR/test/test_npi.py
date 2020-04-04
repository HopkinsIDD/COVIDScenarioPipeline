import datetime
import numpy as np
import pandas as pd
import os
import pytest
import confuse

from SEIR.NPI import ReduceR0

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"

class TestInterventionsReduceR0:
    def test_ReduceR0_constructor_trivial(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        test_result = ReduceR0(global_config = config["trivial"],npi_config = config["trivial"]["interventions"]["settings"]["Test"],geoids = ["10001"])
        assert type(test_result.get()).__module__ == 'pandas.core.frame'
        assert test_result.get().shape == (1,1)
        assert (test_result.get().keys() == [config["trivial"]["start_date"].as_date()]).all()
        assert (test_result.get().index.values == ["10001"]).all()
    def test_ReduceR0_constructor_geoidrage(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        test_result = ReduceR0(global_config = config["trivial"],npi_config = config["trivial"]["interventions"]["settings"]["Test"],geoids = ["10001","2020","40"])
        assert type(test_result.get()).__module__ == 'pandas.core.frame'
        assert test_result.get().shape == (3,1)
        assert (test_result.get().keys() == [config["trivial"]["start_date"].as_date()]).all()
        assert (test_result.get().index.values == ["10001","2020","40"]).all()
    def test_ReduceR0_constructor_daterange(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        test_result = ReduceR0(global_config = config["trivial"],npi_config = config["daterange"]["interventions"]["settings"]["Test"],geoids = ["10001"])
        assert type(test_result.get()).__module__ == 'pandas.core.frame'
        assert test_result.get().shape == (1,1)
        assert (test_result.get().keys() == pd.date_range(config["trivial"]["start_date"].as_date(), config["trivial"]["end_date"].as_date())).all()
        assert (test_result.get().index.values == ["10001"]).all()
    def test_ReduceR0_constructor_fullrange(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        test_result = ReduceR0(global_config = config["trivial"],npi_config = config["daterange"]["interventions"]["settings"]["Test"],geoids = ["10001","2020","40"])
        assert type(test_result.get()).__module__ == 'pandas.core.frame'
        assert test_result.get().shape == (3,1)
        assert (test_result.get().keys() == pd.date_range(config["trivial"]["start_date"].as_date(), config["trivial"]["end_date"].as_date())).all()
        assert (test_result.get().index.values == ["10001","2020","40"]).all()
