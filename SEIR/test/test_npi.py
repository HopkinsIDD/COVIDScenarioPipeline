import datetime
import numpy as np
import pandas as pd
import os
import pytest
import confuse

from SEIR.NPI import ReduceR0, Reduce

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"

class TestInterventionsReduceR0:
    def test_ReduceR0_constructor_trivial(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        global_config = config["trivial_global"]
        test_result = ReduceR0(global_config = global_config,
                                npi_config = config["trivial"],
                                geoids = ["10001"])
        assert type(test_result.getReduction('r0')).__module__ == 'pandas.core.frame'
        assert test_result.getReduction('r0').shape == (1,1)
        assert (test_result.getReduction('r0').keys() == [global_config["start_date"].as_date()]).all()
        assert (test_result.getReduction('r0').index.values == ["10001"])

    def test_ReduceR0_constructor_geoidrange(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        global_config = config["trivial_global"]
        test_result = ReduceR0(global_config = global_config,
                                npi_config = config["geoidrange"],
                                geoids = ["10001","2020","40"])

        reduction = test_result.getReduction('r0')
        assert type(reduction).__module__ == 'pandas.core.frame'
        assert reduction.shape == (3,1)
        assert (reduction.keys() == [global_config["start_date"].as_date()]).all()
        assert (reduction.index.values == ["10001","2020","40"]).all()
        assert (reduction.index.values == ["10001", "2020", "40"]).all()
        assert (reduction.loc[["10001", "2020"]] == 0.5).all(axis=None)
        assert (reduction.loc["40"] == 0).all()

    def test_ReduceR0_constructor_daterange(self):
        # Test period date range
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        global_config = config["month_global"]
        npi_config = config["daterange"]
        test_result = ReduceR0(global_config = global_config,
                                npi_config = npi_config,
                                geoids = ["10001"])
        reduction = test_result.getReduction('r0')

        global_start_date = global_config["start_date"].as_date()
        global_end_date = global_config["end_date"].as_date()
        period_start_date = npi_config["period_start_date"].as_date()
        period_end_date = npi_config["period_end_date"].as_date()
        assert type(reduction).__module__ == 'pandas.core.frame'
        assert reduction.shape == (1,32)
        assert (reduction.keys() == pd.date_range(global_start_date, global_end_date)).all()
        assert (reduction.index.values == ["10001"])

        # Test values in date ranges
        assert (reduction[pd.date_range(global_start_date, period_start_date, closed="left")] == 0).all(axis=None)
        assert (reduction[pd.date_range(period_start_date, period_end_date)] == 0.5).all(axis=None)
        assert (reduction[pd.date_range(period_end_date, global_end_date, closed="right")] == 0).all(axis=None)

    def test_ReduceR0_constructor_fullrange(self):
        # Test affected geoids and period date range
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        global_config = config["month_global"]
        npi_config = config["fullrange"]
        test_result = ReduceR0(global_config = global_config,
                                npi_config = npi_config,
                                geoids = ["10001","2020","40"])
        reduction = test_result.getReduction('r0')

        assert type(reduction).__module__ == 'pandas.core.frame'
        assert reduction.shape == (3,32)
        assert (reduction.keys() == pd.date_range(global_config["start_date"].as_date(),
                                                                        global_config["end_date"].as_date())).all()
        assert (reduction.index.values == ["10001","2020","40"]).all()
        assert (reduction.loc[["10001","2020"],pd.date_range(npi_config["period_start_date"].as_date(),
                                                                npi_config["period_end_date"].as_date())] == 0.5).all(axis=None)

class TestInterventionsReduce:
    def test_Reduce_constructor_trivial(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        global_config = config["trivial_global"]
        test_result = Reduce(global_config = global_config,
                                npi_config = config["trivial"],
                                geoids = ["10001"])
        reduction = test_result.getReduction('gamma')

        assert type(reduction).__module__ == 'pandas.core.frame'
        assert reduction.shape == (1,1)
        assert (reduction.keys() == [global_config["start_date"].as_date()])
        assert (reduction.index.values == ["10001"]).all()
        assert (reduction == 0.5).all(axis=None)

    def test_Reduce_constructor_geoidrange(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        test_result = Reduce(global_config = config["trivial_global"],
                                npi_config = config["geoidrange"],
                                geoids = ["10001","2020","40"])
        reduction = test_result.getReduction('gamma')

        assert reduction.shape == (3,1)
        assert (reduction.index.values == ["10001", "2020", "40"]).all()
        assert (reduction.loc[["10001", "2020"]] == 0.5).all(axis=None)
        assert (reduction.loc["40"] == 0).all()

    def test_Reduce_constructor_daterange(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        global_config = config["month_global"]
        npi_config = config["daterange"]
        test_result = Reduce(global_config = global_config,
                                npi_config = npi_config,
                                geoids = ["10001"])
        reduction = test_result.getReduction('gamma')

        global_start_date = global_config["start_date"].as_date()
        global_end_date = global_config["end_date"].as_date()
        period_start_date = npi_config["period_start_date"].as_date()
        period_end_date = npi_config["period_end_date"].as_date()

        assert type(reduction).__module__ == 'pandas.core.frame'
        assert reduction.shape == (1,32)
        assert (reduction.keys() == pd.date_range(global_start_date, global_end_date)).all()
        assert (reduction.index.values == ["10001"])

        # Test values in date ranges
        assert (reduction[pd.date_range(global_start_date, period_start_date, closed="left")] == 0).all(axis=None)
        assert (reduction[pd.date_range(period_start_date, period_end_date)] == 0.5).all(axis=None)
        assert (reduction[pd.date_range(period_end_date, global_end_date, closed="right")] == 0).all(axis=None)

    def test_Reduce_constructor_fullrange(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        global_config = config["month_global"]
        npi_config = config["fullrange"]
        test_result = Reduce(global_config = global_config,
                                npi_config = npi_config,
                                geoids = ["10001","2020","40"])
        reduction = test_result.getReduction('gamma')

        assert type(reduction).__module__ == 'pandas.core.frame'
        assert reduction.shape == (3,32)
        assert (reduction.keys() == pd.date_range(global_config["start_date"].as_date(),
                                                    global_config["end_date"].as_date())).all()
        assert (reduction.index.values == ["10001","2020","40"]).all()
        assert (reduction.loc[["10001","2020"],pd.date_range(npi_config["period_start_date"].as_date(),
                                                                npi_config["period_end_date"].as_date())] == 0.5).all(axis=None)

    def test_Reduce_constructor_toobig(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        with pytest.raises(ValueError, match=r'.*greater than.*'):
            test_result = Reduce(global_config = config["trivial_global"],
                                    npi_config = config["toobig"],
                                    geoids = ["10001","2020","40"])

    def test_Reduce_invalid_parameter(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        with pytest.raises(ValueError, match=r'Invalid parameter name.*'):
            test_result = Reduce(global_config = config["trivial_global"],
                                    npi_config = config["invalid_parameter"],
                                    geoids = ["10001","2020","40"])

    def test_Reduce_bad_start_date(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        with pytest.raises(ValueError, match=r'.*not between global dates.*'):
            test_result = Reduce(global_config = config["trivial_global"],
                                    npi_config = config["bad_start_date"],
                                    geoids = ["10001","2020","40"])

    def test_Reduce_bad_end_date(self):
        config.set_file(f"{DATA_DIR}/test_ReduceR0_trivial.yml")
        with pytest.raises(ValueError, match=r'.*not between global dates.*'):
            test_result = Reduce(global_config = config["trivial_global"],
                                    npi_config = config["bad_end_date"],
                                    geoids = ["10001","2020","40"])


# TODO Stacked Tests
