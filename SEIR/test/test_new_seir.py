import numpy as np
import os
import pytest
import warnings
import shutil

import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
from functools import reduce

from SEIR import setup, seir, NPI, file_paths, compartments

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"


def test_constant_population():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(setup_name="test_seir",
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_seir",
                    spatial_setup=ss,
                    nsim=1,
                    npi_scenario="None",
                    npi_config=config["interventions"]["settings"]["None"],
                    parameters_config=config["seir"]["parameters"],
                    seeding_config={},
                    initial_conditions_config=config["initial_conditions"],
                    ti=config["start_date"].as_date(),
                    tf=config["end_date"].as_date(),
                    interactive=True,
                    write_csv=False,
                    dt=0.25)

    initial_conditions = s.seedingAndIC.draw_ic(sim_id=0, setup=s)
    seeding_data = s.seedingAndIC.draw_seeding(sim_id=0, setup=s)
    ## This function needs to be written, but even when it is, it won't work with this config
    ## Because the seeding isn't working
    ## The seeding is all done manually
    # seeding_data, seeding_starts = s.get_seeding(0)

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    # parameters = setup.parameters_quick_draw(s.parameters, len(s.t_inter), s.nnodes)
    # parameters = setup.parameters_reduce(parameters, npi, s.dt)
    parameters = s.parameters.parameters_quick_draw(nt_inter=s.n_days, nnodes=s.nnodes)
    parameter_names = [x for x in s.parameters.pnames]

    parsed_parameters, unique_strings, transition_array, proportion_array, proportion_info = \
        s.compartments.get_transition_array(parameters, parameter_names)

    assert (type(s.compartments.compartments.shape[0]) == int)
    assert (type(s.nnodes) == int)
    assert (s.n_days > 1)
    assert (parsed_parameters.shape == (5, s.n_days, s.nnodes))
    assert (type(s.dt) == float)
    assert (transition_array.shape == (5, 5))
    assert (type(transition_array[0][0]) == np.int64)
    assert (proportion_array.shape == (9,))
    assert (type(proportion_array[0]) == np.int64)
    assert (proportion_info.shape == (2, 6))
    assert (type(proportion_info[0][0]) == np.int64)
    assert (initial_conditions.shape == (s.compartments.compartments.shape[0], s.nnodes))
    assert (type(initial_conditions[0][0]) == np.float64)
    # Test of empty seeding:
    assert len(seeding_data.keys()) == 5
    keys_ref = ['seeding_sources', 'seeding_destinations', 'seeding_places', 'seeding_amounts', 'day_start_idx']
    for key, item in seeding_data.items():
        assert key in keys_ref
        if key == 'day_start_idx':
            assert (len(item) == s.n_days + 1)
            assert (item == np.zeros(s.n_days + 1, dtype=np.int64)).all()
        else:
            assert item.size == 0# == np.array([], dtype=np.int64)
        assert item.dtype == np.int64


    assert (len(mobility_data) > 0)
    assert (type(mobility_data[0]) == np.float64)
    assert (len(mobility_geoid_indices) == s.nnodes)
    assert (type(mobility_geoid_indices[0]) == np.int32)
    assert (len(mobility_data_indices) == s.nnodes + 1)
    assert (type(mobility_data_indices[0]) == np.int32)
    assert (len(s.popnodes) == s.nnodes)
    assert (type(s.popnodes[0]) == np.int64)

    # print(s.compartments.transitions["proportional_to"])
    # print(s.compartments.compartments)

    print(transition_array)
    print(proportion_array)
    print(proportion_info)

    states = seir.steps_SEIR_nb(
        s.compartments.compartments.shape[0], s.nnodes, s.n_days,  # 1 #2 #3
        parsed_parameters, s.dt,  # 4 #5
        transition_array, proportion_array, proportion_info,  # transitions #6 #7
        initial_conditions,  # initial_conditions #8
        seeding_data,  # seeding #9
        mobility_data, mobility_geoid_indices, mobility_data_indices,  # mobility  #11 #12 #13
        s.popnodes, True)  # 14 #15



    #assert # TODO


