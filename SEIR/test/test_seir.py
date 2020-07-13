import numpy as np
import os
import pytest
import warnings


from SEIR import setup, seir, NPI

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"



def test_check_values():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(setup_name="test_values",
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_values",
                        spatial_setup=ss,
                        nsim=1,
                        npi_scenario="None",
                        npi_config=config["interventions"]["settings"]["None"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    with warnings.catch_warnings(record=True) as w:

        seeding = np.zeros((len(s.t_inter), s.nnodes))

        if np.all(seeding == 0):
            warnings.warn("provided seeding has only value 0", UserWarning)

        seeding[0,0] = 1

        if np.all(seeding == 0):
            warnings.warn("provided seeding has only value 0", UserWarning)

        if(np.all(s.mobility.data < 1)):
            warnings.warn("highest mobility value is less than 1", UserWarning)

        s.mobility.data[0] = 0.8
        s.mobility.data[1] = 0.5

        if(np.all(s.mobility.data < 1)):
            warnings.warn("highest mobility value is less than 1", UserWarning)

        assert(len(w) == 2)
        assert(issubclass(w[0].category, UserWarning))
        assert(issubclass(w[1].category, UserWarning))
        assert("seeding" in str(w[0].message))
        assert("mobility" in str(w[1].message))


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
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    seeding = np.zeros((len(s.t_inter), s.nnodes))


    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    parameters = setup.parameters_quick_draw(config["seir"]["parameters"], len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(parameters, npi, s.dt)

    states = seir.steps_SEIR_nb(*parameters,
                       seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                       mobility_geoid_indices, mobility_data_indices, mobility_data, s.dynfilter)

    completepop = s.popnodes.sum()
    origpop = s.popnodes
    for it in range(len(s.t_inter)):
        totalpop = 0
        for i in range(s.nnodes):
            totalpop += states[:5, i, it].sum()
            #Sum of S, E, I#, R for the geoid that is 'i'
            assert(origpop[i] == states[:5, i, it].sum())
        assert(completepop == totalpop)


def test_steps_SEIR_nb_simple_spread():
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
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    seeding = np.zeros((len(s.t_inter), s.nnodes))
    seeding[:,0] = 100

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    parameters = setup.parameters_quick_draw(config["seir"]["parameters"], len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(parameters, npi, s.dt)

    for i in range(100):
        states = seir.steps_SEIR_nb(*parameters,
                           seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices, mobility_data, s.dynfilter)


        assert states[seir.cumI][1].max() > 0

def test_steps_SEIR_no_spread():
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
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    seeding = np.zeros((len(s.t_inter), s.nnodes))
    seeding[:,0] = 100

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data * 0

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    parameters = setup.parameters_quick_draw(config["seir"]["parameters"], len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(parameters, npi, s.dt)

    for i in range(100):
        states = seir.steps_SEIR_nb(*parameters,
                           seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices, mobility_data, s.dynfilter)


        assert states[seir.cumI][1].max() == 0
