import numpy as np
import os
import pytest

from SEIR import setup, seir, NPI

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"

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
    seeding[:,0] = 1

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)
    npi = npi.get().T

    for i in range(100):
        states = seir.steps_SEIR_nb(setup.parameters_quick_draw(s, npi),
                           seeding, 1234, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices, mobility_data, s.dynfilter)


        assert states[seir.cumI][1].max() > 0

