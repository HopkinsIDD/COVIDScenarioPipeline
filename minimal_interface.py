#!/usr/bin/env python

##
# @file
# @brief Runs hospitalization model
#
# @details
#
# ## Configuration Items
#
# ```yaml
# name: <string>
# start_date: <date>
# end_date: <date>
# dt: float
# dynfilter_path: <path to file> optional. Will not do filter step if not present
# nsimulations: <integer> overridden by the -n/--nsim script parameter
# spatial_setup:
#   setup_name: <string>
#   base_path: <path to directory>
#   geodata: <path to file>
#   mobility: <path to file>
#   nodenames: <string>
#   popnodes: <string>
#
# seir:
#   parameters
#     alpha: <float>
#     sigma: <float>
#     gamma: <random distribution>
#     R0s: <random distribution>
#
# interventions:
#   scenarios:
#     - <scenario 1 name>
#     - <scenario 2 name>
#     - ...
#   settings:
#     <scenario 1 name>:
#       template: choose one - "Reduce", ReduceR0", "Stacked"
#       ...
#     <scenario 2 name>:
#       template: choose one - "Reduce", "ReduceR0", "Stacked"
#       ...
#
# seeding:
#   method: choose one - "PoissonDistributed", "FolderDraw"
# ```
#
# ### interventions::scenarios::settings::<scenario name>
#
# If {template} is ReduceR0
# ```yaml
import multiprocessing
import pathlib
import time

import click

from COVIDScenarioPipeline.SEIR import seir, setup
from COVIDScenarioPipeline.SEIR.utils import config
from COVIDScenarioPipeline.SEIR.profile import profile_options

config.set_file(config_path)

#config.set_file('config.yml')

spatial_config = config["spatial_setup"]
spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
scenario = scenario
nsim = 10
interactive = False
write_csv = False
write_parquet = True
index = 1



s = setup.Setup(setup_name=config["name"].get() + "_" + str(scenario),
                        spatial_setup=setup.SpatialSetup(
                            setup_name=spatial_config["setup_name"].get(),
                            geodata_file=spatial_base_path / spatial_config["geodata"].get(),
                            mobility_file=spatial_base_path / spatial_config["mobility"].get(),
                            popnodes_key=spatial_config["popnodes"].get(),
                            nodenames_key=spatial_config["nodenames"].get()
                        ),
                        nsim=nsim,
                        npi_scenario=scenario,
                        npi_config=config["interventions"]["settings"][scenario],
                        seeding_config=config["seeding"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=interactive,
                        write_csv=write_csv,
                        write_parquet=write_parquet,
                        dt=config["dt"].as_number(),
                        first_sim_index = index)


print(f"""
>> Scenario: {scenario}
>> Starting {s.nsim} model runs beginning from {s.first_sim_index}
>> Setup *** {s.setup_name} *** from {s.ti} to {s.tf}
>> writing to folder : {s.datadir}{s.setup_name}
    """)

onerun_SEIR_loadID = lambda sim_id2write, s, sim_id2load: seir.onerun_SEIR_loadID(int(sim_id2write), s, int(sim_id2load))
onerun_SEIR = lambda sim_id2write, s: seir.onerun_SEIR(int(sim_id2write), s)
