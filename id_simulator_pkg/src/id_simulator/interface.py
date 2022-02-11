#!/usr/bin/env python

##
# interface.py defines handlers to the id_simulator epidemic module 
# (both (SEIR) and the pipeline outcomes module (Outcomes))
# so they can be used from R for inference.
# R folks needs to define start a python, and set some variable as follow
# ```R`
# reticulate::use_python(Sys.which(opt$python),require=TRUE)
# reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
# reticulate::py_run_string(paste0("run_id = '", opt$run_id, "'"))
# reticulate::import_from_path("SEIR", path=opt$pipepath)
# reticulate::import_from_path("Outcomes", path=opt$pipepath)
# reticulate::py_run_string(paste0("index = ", 1))
# reticulate::py_run_string(paste0("stoch_traj_flag = True"))
# reticulate::py_run_string(paste0("scenario = '", scenario, "'"))   # NPI Scenario
# reticulate::py_run_string(paste0("deathrate = '", deathrate, "'")) # Outcome Scenario
# reticulate::py_run_string(paste0("prefix = '", global_block_prefix, "'"))
# reticulate::py_run_file(paste(opt$pipepath, "minimal_interface.py", sep = '/'))
# ```
# This populate the namespace with four functions, with return value 1 if the
# function terminated.
# err < - py$onerun_SEIR_loadID(this_index, py$s, this_index)
# err <- py$onerun_OUTCOMES_loadID(this_index)  # err is one if the function
#

import pathlib
from id_simulator import seir, setup, file_paths
from id_simulator.utils import config, Timer, profile

# from id_simulator.profile import profile_options
from id_simulator import outcomes
import numpy as np

### Logger configuration
import logging
import os

logging.basicConfig(level=os.environ.get("COVID_LOGLEVEL", "INFO").upper())
logger = logging.getLogger()
handler = logging.StreamHandler()
# '%(asctime)s %(name)-12s %(levelname)-8s %(message)s'
formatter = logging.Formatter(
    "%(asctime)s [%(filename)s:%(lineno)s - %(funcName)20s() ] %(message)s"
)

handler.setFormatter(formatter)


class Simulator():
    def __init__(self, config_path, run_id = 'testrun', prefix='prefix', index=None, scenario='inference', deathrate='med', stoch_traj_flag=False, rng_seed = None):
        self.scenario = scenario
        self.deathrate = deathrate
        self.stoch_traj_flag = stoch_traj_flag  # Truthy: stochastic simulation, Falsy: determnistic mean of the binomial draws
        self.run_id = run_id
        self.prefix = prefix
        self.deathrate = deathrate


        # Config prep
        config.clear()
        config.read(user=False)
        config.set_file(config_path)
        spatial_config = config["spatial_setup"]
        spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
       
        np.random.seed(rng_seed)
        nsim = 1
        interactive = False
        write_csv = False
        write_parquet = True
        self.s = setup.Setup(
            setup_name=config["name"].get() + "_" + str(scenario),
            spatial_setup=setup.SpatialSetup(
                setup_name=spatial_config["setup_name"].get(),
                geodata_file=spatial_base_path / spatial_config["geodata"].get(),
                mobility_file=spatial_base_path / spatial_config["mobility"].get(),
                popnodes_key=spatial_config["popnodes"].get(),
                nodenames_key=spatial_config["nodenames"].get(),
            ),
            nsim=nsim,
            npi_scenario=scenario,
            npi_config=config["interventions"]["settings"][scenario],
            seeding_config=config["seeding"],
            initial_conditions_config=config["initial_conditions"],
            parameters_config=config["seir"]["parameters"],
            seir_config=config["seir"],
            ti=config["start_date"].as_date(),
            tf=config["end_date"].as_date(),
            interactive=interactive,
            write_csv=write_csv,
            write_parquet=write_parquet,
            dt=config["dt"].as_number(),
            first_sim_index=index,
            in_run_id=self.run_id,
            in_prefix=self.prefix,
            out_run_id=self.run_id,
            out_prefix=self.prefix,
        )

        print(
            f"""
        >> Running ***{'STOCHASTIC' if stoch_traj_flag else 'DETERMINISTIC'}*** simulation;
        >> Setup {self.s.setup_name}; index: {self.s.first_sim_index}; run_id: {self.run_id}, prefix: {self.prefix};""" # ti: {s.ti}; tf: {s.tf};
        )

        setup_name = self.s.setup_name
    
    # profile()
    def one_simulation(self, sim_id2write):
        with Timer("onerun_SEIR"):
            seir.onerun_SEIR(int(sim_id2write), self.s, self.stoch_traj_flag)

        with Timer("onerun_OUTCOMES"):
            outcomes.run_delayframe_outcomes(
                config,
                int(sim_id2write),
                self.run_id,
                self.prefix,  # input
                int(sim_id2write),
                self.run_id,
                self.prefix,  # output
                self.deathrate,
                nsim=1,
                n_jobs=1,
                stoch_traj_flag=self.stoch_traj_flag,
            )
        return 0
    
    # profile()
    def one_simulation_loadID(self, sim_id2write, sim_id2load):
        with Timer("onerun_SEIR_loadID"):
            seir.onerun_SEIR_loadID(int(sim_id2write), s, int(sim_id2load), self.stoch_traj_flag)

        with Timer("onerun_OUTCOMES_loadID"):
            outcomes.onerun_delayframe_outcomes_load_hpar(
                config,
                int(sim_id2write),
                self.run_id,
                self.prefix,  # input
                int(sim_id2write),  # TODO: check that this does the correct thing
                self.run_id,
                self.prefix,  # output
                self.deathrate,
                self.stoch_traj_flag,
            )

        return 0
