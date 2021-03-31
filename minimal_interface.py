#!/usr/bin/env python

##
# mininimal_interface.py defines handlers to the CSP epidemic module (SEIR) and the pipeline outcomes module (Outcomes)
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
from SEIR import seir, setup, file_paths
from SEIR.utils import config
from Outcomes import outcomes



config.set_file(config_path)

spatial_config = config["spatial_setup"]
spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
scenario = scenario
deathrate = deathrate
stoch_traj_flag= stoch_traj_flag # Truthy: stochastic simulation, Falsy: determnistic mean of the binomial draws
nsim = 10
interactive = False
write_csv = False
write_parquet = True



s = setup.Setup(
    setup_name=config["name"].get() + "_" + str(scenario),
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
    first_sim_index = index,
    in_run_id = run_id,
    in_prefix = prefix,
    out_run_id = run_id,
    out_prefix = prefix
)

print(f"""
>> Running ***{'STOCHASTIC' if stoch_traj_flag else 'DETERMINISTIC'}*** SEIR and Outcomes modules
>> Scenario: {scenario}
>> Starting {s.nsim} model runs beginning from {s.first_sim_index}
>> Setup *** {s.setup_name} *** from {s.ti} to {s.tf}
>> writing to folder : {s.datadir}{s.setup_name}
    """)

setup_name = s.setup_name
print(scenario, deathrate, index, run_id, prefix)
onerun_OUTCOMES_loadID = lambda index: outcomes.onerun_delayframe_outcomes_load_hpar(config,
                                                                                     run_id, prefix, int(index), # input
                                                                                     run_id, prefix, int(index), # output
                                                                                     deathrate, stoch_traj_flag)
onerun_OUTCOMES = lambda index: outcomes.run_delayframe_outcomes(config,
                                                                 run_id, prefix, int(index), # input
                                                                 run_id, prefix, int(index), # output
                                                                 deathrate, nsim=1, n_jobs=1, stoch_traj_flag = stoch_traj_flag)
onerun_SEIR_loadID = lambda sim_id2write, s, sim_id2load: seir.onerun_SEIR_loadID(int(sim_id2write), s, int(sim_id2load), stoch_traj_flag)
onerun_SEIR = lambda sim_id2write, s: seir.onerun_SEIR(int(sim_id2write), s, stoch_traj_flag)
