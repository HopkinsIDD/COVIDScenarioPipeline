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

from SEIR import seir, setup, file_paths
from SEIR.utils import config, Timer
from SEIR.profile import profile_options
from Outcomes import outcomes
import numpy as np

config.set_file(config_path)

# config.set_file('config.yml')

spatial_config = config["spatial_setup"]
spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
scenario = scenario
deathrate = deathrate
stoch_traj_flag= stoch_traj_flag # Truthy: stochastic simulation, Falsy: determnistic mean of the binomial draws
nsim = 1
interactive = False
write_csv = False
write_parquet = True
try:
    rng_seed
except NameError:
    rng_seed = None

np.random.seed(rng_seed)


### Profile configuration
import cProfile
import pstats
from functools import wraps


def profile(output_file=None, sort_by='cumulative', lines_to_print=None, strip_dirs=False):
    """A time profiler decorator.
    Inspired by and modified the profile decorator of Giampaolo Rodola:
    http://code.activestate.com/recipes/577817-profile-decorator/
    Args:
        output_file: str or None. Default is None
            Path of the output file. If only name of the file is given, it's
            saved in the current directory.
            If it's None, the name of the decorated function is used.
        sort_by: str or SortKey enum or tuple/list of str/SortKey enum
            Sorting criteria for the Stats object.
            For a list of valid string and SortKey refer to:
            https://docs.python.org/3/library/profile.html#pstats.Stats.sort_stats
        lines_to_print: int or None
            Number of lines to print. Default (None) is for all the lines.
            This is useful in reducing the size of the printout, especially
            that sorting by 'cumulative', the time consuming operations
            are printed toward the top of the file.
        strip_dirs: bool
            Whether to remove the leading path info from file names.
            This is also useful in reducing the size of the printout
    Returns:
        Profile of the decorated function
    """

    def inner(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            _output_file = output_file or func.__name__ + '.prof'
            pr = cProfile.Profile()
            pr.enable()
            retval = func(*args, **kwargs)
            pr.disable()
            pr.dump_stats(_output_file)
            return retval

        return wrapper

    return inner

### Logger configuration
import logging
import os
logging.basicConfig(level=os.environ.get('COVID_LOGLEVEL', 'INFO').upper())
logger = logging.getLogger()
handler = logging.StreamHandler()
# '%(asctime)s %(name)-12s %(levelname)-8s %(message)s'
formatter = logging.Formatter("%(asctime)s [%(filename)s:%(lineno)s - %(funcName)20s() ] %(message)s")

handler.setFormatter(formatter)

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
    parameters_config=config["seir"]["parameters"],
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
>> Running ***{'STOCHASTIC' if stoch_traj_flag else 'DETERMINISTIC'}*** SEIR and Outcomes modules;
>> Setup {s.setup_name}; ti: {s.ti}; tf: {s.tf}; Scenario SEIR: {scenario}; Scenario Outcomes: {deathrate};
>> index: {s.first_sim_index}; run_id: {run_id}, prefix: {prefix};""")

setup_name = s.setup_name

#@profile()
def onerun_OUTCOMES_loadID(index):
    with Timer('onerun_OUTCOMES_loadID'):
        outcomes.onerun_delayframe_outcomes_load_hpar(config,
                                                        run_id, prefix, int(index), # input
                                                        run_id, prefix, int(index), # output
                                                        deathrate, stoch_traj_flag)
    return 1

#@profile()                                                   
def onerun_OUTCOMES(index):
    with Timer('onerun_OUTCOMES'):
        outcomes.run_delayframe_outcomes(config,
                                            run_id, prefix, int(index), # input
                                            run_id, prefix, int(index), # output
                                            deathrate, nsim=1, n_jobs=1, stoch_traj_flag = stoch_traj_flag)
    return 1

#@profile()
def onerun_SEIR_loadID(sim_id2write, s, sim_id2load):
    with Timer('onerun_SEIR_loadID'):
        seir.onerun_SEIR_loadID(int(sim_id2write), s, int(sim_id2load), stoch_traj_flag)
    return 1

#@profile()
def onerun_SEIR(sim_id2write, s):
    with Timer('onerun_SEIR'):
        seir.onerun_SEIR(int(sim_id2write), s, stoch_traj_flag)
    return 1
