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
# interventions:
#   scenarios:
#     <scenario name>:
#       template: Reduce
#       parameter: choose one - "alpha, sigma, gamma, r0"
#       period_start_date: <date>
#       period_end_date: <date>
#       value: <random distribution>
#       affected_geoids: <list of strings> optional
# ```
#
# If {template} is ReduceR0
# ```yaml
# interventions:
#   scenarios:
#     <scenario name>:
#       template: ReduceR0
#       period_start_date: <date>
#       period_end_date: <date>
#       value: <random distribution>
#       affected_geoids: <list of strings> optional
# ```
#
# If {template} is Stacked
# ```yaml
# interventions:
#   scenarios:
#     <scenario name>:
#       template: Stacked
#       scenarios: <list of scenario names>
# ```
#
# ### seeding
#
# If {seeding::method} is PoissonDistributed
# ```yaml
# seeding:
#   method: PoissonDistributed
#   lambda_file: <path to file>
# ```
#
# If {seeding::method} is FolderDraw
# ```yaml
# seeding:
#   method: FolderDraw
#   folder_path: \<path to dir\>; make sure this ends in a '/'
# ```
#
# ## Input Data
#
# * <b>{spatial_setup::base_path}/{spatial_setup::geodata}</b> is a csv with columns {spatial_setup::nodenames} and {spatial_setup::popnodes}
# * <b>{spatial_setup::base_path}/{spatial_setup::mobility}</b>
#
# If {seeding::method} is PoissonDistributed
# * {seeding::lambda_file}
#
# If {seeding::method} is FolderDraw
# * {seeding::folder_path}/[simulation ID].impa.csv
#
# ## Output Data
#
# * model_output/{spatial_setup::setup_name}_[scenario]/[simulation ID].seir.[csv/parquet]
# * model_parameters/{spatial_setup::setup_name}_[scenario]/[simulation ID].spar.[csv/parquet]
# * model_parameters/{spatial_setup::setup_name}_[scenario]/[simulation ID].snpi.[csv/parquet]


## @cond

import multiprocessing
import pathlib
import time

import click

from gempyor import seir, setup, file_paths
from gempyor.utils import config

# from .profile import profile_options


@click.command()
@click.option(
    "-c",
    "--config",
    "config_file",
    envvar=["COVID_CONFIG_PATH", "CONFIG_PATH"],
    type=click.Path(exists=True),
    required=True,
    help="configuration file for this simulation",
)
@click.option(
    "-s",
    "--scenario",
    "scenarios",
    envvar="COVID_SCENARIOS",
    type=str,
    default=[],
    multiple=True,
    help="override the scenario(s) run for this simulation [supports multiple scenarios: `-s Wuhan -s None`]",
)
@click.option(
    "-n",
    "--nsim",
    envvar="COVID_NSIMULATIONS",
    type=click.IntRange(min=1),
    help="override the # of simulation runs in the config file",
)
@click.option(
    "-i",
    "--index",
    envvar="COVID_INDEX",
    type=click.IntRange(min=1),
    default=1,
    show_default=True,
    help="The index of the first simulation",
)
@click.option(
    "-j",
    "--jobs",
    envvar="COVID_NJOBS",
    type=click.IntRange(min=1),
    default=multiprocessing.cpu_count(),
    show_default=True,
    help="the parallelization factor",
)
@click.option(
    "--stochastic/--non-stochastic",
    "--stochastic/--non-stochastic",
    "stoch_traj_flag",
    envvar="COVID_STOCHASTIC",
    type=bool,
    default=False,
    help="Flag determining whether to run stochastic simulations or not",
)
@click.option(
    "--in-id",
    "--in-id",
    "in_run_id",
    envvar="COVID_RUN_INDEX",
    type=str,
    default=file_paths.run_id(),
    show_default=True,
    help="Unique identifier for the run",
)  # Default does not make sense here
@click.option(
    "--out-id",
    "--out-id",
    "out_run_id",
    envvar="COVID_RUN_INDEX",
    type=str,
    default=file_paths.run_id(),
    show_default=True,
    help="Unique identifier for the run",
)
@click.option(
    "--interactive/--batch",
    default=False,
    help="run in interactive or batch mode [default: batch]",
)
@click.option(
    "--write-csv/--no-write-csv",
    default=False,
    show_default=True,
    help="write CSV output at end of simulation",
)
@click.option(
    "--write-parquet/--no-write-parquet",
    default=True,
    show_default=True,
    help="write parquet file output at end of simulation",
)
# @profile_options
def simulate(
    config_file,
    in_run_id,
    out_run_id,
    scenarios,
    nsim,
    jobs,
    interactive,
    write_csv,
    write_parquet,
    index,
    stoch_traj_flag,
):

    spatial_path_prefix = ""
    config.clear()
    config.read(user=False)
    config.set_file(config_file)
    spatial_config = config["spatial_setup"]
    spatial_base_path = spatial_config["base_path"].get()
    spatial_base_path = pathlib.Path(spatial_path_prefix + spatial_base_path)

    if not scenarios:
        scenarios = config["interventions"]["scenarios"].as_str_seq()
    print(f"Scenarios to be run: {', '.join(scenarios)}")

    if not nsim:
        nsim = config["nsimulations"].as_number()

    spatial_setup = setup.SpatialSetup(
        setup_name=spatial_config["setup_name"].get(),
        geodata_file=spatial_base_path / spatial_config["geodata"].get(),
        mobility_file=spatial_base_path / spatial_config["mobility"].get()
        if spatial_config["mobility"].exists()
        else None,
        popnodes_key=spatial_config["popnodes"].get(),
        nodenames_key=spatial_config["nodenames"].get(),
    )

    start = time.monotonic()
    for scenario in scenarios:

        s = setup.Setup(
            setup_name=config["name"].get() + "/" + str(scenario) + "/",
            spatial_setup=spatial_setup,
            nsim=nsim,
            npi_scenario=scenario,
            npi_config_seir=config["interventions"]["settings"][scenario],
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
            in_run_id=in_run_id,
            in_prefix=config["name"].get() + "/",
            out_run_id=out_run_id,
            out_prefix=config["name"].get() + "/" + str(scenario) + "/" + out_run_id + "/",
            stoch_traj_flag=stoch_traj_flag,
        )

        print(
            f"""
>> Scenario: {scenario} from config {config_file}
>> Starting {s.nsim} model runs beginning from {s.first_sim_index} on {jobs} processes
>> Setup *** {s.setup_name} *** from {s.ti} to {s.tf}
    """
        )
        seir.run_parallel_SEIR(s, config=config, n_jobs=jobs)
    print(f">> All runs completed in {time.monotonic() - start:.1f} seconds")


if __name__ == "__main__":
    simulate()

## @endcond
