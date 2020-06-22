#!/usr/bin/env python

##
# @file
# @brief Runs outcomes model after an SEIR run
#
# @details
#
# ## Configuration Items
#
# ```yaml
#outcomes:
#  method: delayframe                   # Only fast is supported atm. Makes fast delay_table computations. Later agent-based method ?
#  paths:
#    param_from_file: TRUE               #
#    param_place_file: <path.csv>       # OPTIONAL: File with param per csv. For each param in this file 
#  scenarios:                           # Outcomes scenarios to run
#    - low_death_rate
#    - mid_death_rate
#  settings:                            # Setting for each scenario
#    low_death_rate:
#      new_comp1:                               # New compartement name 
#        source: incidence                      # Source of the new compartement: either an previously defined compartement or "incidence" for diffI of the SEIR
#        probability:  <random distribution>           # Branching probability from source
#        delay: <random distribution>                  # Delay from incidence of source to incidence of new_compartement
#        duration: <random distribution>               # OPTIONAL ! Duration in new_comp. If provided, the model add to it's output "new_comp1_curr" with current amount in new_comp1
#      new_comp2:                               # Example for a second compatiment
#        source: new_comp1                      
#        probability: <random distribution> 
#        delay: <random distribution> 
#        duration: <random distribution>
#      death_tot:                               # Possibility to combine compartements for death.
#        sum: ['death_hosp', 'death_ICU', 'death_incid']
#         
#    mid_death_rate:
#      ...
#
# ## Input Data
#
# * <b>{param_place_file}</b> is a csv with columns place, parameter, value. Parameter is constructed as, e.g for comp1:
#                probability: Pnew_comp1|source
#                delay:       Dnew_comp1
#                duration:    Lnew_comp1


# ## Output Data
# * {output_path}/model_output/{spatial_setup::setup_name}_[scenario]/[simulation ID].hosp.parquet



## @cond
import multiprocessing
import pathlib
import time, os

import click

from SEIR import file_paths
from SEIR.utils import config 
from Outcomes import outcomes


@click.command()
@click.option("-c", "--config", "config_file", envvar=["COVID_CONFIG_PATH", "CONFIG_PATH"], type=click.Path(exists=True), required=True,
              help="configuration file for this simulation")
@click.option("-s", "--scenario", "scenarios_seir", envvar="COVID_SCENARIOS", type=str, default=[], multiple=True,
              help="override the scenario(s) run for this simulation [supports multiple scenarios: `-s Wuhan -s None`]")
@click.option("-d", "--scenarios_outcomes", "scenarios_outcomes", envvar="COVID_DEATHRATES", type=str, default=[], multiple=True,
              help="Scenario of outcomes to run")
@click.option("-n", "--nsim", envvar="COVID_NSIMULATIONS", type=click.IntRange(min=1),
              help="override the # of outcomes simulation to run runs in the config file")
@click.option("-i", "--index", envvar="COVID_INDEX", type=click.IntRange(min=1),
              default=1, show_default=True,
              help="he index of the first simulation to run against")
@click.option("-j", "--jobs", envvar="COVID_NJOBS", type=click.IntRange(min=1),
              default=multiprocessing.cpu_count(), show_default=True,
              help="the parallelization factor")
@click.option("-I","--id", "run_id", envvar="COVID_RUN_INDEX", type = str, default=file_paths.run_id(),show_default=True, help= "unique identifier for the run")
@click.option("--prefix","--prefix", "prefix", envvar="COVID_PREFIX", type = str, default=None, show_default=True, help= "unique identifier for the run")

def simulate(config_file, run_id, prefix, scenarios_seir, scenarios_outcomes, nsim, jobs,index):
    config.set_file(config_file)
    if not scenarios_outcomes:
        scenarios_outcomes = config["outcomes"]["scenarios"].as_str_seq()
    print(f"Outcomes scenarios to be run: {', '.join(scenarios_outcomes)}")

    if not scenarios_seir:
        scenarios_seir = config["interventions"]["scenarios"].as_str_seq()
    print(f"SEIR Scenarios to be run: {', '.join(scenarios_seir)}")

    if not nsim:
        nsim = config["nsimulations"].as_number()


    start = time.monotonic()
    for scenario_seir in scenarios_seir:
        for scenario_outcomes in scenarios_outcomes:
            if prefix is None:
                prefix = config["name"].get() + "_" + str(scenario_seir) + "_" + str(scenario_outcomes)
            setup_name = config["name"].get() + "_" + str(scenario_seir)
            #outdir = f'model_output/outcomes/{setup_name}/'
            outdir = f'hospitalization/model_output/{setup_name}/'
            os.makedirs(outdir, exist_ok=True)

            print(f"""
>> Scenario: {scenario_seir} -- {scenario_outcomes} 
>> Starting {nsim} model runs beginning from {index} on {jobs} processes
>> writing to folder : {outdir}
    """)
            if (config["outcomes"]["method"].get() == 'delayframe'):
                outcomes.run_delayframe_outcomes(config, 
                            run_id,
                            prefix,
                            scenario_outcomes,
                            config["outcomes"]["param_place_file"],
                            nsim, 
                            index,
                            jobs)
            else:
                 raise ValueError(f"Only method 'delayframe' is supported at the moment.")


    print(f">> All runs completed in {time.monotonic() - start:.1f} seconds")


if __name__ == "__main__":
    simulate()

## @endcond
