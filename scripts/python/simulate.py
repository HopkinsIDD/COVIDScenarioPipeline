#!/usr/bin/env python
import multiprocessing
import pathlib
import time

import click

from SEIR import seir, setup
from SEIR.utils import config
from SEIR.profile import profile_options


@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this simulation")
@click.option("-s", "--scenario", "scenarios", type=str, default=[], multiple=True,
              help="override the scenario(s) run for this simulation [supports multiple scenarios: `-s Wuhan -s None`]")
@click.option("-n", "--nsim", type=click.IntRange(min=1),
              help="override the # of simulation runs in the config file")
@click.option("-j", "--jobs", type=click.IntRange(min=1),
              default=multiprocessing.cpu_count(), show_default=True,
              help="the parallelization factor")
@click.option("--interactive/--batch", default=False,
              help="run in interactive or batch mode [default: batch]")
@click.option("--write-csv/--no-write-csv", default=True, show_default=True,
              help="write CSV output at end of simulation")
@profile_options
def simulate(config_file, scenarios, nsim, jobs, interactive, write_csv):
    config.set_file(config_file)

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())

    if not scenarios:
        scenarios = config["interventions"]["scenarios"].as_str_seq()
    print(f"Scenarios to be run: {', '.join(scenarios)}")

    if not nsim:
        nsim = config["nsimulations"].as_number()

    start = time.monotonic()
    for scenario in scenarios:
        s = setup.Setup(setup_name=config["name"].get() + "_" + str(scenario),
                        spatial_setup=setup.SpatialSetup(
                            setup_name=spatial_config["setup_name"].get(),
                            folder=spatial_base_path.as_posix(),
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
                        dt=config["dt"].as_number())
        try:
            s.load_filter(config["dynfilter_path"].get())
            print(' We are using a filter')
        except:
            print('No filter used')

        print(f"""
>> Scenario: {scenario}
>> Starting {s.nsim} model runs on {jobs} processes
>> Setup *** {s.setup_name} *** from {s.ti} to {s.tf}
>> writing to folder : {s.datadir}{s.setup_name}
    """)

        seir.run_parallel(s, n_jobs=jobs)
    print(f">> All runs completed in {time.monotonic() - start:.1f} seconds")


if __name__ == "__main__":
    simulate()
