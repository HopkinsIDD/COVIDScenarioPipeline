import multiprocessing
import pathlib
import time

import click
import confuse

from SEIR import seir, setup


@click.command()
@click.option("-c", "--config", "config_file", type=click.Path(exists=True), required=True,
              help="configuration file for this simulation")
@click.option("-s", "--scenario", type=str, required=True,
              help="the scenario to run for this simulation")
@click.option("-n", "--nsim", type=int, required=True,
              default=1000, show_default=True,
              help="the # of model runs")
@click.option("-j", "--jobs", type=int,
              default=multiprocessing.cpu_count(), show_default=True,
              help="the parallelization factor")
@click.option("--interactive/--batch", default=False,
              help="run in interactive or batch mode")
def simulate(config_file, scenario, nsim, jobs, interactive):
    config = confuse.Configuration("")
    config.set_file(config_file)

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())

    s = setup.Setup(setup_name=config["name"].get() + "_" + str(nsim),
                    spatial_setup=setup.SpatialSetup(
                        setup_name=spatial_config["setup_name"].get(),
                        folder=spatial_base_path.as_posix(),
                        geodata_file=spatial_base_path / spatial_config["geodata"].get(),
                        mobility_file=spatial_base_path / spatial_config["mobility"].get(),
                        popnodes_key=spatial_config["popnodes"].get(),
                    ),
                    nsim=nsim,
                    script_npi=(pathlib.Path(config["interventions"]["base_path"].get()) /
                                config["interventions"][scenario].get()).as_posix(),
                    ti=config["start_date"].get(),
                    tf=config["end_date"].get(),
                    interactive=interactive,
                    write_csv=True,
                    dt=config["dt"].as_number())

    s.load_filter(config["dynfilter_path"].get())

    print(f"""

>> Starting {s.nsim} model runs on {jobs} processes")
>> Setup *** {s.setup_name} *** from {s.ti} to {s.tf}")
>> writing to folder : {s.datadir}{s.setup_name}

""")

    start = time.monotonic()
    seir.run_parallel(s, jobs)
    print(f">> Runs done in {time.monotonic() - start} seconds...")


if __name__ == "__main__":
    simulate()
