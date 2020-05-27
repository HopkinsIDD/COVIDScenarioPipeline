config_file = 'config_new_hosp.yml'
scenarios = ['low_death_rate']
nsim=1
jobs=1
interactive=True
write_csv=False
write_parquet=True
index=1

from SEIR.utils import config
import multiprocessing
import pathlib
import time


import click
config.set_file(config_file)


if not scenarios:
    scenarios = config["outcomes"]["scenarios"].as_str_seq()
print(f"Scenarios to be run: {', '.join(scenarios)}")

if not nsim:
    nsim = config["nsimulations"].as_number()

start = time.monotonic()
for scenario in scenarios:

    print(f"""
>> Scenario: {scenario}
>> Starting {nsim} model runs beginning from {index} on {jobs} processes
>> writing to folder : {2}
""")