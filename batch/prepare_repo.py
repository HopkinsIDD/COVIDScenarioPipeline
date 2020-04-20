#!/usr/bin/env python

import click
from datetime import datetime
import re
import subprocess

@click.command()
@click.option("-d", "--data-repo", "data_repo", type=str, required=True,
              help="The name of the HopkinsIDD/ repo whose data should be used for the run (e.g., COVID19_Minimal)")
@click.option("-u", "--user", "user", envvar="USER", required=True,
              help="The user who is kicking off this run")
@click.option("-c", "--config", "config_file", type=str, default="config.yml", show_default=True,
              help="The name of the config file in the data repo to use for the current run")
@click.option("-n", "--num-test-sims", "num_test_sims", type=click.IntRange(min=1), default=15, show_default=True,
              help="The number of local test simulations to run in the config")
def prepare_repo(data_repo, user, config_file, num_test_sims):

    # Create a new branch to track the run
    branch_name = f"run_{data_repo.lower()}_{user}_{datetime.today().strftime('%Y%m%d%H%M%S')}"
    print(f"Creating run branch named {branch_name}...")
    branch_proc = subprocess.Popen(["git", "checkout", "-b", branch_name], stdout=subprocess.PIPE)
    print(branch_proc.communicate()[0])

    # Import the data/ directory from the data_repo with dvc
    if not data_repo.endswith(".git"):
        data_repo = f"git@github.com:HopkinsIDD/{data_repo}.git"
    print(f"Importing data/ from {data_repo}...")
    import_proc = subprocess.Popen(["dvc", "import", data_repo, "data"], stdout=subprocess.PIPE)
    print(import_proc.communicate()[0])

    # Get the config file for the run
    print(f"Getting {config_file} from {data_repo}...")
    import_proc = subprocess.Popen(["dvc", "get", data_repo, config_file], stdout=subprocess.PIPE)
    print(import_proc.communicate()[0])

    print(f"Updating config file {config_file} to run {num_test_sims} simulations...")
    config = open(config_file).read()
    config = re.sub("nsimulations: \d+", "nsimulations: %d" % num_test_sims, config)
    with open(config_file, "w") as f:
        f.write(config)

    print("Committing data and config for run...")
    add_proc = subprocess.Popen(["git", "add", "data.dvc", config_file], stdout=subprocess.PIPE)
    print(add_proc.communicate()[0])
    commit_proc = subprocess.Popen(["git", "commit", "-m", "'Commiting data and config for run'"], stdout=subprocess.PIPE)
    print(commit_proc.communicate()[0])

    print(f"Branch {branch_name} is ready; execute 'run_dvc.sh {config_file}' to setup the commands for the batch run")


if __name__ == '__main__':
  prepare_repo()
