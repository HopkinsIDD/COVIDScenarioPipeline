#!/usr/bin/env python

import click
import itertools
import json
import math
import os
import pathlib
import subprocess
import sys
import tarfile
from datetime import datetime, timezone, date
import yaml
from SEIR import file_paths

@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this run")
@click.option("--id","--id", "run_id", envvar="COVID_RUN_INDEX", type=str, default=file_paths.run_id(),
              help="Unique identifier for this run")
@click.option("-n", "--num-jobs", "num_jobs", type=click.IntRange(min=1, max=1000), default=None,
              help="number of output slots to generate")
@click.option("-j", "--sims-per-job", "sims_per_job", type=click.IntRange(min=1), default=None,
              help="the number of sims to run on each child job")
@click.option("-k", "--num-blocks", "num_blocks", type=click.IntRange(min=1), default=None,
              help="The number of sequential blocks of jobs to run; total sims per slot = sims-per-slot * num-blocks")
@click.option("-o", "--output", "outputs", multiple=True, default=["model_output", "model_parameters", "importation", "hospitalization"],
              show_default=True, help="The output directories whose contents are captured and saved in S3")
@click.option("-b", "--s3-bucket", "s3_bucket", type=str, default="idd-inference-runs", show_default=True,
              help="The S3 bucket to use for keeping state for the batch jobs")
@click.option("-d", "--job-definition", "batch_job_definition", type=str, default="Batch-CovidPipeline-Job", show_default=True,
              help="The name of the AWS Batch Job Definition to use for the job")
@click.option("-q", "--job-queue-prefix", "job_queue_prefix", type=str, default="Inference-JQ", show_default=True,
              help="The prefix string of the job queues we should use for this run")
@click.option("-r", "--restart-from-s3-bucket", "restart_from_s3_bucket", type=str, default=None,
              help="The location of an S3 run to use as the initial to the first block of the current run")
@click.option("-r", "--restart-from-run-id", "restart_from_run_id", type=str, default=None,
              help="The location of an S3 run to use as the initial to the first block of the current run")
@click.option("--stochastic/--non-stochastic", "--stochastic/--non-stochastic", "stochastic", envvar="COVID_STOCHASTIC", type=bool, default=True,
              help="Flag determining whether to run stochastic simulations or not")
@click.option("--stacked-max","--stacked-max", "max_stacked_interventions", envvar="COVID_MAX_STACK_SIZE", type=click.IntRange(min=350), default=5000,
              help="Maximum number of interventions to allow in a stacked intervention")
@click.option("--validation-end-date","--validation-end-date", "last_validation_date", envvar="VALIDATION_DATE", type=click.DateTime(formats=["%Y-%m-%d"]), default=str(date.today()),
              help="Last date to pull for ground truth data")
@click.option("-p","--pipepath", "csp_path", envvar="COVID_PATH", type=click.Path(exists=True), required=True,
              help="path to the COVIDScenarioPipeline directory")
@click.option("--data-path","--data-path", "data_path", envvar="DATA_PATH", type=click.Path(exists=True), required=True,
              help="path to the data directory")

def launch_batch(config_file, run_id, num_jobs, sims_per_job, num_blocks, outputs, s3_bucket, batch_job_definition, job_queue_prefix, restart_from_s3_bucket, restart_from_run_id, stochastic, max_stacked_interventions, last_validation_date, csp_path, data_path):

    config = None
    with open(config_file) as f:
        config = yaml.full_load(f)

    # A unique name for this job run, based on the config name and current time
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S")
    job_name = f"{config['name']}-{timestamp}"

    num_jobs, sims_per_job, num_blocks = autodetect_params(config, num_jobs=num_jobs, sims_per_job=sims_per_job,
                                                           num_blocks=num_blocks)

    # Update and save the config file with the number of sims to run
    if 'filtering' in config:
        config['filtering']['simulations_per_slot'] = sims_per_job
        if not os.path.exists(config['filtering']['data_path']):
            print(f"ERROR: filtering.data_path path {config['filtering']['data_path']} does not exist!")
            return 1
    else:
        print(f"WARNING: no filtering section found in {config_file}!")

    if restart_from_run_id is None:
        restart_from_run_id = run_id
    handler = BatchJobHandler(run_id, num_jobs, sims_per_job, num_blocks, outputs, s3_bucket, batch_job_definition, restart_from_s3_bucket, restart_from_run_id, stochastic, max_stacked_interventions, last_validation_date, csp_path, data_path)

    scenarios = config['interventions']['scenarios']
    p_death_names = config['outcomes']['scenarios']

    handler.launch(job_name, config_file, scenarios, p_death_names)

    (rc, txt) = subprocess.getstatusoutput(f"git checkout -b run_{job_name}")
    print(txt)
    return rc


def autodetect_params(config, *, num_jobs=None, sims_per_job=None, num_blocks=None):
    if num_jobs and sims_per_job and num_blocks:
        return (num_jobs, sims_per_job, num_blocks)

    if "filtering" not in config or "simulations_per_slot" not in config["filtering"]:
        raise click.UsageError("filtering::simulations_per_slot undefined in config, can't autodetect parameters")
    sims_per_slot = int(config["filtering"]["simulations_per_slot"])

    if num_jobs is None:
        num_jobs = config["nsimulations"]
        print(f"Setting number of output slots to {num_jobs} [via config file]")

    if sims_per_job is None:
        if num_blocks is not None:
            sims_per_job = int(math.ceil(sims_per_slot / num_blocks))
            print(f"Setting number of blocks to {num_blocks} [via num_blocks (-k) argument]")
            print(f"Setting sims per job to {sims_per_job} [via {sims_per_slot} simulations_per_slot in config]")
        else:
            geoid_fname = pathlib.Path(config["spatial_setup"]["base_path"]) / config["spatial_setup"]["geodata"]
            with open(geoid_fname) as geoid_fp:
                num_geoids = sum(1 for line in geoid_fp)

            # formula based on a simple regression of geoids (based on known good performant params)
            sims_per_job = max(60 - math.sqrt(num_geoids), 10)
            sims_per_job = 5 * int(math.ceil(sims_per_job / 5))  # multiple of 5

            num_blocks = int(math.ceil(sims_per_slot / sims_per_job))

            # now launch full sims:
            sims_per_job = sims_per_slot
            num_blocks = 1

            print(f"Setting sims per job to {sims_per_job} "
                  f"[estimated based on {num_geoids} geoids and {sims_per_slot} simulations_per_slot in config]")
            print(f"Setting number of blocks to {num_blocks} [via math]")

    if num_blocks is None:
        num_blocks = int(math.ceil(sims_per_slot / sims_per_job))
        print(f"Setting number of blocks to {num_blocks} [via {sims_per_slot} simulations_per_slot in config]")


    return (num_jobs, sims_per_job, num_blocks)


class BatchJobHandler(object):
    def __init__(self, run_id, num_jobs, sims_per_job, num_blocks, outputs, s3_bucket, batch_job_definition, restart_from_s3_bucket, restart_from_run_id, stochastic, max_stacked_interventions, last_validation_date, csp_path, data_path):
        self.run_id = run_id
        self.num_jobs = num_jobs
        self.sims_per_job = sims_per_job
        self.num_blocks = num_blocks
        self.outputs = outputs
        self.s3_bucket = s3_bucket
        self.batch_job_definition = batch_job_definition
        self.restart_from_s3_bucket = restart_from_s3_bucket
        self.restart_from_run_id = restart_from_run_id
        self.stochastic = stochastic
        self.max_stacked_interventions = max_stacked_interventions
        self.last_validation_date = last_validation_date
        self.csp_path = csp_path
        self.data_path = data_path

    def launch(self, job_name, config_file, scenarios, p_death_names):

        manifest = {}
        manifest['cmd'] = " ".join(sys.argv[:])
        manifest['job_name'] = job_name
        manifest['data_sha'] = subprocess.getoutput('git rev-parse HEAD')
        manifest['csp_sha'] = subprocess.getoutput('cd COVIDScenarioPipeline; git rev-parse HEAD')

        # Prepare to tar up the current directory, excluding any dvc outputs, so it
        # can be shipped to S3
        os.makedirs(f"{job_name}/metadata/", exist_ok=True)
        if False:
            tarfile_name = f"{job_name}/metadata/{job_name}.tar.gz"
            tar = tarfile.open(tarfile_name, "x:gz", dereference=True)
            for p in os.listdir('.'):
                if p == 'COVIDScenarioPipeline':
                    for q in os.listdir('COVIDScenarioPipeline'):
                        if not (q == 'packrat' or q == 'sample_data' or q == 'build' or q.startswith('.')):
                            tar.add(os.path.join('COVIDScenarioPipeline', q))
                        elif q == 'sample_data':
                            for r in os.listdir('COVIDScenarioPipeline/sample_data'):
                                if r != 'united-states-commutes':
                                    tar.add(os.path.join('COVIDScenarioPipeline', 'sample_data', r))
                elif not (p.startswith(".") or p.endswith("tar.gz") or p in self.outputs):
                    tar.add(p, filter=lambda x: None if os.path.basename(x.name).startswith('.') else x)
            tar.close()

        # Save the manifest file
        with open(f'{job_name}/metadata/manifest.json', 'x') as f:
            json.dump(manifest, f, indent=4)
    
        # Prepare and launch the num_jobs
        results_path = f"{job_name}/model_output"
        base_env_vars = [
                {"name": "RESULTS_PATH", "value": results_path},
                {"name": "COVID_CONFIG_PATH", "value": config_file},
                {"name": "COVID_NSIMULATIONS", "value": str(self.num_jobs)},
                {"name": "COVID_MAX_STACK_SIZE", "value": str(self.max_stacked_interventions)},
                {"name": "VALIDATION_DATE", "value": str(self.last_validation_date)},
                {"name": "SIMS_PER_JOB", "value": str(self.sims_per_job) },
                {"name": "COVID_SIMULATIONS_PER_SLOT", "value": str(self.sims_per_job) },
                {"name": "COVID_STOCHASTIC", "value": str(self.stochastic) },
                {"name": "DATA_PATH", "value": str(self.data_path) },
                {"name": "COVID_PATH", "value": str(self.csp_path) }
        ]

        for envar in base_env_vars:
            os.environ[envar["name"]] = envar["value"]
            print(f"""{envar["name"]} = {envar["value"]}""")

        with open(config_file) as f:
          config = yaml.full_load(f)

        for ctr, (s, d) in enumerate(itertools.product(scenarios, p_death_names)):
            cur_job_name = f"{job_name}_{s}_{d}"
            # Create first job
            cur_env_vars = base_env_vars.copy()
            cur_env_vars.append({"name": "COVID_SCENARIOS", "value": s})
            cur_env_vars.append({"name": "COVID_DEATHRATES", "value": d})
            cur_env_vars.append({"name": "COVID_PREFIX", "value": f"{config['name']}/{s}/{d}"})
            cur_env_vars.append({"name": "COVID_BLOCK_INDEX", "value": "1"})
            cur_env_vars.append({"name": "COVID_RUN_INDEX", "value": f"{self.run_id}"})
            if not (self.restart_from_s3_bucket is None):
                cur_env_vars.append({"name": "S3_LAST_JOB_OUTPUT", "value": self.restart_from_s3_bucket})
                cur_env_vars.append({"name": "COVID_OLD_RUN_INDEX", "value": f"{self.restart_from_run_id}"})
                cur_env_vars.append({"name": "COVID_IS_RESUME", "value": f"TRUE"})
            cur_env_vars.append({"name": "JOB_NAME", "value": f"{cur_job_name}_block0"})

            for envar in cur_env_vars:
                os.environ[envar["name"]] = envar["value"]
                print(f"""{envar["name"]} = {envar["value"]}""")

        
            export_str = "--export=ALL,"
            for envar in base_env_vars:
                export_str += f"""{envar["name"]}="{envar["value"]}","""
            for envar in cur_env_vars:
                export_str += f"""{envar["name"]}="{envar["value"]}","""
            export_str = export_str[:-1]

            # submit job (idea: use slumpy to get the "depend on")
            command = ["sbatch", export_str , f"--array=1-{self.num_jobs}", f"{self.csp_path}/slurm_batch/inference_job.run"]
            print(' '.join(command))
            subprocess.run(command,  check=True, shell=True)
            
            run_id_restart = self.run_id
            print(f"Launching {cur_job_name}...")
        if not (self.restart_from_s3_bucket is None):
            print(f"Resuming from run id is {self.restart_from_run_id} located in {self.restart_from_s3_bucket}")
        print(f"Final output will be: {results_path}/model_output/")
        print(f"Run id is {self.run_id}")


if __name__ == '__main__':
    launch_batch()
