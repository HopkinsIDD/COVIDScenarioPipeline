#!/usr/bin/env python

import boto3
import click
import glob
import os
import re
import subprocess
import tarfile
import time
import yaml

@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this run")
@click.option("-n", "--num-slots", "num_slots"", type=click.IntRange(min=1, max=1000), required=True,
              help="number of output slots to generate")
@click.option("-j", "--sims-per-slot", "sims_per_slot", type=click.IntRange(min=1), default=10, show_default=True,
              help="the number of sims to run on each child job")
@click.option("-k", "--num-blocks", "num_blocks", type=click.intRange(min=1), default=10, show_default=True,
              help="The number of sequential blocks of jobs to run; total sims per slot = sims-per-slot * num-blocks")
@click.option("-t", "--dvc-target", "dvc_target", type=click.Path(exists=True), required=True,
              help="name of the .dvc file that is the last step in the dvc run pipeline")
@click.option("-b", "--s3-bucket", "s3_bucket", type=str, default="idd-input-data-sets", show_default=True,
              help="The S3 bucket to use for keeping state for the batch jobs")
@click.option("-d", "--job-definition", "batch_job_definition", type=str, default="Batch-CovidPipeline-Job", show_default=True,
              help="The name of the AWS Batch Job Definition to use for the job")
@click.option("-q", "--job-queue", "batch_job_queue", type=str, default="Batch-CovidPipeline", show_default=True,
              help="The name of the AWS Batch Job Queue to use for the job")
@click.option("-p", "--parallelize-scenarios", "parallelize_scenarios", is_flag=True, default=False, show_default=True,
              help="Launch a different batch job for each scenario")
@click.option("-m", "--memory", "memory", type=click.IntRange(min=1000, max=6000), default=4000, show_default=True,
              help="The amount of RAM in megabytes needed per CPU running simulations")
def launch_batch(config_file, num_jobs, sims_per_slot, num_blocks, dvc_target, s3_bucket, batch_job_definition, batch_job_queue, parallelize_scenarios, memory):

    config = None
    with open(config_file) as f:
        config = yaml.full_load(f)

    # A unique name for this job run, based on the config name and current time
    job_name = f"{config['name']}-{int(time.time())}"

    # Update and save the config file with the number of sims to run
    config['filtering']['simulations_per_slot'] = sims_per_slot

    if parallelize_scenarios:
        scenarios = config['interventions']['scenarios']
        for s in scenarios:
            scenario_job_name = f"{job_name}_{s}"
            config['interventions']['scenarios'] = [s]
            with open(config_file, "w") as f:
                yaml.dump(config, f, sort_keys=False)
            launch_job_inner(scenario_job_name, config_file, num_jobs, , dvc_target, s3_input_bucket, s3_output_bucket, batch_job_definition, batch_job_queue, memory)
        config['interventions']['scenarios'] = scenarios
        with open(config_file, "w") as f:
            yaml.dump(config, f, sort_keys=False)
    else:
        with open(config_file, "w") as f:
            yaml.dump(config, f, sort_keys=False)
        launch_job_inner(job_name, config_file, num_jobs, slots_per_job, dvc_target, s3_input_bucket, s3_output_bucket, batch_job_definition, batch_job_queue, vcpu, memory)

    (rc, txt) = subprocess.getstatusoutput(f"git checkout -b run_{job_name}")
    print(txt)
    return rc


class BatchJobHandler(object):
    def __init__(self, num_jobs, sims_per_job, dvc_target, s3_bucket, batch_job_definition, batch_job_queue, memory):
        self.num_jobs = num_jobs
        self.sims_per_job = sims_per_job
        self.dvc_target = dvc_target
        self.s3_bucket = s3_bucket
        self.batch_job_definition = batch_job_definition
        self.batch_job_queue = batch_job_queue
        self.memory = memory

    def launch(self, job_name, config_file):

        # Prepare to tar up the current directory, excluding any dvc outputs, so it
        # can be shipped to S3
        dvc_outputs = get_dvc_outputs()
        tarfile_name = f"{job_name}.tar.gz"
        tar = tarfile.open(tarfile_name, "w:gz")
        for p in os.listdir('.'):
            if not (p.startswith(".") or p.endswith("tar.gz") or p in dvc_outputs or p == "batch"):
                tar.add(p, filter=lambda x: None if x.name.startswith('.') else x)
        tar.close()
 
        # Upload the tar'd contents of this directory and the runner script to S3 
        runner_script_name = f"{job_name}-runner.sh"
        local_runner_script = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'inference_runner.sh')
        s3_client = boto3.client('s3')
        s3_client.upload_file(local_runner_script, self.s3_bucket, runner_script_name)
        s3_client.upload_file(tarfile_name, self.s3_bucket, tarfile_name)
        os.remove(tarfile_name)

        # Prepare and launch the num_jobs via AWS Batch.
        model_data_path = f"s3://{self.s3_bucket}/{tarfile_name}"
        results_path = f"s3://{self.s3_bucket}/{job_name}"
        env_vars = [
                {"name": "CONFIG_PATH", "value": config_file},
                {"name": "S3_MODEL_DATA_PATH", "value": model_data_path},
                {"name": "DVC_TARGET", "value": self.dvc_target},
                {"name": "DVC_OUTPUTS", "value": " ".join(dvc_outputs)},
                {"name": "S3_RESULTS_PATH", "value": results_path},
        ]

        s3_cp_run_script = f"aws s3 cp s3://{self.s3_bucket}/{runner_script_name} $PWD/run-covid-pipeline"
        command = ["sh", "-c", f"{s3_cp_run_script}; /bin/bash $PWD/run-covid-pipeline"]

        batch_client = boto3.client('batch')
        last_job = batch_client.submit_job(
            jobName=f"{job_name}_block0",
            jobQueue=self.batch_job_queue,
            arrayProperties={'size': self.num_jobs},
            jobDefinition=self.batch_job_definition,
            containerOverrides={
                'vcpus': 2, # one for the controller and one for the forked processes
                'memory': self.memory,
                'environment': env_vars,
                'command': command
            })
        block_idx = 1
        while block_idx < self.num_blocks:
            #TODO: update this to pass in the path to the output from the last job
            cur_env_vars = env_vars.copy()
            cur_env_vars.append({
                "name": "S3_LAST_JOB_OUTPUT",
                "value": f"{results_path}/{last_job['jobId']}"
            })

            cur_job = batch_client.submit_job(
                jobName=f"{job_name}_block{block_idx}",
                jobQueue=self.batch_job_queue,
                arrayProperties={'size': self.num_jobs},
                dependsOn=[{'jobId': last_job['jobId'], 'type': 'N_TO_N'}],
                jobDefinition=self.batch_job_definition,
                containerOverrides={
                    'vcpus': 2, # one for the controller and one for the forked processes
                    'memory': self.memory,
                    'environment': cur_env_vars,
                    'command': command
                })
            last_job = cur_job
            block_idx += 1



def get_dvc_outputs():
    ret = []
    for dvc_file in glob.glob("*.dvc"):
        with open(dvc_file) as df:
            d = yaml.full_load(df)
            if 'cmd' in d and 'outs' in d:
                ret.extend([x['path'] for x in d['outs'] if x['path']])
    return ret


if __name__ == '__main__':
    launch_batch()
