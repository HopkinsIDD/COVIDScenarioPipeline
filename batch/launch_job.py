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
@click.option("-j", "--num-jobs", "num_jobs", type=click.IntRange(min=1), required=True,
              help="total number of jobs to run in this batch")
@click.option("-s", "--sims-per-job", "sims_per_job", type=click.IntRange(min=1), required=True,
              help="how many sims each job should run")
@click.option("-t", "--dvc-target", "dvc_target", type=click.Path(exists=True), required=True,
              help="name of the .dvc file that is the last step in the dvc run pipeline")
@click.option("-i", "--s3-input-bucket", "s3_input_bucket", type=str, default="idd-input-data-sets", show_default=True,
              help="The S3 bucket to use for uploading the code and configuration used by the batch job")
@click.option("-o", "--s3-output-bucket", "s3_output_bucket", type=str, default="idd-pipeline-results", show_default=True,
              help="The S3 bucket for storing the job's outputs")
@click.option("-d", "--job-definition", "batch_job_definition", type=str, default="Batch-CovidPipeline-Job", show_default=True,
              help="The name of the AWS Batch Job Definition to use for the job")
@click.option("-q", "--job-queue", "batch_job_queue", type=str, default="Batch-CovidPipeline", show_default=True,
              help="The name of the AWS Batch Job Queue to use for the job")
@click.option("-p", "--parallelize-scenarios", "parallelize_scenarios", type=bool, default=False, show_default=True,
              help="Launch a different batch job for each scenario")
def launch_batch(config_file, num_jobs, sims_per_job, dvc_target, s3_input_bucket, s3_output_bucket, batch_job_definition, batch_job_queue, parallelize_scenarios):

    config = None
    with open(config_file) as f:
        config = yaml.full_load(f)

    # A unique name for this job run, based on the config name and current time
    job_name = f"{config['name']}-{int(time.time())}"

    # Update and save the config file with the number of sims to run
    print(f"Updating {config_file} to run {sims_per_job} simulations...")
    config['nsimulations'] = sims_per_job

    if parallelize_scenarios:
        scenarios = config['interventions']['scenarios']
        for s in scenarios:
            scenario_job_name = f"{job_name}_{s}"
            config['interventions']['scenarios'] = [s]
            with open(config_file, "w") as f:
                yaml.dump(config, f, sort_keys=False)
            launch_job_inner(scenario_job_name, config_file, num_jobs, dvc_target, s3_input_bucket, s3_output_bucket, batch_job_definition, batch_job_queue)
        config['interventions']['scenarios'] = scenarios
        with open(config_file, "w") as f:
            yaml.dump(config, f, sort_keys=False)
    else:
        with open(config_file, "w") as f:
            yaml.dump(config, f, sort_keys=False)
        launch_job_inner(job_name, config_file, num_jobs, dvc_target, s3_input_bucket, s3_output_bucket, batch_job_definition, batch_job_queue)

def launch_job_inner(job_name, config_file, num_jobs, dvc_target, s3_input_bucket, s3_output_bucket, batch_job_definition, batch_job_queue):

    # Prepare to tar up the current directory, excluding any dvc outputs, so it
    # can be shipped to S3
    dvc_outputs = get_dvc_outputs()
    tarfile_name = f"{job_name}.tar.gz"
    tar = tarfile.open(tarfile_name, "w:gz")
    for p in os.listdir('.'):
        if not (p.startswith(".") or p.endswith("tar.gz") or p in dvc_outputs or p == "batch"):
            tar.add(p)
    tar.close()
 
    # Upload the tar'd contents of this directory and the runner script to S3 
    runner_script_name = f"{job_name}-runner.sh"
    s3_client = boto3.client('s3')
    s3_client.upload_file("batch/runner.sh", s3_input_bucket, runner_script_name)
    s3_client.upload_file(tarfile_name, s3_input_bucket, tarfile_name)
    os.remove(tarfile_name)

    # Prepare and launch the num_jobs via AWS Batch.
    model_data_path = f"s3://{s3_input_bucket}/{tarfile_name}"
    results_path = f"s3://{s3_output_bucket}/{job_name}"
    env_vars = [
            {"name": "CONFIG_PATH", "value": config_file},
            {"name": "S3_MODEL_DATA_PATH", "value": model_data_path},
            {"name": "DVC_TARGET", "value": dvc_target},
            {"name": "DVC_OUTPUTS", "value": " ".join(dvc_outputs)},
            {"name": "S3_RESULTS_PATH", "value": results_path}
    ]
    s3_cp_run_script = f"aws s3 cp s3://{s3_input_bucket}/{runner_script_name} $PWD/run-covid-pipeline"
    command = ["sh", "-c", f"{s3_cp_run_script}; /bin/bash $PWD/run-covid-pipeline"]
    container_overrides = {
            'vcpus': 72,
            'memory': 184000,
            'environment': env_vars,
            'command': command
    }

    print("Preparing to run job: %s" % job_name)
    batch_client = boto3.client('batch')
    if num_jobs > 1:
        resp = batch_client.submit_job(
                jobName=job_name,
                jobQueue=batch_job_queue,
                arrayProperties={'size': num_jobs},
                jobDefinition=batch_job_definition,
            containerOverrides=container_overrides)
    else:
        resp = batch_client.submit_job(
                jobName=job_name,
                jobQueue=batch_job_queue,
                jobDefinition=batch_job_definition,
                containerOverrides=container_overrides)

    # TODO: record batch job info to a file so it can be tracked


def get_dvc_outputs():
    ret = []
    for dvc_file in glob.glob("*.dvc"):
        with open(dvc_file) as df:
            d = yaml.full_load(df)
            if 'cmd' in d and 'outs' in d:
                ret.extend([x['path'] for x in d['outs']])
    return ret


if __name__ == '__main__':
    launch_batch()
