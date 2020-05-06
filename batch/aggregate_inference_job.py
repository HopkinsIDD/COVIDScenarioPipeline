#!/usr/bin/env python

import boto3
import click
import os
import re
import tarfile
import time
import yaml

@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this run")
@click.option("-n", "--num-jobs", "num_jobs", type=click.IntRange(min=1, max=1000), default=1000,
              show_default=True, help="number of output jobs to process")
@click.option("-l", "--slots-per-job", "slots_per_job", type=click.IntRange(min=1, max=1000), default=1,
              show_default=True, help="number of output slots per job")
@click.option("-i", "--inference-path", "inference_paths", type=str, multiple=True, required=True,
              help="S3 paths of output files to be aggregated and analyzed")
@click.option("-b", "--s3-bucket", "s3_bucket", type=str, default="idd-inference-runs",
              help="The S3 bucket to use for job data")
@click.option("-o", "--output-path", "output_path", type=str, default="quantiles_output",
              help="The output path within the S3 bucket for the aggregated data")
@click.option("-q", "--job-queue", "batch_job_queue", type=str, default="Batch-CovidPipeline",
              help="The name of the job queue to submit the aggregation job to")
def aggregation_job(config_file, num_jobs, slots_per_job, inference_paths, s3_bucket, output_path, batch_job_queue):

    config = None
    with open(config_file) as f:
        config = yaml.full_load(f)

    job_name = f"{config['name']}-aggregation-{int(time.time())}"

    # get the dependent job ids from the inference paths
    dependent_jobs = [{"jobId": x.split("/")[-1]} for x in inference_paths]

    # Code we need to run on the batch machine is tar'd here
    tarfile_name = f"{job_name}.tar.gz"
    tar = tarfile.open(tarfile_name, "w:gz")
    for p in os.listdir('.'):
        if not (p.startswith(".") or p.endswith("tar.gz")):
            tar.add(p, filter=lambda x: None if x.name.startswith('.') else x)
    tar.close()

    # Upload the scripts we need to run to S3
    runner_script_name = f"{job_name}-runner.sh"
    this_dir = os.path.dirname(os.path.realpath(__file__))
    local_runner_script = os.path.join(this_dir, 'aggregate_runner.sh')
    s3_client = boto3.client('s3')
    s3_client.upload_file(local_runner_script, s3_bucket, runner_script_name)
    s3_client.upload_file(tarfile_name, s3_bucket, tarfile_name)
    os.remove(tarfile_name)

    # Prepare and launch the num_jobs via AWS Batch.
    s3_results_path = f"s3://{s3_bucket}/{job_name}"
    env_vars = [
        {"name": "S3_AGG_CODE_PATH", "value": f"s3://{s3_bucket}/{tarfile_name}" },
        {"name": "CONFIG_PATH", "value": config_file },
        {"name": "INFERENCE_PATHS", "value": ",".join(inference_paths) },
        {"name": "NUM_JOBS", "value": str(num_jobs) },
        {"name": "SLOTS_PER_JOB", "value": str(slots_per_job) },
        {"name": "OUTPUT_PATH", "value": output_path },
        {"name": "S3_RESULTS_PATH", "value": s3_results_path }
    ]

    s3_cp_run_script = f"aws s3 cp s3://{s3_bucket}/{runner_script_name} $PWD/run-aggregation"
    command = ["sh", "-c", f"{s3_cp_run_script}; /bin/bash $PWD/run-aggregation"]

    batch_client = boto3.client('batch')
    resp = batch_client.submit_job(
        jobName=job_name,
        jobQueue=batch_job_queue,
        jobDefinition=self.batch_job_definition,
        dependsOn=dependent_jobs,
        containerOverrides={
            'vcpus': 96,
            'memory': 768000,
            'environment': env_vars,
            'command': command
        },
        retryStrategy = {'attempts': 2})


if __name__ == '__main__':
    aggregation_job()
