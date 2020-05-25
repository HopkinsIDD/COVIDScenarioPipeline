#!/usr/bin/env python

import boto3
import click
import glob
import itertools
import json
import os
import re
import subprocess
import sys
import tarfile
from datetime import datetime, timezone
import yaml

@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this run")
@click.option("-n", "--num-jobs", "num_jobs", type=click.IntRange(min=1, max=1000), required=True,
              help="number of output slots to generate")
@click.option("-j", "--sims-per-job", "sims_per_job", type=click.IntRange(min=1), default=10, show_default=True,
              help="the number of sims to run on each child job")
@click.option("-k", "--num-blocks", "num_blocks", type=click.IntRange(min=1), default=10, show_default=True,
              help="The number of sequential blocks of jobs to run; total sims per slot = sims-per-slot * num-blocks")
@click.option("-o", "--output", "outputs", multiple=True, default=["model_output", "model_parameters", "importation", "hospitalization"],
              show_default=True, help="The output directories whose contents are captured and saved in S3")
@click.option("-b", "--s3-bucket", "s3_bucket", type=str, default="idd-inference-runs", show_default=True,
              help="The S3 bucket to use for keeping state for the batch jobs")
@click.option("-d", "--job-definition", "batch_job_definition", type=str, default="Batch-CovidPipeline-Job", show_default=True,
              help="The name of the AWS Batch Job Definition to use for the job")
@click.option("-q", "--job-queue-prefix", "job_queue_prefix", type=str, default="Inference-JQ", show_default=True,
              help="The prefix string of the job queues we should use for this run")
@click.option("-v", "--vcpus", "vcpus", type=click.IntRange(min=1, max=96), default=2, show_default=True,
              help="The number of CPUs to request for running jobs")
@click.option("-m", "--memory", "memory", type=click.IntRange(min=1000, max=6000), default=4000, show_default=True,
              help="The amount of RAM in megabytes needed per CPU running simulations")
@click.option("-r", "--restart-from", "restart_from", type=str, default=None,
              help="The location of an S3 run to use as the initial to the first block of the current run")
def launch_batch(config_file, num_jobs, sims_per_job, num_blocks, outputs, s3_bucket, batch_job_definition, job_queue_prefix, vcpus, memory, restart_from):

    config = None
    with open(config_file) as f:
        config = yaml.full_load(f)

    # A unique name for this job run, based on the config name and current time
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S")
    job_name = f"{config['name']}-{timestamp}"

    handler = BatchJobHandler(num_jobs, sims_per_job, num_blocks, outputs, s3_bucket, batch_job_definition, vcpus, memory, restart_from)

    job_queues = get_job_queues(job_queue_prefix)
    scenarios = config['interventions']['scenarios']
    p_death_names = config['hospitalization']['parameters']['p_death_names']
    handler.launch(job_name, config_file, scenarios, p_death_names, job_queues)

    (rc, txt) = subprocess.getstatusoutput(f"git checkout -b run_{job_name}")
    print(txt)
    return rc


def get_job_queues(job_queue_prefix):
    batch_client = boto3.client('batch')
    queues_with_jobs = {}
    resp = batch_client.describe_job_queues()
    for q in resp['jobQueues']:
        queue_name = q['jobQueueName']
        if queue_name.startswith(job_queue_prefix):
           job_list_resp = batch_client.list_jobs(jobQueue=queue_name, jobStatus='PENDING')
           queues_with_jobs[queue_name] = len(job_list_resp['jobSummaryList'])
    # Return the least-loaded queues first
    return sorted(queues_with_jobs, key=queues_with_jobs.get)


class BatchJobHandler(object):
    def __init__(self, num_jobs, sims_per_job, num_blocks, outputs, s3_bucket, batch_job_definition, vcpus, memory, restart_from):
        self.num_jobs = num_jobs
        self.sims_per_job = sims_per_job
        self.num_blocks = num_blocks
        self.outputs = outputs
        self.s3_bucket = s3_bucket
        self.batch_job_definition = batch_job_definition
        self.vcpus = vcpus
        self.memory = memory
        self.restart_from = restart_from

    def launch(self, job_name, config_file, scenarios, p_death_names, job_queues):

        manifest = {}
        manifest['cmd'] = " ".join(sys.argv[:])
        manifest['job_name'] = job_name
        manifest['data_sha'] = subprocess.getoutput('git rev-parse HEAD')
        manifest['csp_sha'] = subprocess.getoutput('cd COVIDScenarioPipeline; git rev-parse HEAD')

        # Prepare to tar up the current directory, excluding any dvc outputs, so it
        # can be shipped to S3
        tarfile_name = f"{job_name}.tar.gz"
        tar = tarfile.open(tarfile_name, "w:gz", dereference=True)
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

        # Upload the tar'd contents of this directory and the runner script to S3
        runner_script_name = f"{job_name}-runner.sh"
        local_runner_script = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'inference_runner.sh')
        s3_client = boto3.client('s3')
        s3_client.upload_file(local_runner_script, self.s3_bucket, runner_script_name)
        s3_client.upload_file(tarfile_name, self.s3_bucket, tarfile_name)
        os.remove(tarfile_name)

        # Save the manifest file to S3
        with open('manifest.json', 'w') as f:
            json.dump(manifest, f, indent=4)
        s3_client.upload_file('manifest.json', self.s3_bucket, f"{job_name}/manifest.json")

        # Create job to copy output to appropriate places
        copy_script_name = f"{job_name}-copy.sh"
        local_runner_script = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'inference_copy.sh')
        s3_client.upload_file(local_runner_script, self.s3_bucket, copy_script_name)

        # Prepare and launch the num_jobs via AWS Batch.
        model_data_path = f"s3://{self.s3_bucket}/{tarfile_name}"
        results_path = f"s3://{self.s3_bucket}/{job_name}"
        base_env_vars = [
                {"name": "S3_MODEL_DATA_PATH", "value": model_data_path},
                {"name": "DVC_OUTPUTS", "value": " ".join(self.outputs)},
                {"name": "S3_RESULTS_PATH", "value": results_path},
                {"name": "COVID_CONFIG_PATH", "value": config_file},
                {"name": "SIMS_PER_JOB", "value": str(self.sims_per_job) }
        ]

        runner_script_path = f"s3://{self.s3_bucket}/{runner_script_name}"
        s3_cp_run_script = f"aws s3 cp {runner_script_path} $PWD/run-covid-pipeline"
        command = ["sh", "-c", f"{s3_cp_run_script}; /bin/bash $PWD/run-covid-pipeline"]

        # Create first job
        cur_env_vars = env_vars.copy()
        if self.restart_from:
            cur_env_vars.append({"name": "S3_LAST_JOB_OUTPUT", "value": self.restart_from})
        cur_env_vars.append({"name": "JOB_NAME", "value": f"{job_name}_block0"})

        batch_client = boto3.client('batch')

        ctr = 0
        for (s, d) in itertools.product(scenarios, p_death_names):
            cur_job_name = f"{job_name}_{s}_{d}"
            # Create first job
            cur_env_vars = base_env_vars.copy()
            cur_env_vars.append({"name": "COVID_SCENARIOS", "value": s})
            cur_env_vars.append({"name": "COVID_DEATHRATES", "value": d})
            if self.restart_from:
                cur_env_vars.append({"name": "S3_LAST_JOB_OUTPUT", "value": self.restart_from})
            cur_env_vars.append({"name": "JOB_NAME", "value": f"{cur_job_name}_block0"})

            cur_job_queue = job_queues[ctr % len(job_queues])
            last_job = batch_client.submit_job(
                jobName=f"{cur_job_name}_block0",
                jobQueue=cur_job_queue,
                arrayProperties={'size': self.num_jobs},
                jobDefinition=self.batch_job_definition,
                containerOverrides={
                    'vcpus': self.vcpus,
                    'memory': self.vcpus * self.memory,
                    'environment': cur_env_vars,
                    'command': command
                },
                retryStrategy = {'attempts': 3})

            # Create all other jobs
            block_idx = 1
            while block_idx < self.num_blocks:
                cur_env_vars = base_env_vars.copy()
                cur_env_vars.append({"name": "COVID_SCENARIOS", "value": s)
                cur_env_vars.append({"name": "COVID_DEATHRATES", "value": d})
                cur_env_vars.append({
                    "name": "S3_LAST_JOB_OUTPUT",
                    "value": f"{results_path}/{last_job['jobName']}"
                })
                cur_env_vars.append({
                    "name": "JOB_NAME",
                    "value": f"{job_name}_block{block_idx}"
                })
                cur_job = batch_client.submit_job(
                    jobName=f"{cur_job_name}_block{block_idx}",
                    jobQueue=cur_job_queue,
                    arrayProperties={'size': self.num_jobs},
                    dependsOn=[{'jobId': last_job['jobId'], 'type': 'N_TO_N'}],
                    jobDefinition=self.batch_job_definition,
                    containerOverrides={
                        'vcpus': self.vcpus,
                        'memory': self.vcpus * self.memory,
                        'environment': cur_env_vars,
                        'command': command
                    },
                    retryStrategy={'attempts': 3})
                last_job = cur_job
                block_idx += 1


            # Prepare and launch the num_jobs via AWS Batch.
            cp_env_vars = [
                {"name": "S3_RESULTS_PATH", "value": results_path},
                {"name": "S3_LAST_JOB_OUTPUT", "value": f"{results_path}/{last_job['jobName']}"},
                {"name": "NSLOT", "value": str(self.num_jobs)},
            ]

            copy_script_path = f"s3://{self.s3_bucket}/{copy_script_name}"
            s3_cp_run_script = f"aws s3 cp {copy_script_path} $PWD/run-covid-pipeline"
            cp_command = ["sh", "-c", f"{s3_cp_run_script}; /bin/bash $PWD/run-covid-pipeline"]

            print(f"Launching {job_name}_copy...")
            copy_job = batch_client.submit_job(
                jobName=f"{cur_job_name}_copy",
                jobQueue=cur_job_queue,
                jobDefinition=self.batch_job_definition,
                dependsOn=[{'jobId': last_job['jobId']}],
                containerOverrides={
                    'vcpus': 1,
                    'environment': cp_env_vars,
                    'command': cp_command
                },
                retryStrategy = {'attempts': 3})

        print(f"Final output will be: {results_path}/final_output/")


if __name__ == '__main__':
    launch_batch()
