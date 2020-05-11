#!/usr/bin/env python

import boto3
import click
import glob
import itertools
import json
import os
import re
import subprocess
import tarfile
import time
import yaml

@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this run")
@click.option("-n", "--num-slots", "num_jobs", type=click.IntRange(min=1, max=1000), required=True,
              help="number of output slots to generate")
@click.option("-j", "--sims-per-slot", "sims_per_slot", type=click.IntRange(min=1), default=10, show_default=True,
              help="the number of sims to run on each child job")
@click.option("-k", "--num-blocks", "num_blocks", type=click.IntRange(min=1), default=10, show_default=True,
              help="The number of sequential blocks of jobs to run; total sims per slot = sims-per-slot * num-blocks")
@click.option("-t", "--dvc-target", "dvc_target", type=click.Path(exists=True), required=True,
              help="name of the .dvc file that is the last step in the dvc run pipeline")
@click.option("-b", "--s3-bucket", "s3_bucket", type=str, default="idd-inference-runs", show_default=True,
              help="The S3 bucket to use for keeping state for the batch jobs")
@click.option("-d", "--job-definition", "batch_job_definition", type=str, default="Batch-CovidPipeline-Job", show_default=True,
              help="The name of the AWS Batch Job Definition to use for the job")
@click.option("-p", "--parallelize-scenarios", "parallelize_scenarios", is_flag=True, default=False, show_default=True,
              help="Launch a different batch job for each scenario/death combination in the config file")
@click.option("-v", "--vcpus", "vcpus", type=click.IntRange(min=1, max=96), default=2, show_default=True,
              help="The number of CPUs to request for running jobs")
@click.option("-m", "--memory", "memory", type=click.IntRange(min=1000, max=6000), default=4000, show_default=True,
              help="The amount of RAM in megabytes needed per CPU running simulations")
@click.option("-r", "--restart-from", "restart_from", type=str, default=None,
              help="The location of an S3 run to use as the initial to the first block of the current run")
def launch_batch(config_file, num_jobs, sims_per_slot, num_blocks, dvc_target, s3_bucket, batch_job_definition, parallelize_scenarios, vcpus, memory, restart_from):

    config = None
    with open(config_file) as f:
        config = yaml.full_load(f)

    # A unique name for this job run, based on the config name and current time
    job_name = f"{config['name']}-{int(time.time())}"

    # Update and save the config file with the number of sims to run
    if 'filtering' in config:
        config['filtering']['simulations_per_slot'] = sims_per_slot
        if not os.path.exists(config['filtering']['data_path']):
            print(f"ERROR: filtering.data_path path {config['filtering']['data_path']} does not exist!")
            return 1
    else:
        print(f"WARNING: no filtering section found in {config_file}!")

    handler = BatchJobHandler(num_jobs, num_blocks, dvc_target, s3_bucket, batch_job_definition, vcpus, memory, restart_from)

    if parallelize_scenarios:
        job_queues = get_job_queues()
        scenarios = config['interventions']['scenarios']
        p_death_names = config['hospitalization']['parameters']['p_death_names']
        p_deaths = config['hospitalization']['parameters']['p_death']
        p_hosp_inf = config['hospitalization']['parameters']['p_hosp_inf']
        ctr = 0
        for (s, d) in itertools.product(scenarios, zip(p_death_names, p_deaths, p_hosp_inf)):
            scenario_job_name = f"{job_name}_{s}_{d[0]}"
            config['interventions']['scenarios'] = [s]
            config['hospitalization']['parameters']['p_death_names'] = [d[0]]
            config['hospitalization']['parameters']['p_death'] = [d[1]]
            config['hospitalization']['parameters']['p_hosp_inf'] = [d[2]]
            with open("config_runme.yml", "w") as launch_config_file:
                yaml.dump(config, launch_config_file, sort_keys=False)
            handler.launch(scenario_job_name, "config_runme.yml", job_queues[ctr % len(job_queues)])
            ctr += 1
        config['interventions']['scenarios'] = scenarios
        config['hospitalization']['parameters']['p_death_names'] = p_death_names
        config['hospitalization']['parameters']['p_death'] = p_deaths
        config['hospitalization']['parameters']['p_hosp_inf'] = p_hosp_inf
    else:
        with open("config_runme.yml", "w") as launch_config_file:
            yaml.dump(config, launch_config_file, sort_keys=False)
        handler.launch(job_name, "config_runme.yml", batch_job_queue=get_job_queues()[0])

    (rc, txt) = subprocess.getstatusoutput(f"git checkout -b run_{job_name}")
    print(txt)
    return rc


def get_job_queues():
    batch_client = boto3.client('batch')
    queues_with_jobs = {}
    resp = batch_client.describe_job_queues()
    for q in resp['jobQueues']:
        queue_name = q['jobQueueName']
        if queue_name.startswith('Inference-JQ-'):
           job_list_resp = batch_client.list_jobs(jobQueue=queue_name, jobStatus='PENDING')
           queues_with_jobs[queue_name] = len(job_list_resp['jobSummaryList'])
    # Return the least-loaded queues first
    return sorted(queues_with_jobs, key=queues_with_jobs.get)

class BatchJobHandler(object):
    def __init__(self, num_jobs, num_blocks, dvc_target, s3_bucket, batch_job_definition, vcpus, memory, restart_from):
        self.num_jobs = num_jobs
        self.num_blocks = num_blocks
        self.dvc_target = dvc_target
        self.s3_bucket = s3_bucket
        self.batch_job_definition = batch_job_definition
        self.vcpus = vcpus
        self.memory = memory
        self.restart_from = restart_from

    def launch(self, job_name, config_file, batch_job_queue):

        manifest = {}
        manifest['job_name'] = job_name
        manifest['job_queue'] = batch_job_queue
        manifest['data_sha'] = subprocess.getoutput('git rev-parse HEAD')
        manifest['csp_sha'] = subprocess.getoutput('cd COVIDScenarioPipeline; git rev-parse HEAD')

        # Prepare to tar up the current directory, excluding any dvc outputs, so it
        # can be shipped to S3
        dvc_outputs = get_dvc_outputs()
        tarfile_name = f"{job_name}.tar.gz"
        tar = tarfile.open(tarfile_name, "w:gz")
        for p in os.listdir('.'):
            if p == 'COVIDScenarioPipeline':
                for q in os.listdir('COVIDScenarioPipeline'):
                    if not q.startswith('.'):
                        tar.add(os.path.join('COVIDScenarioPipeline', q))
            elif not (p.startswith(".") or p.endswith("tar.gz") or p in dvc_outputs or p == "batch"):
                tar.add(p)
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

        cur_env_vars = env_vars.copy()
        if self.restart_from:
            cur_env_vars.append({"name": "S3_LAST_JOB_OUTPUT", "value": self.restart_from})

        batch_client = boto3.client('batch')
        print(f"Launching {job_name}_block0...")
        last_job = batch_client.submit_job(
            jobName=f"{job_name}_block0",
            jobQueue=batch_job_queue,
            arrayProperties={'size': self.num_jobs},
            jobDefinition=self.batch_job_definition,
            containerOverrides={
                'vcpus': self.vcpus,
                'memory': self.vcpus * self.memory,
                'environment': cur_env_vars,
                'command': command
            },
            retryStrategy = {'attempts': 3})
        manifest['blocks'] = [last_job['jobId']]
        block_idx = 1
        while block_idx < self.num_blocks:
            cur_env_vars = env_vars.copy()
            cur_env_vars.append({
                "name": "S3_LAST_JOB_OUTPUT",
                "value": f"{results_path}/{last_job['jobId']}"
            })
            cur_job = batch_client.submit_job(
                jobName=f"{job_name}_block{block_idx}",
                jobQueue=batch_job_queue,
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
            manifest['blocks'].append(cur_job['jobId'])
            last_job = cur_job
            block_idx += 1

        # Save the manifest file to S3
        with open('manifest.json', 'w') as f:
            json.dump(manifest, f, indent=4)
        s3_client.upload_file('manifest.json', self.s3_bucket, f"{job_name}/manifest.json")
 
        print(f"Final output will be for job id: {results_path}/{last_job['jobId']}")


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
