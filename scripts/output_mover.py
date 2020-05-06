#!/usr/bin/env python

import boto3
import click
import multiprocessing
import os
import re

@click.command()
@click.option("-n", "--num-jobs", "num_jobs", type=click.IntRange(min=1, max=1000), required=True,
              help="number of output jobs to process")
@click.option("-l", "--slots-per-job", "slots_per_job", type=click.IntRange(min=1, max=1000), default=1,
              show_default=True, help="number of output slots from each job")
@click.option("-i", "--inference-paths", "inference_paths_str", type=str, required=True,
              help="comma-separated list of paths to move from S3 to the local output_dir")
@click.option("-o", "--output-dir", "output_dir", type=click.Path(), default="hospitalization/model_output",
              show_default=True, help="The local path to write the output data from S3 to")
def move_outputs(num_jobs, slots_per_job, inference_paths_str, output_dir):

    inference_paths = inference_paths_str.split(',')
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    uri_prefix = "s3://"
    workers = []
    for path in inference_paths:
       if path.startswith(uri_prefix):
           path = path[len(uri_prefix):]
       bucket, name_scenario_death, hash = path.split("/")
       name, scenario_death = re.split(r"-\d{10}_", name_scenario_death)
       split_at = scenario_death.rfind('_')
       scenario = scenario_death[:split_at]
       death = scenario_death[(split_at+1):]
       hosp_dir = os.path.join(output_dir, f"{name}_{scenario}")
       if not os.path.exists(hosp_dir):
           os.makedirs(hosp_dir)
       worker = Worker(num_jobs, slots_per_job, bucket, name_scenario_death, hash, hosp_dir, death)
       workers.append(worker)
       worker.start()

    for w in workers:
        w.join()


class Worker(multiprocessing.Process):
    def __init__(self, num_jobs, slots_per_job, bucket, name_scenario_death, hash, hosp_dir, death):
        super(Worker, self).__init__()
        self.num_jobs = num_jobs
        self.slots_per_job = slots_per_job
        self.bucket = bucket
        self.name_scenario_death = name_scenario_death
        self.hash = hash
        self.hosp_dir = hosp_dir
        self.death = death

    def run(self):
        s3_client = boto3.client('s3')
        index = 1
        for j in range(self.num_jobs):
            for k in range(self.slots_per_job):
                key = f"{self.name_scenario_death}/{self.hash}:{j}/{self.hosp_dir}/{self.death}_death_death-{k+1:09d}.hosp.parquet"
                local_file = os.path.join(self.hosp_dir, f"{self.death}_death_death-{index:09d}.hosp.parquet")
                s3_client.download_file(self.bucket, key, local_file)
                index += 1
                if index % 100 == 0:
                    print(f"Moved {index} files from {self.name_scenario_death}/{self.hash} to {self.hosp_dir}")

if __name__ == '__main__':
    move_outputs()
