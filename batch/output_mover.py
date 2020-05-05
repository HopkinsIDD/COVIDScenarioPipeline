#!/usr/bin/env python

import boto3
import click
import os
import re

@click.command()
@click.option("-n", "--num-jobs", "num_jobs", type=click.IntRange(min=1, max=1000), required=True,
              help="number of output jobs to process")
@click.option("-l", "--slots-per-job", "slots_per_job", type=click.IntRange(min=1, max=1000), default=1,
              show_default=True, help="number of output slots from each job")
@click.option("-i", "--inference-path", "inference_paths", type=str, multiple=True, required=True,
              help="S3 paths of output files to be aggregated and analyzed")
@click.option("-o", "--output-dir", "output_dir", type=click.Path(), default="hospitalization/model_output",
              help="The local path to write the output data from S3 to")
def move_outputs(num_jobs, slots_per_job, inference_paths, output_dir):

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    s3_client = boto3.client('s3')
    uri_prefix = "s3://"
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
       index = 1
       for j in range(num_jobs):
           for k in range(slots_per_job):
               key = f"{name_scenario_death}/{hash}:{j}/hospitalization/model_output/{name}_{scenario}/{death}_death_death-{k+1:09d}.hosp.parquet"
               local_file = os.path.join(hosp_dir, f"{death}_death_death-{index:09d}.hosp.parquet")
               print(f"Moving {key} to {local_file}...")
               s3_client.download_file(bucket, key, local_file)
               index += 1


if __name__ == '__main__':
    move_outputs()
