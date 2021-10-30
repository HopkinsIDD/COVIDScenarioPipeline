import boto3
import datetime
import subprocess

bucket = "idd-inference-deletetest" # 'idd-inference-runs'

s3 = boto3.client('s3')
paginator = s3.get_paginator('list_objects_v2') 
pages = paginator.paginate(Bucket=bucket, Prefix='', Delimiter = "/") # needs paginator cause more than 1000 files

to_prun = []
# folders:
for page in pages:
    for cp in page['CommonPrefixes']:
        prefix = cp['Prefix']
        timestamp = prefix.split('-')[-1].replace('/','')
        rundate = datetime.datetime.strptime(timestamp, "%Y%m%dT%H%M%S")
        if rundate < datetime.datetime.now() - datetime.timedelta(weeks=8):
            print(f"Will prun files in {prefix}")
            to_prun.append(prefix)
        else:
            print(f"NOT pruning {prefix}")


for run in to_prun:
    print(f"Pruning chimeric & intermediate in {run}...", end='')
    command = f"aws s3 rm --recursive --exclude '*' --include '*/intermediate/*' --include '*/chimeric/*' s3://{bucket}/{run}"
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
    process.wait()
    print(f"Done, return code is {process.returncode} !")
    if process.returncode != 0:
        raise ValueError(f"STOPPING, aws s3 rm failed for {run} !")






# files
#for page in pages:
#  for obj in page['Contents']:
#      print(obj['Key'], obj['LastModified'])

# filter and aws s3 rm --recursive --exclude '*' --include '*/intermediate/*' --include '*/chimeric/*' s3://idd-inference-deletetest/USA-20210903T192833/