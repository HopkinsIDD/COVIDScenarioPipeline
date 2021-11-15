import boto3
import datetime
import subprocess
import re
import time


bucket = "idd-inference-runs" # 'idd-inference-runs'

s3 = boto3.client('s3')
paginator = s3.get_paginator('list_objects_v2') 
pages = paginator.paginate(Bucket=bucket, Prefix='', Delimiter = "/") # needs paginator cause more than 1000 files

to_prun = []
# folders:
print("Searching what to delete or keep: ")
for page in pages:
    for cp in page['CommonPrefixes']:
        prefix = cp['Prefix']
        # Find the date, not always this easy: prefix.split('-')[1].replace('/','')
        m = re.search(r"\d", prefix)
        timestamp = prefix[m.start():m.start()+15]
        rundate = datetime.datetime.strptime(timestamp, "%Y%m%dT%H%M%S")
        if rundate < datetime.datetime.now() - datetime.timedelta(weeks=8):
            print(f"- Will prun files in {prefix}, rundate {rundate}")
            to_prun.append(prefix)
        else:
            print(f"- NOT pruning {prefix}, rundate {rundate}")


def perform_deletions(to_prun, do_it_for_real = False):
    dry_run_str = '--dryrun'
    if do_it_for_real == True:
        print("this is not a dry run, waiting 3 second so you reconsider")
        time.sleep(3)
        dry_run_str = ''
    with open('toruntodelete.sh', 'w') as script_file:
        for run in to_prun:
            print(f"Pruning chimeric & intermediate in {run}...", end='')
            command = f"aws s3 rm {dry_run_str} --recursive --exclude '*' --include '*/intermediate/*' --include '*/chimeric/*' s3://{bucket}/{run}"
            print(f">>> {command}")
            print(f"echo deleting {run}", file=script_file)
            print(f"{command} || {{ echo 'failed for {run} !' ; exit 1; }}", file=script_file)
            #process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
            #process.wait()
            #print(f"Done, return code is {process.returncode} !")
            #if process.returncode != 0:
            #    raise ValueError(f"STOPPING, aws s3 rm failed for {run} !")

        



print("I'll perform the deletion, this is dangerous...")
if input("... Do you wish to continue? [yes/no] ") == "yes":
    do_it_for_real = True
    #do_it_for_real = False
    if do_it_for_real:
        if input("... NOT A DRY RUN, is that really ok ? [yes/no] ") == "yes":
            perform_deletions(to_prun = to_prun, do_it_for_real = do_it_for_real)
        else:
            print("wise choice, abording")
            exit()
    else:
        perform_deletions(to_prun = to_prun, do_it_for_real = do_it_for_real)
    

else:
    print("wise choice, abording")
    exit()




# files
#for page in pages:
#  for obj in page['Contents']:
#      print(obj['Key'], obj['LastModified'])

# filter and aws s3 rm --recursive --exclude '*' --include '*/intermediate/*' --include '*/chimeric/*' s3://idd-inference-deletetest/USA-20210903T192833/