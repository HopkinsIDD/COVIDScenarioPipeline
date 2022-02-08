#!/usr/bin/env python

import boto3

batch = boto3.client('batch')

def process_child_jobs(parent_job_id, tracker, next_token=None):
   if next_token is not None:
       child_jobs = batch.list_jobs(arrayJobId=parent_job_id, jobStatus='RUNNING', nextToken=next_token)
   else:
       child_jobs = batch.list_jobs(arrayJobId=parent_job_id, jobStatus='RUNNING')
   tracker['RUNNING'] = tracker.get('RUNNING', 0) + len(child_jobs['jobSummaryList'])
   return child_jobs['nextToken'] if 'nextToken' in child_jobs else None

def process_parent_jobs(job_queue, parent_tracker, next_token=None):
    if next_token is not None:
        parent_jobs = batch.list_jobs(jobQueue=job_queue, jobStatus='PENDING', nextToken=next_token)
    else:
        parent_jobs = batch.list_jobs(jobQueue=job_queue, jobStatus='PENDING')

    for job in parent_jobs['jobSummaryList']:
        parent_tracker['RUNNING'] = parent_tracker.get('RUNNING', 0) + 1
        tracker = {}
        next_child_token = process_child_jobs(job['jobId'], tracker)
        while next_child_token is not None:
            next_child_token = process_child_jobs(job['jobId'], tracker, next_child_token)
        if tracker['RUNNING']:
            print(f"Parent job {job['jobName']} had {tracker['RUNNING']} running child jobs.")
            parent_tracker['CHILD_JOBS'] = parent_tracker.get('CHILD_JOBS', 0) + tracker['RUNNING']

    return parent_jobs['nextToken'] if 'nextToken' in parent_jobs else None

parent_tracker = {}
queues = []
queues_resp = batch.describe_job_queues()
for q in queues_resp['jobQueues']:
    queues.append(q['jobQueueName'])

for q in queues:
  print(f"Processing job queue {q}...")
  next_token = process_parent_jobs(q, parent_tracker)
  while next_token is not None:
    next_token = process_parent_jobs(q, parent_tracker, next_token)
print(f"Processed {parent_tracker.get('RUNNING', 0)} parent jobs with {parent_tracker.get('CHILD_JOBS', 0)} running child jobs")

