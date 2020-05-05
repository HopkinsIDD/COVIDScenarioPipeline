run_ids <- c("1588636173","1588636205","1588636235")
scenarios <- rep("local_variance_LockdownWideEffect_post50_firstcase_10x",times=3)
deathrates <- c("low","med","high")
hashes <- c(
  "8d1b7049-92c5-42cb-a87a-5cf87c6848be",
  "85cc39b7-56b8-4ff4-b98b-23f096a9e151",
  "86e560f2-582f-48e3-a648-ef635b9bbc83"
  )
name <- "USA"
# s3://idd-inference-runs/USA-1588636173_local_variance_LockdownWideEffect_post50_firstcase_10x_low/8d1b7049-92c5-42cb-a87a-5cf87c6848be
# s3://idd-inference-runs/USA-1588636205_local_variance_LockdownWideEffect_post50_firstcase_10x_med/85cc39b7-56b8-4ff4-b98b-23f096a9e151
# s3://idd-inference-runs/USA-1588636235_local_variance_LockdownWideEffect_post50_firstcase_10x_high/86e560f2-582f-48e3-a648-ef635b9bbc83
for(job_ind in seq_len(length(run_ids))){
  hash <- hashes[job_ind]
  deathrate <- deathrates[job_ind]
  scenario <- scenarios[job_ind]
  run_id <- run_ids[job_ind]
  print(c(hash,deathrate,scenario,run_id))
  for(index in seq_len(10) - 1){
    pull_command <- sprintf('aws s3 cp --recursive s3://idd-inference-runs/USA-%s_%s_%s/%s:%d ./tmp/ --exclude="*" --include="*0001.hosp.parquet"',run_id,scenario,deathrate,hash,index)
    rename_command <- sprintf('mv tmp/hospitalization/model_output/%s_%s/%s_death_death-%09d.hosp.parquet hospitalization/model_output/%s_%s/%s_death_death-%09d.hosp.parquet',name,scenario,deathrate,1,name,scenario,deathrate,index)
    # cat(command)
    # cat("\n")
    system(pull_command)
    system(rename_command)
  }
}

