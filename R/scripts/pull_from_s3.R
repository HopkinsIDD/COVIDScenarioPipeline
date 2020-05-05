run_ids <- c(
  "1588652471",
  "1588652499",
  "1588652527"
)
scenarios <- rep("local_variance_LockdownWideEffect_post50_firstcase_10x",times=3)
deathrates <- c("low","med","high")
hashes <- c(
  "107bff91-ca73-4da3-8e3a-451a986b41cc",
  "d27e1a0b-8c39-4c8e-a9cd-87178b8e83d4",
  "1dc299a9-528e-40a7-b9dd-4f438d45f978"
  )
name <- "USA"
nruns <- 1000
# s3://idd-inference-runs/USA-1588636173_local_variance_LockdownWideEffect_post50_firstcase_10x_low/8d1b7049-92c5-42cb-a87a-5cf87c6848be
# s3://idd-inference-runs/USA-1588636205_local_variance_LockdownWideEffect_post50_firstcase_10x_med/85cc39b7-56b8-4ff4-b98b-23f096a9e151
# s3://idd-inference-runs/USA-1588636235_local_variance_LockdownWideEffect_post50_firstcase_10x_high/86e560f2-582f-48e3-a648-ef635b9bbc83
for(job_ind in seq_len(length(run_ids))){
  hash <- hashes[job_ind]
  deathrate <- deathrates[job_ind]
  scenario <- scenarios[job_ind]
  run_id <- run_ids[job_ind]
  system(sprintf("mkdir -p hospitalization/model_output/%s_%s/",name,scenario))
  print(c(hash,deathrate,scenario,run_id))
  pull_command <- sprintf('aws s3 cp --recursive s3://idd-inference-runs/USA-%s_%s_%s ./tmp/ --exclude="*" --include="%s:*/hospitalization/model_output/*/*000000001.hosp.parquet"',run_id,scenario,deathrate,hash)
  system(pull_command)
  for(index in seq_len(nruns) - 1){
    rename_command <- sprintf('mv tmp/%s:%d/hospitalization/model_output/%s_%s/%s_death_death-%09d.hosp.parquet hospitalization/model_output/%s_%s/%s_death_death-%09d.hosp.parquet',hash,index,name,scenario,deathrate,1,name,scenario,deathrate,index)
    # cat(command)
    # cat("\n")
    system(rename_command)
  }
}

