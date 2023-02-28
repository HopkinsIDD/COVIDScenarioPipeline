
# START: your postprocessing scripts goes here.

export FULL_FIT=TRUE
export DIAGNOSTICS=TRUE
Rscript $COVID_PATH/R/scripts/postprocess/run_sim_processing_SLURM.R -c $COVID_CONFIG_PATH -p $FS_RESULTS_PATH

export FULL_FIT=FALSE
export DIAGNOSTICS=FALSE
Rscript $COVID_PATH/R/scripts/postprocess/run_sim_processing_SLURM.R -c $COVID_CONFIG_PATH -p $FS_RESULTS_PATH 

# END: your postprocessing scripts goes here.


# From chadi: this makes a plot of the llik files and send everything in pplot/ to slack with a little message from the bot. 

# --fs-results-path . instead of --fs-results-path $FS_RESULTS_PATH so it can takes advantage of all simulations and not just the copied one.
python $COVID_PATH/scripts/postprocess_auto.py -c $COVID_CONFIG_PATH --run-id $COVID_RUN_INDEX --job-name $JOB_NAME --fs-results-path . --slack-token $SLACK_TOKEN

