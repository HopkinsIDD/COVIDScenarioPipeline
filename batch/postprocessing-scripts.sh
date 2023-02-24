
# START: your postprocessing scripts goes here.

export JOB_F1=/data/struelo1/flepimop-runs/$JOB_NAME
export JOB_FOLDER=${JOB_F1%_inference_med/}
Rscript $COVID_PATH/R/scripts/postprocess/run_sim_processing_SLURM_TEMPLATE.R -c $COVID_CONFIG_PATH -p $JOB_CLEAN

# END: your postprocessing scripts goes here.


# From chadi: this makes a plot of the llik files and send everything in pplot/ to slack with a little message from the bot. 

# --fs-results-path . instead of --fs-results-path $FS_RESULTS_PATH so it can takes advantage of all simulations and not just the copied one.
python $COVID_PATH/scripts/postprocess_auto.py -c $COVID_CONFIG_PATH --run-id $COVID_RUN_INDEX --job-name $JOB_NAME --fs-results-path . --slack-token $SLACK_TOKEN

