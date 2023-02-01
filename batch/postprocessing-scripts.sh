
# START: your postprocessing scripts goes here.



# END: your postprocessing scripts goes here.


# From chadi: this makes a plot of the llik files and send everything in pplot/ to slack with a little message from the bot. 
python $COVID_PATH/scripts/postprocess_auto.py -c $COVID_CONFIG_PATH --run-id $COVID_RUN_INDEX --job-name $JOB_NAME --fs-results-path $FS_RESULTS_PATH --slack-token $SLACK_TOKEN

