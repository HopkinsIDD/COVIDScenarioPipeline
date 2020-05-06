#!/bin/bash

set -x

# setup the python environment
HOME=/home/app
PYENV_ROOT=$HOME/.pyenv
PYTHON_VERSION=3.7.6
PYTHON_VENV_DIR=$HOME/python_venv
PATH=$PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH
. $PYTHON_VENV_DIR/bin/activate

# set optimized S3 configuration
aws configure set default.s3.max_concurrent_requests 100
aws configure set default.s3.max_queue_size 100
aws configure set default.s3.multipart_threshold 8MB
aws configure set default.s3.multipart_chunksize 8MB

# Copy the complete model + data package from S3 and
# install the local R packages
aws s3 cp --quiet $S3_AGG_CODE_PATH agg_code.tar.gz
mkdir agg_code
tar -xvzf agg_code.tar.gz -C agg_code
cd agg_code

python3 COVIDScenarioPipeline/scripts/output_mover.py -n $NUM_JOBS -l $SLOTS_PER_JOB -i $INFERENCE_PATHS -o "hospitalization/model_output"

# Copy the output of this aggregation step to S3
aws s3 cp --quiet --recursive hospitalization/ $S3_RESULTS_PATH/hospitalization/

# Run the Spark code to do the quantile summarization
SPARK_DRIVER_MEMORY=20g
SPARK_EXECUTOR_MEMORY=20g
# Update R packages
Rscript COVIDScenarioPipeline/local_install.R

mkdir -p $OUTPUT_PATH
Rscript COVIDScenarioPipeline/scripts/QuantileSummarizeGeoExtent.R -o $OUTPUT_PATH/quantile_geo_extent.csv $SCENARIO
Rscript COVIDScenarioPipeline/scripts/QuantileSummarizeStateLevel.R -o $OUTPUT_PATH/quantile_state_level.csv $SCENARIO
/opt/spark/bin/spark-submit --driver-memory $SPARK_DRIVER_MEMORY --executor-memory $SPARK_EXECUTOR_MEMORY COVIDScenarioPipeline/scripts/quantile_summarize_geoid_level.py $SCENARIO -o $OUTPUT_PATH/quantile_geoid_level.csv

# Copy the output of the quantile analysis to S3
aws s3 cp --quiet --recursive $OUTPUT_PATH $S3_RESULTS_PATH/$OUTPUT_PATH

echo "Done"
exit 0
