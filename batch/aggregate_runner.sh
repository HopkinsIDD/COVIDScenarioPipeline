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

INFERENCE_PATHS_ARRAY=($INFERENCE_PATHS)
for inference_path in "${INFERENCE_PATHS_ARRAY[@]}"
do
	python3 output_mover.py -n $NUM_JOBS -l $SLOTS_PER_JOB -i $inference_path -o $LOCAL_OUTPUT_PATH
done

aws s3 cp --quiet --recursive $LOCAL_OUTPUT_PATH $S3_RESULTS_PATH

echo "Done"
exit 0
