#!/bin/bash

set -x

# Expected environment variables from AWS Batch env
# S3_MODEL_DATA_PATH location in S3 with the code, data, and dvc pipeline to run
# DVC_TARGET the name of the dvc file in the model that should be reproduced locally.
# DVC_OUTPUTS the names of the directories with outputs to save in S3, separated by a space
# S3_RESULTS_PATH location in S3 to store the results

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
aws s3 cp --quiet $S3_MODEL_DATA_PATH model_data.tar.gz
mkdir model_data
tar -xvzf model_data.tar.gz -C model_data
cd model_data

# check for presence of S3_LAST_JOB_OUTPUT and download the
# output from the corresponding last job here
DVC_OUTPUTS_ARRAY=($DVC_OUTPUTS)
if [ -n "$S3_LAST_JOB_OUTPUT" ]; then
	for output in "${DVC_OUTPUTS_ARRAY[@]}"
	do
		aws s3 cp --quiet --recursive $S3_LAST_JOB_OUTPUT:AWS_BATCH_JOB_ARRAY_INDEX/$output/ $output/
		ls -ltr $output
	done
fi

# Pick up stuff that changed
# TODO(jwills): maybe move this to like a prep script?
Rscript COVIDScenarioPipeline/local_install.R
local_install_ret=$?

if [ $local_install_ret -ne 0 ]; then
	echo "Error code returned from running local_install.R: $local_install_ret"
	exit 1
fi

(cd COVIDScenarioPipeline && python setup.py install)
python_install_ret=$?

if [ $python_install_ret -ne 0 ]; then
	echo "Error code returned from running `python setup.py install`: $python_install_ret"
	exit 1
fi

# Initialize dvc and run the pipeline to re-create the
# dvc target
dvc init --no-scm
dvc repro $DVC_TARGET

dvc_ret=$?
if [ $dvc_ret -ne 0 ]; then
        echo "Error code returned from dvc_repro: $dvc_ret"
	exit 1
fi

for output in "${DVC_OUTPUTS_ARRAY[@]}"
do
	if [ -d "$output" ]; then
		aws s3 cp --quiet --recursive $output $S3_RESULTS_PATH/$AWS_BATCH_JOB_ID/$output/
	fi
done

echo "Done"
exit 0
