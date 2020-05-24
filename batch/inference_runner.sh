#!/bin/bash

set -x

# Expected environment variables from AWS Batch env
# S3_MODEL_DATA_PATH location in S3 with the code, data, and dvc pipeline to run
# DVC_OUTPUTS the names of the directories with outputs to save in S3, separated by a space
# SIMS_PER_JOB is the number of sims to run per job
# JOB_NAME the name of the job
# S3_RESULTS_PATH location in S3 to store the results

# Check to see if we should bail on this run because of accumulated errors in other runs
failure_count=$(aws s3 ls $S3_RESULTS_PATH/failures/ | wc -l)
if [ $failure_count -gt 10 ]; then
	echo "Failing run because number of child job failurs is $failure_count"
	exit 1
fi

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
		aws s3 cp --quiet --recursive $S3_LAST_JOB_OUTPUT:$AWS_BATCH_JOB_ARRAY_INDEX/$output/ $output/
		ls -ltr $output
	done
fi

error_handler() {
	msg=$1
	if [ $AWS_BATCH_JOB_ATTEMPT -eq 3]; then
		echo $JOB_NAME >> errorfile
		echo $msg >> errorfile
		aws s3 cp errorfile $S3_RESULTS_PATH/failures/$AWS_BATCH_JOB_ARRAY_INDEX
		exit 0
	else
		echo $1
		exit 1
	fi
}

# Pick up stuff that changed
# TODO(jwills): maybe move this to like a prep script?
Rscript COVIDScenarioPipeline/local_install.R
local_install_ret=$?

if [ $local_install_ret -ne 0 ]; then
	error_handler "Error code returned from running local_install.R: $local_install_ret"
fi


(cd COVIDScenarioPipeline && python setup.py build install)
python_install_ret=$?

if [ $python_install_ret -ne 0 ]; then
	error_handler "Error code returned from running `python setup.py install`: $python_install_ret"
fi

echo "State of directory before we start"
echo "==="
ls
echo "---"
find hospitalization
echo "---"
find model_parameters
echo "---"
find model_output
echo "---"
find importation
echo "---"
find data
echo "==="

# NOTE(jwills): hard coding this for now
Rscript COVIDScenarioPipeline/R/scripts/full_filter.R -p COVIDScenarioPipeline -n 1 -k $SIMS_PER_JOB -j 1

dvc_ret=$?
if [ $dvc_ret -ne 0 ]; then
        error_handler "Error code returned from full_filter.R: $dvc_ret"
fi

for output in "${DVC_OUTPUTS_ARRAY[@]}"
do
	if [ -d "$output" ]; then
		aws s3 cp --quiet --recursive $output $S3_RESULTS_PATH/$JOB_NAME:$AWS_BATCH_JOB_ARRAY_INDEX/$output/
	fi
done

echo "Done"
exit 0
