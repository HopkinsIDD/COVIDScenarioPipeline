#!/bin/bash

# set -x

# Expected environment variables from AWS Batch env
# S3_MODEL_DATA_PATH location in S3 with the code, data, and dvc pipeline to run
# S3_RESULTS_PATH location in S3 to store the results

# setup the python environment
# HOME=/home/app
# PYENV_ROOT=$HOME/.pyenv
# PYTHON_VERSION=3.7.6
# PYTHON_VENV_DIR=$HOME/python_venv
# PATH=$PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH
# . $PYTHON_VENV_DIR/bin/activate

# set optimized S3 configuration
aws configure set default.s3.max_concurrent_requests 100
aws configure set default.s3.max_queue_size 100
aws configure set default.s3.multipart_threshold 8MB
aws configure set default.s3.multipart_chunksize 8MB

# S3_LAST_JOB_OUTPUT=s3://idd-inference-runs/USA-1588740631_local_variance_LockdownWideEffect_post50_firstcase_10x_med/0834223c-e1f6-44a3-8819-4515aafd6ca4
# S3_RESULTS_PATH=s3://idd-inference-runs/USA-15888740631_local_variance_LockdownWideEffect_post50_firstcase_10x_med
# S3_RESULTS_PATH="./test_output"
# NSIM=1

printf -v S3_LAST_JOB_OUTPUT_esc "%q" $S3_LAST_JOB_OUTPUT
printf -v S3_MODEL_DATA_PATH_esc "%q" $S3_MODEL_DATA_PATH

copy_file_of_type() {
	all_files=$1
	filetype=$2
	i=$3

	in_file=$(printf '%s\n' "${all_files[@]}" | grep 000000001.$filetype.parquet)
	in_file=s3://idd-inference-runs/$in_file
	# Replace folder of last block with destination folder
	out_file=${in_file/$S3_LAST_JOB_OUTPUT_esc:$i/$S3_MODEL_DATA_PATH_esc}
	# Replace sim ID 000000001 with correct sim ID
	printf -v sim "%09d" $i
	out_file=${out_file/000000001.$filetype.parquet/$sim.$filetype.parquet}
	echo "Copying $in_file to $out_file"
	aws s3 cp $in_file $out_file
}

for i in $(seq 0 $NSLOT)
do
	# Get list of files for slot
	all_files=($(aws s3 ls --recursive $S3_LAST_JOB_OUTPUT:$i/ | awk '{print $4}'))

	# Copy the data
	copy_file_of_type $all_files "hosp" $i
	copy_file_of_type $all_files "snpi" $i
	copy_file_of_type $all_files "spar" $i
done

echo "Done"
exit 0
