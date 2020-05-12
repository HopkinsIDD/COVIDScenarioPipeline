#!/bin/bash

# This script copies and renames the hospitalization and parameter files from S3_RESULTS_PATH to S3_MODEL_DATA_PATH.
# To download the results from S3_MODEL_DATA_PATH, run
# aws s3 sync $S3_MODEL_DATA_PATH/final_output <local_output_dir>

# Expected environment variables from AWS Batch env
# S3_MODEL_DATA_PATH location in S3 with the code, data, and dvc pipeline to run
# S3_RESULTS_PATH location in S3 to store the results
# NSLOT number of slots

# set optimized S3 configuration
aws configure set default.s3.max_concurrent_requests 100
aws configure set default.s3.max_queue_size 100
aws configure set default.s3.multipart_threshold 8MB
aws configure set default.s3.multipart_chunksize 8MB

# NSLOT=2
# S3_RESULTS_PATH="./test_output"
# S3_RESULTS_PATH=s3://idd-inference-runs/test-east-coast-20200512T003641
# S3_LAST_JOB_OUTPUT=s3://idd-inference-runs/test-east-coast-20200512T003641/e4c7f6d2-10f3-4f8f-840f-986d77962fc9

printf -v S3_LAST_JOB_OUTPUT_esc "%q" $S3_LAST_JOB_OUTPUT
printf -v S3_RESULTS_PATH_esc "%q" $S3_RESULTS_PATH
bucket=${S3_LAST_JOB_OUTPUT#s3:\/\/}
bucket=${bucket%%/*}

copy_file_of_type() {
	all_files=$1
	filetype=$2
	i=$3

	in_file=$(printf '%s\n' "${all_files[@]}" | grep 000000001.$filetype.parquet)
	if [[ $in_file == "" ]]; then
		echo "000000001.$filetype.parquet not found in files"
		exit 1
	fi
	in_file=s3://$bucket/$in_file
	# Replace folder of last block with destination folder
	out_file=${in_file/$S3_LAST_JOB_OUTPUT_esc:$i/$S3_RESULTS_PATH_esc\/final_output}
	# Replace sim ID 000000001 with correct sim ID
	printf -v sim "%09d" $(($i+1))
	out_file=${out_file/000000001.$filetype.parquet/$sim.$filetype.parquet}
	echo "Copying $in_file to $out_file"
	aws s3 cp $in_file $out_file
}

for i in $(seq 0 $(($NSLOT-1)))
do
	# Get list of files for slot
	all_files=($(aws s3 ls --recursive $S3_LAST_JOB_OUTPUT:$i/ | awk '{print $4}'))

	if [ ${#all_files[@]} -eq 0 ]; then
		echo "No files found at $S3_LAST_JOB_OUTPUT:$i/"
		exit 1
	fi

	# Copy the data
	copy_file_of_type $all_files "hosp" $i
	copy_file_of_type $all_files "snpi" $i
	copy_file_of_type $all_files "spar" $i
done

echo "Done"
exit 0
