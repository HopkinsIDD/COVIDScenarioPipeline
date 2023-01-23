#!/bin/bash

# This script copies and renames the hospitalization and parameter files from LAST_JOB_OUTPUT to S3_RESULTS_PATH/final_output.
# To download the results from S3_RESULTS_PATH, run
# aws s3 sync $S3_RESULTS_PATH/final_output <local_output_dir>

# Expected environment variables from AWS Batch env
# LAST_JOB_OUTPUT location in S3 with the code, data, and dvc pipeline to run
#    Example: s3://idd-inference-runs/test-east-coast-20200512T003641/e4c7f6d2-10f3-4f8f-840f-986d77962fc9
# S3_RESULTS_PATH location in S3 to store the results. Usually, s3://<run name>.
#    Example: s3://idd-inference-runs/test-east-coast-20200512T003641
# NSLOT number of slots 
#    Example: 2

# set optimized S3 configuration
aws configure set default.s3.max_concurrent_requests 100
aws configure set default.s3.max_queue_size 100
aws configure set default.s3.multipart_threshold 8MB
aws configure set default.s3.multipart_chunksize 8MB

printf -v LAST_JOB_OUTPUT_esc "%q" $LAST_JOB_OUTPUT
printf -v S3_RESULTS_PATH_esc "%q" $S3_RESULTS_PATH
bucket=${LAST_JOB_OUTPUT#s3:\/\/}
bucket=${bucket%%/*}

copy_file_of_type() {
	all_files=$1
	filetype=$2
	i=$3

	in_file=$(printf '%s\n' "${all_files[@]}" | grep 000000001.$filetype.parquet)
	if [[ $in_file == "" ]]; then
		echo "000000001.$filetype.parquet not found in files"
	else
		in_file=s3://$bucket/$in_file
		# Replace folder of last block with destination folder
		out_file=${in_file/$LAST_JOB_OUTPUT_esc:$i/$S3_RESULTS_PATH_esc\/final_output}
		# Replace sim ID 000000001 with correct sim ID
		printf -v sim "%09d" $(($i+1))
		out_file=${out_file/000000001.$filetype.parquet/$sim.$filetype.parquet}
		echo "Copying $in_file to $out_file"
		aws s3 cp $in_file $out_file
	fi
}

for i in $(seq 0 $(($NSLOT-1)))
do
	# Get list of files for slot
	all_files=($(aws s3 ls --recursive $LAST_JOB_OUTPUT:$i/ | awk '{print $4}'))

	if [ ${#all_files[@]} -eq 0 ]; then
		echo "No files found at $LAST_JOB_OUTPUT:$i/"
	else
		# Copy the data
		copy_file_of_type $all_files "hosp" $i
		copy_file_of_type $all_files "snpi" $i
		copy_file_of_type $all_files "hnpi" $i
		copy_file_of_type $all_files "spar" $i
	fi
done

echo "Done"
exit 0
