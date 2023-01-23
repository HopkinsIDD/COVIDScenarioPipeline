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
if [ $failure_count -gt 100 ]; then
	echo "Failing run because total number of previous child job failures is $failure_count"
	exit 1
fi
echo "***************** LOADING ENVIRONMENT *****************"
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
tar -xzf model_data.tar.gz -C model_data # chadi: removed v(erbose) option here as it floods the log with data we have anyway from the s3 bucket
cd model_data

# check for presence of LAST_JOB_OUTPUT and download the
# output from the corresponding last job here
export COVID_SLOT_INDEX=$(python -c "print($AWS_BATCH_JOB_ARRAY_INDEX + 1)")

error_handler() {
	msg=$1
	if [ $AWS_BATCH_JOB_ATTEMPT -eq 3 ]; then
		echo $JOB_NAME >> errorfile
		echo $msg >> errorfile
		aws s3 cp integration_dump.pkl $S3_RESULTS_PATH/failures/integration_dump.$AWS_BATCH_JOB_ARRAY_INDEX.pkl
		aws s3 cp errorfile $S3_RESULTS_PATH/failures/$AWS_BATCH_JOB_ARRAY_INDEX
		exit 0
	else
		echo $msg
		exit 1
	fi
}

# Note $COVID_PATH because here we're using the tar file of the pipeline, untarred in pwd.
Rscript COVIDScenarioPipeline/local_install.R
local_install_ret=$?

if [ $local_install_ret -ne 0 ]; then
	error_handler "Error code returned from running local_install.R: $local_install_ret"
fi

python -m pip install --upgrade pip # needs new pip for toml file

(cd COVIDScenarioPipeline && pip install -e gempyor_pkg)
python_install_ret=$?
if [ $python_install_ret -ne 0 ]; then
	error_handler "Error code returned from running `pip install -e gempyor_pkg`: $python_install_ret"
fi
echo "***************** DONE LOADING ENVIRONMENT *****************"

echo "***************** FETCHING RESUME FILES *****************"
### In case of resume, download the right files from s3
## Remove trailing slashes
export LAST_JOB_OUTPUT=$(echo $LAST_JOB_OUTPUT | sed 's/\/$//')
if [ -n "$LAST_JOB_OUTPUT" ]; then  # -n Checks if the length of a string is nonzero --> if LAST_JOB_OUTPUT is not empty, the we download the output from the last job
	if [ $COVID_BLOCK_INDEX -eq 1 ]; then
		export RESUME_RUN_INDEX=$COVID_OLD_RUN_INDEX
		echo "RESUME_DISCARD_SEEDING is set to $RESUME_DISCARD_SEEDING"
		if [ $RESUME_DISCARD_SEEDING == "true" ]; then
			export PARQUET_TYPES="spar snpi hpar hnpi"
		else
			export PARQUET_TYPES="seed spar snpi hpar hnpi"
		fi
	else                                 # if we are not in the first block, we need to resume from the last job, with seeding an all.
		export RESUME_RUN_INDEX=$COVID_RUN_INDEX
		export PARQUET_TYPES="seed spar snpi seir hpar hnpi hosp llik"
	fi
	for filetype in $PARQUET_TYPES
	do
		if [ $filetype == "seed" ]; then
			export extension="csv"
		else
			export extension="parquet"
		fi
		for liketype in "global" "chimeric"
		do
			export OUT_FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$COVID_RUN_INDEX','$COVID_PREFIX/$COVID_RUN_INDEX/$liketype/intermediate/%09d.'% $COVID_SLOT_INDEX,$COVID_BLOCK_INDEX-1,'$filetype','$extension'))")
			if [ $COVID_BLOCK_INDEX -eq 1 ]; then
				export IN_FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$RESUME_RUN_INDEX','$COVID_PREFIX/$RESUME_RUN_INDEX/$liketype/final/',$COVID_SLOT_INDEX,'$filetype','$extension'))")
			else
				export IN_FILENAME=$OUT_FILENAME
			fi
			aws s3 cp --quiet $LAST_JOB_OUTPUT/$IN_FILENAME $OUT_FILENAME
			if [ -f $OUT_FILENAME ]; then
				echo "Copy successful for file of type $filetype ($IN_FILENAME -> $OUT_FILENAME)"
			else
				echo "Could not copy file of type $filetype ($IN_FILENAME -> $OUT_FILENAME)"
				if [ $liktype -eq "global" ]; then
					exit 2
				fi
			fi
		done
	done
	ls -ltr model_output
fi
echo "***************** DONE FETCHING RESUME FILES *****************"

echo "State of directory before we start"
echo "==="
find model_output
echo "---"
find data
echo "==="

echo "***************** RUNNING FILTER_MC.R *****************"
# NOTE(jwills): hard coding this for now
Rscript COVIDScenarioPipeline/R/scripts/filter_MC.R -p COVIDScenarioPipeline
dvc_ret=$?
if [ $dvc_ret -ne 0 ]; then
        error_handler "Error code returned from full_filter.R: $dvc_ret"
fi
echo "***************** DONE RUNNING FILTER_MC.R *****************"

echo "***************** UPLOADING RESULT TO S3 *****************"
for type in "seir" "hosp" "llik" "spar" "snpi" "hnpi" "hpar"
do
	export FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$COVID_RUN_INDEX','$COVID_PREFIX/$COVID_RUN_INDEX/chimeric/intermediate/%09d.'% $COVID_SLOT_INDEX,$COVID_BLOCK_INDEX,'$type','parquet'))")
	aws s3 cp --quiet $FILENAME $S3_RESULTS_PATH/$FILENAME
done
	for type in "seed"
	do
		export FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$COVID_RUN_INDEX','$COVID_PREFIX/$COVID_RUN_INDEX/chimeric/intermediate/%09d.'% $COVID_SLOT_INDEX,$COVID_BLOCK_INDEX,'$type','csv'))")
	aws s3 cp --quiet $FILENAME $S3_RESULTS_PATH/$FILENAME
done
	for type in "seed"
	do
		export FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$COVID_RUN_INDEX','$COVID_PREFIX/$COVID_RUN_INDEX/global/intermediate/%09d.'% $COVID_SLOT_INDEX,$COVID_BLOCK_INDEX,'$type','csv'))")
	aws s3 cp --quiet $FILENAME $S3_RESULTS_PATH/$FILENAME
done
	for type in "seir" "hosp" "llik" "spar" "snpi" "hnpi" "hpar"
do
	export FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$COVID_RUN_INDEX','$COVID_PREFIX/$COVID_RUN_INDEX/global/intermediate/%09d.'% $COVID_SLOT_INDEX,$COVID_BLOCK_INDEX,'$type','parquet'))")
	aws s3 cp --quiet $FILENAME $S3_RESULTS_PATH/$FILENAME
done
	for type in "seir" "hosp" "llik" "spar" "snpi" "hnpi" "hpar"
do
	export FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$COVID_RUN_INDEX','$COVID_PREFIX/$COVID_RUN_INDEX/global/final/', $COVID_SLOT_INDEX,'$type','parquet'))")
	aws s3 cp --quiet $FILENAME $S3_RESULTS_PATH/$FILENAME
done
	for type in "seed"
do
	export FILENAME=$(python -c "from gempyor import file_paths; print(file_paths.create_file_name('$COVID_RUN_INDEX','$COVID_PREFIX/$COVID_RUN_INDEX/global/final/', $COVID_SLOT_INDEX,'$type','csv'))")
	aws s3 cp --quiet $FILENAME $S3_RESULTS_PATH/$FILENAME
done
echo "***************** DONE UPLOADING RESULT TO S3 *****************"

echo "Done"
exit 0
