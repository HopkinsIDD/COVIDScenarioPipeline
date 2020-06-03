#!/bin/bash

if [ $# -ne 1 ]; then
	echo "Add argument <run name>"
	exit 1
fi

RUN_NAME=$1

aws s3 sync s3://idd-inference-runs/$RUN_NAME/final_output $RUN_NAME/final_output
aws s3 cp s3://idd-inference-runs/$RUN_NAME.tar.gz $RUN_NAME/input.tar.gz
aws s3 cp s3://idd-inference-runs/$RUN_NAME/manifest.json $RUN_NAME/manifest.json
