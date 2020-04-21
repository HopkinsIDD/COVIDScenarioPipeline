#!/bin/bash

if [[ $# -eq 0 ]] ; then
    echo 'Usage: run_dvc.sh <config_file.yml>'
    exit 0
fi

CONFIG=$1

# First, make sure local R packages are up to date.
Rscript local_install.R

if [[ ! -f model_output.dvc ]]; then
  dvc run \
    -d data -d $CONFIG \
    -o model_output -o model_parameters \
    python3 simulate.py -c $CONFIG
  git add model_output.dvc
  git commit -m "Commit model_output.dvc from run"
fi

if [[ ! -f hospitalization.dvc ]]; then
  dvc run \
    -d model_output \
    -o hospitalization \
    Rscript R/scripts/hosp_run.R -c $CONFIG -p .
  git add hospitalization.dvc
  git commit -m "Commit hospitalization.dvc from run"
fi

echo "dvc run commands are saved; batch job is ready to be launched on AWS via batch/launch_job.py"
