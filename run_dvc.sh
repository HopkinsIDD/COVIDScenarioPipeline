#!/bin/bash

if [[ $# -lt 2 ]] ; then
    echo 'Usage: run_dvc.sh <# of slots> <# sims per slot>'
    exit 0
fi

SLOTS=$1
SIMS_PER_SLOT=$2

echo $SLOTS
echo $SIMS_PER_SLOT

if [[ ! -f importation.dvc ]]; then
  dvc run \
    -o importation -o model_output -o model_parameters -o hospitalization \
    Rscript COVIDScenarioPipeline/R/scripts/full_filter.R -p COVIDScenarioPipeline -n $SLOTS -k $SIMS_PER_SLOT -j 1
  git add importation.dvc
  git commit -m "Commit importation.dvc from run"
fi

echo "dvc run commands are saved; batch job is ready to be launched on AWS via batch/launch_job.py"
