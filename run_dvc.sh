#!/bin/bash

if [[ ! -f importation.dvc ]]; then
  dvc run \
    -o importation -o model_output -o model_parameters -o hospitalization \
    Rscript COVIDScenarioPipeline/R/scripts/full_filter.R -p COVIDScenarioPipeline -n 1 -k 10 -j 1
  git add importation.dvc
  git commit -m "Commit model_output.dvc from run"
fi

echo "dvc run commands are saved; batch job is ready to be launched on AWS via batch/launch_job.py"
