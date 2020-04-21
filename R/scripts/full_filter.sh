CSP=~/git/COVIDScenarioPipeline/
RSCRIPT=Rscript
J=8
CONFIG=config.yml
PYTHON=python3
K=1000
for N in {1..100}
do
  $PYTHON $CSP/simulate.py -j $J -c $CONFIG -n $K --write-parquet
  $RSCRIPT $CSP/R/scripts/hosp_run.R -j $J -c $CONFIG -p $CSP
  echo "HERE"
  echo $N
  $RSCRIPT $CSP/R/scripts/filter_MC.R -j $J -c $CONFIG -k $N -d high
done
