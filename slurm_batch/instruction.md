# CSP slurm submission

## Setup (DO ONCE)

```
bash Miniconda3-py39_4.10.3-Linux-x86_64.sh
conda env create --name covidSProd6 --file=environment_cross.yml
```

## Everytime

0. Make sure you have the pipeline and data repo up to date, in the right branch.

1. Make COVID_PATH and DATA_PATH point to the right folders, **in absolute path** !

if you are folder with subfolders
```
COVID19_USA/
COVIDScenarioPipeline/
```
then:
```
export COVID_PATH=$(pwd)/COVIDScenarioPipeline
export DATA_PATH=$(pwd)/COVID19_USA
```
or, depending on your configuration
```
export COVID_PATH=$(pwd)/COVID19_USA/COVIDScenarioPipeline
export DATA_PATH=$(pwd)/COVID19_USA
```

& both are up to date & at the right branch.

Setup site wide Rprofile. 
```
export R_PROFILE=$COVID_PATH/slurm_batch/Rprofile
```

at some point:
````
  export CENSUS_API_KEY=_YOURAPIKEYREPLACETHIS_
```

2. Install everything and cleanup

```bash
conda activate covidSProd6

cd $COVID_PATH &&
  Rscript local_install.R &&
  python setup.py develop --no-deps && ## /!\ THis has changed
  cd $DATA_PATH && 
  rm -rf model_output && 
  rm -rf data/mobility_territories.csv data/geodata_territories.csv data/us_data.csv
```

3. Setup environment variable

````
export SCENARIO=R7_lowVac_highVar_CA-WA-FL-DE-MA &&
  export VALIDATION_DATE="2021-07-04" &&
  export COVID_RUN_INDEX=$SCENARIO &&
  export CONFIG_NAME=config_SMH_$SCENARIO.yml &&
  export CONFIG_PATH=$DATA_PATH/$CONFIG_NAME &&
  export INTERVENTION_NAME="med" &&
  export COVID_STOCHASTIC=FALSE
```


4. Run the full model once to pull & check that everything is alright
```
cd $DATA_PATH
  Rscript $COVID_PATH/R/scripts/build_US_setup.R -c $CONFIG_NAME &&
  Rscript $COVID_PATH/R/scripts/full_filter.R -c $CONFIG_NAME -j 2 -n 1 -k 1 &&
```
