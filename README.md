# COVIDScenarioPipeline
Public shared code for doing scenario forecasting and creating reports for various governmental entities.

# Howto

### Set up the repository

**Please see the [Wiki for this repository](https://github.com/HopkinsIDD/COVIDScenarioPipeline/wiki) for updated instructions on how to clone the repository and push/pull changes.**

If making changes to this repository, please do it directly instead of through the submodule of another repository.

### Run the code

After cloning the repository (see [Wiki](https://github.com/HopkinsIDD/COVIDScenarioPipeline/wiki) for instructions on how to clone a repository with submodules) create setup in a subfolder of the `data` using the notebook `data/build-model-input.ipynb`. This creates two files:

* `mobility.txt` : mobility matrix from us commute, as in [An Economic Geography of the United States: From Commutes to Megaregions by Garrett Dash Nelson and Alasdair Rae](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0166083) processed by [Ed King on Kaggle](https://www.kaggle.com/kinguistics/visualizing-u-s-commutes)
* `geodata.csv` : specification of the spatial nodes, with at least column for the index, the geoid or name, the population.

Then copy the `main_template.py` from this repo into the root of the SPATIAL_SETUP_REPO folder. Changes the first line and you're ready to run it.

if you haven't installed python packages, run

```
pip3 install -r requirements.txt
```

You can now run the code:
```
nohup python3 main.py > out.txt &
```
creates a `figure/` and a `SCENARIO_model_output_TIMESTAMP` folder.


# Docker

A containerized environment is a packaged environment where all
dependencies are bundled together. This means you're guaranteed to be
using the same libraries and system configuration as everyone else and in
any runtime environment. To learn more, [Docker
Curriculum](https://docker-curriculum.com/) is a good starting point.

## Starting environment

A pre-built container can be pulled from Docker Hub via:
```
docker pull hopkinsidd/covidscenariopipeline:latest
```

To start the container:
```
docker run -v ~/mysrcdir:/home/app/src -it hopkinsidd/covidscenariopipeline:latest
```

Replace `mysrcdir` with where the code is mounted on your machine; it will
be available in the `/home/app/src` directory inside the container.

You'll be dropped to the bash prompt where you can run the Python or
R scripts (with dependencies already installed).

## Building the container

Run `docker build .` if you ever need to rebuild the container.

# Configuration files

The pipeline now uses a configuration file to set simulation parameters.
A template can be found in `config.yml`. The easiest way to specify this
config file to jobs is to use the `CONFIG_PATH` environment variable:

```
$ CONFIG_PATH=/path/to/config.yml python COVIDScenarioPipeline/simulate.py -s Wuhan
    [...]
$ CONFIG_PATH=/path/to/config.yml Rscript hosp_run.R
```

# Profiling

The Python simulation supports profiling as a command-line option with the
`--profile` flag. To write output to a specific file, use the
`--profile-output` command line option. If you're profiling, it's a good
idea to run single-threaded (`-j 1`) to capture the statistics that would
be lost within the child processes.

Here's an example to run 10 simulations while profiling the simulation and
outputting to `~/profile.output`.

```
$ ./simulate.py -n 10 --profile --profile-output $HOME/profile.output -j 1
```

# To generate the code documents

```
$ doxygen doc/Doxyfile
```
