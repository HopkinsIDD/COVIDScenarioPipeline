# COVIDScenarioPipeline
Public shared code for doing scenario forecasting and creating reports for various governmental entities.

# Howto

### Set up the repository

**Please see the [Wiki for this repository](https://github.com/HopkinsIDD/COVIDScenarioPipeline/wiki) for updated instructions on how to clone the repository and push/pull changes.**

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
docker run -v /path/to/src:/home/app/src -it hopkinsidd/covidscenariopipeline:latest
```

Replace `/path/to/src` with where the code is mounted on your machine; it will
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
$ CONFIG_PATH=/path/to/config.yml simulate.py -s Wuhan
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
$ simulate.py -n 10 --profile --profile-output $HOME/profile.output -j 1
```

# RStudio

RStudio is installed in the container. To start a new container and connect to RStudio:
```
docker run -v /path/to/src:/home/app/src -p 8787:8787 -it hopkinsidd/covidscenariopipeline:latest rstudio-server start
```

Open [http://localhost:8787](http://localhost:8787) to connect to RStudio. The `-p` argument tells Docker to expose the port at 8787 inside the container _outside_ the container again to port 8787.

You can also start RStudio anytime when bashed in the container by running `rstudio-server start`, but you must have started the container with `-p 8787:8787` to expose the RStudio port.

# To generate the code documents

```
$ doxygen doc/Doxyfile
```
