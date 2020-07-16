# COVIDScenarioPipeline

Welcome to the Johns Hopkins University Infectious Disease Dynamics COVID-19 Working Group's `COVID Scenario Pipeline`, a flexible modeling framework that projects epidemic trajectories and healthcare impacts under different suites of interventions in order to aid in scenario planning. The model is generic enough to be applied to different spatial scales given shapefiles, population data, and COVID-19 confirmed case data. There are multiple components to the pipeline, which may be characterized as follows: 1) epidemic seeding; 2) disease transmission and non-pharmaceutical intervention scenarios; 3) calculation of health outcomes (hospital and ICU admissions and bed use, ventilator use, and deaths); and 4) summarization of model outputs.

We recommend that most new users use the code from the stable `master` branch. Please post questions to GitHub issues with the `question` tag. We are prioritizing direct support for individuals engaged in public health planning and emergency response.

For more information on getting started, please visit our [wiki](https://github.com/HopkinsIDD/COVID19_Minimal/wiki) at [HopkinsIDD/COVID19_Minimal](https://github.com/HopkinsIDD/COVID19_Minimal). We are trying to keep this page up-to-date for use with the `master` branch.

This open-source project is licensed under GPL v3.0.


# Tools for using this repository
## Docker

A containerized environment is a packaged environment where all
dependencies are bundled together. This means you're guaranteed to be
using the same libraries and system configuration as everyone else and in
any runtime environment. To learn more, [Docker
Curriculum](https://docker-curriculum.com/) is a good starting point.

### Starting environment

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

### Building the container

Run `docker build .` if you ever need to rebuild the container.

# Tools for development
## Profiling

The Python SEIR simulation supports profiling as a command-line option with the
`--profile` flag. To write output to a specific file, use the
`--profile-output` command line option. If you're profiling, it's a good
idea to run single-threaded (`-j 1`) to capture the statistics that would
be lost within the child processes.

Here's an example to run 10 simulations while profiling the simulation and
outputting to `~/profile.output`.

```
$ ./simulate.py -n 10 --profile --profile-output $HOME/profile.output -j 1
```

