
## Introduction

This document contains instructions for setting up and running
the two different kinds of SEIR modeling jobs supported by the
COVIDScenarioPipeline repository on AWS:

1. *Inference* jobs, using AWS Batch to coordinate hundreds/thousands
of jobs across a fleet of servers, and
1. *Planning* jobs, using a single relatively large EC2 instance (usually
an `r5.24xlarge`) to run one or more planning scenarios on a single
high-powered machine.

Most of the steps required to setup and run the two different types of jobs
on AWS are identical, and I will explicitly call out the places where
different steps are required. Throughout the document, we assume that
your client machine is a UNIX-like environment (e.g., OS X, Linux, or WSL).

## Local Client Setup

I need a few things to be true about the local machine that you will be using
to connect to AWS that I'll outline here:

1. You have created and downloaded a `.pem` file for connecting to an EC2
instance to your `~/.ssh` directory. When we provision machines, you'll need to
use the `.pem` file for connecting.
1. You have created a `~/.ssh/config` file that contains an entry that looks like this so we can use `staging` as an alias for your
   provisioned EC2 instance in the rest of the runbook:

    ```
    host staging
    HostName <IP address of provisioned server goes here>
    IdentityFile ~/.ssh/<your .pem file goes here>
    User ec2-user
    IdentitiesOnly yes
    StrictHostKeyChecking no 
    ```
1. You can [connect to Github via SSH.](https://help.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh) This is
important because we will need to use your Github SSH key to interact with private repositories from the `staging` server on EC2.

## Provisioning The Staging Server

If you are running an *Inference* job, you should
use a small instance type for your staging server (e.g., an `m5.xlarge` will be more than enough.) If you are running a *Planning* job, you
should provision a beefy instance type (I am especially partial to the memory-and-CPU heavy `r5.24xlarge`, but given how fast the planning code
has become, an `r5.8xlarge` should be perfectly adequate.)

If you have access to the `jh-covid` account, you should use the *IDD Staging AMI* (`ami-03641dd0c8554e5d0`) to provision and launch new
staging servers; it is already setup with all of the dependencies described in this section. You can find the AMI [here](https://us-west-2.console.aws.amazon.com/ec2/v2/home?region=us-west-2#Images:sort=imageId), select it, and press the *Launch* button to walk you through the Launch Wizard to choose your instance type and `.pem` file to provision your staging server.
Once your instance is provisioned, be sure to put its IP address into the `HostName` section of the `~/.ssh/config` file on your local client so that you can connect to it from your
client by simply typing `ssh staging` in your terminal window.

If you do *not* have access to the `jh-covid` account, you should walk through the regular EC2 Launch Wizard flow and be sure to choose the
*Amazon Linux 2 AMI (HVM), SSD Volume Type* (`ami-0e34e7b9ca0ace12d`, the 64-bit x86 version) AMI. Once the machine is up and running and you can
SSH to it, you will need to run the following code to install the software you will need for the rest of the run:

```
sudo yum -y update
sudo yum -y install awscli 
sudo yum -y install git 
sudo yum -y install docker.io 
sudo yum -y install pbzip2 

curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.rpm.sh | sudo bash
sudo yum -y install git-lfs
git lfs install
```

## Connect to Github

Once your staging server is provisioned and you can connect to it, you should `scp` the private key file that you use for connecting to
Github to the `/home/ec2-user/.ssh` directory on the staging server (e.g., if the local file is named `~/.ssh/id_rsa`, then you should run
`scp ~/.ssh/id_rsa staging:/home/ec2-user/.ssh` to do the copy. For convenience, you should create a `/home/ec2-user/.ssh/config` file on
the staging server that has the following entry:

```
host github.com
 HostName github.com
 IdentityFile ~/.ssh/id_rsa
 User git
```

This way, the `git clone`, `git pull`, etc. operations that you run on the staging server will use your SSH key without constantly prompting
you to login. You should now be able to clone a COVID19 data repository into your home directory on the staging server to do work against.
For this example, I'm going to use the `COVID19_Minimal` repo as my example, so I would run `git clone git@github.com:HopkinsIDD/COVID19_Minimal.git`
to get it onto the staging server. By convention, I usually do runs (for both Planning and Inference jobs) with the `COVIDScenarioPipeline` repository
nested inside of the data repository, so I would then do `cd COVID19_Minimal; git clone git@github.com:HopkinsIDD/COVIDScenarioPipeline.git` to
clone the modeling code itself into a child directory of the data repository.

## Getting and Launching the Docker Container

The previous section is only for getting a minimal set of dependencies setup on your staging server. To do an actual run, you will
need to download the Docker container that contains the more extensive set of dependencies we need for running the code in the
`COVIDScenarioPipeline` repository. To get the development container on your staging server, please run
`sudo docker pull hopkinsidd/covidscenariopipeline:latest-dev`. (There are multiple versions of the container published on DockerHub,
but `latest-dev` contains the latest-and-greatest dependencies and can support both Inference and Planning jobs.)

In order to launch the container and run a job, we need to make our local `COVID19_Minimal` directory visible to the container's runtime.
For Inference jobs, we do this by running:

```sudo docker run -v /home/ec2-user/COVID19_Minimal:/home/app/src -v /home/ec2-user/.ssh:/home/app/.ssh -it hopkinsidd/covidscenariopipeline:latest-dev```

The `-v` option to `docker run` maps a file in the host filesystem (i.e., the path on the left side of the colon) to a file in the container's
filesystem. Here, we are mapping the `/home/ec2-user/COVID19_Minimal` directory on the staging server where we checked out our data repo to
the `/home/app/src` directory in the container (by convention, we run commands inside of the container as a user named `app`.) We also map our
`.ssh` directory from the host filesystem into the container so that we can interact with Github if need be using our SSH keys. Once the
container is launched, we can `cd src; ls -ltr` to look around and ensure that our directory mapping was successful and we see the data and
code files that we are expecting to run with. (One more handy tip: once you're inside of the container, running `export TERM=linux` will give
you a better shell experience inside of the container.)

Once you are in the `src` directory, there are two final steps required to install the R packages and Python modules contained within the
`COVIDScenarioPipeline` repository. Assuming that you created a `COVIDScenarioPipeline` directory within the data repo in the previous step,
you should be able to run `Rscript COVIDScenarioPipeline/local_install.R` to install the local R packages and then
`(cd COVIDScenarioPipeline/; python setup.py build install)` to install the Python modules.

## Running Inference Jobs

Once the container is setup from the previous step, we are ready to test out and then launch an inference job against a configuration
file (I will use the example of `config.yml` for the rest of this document.) First, I setup and run the `build_US_setup.R` script against
my configuration file to ensure that the mobility data is up to date:

```
export CENSUS_API_KEY=<your census api key>
cd COVIDScenarioPipeline
git lfs pull
cd ..
Rscript COVIDScenarioPipelineR/scripts/build_US_setup.R -c config.yml
```

Next, I kick off a small local run of the `full_filter.R` script. This serves two purposes: first, we can verify that the configuration file
is in good shape and can support a few small iterations of the inference calculations before we kick off hundreds/thousands of jobs via AWS
Batch. Second, it downloads the case data that we need for inference calculations to the staging server so that it can be cached locally
and used by the batch jobs on AWS- if we do not have a local cache of this data at the start of the run, then every job will try to download
the data itself, which will force the upstream server to deny service to the worker jobs, which will cause all of the jobs to fail. My small
runs usually look like:

```
Rscript COVIDScenarioPipeline/R/scripts/full_filter.R -c config.yml -k 2 -n 1 -j 1 -p COVIDScenarioPipeline
```

This will run two sequential simulations (`-k 2`) for a single slot (`-n 1`) using a single CPU core (`-j 1`), looking for the modeling
source code in the `COVIDScenarioPipeline` directory (`-p COVIDScenarioPipeline`). (We need to use the command line arguments here to
explicitly override the settings of these parameters inside of `config.yml` since this run is only for local testing.) Assuming that this
run succeeds, we are ready to kick off a batch job on the cluster.

The `COVIDScenarioPipeline/batch/inference_job.py` script will use the contents of the current directory and the values of the config file
and any commandline arguments we pass it to launch a run on AWS Batch via the AWS API. To run this script, you need to have access to your
[AWS access keys](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) so that you can enable access to the
API by running `aws configure` at the command line, which will prompt you to enter your access key, secret, and preferred region, which should always be `us-west-2`
for `jh-covid` runs. (You can leave the `Default format` entry blank by simply hitting Enter.) *IMPORTANT REMINDER:* (Do not give anyone your
access key and secret. If you lose it, deactivate it on the AWS console and create a new one. Keeep it safe.)

The simplest way to launch an inference job is to simply run `./COVIDScenarioPipeline/batch/inference_job.py -c config.yml`. This will use the contents
of the config file to determine how many slots to run, how many simulations to run for each slot, and how to break those simulations up into _blocks_ of
batch jobs that run sequentially. If you need to override any of those settings at the command line, you can run `./COVIDScenarioPipeline/batch/inference_job.py --help`
to see the full list of command line arguments the script takes and how to set them.

Inference jobs are parallelized by NPI scenarios and hospitalization rates, so if your config file defines more than one top-level scenario or more than one
set of hospitalization parameters, the `inference_job.py` script will kick off a separate batch job for the cross product of scenarios * hospitalizations. The
script will announce that it is launching each job and will print out the path on S3 where the final output for the job will be written. You can monitor the
progress of the running jobs using either the [AWS Batch Dashboard](https://us-west-2.console.aws.amazon.com/batch/home?region=us-west-2#/dashboard) or with the
`COVIDScenarioPipeline/batch/inference_job_status.py` script, which you can run from within the container to see the current status of the running array
jobs within each of the batch job queues.



