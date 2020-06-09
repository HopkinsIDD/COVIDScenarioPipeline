
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
`COVIDScenarioPipeline` repository. For *Inference* jobs, please run `sudo docker pull hopkinsidd/covidscenariopipeline:latest-dev`;
for *Planning* jobs, please run `sudo docker pull hopkinsidd/covidscenariopipeline:latest`.

In order to launch the container and run a job, we need to make our local `COVID19_Minimal` directory visible to the container's runtime.
For Inference jobs, we do this by running:

```sudo docker run -v /home/ec2-user/COVID19_Minimal:/home/app/src -it hopkinsidd/covidscenariopipeline:latest-dev```

For Planning jobs, we run the same command, but use the `hopkinsidd/covidscenariopipeline:latest` container instead of `latest-dev`.

The `-v` option to `docker run` maps the `/home/ec2-user/COVID19_Minimal` directory in the host filesystem to the `/home/app/src` directory
inside of the container's filesystem (by convention, we run commands inside of the container as a user named `app`.) Thus when we launch the
container, we can `cd src` and then `ls -l` to see the contents of the data repo from within the container.
