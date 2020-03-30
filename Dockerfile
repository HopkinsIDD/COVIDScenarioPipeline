FROM ubuntu:18.04

USER root
ENV TERM dumb

# set locale info
RUN apt-get update && apt-get install -y locales && locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# set noninteractive installation
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    # needed packages
    tzdata \
    sudo \
    less \
    build-essential \
    git-core \
    curl \
    pandoc \
    pandoc-citeproc \
    postgresql-client \
    libpq-dev \
    libssl-dev \
    openssl \
    libyaml-dev \
    libjpeg-dev \
    libxml2-dev \
    libxslt1-dev \
    libffi-dev \
    libblas-dev \
    liblapack-dev \
    libatlas-base-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libudunits2-dev \
    libgdal-dev \
    gfortran \
    unzip \
    zip \
    vim-nox \
    nano \
    libbz2-dev \
    libsqlite3-dev \
    sqlite3 \
    openssh-server \
    libsnappy-dev \
    libncurses-dev \
    libreadline-dev \
    supervisor \
    # make sure we have up-to-date CA certs or curling some https endpoints (like python.org) may fail
    ca-certificates \
    # app user creation
    && useradd -m app \
    && mkdir -p /home/app \
    && chown app:app /home/app \
    # set up sudo for app user
    && sudo echo "app ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/app \
    && sudo usermod -a -G staff app

WORKDIR /home/app
USER app
ENV HOME /home/app

#####
# R
#####

ENV R_VERSION 3.5.3-1bionic

# see https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04
# https://cran.r-project.org/bin/linux/debian/
# https://cran.r-project.org/bin/linux/ubuntu/README.html
RUN set -e \
      && sudo apt-get -y install --no-install-recommends --no-install-suggests \
      	gnupg2 gnupg1 ca-certificates software-properties-common \
      && sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
      && sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'

# TODO: use packrat (or something else) for R package management
COPY packages.R $HOME
RUN set -e \
      && sudo apt-get -y install --no-install-recommends --no-install-suggests \
                            apt-transport-https apt-utils curl default-jdk g++ gcc gdebi-core \
                            gfortran git libapparmor1 libblas-dev libcurl4-gnutls-dev libedit2 \
                            libgtk2.0-dev libssl1.0-dev liblapack-dev libmagick++-dev \
                            libmariadb-client-lgpl-dev libglu1-mesa-dev libopenmpi-dev libpq-dev \
                            libssh2-1-dev libssl1.0-dev libxml2-dev lsb-release openmpi-bin \
                            psmisc r-base=$R_VERSION r-recommended=$R_VERSION sudo x11-common \
      && sudo apt-get -y autoremove \
      && sudo apt-get clean \
      && sudo rm -rf /var/lib/apt/lists/*
RUN Rscript packages.R

#####
# Python (managed via pyenv)
#####

ENV PYENV_ROOT $HOME/.pyenv
ENV PYTHON_VERSION 3.7.6
ENV PYTHON_VENV_DIR $HOME/python_venv
ENV PATH $PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH

RUN git clone git://github.com/yyuu/pyenv.git $HOME/.pyenv \
    && rm -rf $HOME/.pyenv/.git \
    && pyenv install -s $PYTHON_VERSION --verbose \
    && pyenv rehash \
    && echo 'eval "$(pyenv init -)"' >> ~/.bashrc \
    && echo "PS1=\"\[\e]0;\u@\h: \w\a\] \h:\w\$ \"" >> ~/.bashrc

RUN eval "$(pyenv init -)" \
    && pyenv shell $PYTHON_VERSION \
    && pyvenv $PYTHON_VENV_DIR \
    # automatically activate the python venv when logging in
    && echo ". $HOME/python_venv/bin/activate" >> $HOME/.bashrc \
    && . $PYTHON_VENV_DIR/bin/activate

COPY requirements.txt $HOME/requirements.txt
RUN . $PYTHON_VENV_DIR/bin/activate \
    && pip install --upgrade pip setuptools \
    && pip install -r $HOME/requirements.txt

CMD ["/bin/bash"]
