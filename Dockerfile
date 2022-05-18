FROM ubuntu:20.04

USER root
ENV TERM linux

# set locale info
RUN apt-get update && apt-get install -y locales && locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# set noninteractive installation
ENV DEBIAN_FRONTEND noninteractive
ENV R_VERSION 4.1.0-1.2004.0

# see https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04
# https://cran.r-project.org/bin/linux/debian/
# https://cran.r-project.org/bin/linux/ubuntu/README.html
RUN set -e \
      && apt-get update \
      && apt-get -y install --no-install-recommends --no-install-suggests \
        gnupg2 gnupg1 ca-certificates software-properties-common \
      && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
      && add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/' \
      && add-apt-repository ppa:git-core/ppa

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    # needed packages
    tzdata \
    sudo \
    less \
    build-essential \
    git-core \
    git-lfs \
    curl \
    pandoc \
    pandoc-citeproc \
    postgresql-client \
    libpq-dev \
    libssl-dev \
    openssl \
    libgdal-dev \
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
    gfortran \
    unzip \
    zip \
    pbzip2 \
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
    awscli \
    r-base-dev=$R_VERSION \
    python3-venv \
    # make sure we have up-to-date CA certs or curling some https endpoints (like python.org) may fail
    ca-certificates \
    # app user creation
    && useradd -m app \
    && mkdir -p /home/app \
    && chown -R app:app /home/app \
    # set up sudo for app user
    && sudo echo "app ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/app \
    && sudo usermod -a -G staff app

WORKDIR /home/app
USER app
ENV HOME /home/app

#####
# R
#####

# Use packrat for R package management
RUN sudo Rscript -e "install.packages('renv',repos='https://cloud.r-project.org/')" \
    && cd /home/app
    # && Rscript -e 'arrow::install_arrow()'
COPY --chown=app:app renv.cache $HOME/.cache
COPY --chown=app:app renv.lock $HOME/renv.lock
COPY --chown=app:app renv $HOME/renv
COPY --chown=app:app Docker.Rprofile $HOME/.Rprofile


#####
# Python (managed via pyenv)
#####

ENV PYENV_ROOT $HOME/.pyenv
ENV PYTHON_VERSION 3.10.1
ENV PYTHON_VENV_DIR $HOME/python_venv
ENV PATH $PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH


RUN git clone https://github.com/yyuu/pyenv.git $HOME/.pyenv \
    && rm -rf $HOME/.pyenv/.git \
    && env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install -s $PYTHON_VERSION --verbose \
    && pyenv rehash \
    && echo 'eval "$(pyenv init -)"' >> ~/.bashrc \
    && echo "pyenv shell $PYTHON_VERSION" >> ~/.bashrc \
    && echo "PS1=\"\[\e]0;\u@\h: \w\a\] \h:\w\$ \"" >> ~/.bashrc

# automatically activate the python venv when logging in
COPY --chown=app:app gempyor_pkg $HOME/gempyor_pkg
RUN eval "$(pyenv init -)" \
    && pyenv shell $PYTHON_VERSION \
    && pip install --upgrade pip setuptools \
    && pip install --user $HOME/gempyor_pkg

CMD ["/bin/bash"]
