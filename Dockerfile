FROM ubuntu:18.04

USER root
ENV TERM linux

# set locale info
RUN apt-get update && apt-get install -y locales && locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# set noninteractive installation
ENV DEBIAN_FRONTEND noninteractive
ENV R_VERSION 3.6.3-1bionic
ENV RSTUDIO_VERSION 1.2.5033

# see https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04
# https://cran.r-project.org/bin/linux/debian/
# https://cran.r-project.org/bin/linux/ubuntu/README.html
RUN set -e \
      && apt-get -y install --no-install-recommends --no-install-suggests \
        gnupg2 gnupg1 ca-certificates software-properties-common \
      && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
      && add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/' \
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
    openjdk-8-jdk \
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

# TODO: use packrat (or something else) for R package management
RUN Rscript -e "install.packages('packrat',repos='https://cloud.r-project.org/')" \
    && Rscript -e "install.packages('arrow',repos='https://cloud.r-project.org/')" \
    && Rscript -e 'arrow::install_arrow()'
COPY --chown=app:app packrat $HOME/packrat
COPY --chown=app:app Docker.Rprofile $HOME/.Rprofile
COPY --chown=app:app R/pkgs $HOME/R/pkgs
RUN Rscript -e 'packrat::restore()' \
    && curl -O https://download2.rstudio.org/server/bionic/amd64/rstudio-server-$RSTUDIO_VERSION-amd64.deb \
    && sudo apt-get install -f -y ./rstudio-server-$RSTUDIO_VERSION-amd64.deb \
    && rm -f ./rstudio-server-$RSTUDIO_VERSION-amd64.deb
RUN Rscript -e 'install.packages(list.files("R/pkgs",full.names=TRUE),type="source",repos=NULL)' \

# expose Rstudio port
EXPOSE 8787

#####
# Python (managed via pyenv)
#####

ENV PYENV_ROOT $HOME/.pyenv
ENV PYTHON_VERSION 3.7.6
ENV PYTHON_VENV_DIR $HOME/python_venv
ENV PATH $PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH


RUN git clone git://github.com/yyuu/pyenv.git $HOME/.pyenv \
    && rm -rf $HOME/.pyenv/.git \
    && env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install -s $PYTHON_VERSION --verbose \
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


#####
# Spark
#####

ENV SPARK_VERSION 2.4.5

RUN cd /opt \
    && curl -L http://mirrors.ocf.berkeley.edu/apache/spark/spark-$SPARK_VERSION/spark-$SPARK_VERSION-bin-hadoop2.7.tgz | sudo tar xvfz - \
    && sudo ln -s spark-$SPARK_VERSION-bin-hadoop2.7 spark

CMD ["/bin/bash"]
