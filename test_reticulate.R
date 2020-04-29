
# install.packages('xts', repos='http://cran.us.r-project.org')
# install.packages('zoo', repos='http://cran.us.r-project.org')
# devtools::install_github("HopkinsIDD/covidImportation")

# Preamble ---------------------------------------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(foreach)
library(magrittr)
library(reticulate)
use_python("/usr/local/bin/python3.7")

# Load the setup
py_run_file("COVIDScenarioPipeline/minimal_interface.py")

py$onerun_SEIR_loadID(1, py$s, 1)
print('done a sim')
py$onerun_SEIR_loadID(1, py$s, 1)
print('and another')
py$onerun_SEIR_loadID(1, py$s, 1)
print('its very fast')