# Installs the custom-made packages in this repository

library(devtools)

install.packages(c("covidcast","data.table","vroom","dplyr","jsonlite", "httr"), force=TRUE)
remotes::install_github("hrbrmstr/cdcfluview")

# To run if operating in the container
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)]) # get the name of this file, by looking for the option "--file" in the arguments that were used to start this R instance and getting the term that comes after
pkg.dir <- paste0(dirname(script.name), "/R/pkgs/") # find the directory that this file is within
install.packages(list.files(pkg.dir,full.names=TRUE),type='source',repos=NULL)

# to run within a local instance of R studio

#install.packages(list.files("./R/pkgs/",full.names=TRUE),type='source',repos=NULL) #install from files. Run from COVIDScenarioPipeline folder. Might need to run twice since packages are interdependent and might not be installed in correct order
# devtools::install_github("HopkinsIDD/globaltoolboxlite") #install the covidimportation package from a separate Github repo
# devtools::install_github("HopkinsIDD/covidImportation")
#
