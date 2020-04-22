library(devtools)
  
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
pkg.dir <- paste0(dirname(script.name), "/R/pkgs/")

install.packages(paste0(pkg.dir, "covidcommon"), type='source',repos=NULL)
install.packages(paste0(pkg.dir, "hospitalization"), type='source',repos=NULL)
install.packages(paste0(pkg.dir, "report_generation"), type='source',repos=NULL)
