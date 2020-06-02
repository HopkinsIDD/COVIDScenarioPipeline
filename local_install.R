library(devtools)
  
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
pkg.dir <- paste0(dirname(script.name), "/R/pkgs/")

install_local(paste0(pkg.dir, "covidcommon"), force=TRUE, upgrade="never")
install_local(paste0(pkg.dir, "hospitalization"), force=TRUE, upgrade="never")
install_local(paste0(pkg.dir, "report_generation"), force=TRUE, upgrade="never")
