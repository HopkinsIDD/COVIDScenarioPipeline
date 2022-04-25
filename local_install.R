library(devtools)

install.packages(c("covidcast","data.table","vroom","dplyr"), force=TRUE)
  
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])

pkg.dir <- paste0(dirname(script.name), "/R/pkgs/")
install.packages(list.files(pkg.dir,full.names=TRUE),type='source',repos=NULL)
