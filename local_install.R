# Installs the custom-made packages in this repository
library(devtools)

pkg.dir <- paste0(dirname(script.name), "/R/pkgs/") # find the directory that this file is within
install.packages(list.files(pkg.dir,full.names=TRUE),type='source',repos=NULL)
