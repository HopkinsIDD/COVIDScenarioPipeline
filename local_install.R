pkg.dir <- paste0(dirname(script.name), "/R/pkgs/")
install.packages(list.files(pkg.dir,full.names=TRUE),type='source',repos=NULL)
