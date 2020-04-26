## ----echo=FALSE---------------------------------------------------------------
library("knitr")
Rscript_executable <- paste(file.path(R.home(), "bin", "Rscript"), "--vanilla")
opts_knit$set(root.dir = system.file("exec", package="optparse")) # to access the "Rscript files"
opts_chunk$set(comment=NA, echo=FALSE) 
# setwd(system.file("exec", package="optparse")) # to access the "Rscript files"
list_file_command <- "ls"
chmod_command <- "chmod ug+x display_file.R example.R"
path_command <- "export PATH=$PATH:`pwd`"
run_command <- function(string) { suppressWarnings(cat(system(string, intern=TRUE), sep="\n")) }

## -----------------------------------------------------------------------------
run_command(sprintf("%s", list_file_command))
command <- "display_file.R example.R" # to show file

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))   

## -----------------------------------------------------------------------------
command <- "example.R --help" # same as system("Rscript example.R -h")

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))   
command <- "example.R" # rely only on defaults

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))   
command <- "example.R --mean=10 --sd=10 --count=3" 

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))
command <- "example.R --quiet -c 4 --generator=\"runif\"" #  same as above but "quiet"

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))
command <- "example.R --silent -m 5" #  same as above but "quiet"

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))
command <- "example.R -c 100 -c 2 -c 1000 -c 7" #  same as above but "quiet"

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))

## -----------------------------------------------------------------------------
command <- "display_file.R --help" 

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))
command <- "display_file.R --add_numbers display_file.R" 

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))
command <- "display_file.R non_existent_file.txt" 

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))
command <- "display_file.R"

## -----------------------------------------------------------------------------
run_command(sprintf("%s %s 2>&1", Rscript_executable, command))

