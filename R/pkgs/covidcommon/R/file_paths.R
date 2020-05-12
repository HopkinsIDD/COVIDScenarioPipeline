# File naming ------------------------------------------------------------------

##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##'
##' @return NULL
#' @export
parameter_file_path <- function(config,index, scenario){
  # if(length(config$interventions$scenarios) > 1){
  #   stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  # }

  ## FIX ME
  return(sprintf("model_parameters/%s_%s/%09d.spar.parquet", config$name , scenario, index))
}


##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##'
##' @return NULL
#' @export
npi_file_path <- function(config,index,scenario){
  # if(length(config$interventions$scenarios) > 1){
  #   stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  # }

  ## FIX ME
  return(sprintf("model_parameters/%s_%s/%09d.snpi.parquet", config$name , scenario, index))
}


##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##'
##' @return NULL
#' @export
seeding_file_path <- function(config,index){
  # if(length(config$interventions$scenarios) > 1){
  #   stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  # }

  return(sprintf("%s/importation_%s.csv",config$seeding$folder_path,index))
}


##' Function for determining where to write the SEIR output to file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##' @return NULL
#' @export
simulation_file_path <- function(config,index,scenario){
  return(sprintf("model_output/%s_%s/%09d.snpi.parquet", config$name , scenario, index))
}


##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##' @param deathrate
##' @return NULL
#' @export
hospitalization_file_path <- function(config,index,scenario,deathrate){
  return(sprintf("hospitalization/model_output/%s_%s/%s_death_death-%09d.hosp.parquet", config$name , scenario, deathrate,index))
}


