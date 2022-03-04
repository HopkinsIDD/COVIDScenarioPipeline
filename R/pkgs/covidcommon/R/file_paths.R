# File naming ------------------------------------------------------------------

##' Create a unique identifier for a run via time stamp
##' @export
run_id <- function(){
  rc <- "test"
  try({
    rc <- format(lubridate::now(),"%Y.%m.%d.%H:%M:%S.%Z")
  }, silent=TRUE)
  return(rc)
}


##' @name create_prefix
##' @title create_prefix
##' @description Function for creating scenario tags from components; Intended for use in filename construction
##' @param .dots A set of strings, or lists of value,format (see sprintf).
##' @param sep A character to use to separate different components of the scenario in the tag.  This argument cannot appear in any of the  .dots arguments.
##' @export
create_prefix <- function(...,prefix='',sep='-',trailing_separator=""){
  args <- list(...)
  formats <- sapply(args,function(x){x[2]})
  formats[is.na(formats)] <- '%s'
  values <- c(lapply(args,function(x){x[[1]]}))
  if(any(grepl(sep,values,fixed=TRUE))){
    stop("scenario elements cannot contain the seperator")
  }
  prefix <- paste0(prefix,do.call(purrr::partial(sprintf,fmt=paste(formats,collapse = sep)),values),trailing_separator)
  
  return(prefix)
}

## Function for creating file names from their components
##' @export
create_file_name <- function(run_id,prefix,index,type,extension='parquet',create_directory = TRUE){
  rc <- sprintf("model_output/%s/%ssim_id=%09d/%s.%s.%s",type,prefix,index,run_id,type,extension)
  if(create_directory){
    if(!dir.exists(dirname(rc))){
      dir.create(dirname(rc), recursive = TRUE)
    }
  }
  return(rc)
}


##' @export
count_files_of_type <- function(run_id, prefix, type,extension){
  all_files <- list.files(
    dirname(create_file_name(run_id,prefix,1,type,'test'))
  )
  all_files <- all_files[grepl(paste0(extension,'$'),all_files)]
  return(length(all_files))
}


##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##'
##' @return NULL
##' @export
spar_file_path <- function(config,index, scenario, create_directory = TRUE){
  # if(length(config$interventions$scenarios) > 1){
  #   stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  # }

  ## FIX ME
  rc <- sprintf("model_parameters/%s_%s/%09d.spar.parquet", config$name , scenario, index)
  if(create_directory){suppressWarnings(dir.create(dirname(rc)))}
  return(rc)
}


##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##'
##' @return NULL
##' @export
snpi_file_path <- function(config,index,scenario,create_directory = TRUE){

  rc <- sprintf("model_parameters/%s_%s/%09d.snpi.parquet", config$name , scenario, index)
  if(create_directory){suppressWarnings(dir.create(dirname(rc),recursive = TRUE))}
  return(rc)
}


##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##'
##' @return NULL
##' @export
seeding_file_path <- function(config,index, create_directory = TRUE){
  # if(length(config$interventions$scenarios) > 1){
  #   stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  # }

  rc <- sprintf("%s/%09d.impa.csv",config$seeding$folder_path,index)
  if(create_directory){suppressWarnings(dir.create(dirname(rc),recursive = TRUE))}
  return(rc)
}


##' Function for determining where to write the SEIR output to file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##' @return NULL
##' @export
simulation_file_path <- function(config,index,scenario, create_directory = TRUE){
  rc <- sprintf("model_output/%s_%s/%09d.seir.parquet", config$name , scenario, index)
  if(create_directory){suppressWarnings(dir.create(dirname(rc),recursive = TRUE))}
  return(rc)
}


##' Function for determining where to write hospitalization output file
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##' @param deathrate Name of deathrate scenario
##' @return NULL
##' @export
hospitalization_file_path <- function(config,index,scenario,deathrate, create_directory = TRUE){
  rc <- sprintf("hospitalization/model_output/%s_%s/%s_death_death-%09d.hosp.parquet", config$name , scenario, deathrate,index)
  if(create_directory){suppressWarnings(dir.create(dirname(rc),recursive = TRUE))}
  return(rc)
}


##' Function for determining where to write the hospitalization parameter files
##' @param config The config for this run
##' @param index The index of this simulation
##' @param scenario The scenario of the simulation
##' @param deathrate Name of deathrate scenario
##' @return NULL
##' @export
hpar_file_path <- function(config,index,scenario,deathrate, create_directory = TRUE){
  rc <- sprintf("hospitalization/model_output/%s_%s/%s_death_death-%09d.hpar.parquet", config$name , scenario, deathrate,index)
  if(create_directory){suppressWarnings(dir.create(dirname(rc),recursive = TRUE))}
  return(rc)
}
