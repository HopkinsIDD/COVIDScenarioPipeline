#' Returns a function to read files of a specific type (or automatically detected type based on extension)
#' @param extension The file extension to read files of
#' @param ... Arguments to pass to the reading function
#' @return A function which will read files with that extension.
#'  - We use readr::read_csv for csv files
#'  - We use arrow::read_parquet for parquet files
#'  - We use a function that detects the extension and calls this function if the auto extension is specified.
read_file_of_type <- function(extension,...){
  if(extension == 'csv'){
    return(function(x){suppressWarnings(readr::read_csv(x,,col_types = cols(
      .default = col_double(),
      time=col_date(),
      uid=col_character(),
      comp=col_character(),
      geoid=col_character()
    )))})
  }
  if(extension == 'parquet'){
    return(function(x){
      tmp <- arrow::read_parquet(x) 
      if("POSIXct" %in% class(tmp$time)){
        tmp$time <- lubridate::as_date(tz="GMT",tmp$time)
      }
      tmp
    })
  }
  if(extension == 'auto'){
    return(function(filename){
      extension <- gsub("[^.]*\\.","",filename)
      if(extension == 'auto'){stop("read_file_of_type cannot read files with file extension '.auto'")}
      read_file_of_type(extension)(filename)
    })
  }
  if(extension == 'shp'){
    return(sf::st_read)
  }
  stop(paste("read_file_of_type cannot read files of type",extension))
}

##' Wrapper function for loading multiple hospitalization outcomes with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param model_output folder with hosp outcomes
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param incl_geoids character vector of geoids that are included in the report
##' @param pre_process function that does processing before collection
##' @param post_process function that does processing after collection
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_hosp_sims_filtered <- function(outcome_dir,
                                    model_output = 'hosp',
                                    partitions=c("location", "scenario", "pdeath", "date", "lik_type", "is_final", "sim_id"),
                                    pdeath_filter=c("high", "med", "low"),
                                    incl_geoids,
                                    pre_process=function(x) {x},
                                    post_process=function(x) {x},
                                    inference=TRUE,
                                    ...
) {
  
  require(tidyverse)
  
  if(inference){
    rc<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                            partitioning = partitions) %>%
      dplyr::filter(is_final=="final",
                    lik_type=="global") %>%
      dplyr::filter(pdeath %in% pdeath_filter) %>%
      dplyr::filter(geoid %in% incl_geoids) %>%
      pre_process(...) %>%
      dplyr::collect()
  } else {
    if(length(partitions)==7){partitions <- partitions[-5:-6]}
    
    rc<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                            partitioning = partitions) %>%
      dplyr::filter(pdeath %in% pdeath_filter) %>%
      dplyr::filter(geoid %in% incl_geoids) %>%
      pre_process(...) %>%
      dplyr::collect()
  }
  
  rc <- rc %>%
    dplyr::group_by(pdeath, scenario, geoid, location) %>%
    dplyr::distinct(sim_id)%>%
    dplyr::mutate(sim_num=seq_along(sim_id)) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(rc) %>%
    dplyr::mutate(time=as.Date(time))
  
  rc <- rc %>%
      post_process(...) 
  
  message("Finished loading")
  return(rc)
  
}

##' Wrapper function for loading hpar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param model_output folder with hpar outcomes
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' @param incl_geoids character vector of geoids that are included in the report
##' 
##' @return a combined data frame of all hpar simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_hpar_sims_filtered <- function(outcome_dir,
                                    model_output = 'hpar',
                                    partitions=c("location", "scenario", "pdeath", "date", "lik_type", "is_final", "sim_id"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    incl_geoids,
                                    ...
) {
  
  require(tidyverse)
  
  rc<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                          partitioning = partitions) %>%
    dplyr::filter(is_final=="final",
                  lik_type=="global") %>%
    dplyr::filter(pdeath %in% pdeath_filter) %>%
    dplyr::filter(geoid %in% incl_geoids) %>%
    pre_process(...) %>%
    dplyr::collect() 
  
  rc<-rc%>%
    dplyr::group_by(pdeath, scenario, geoid, location) %>%
    dplyr::distinct(sim_id)%>%
    dplyr::mutate(sim_num=seq_along(sim_id)) %>%
    dplyr::right_join(rc) %>%
    dplyr::ungroup() 
  
  message("Finished loading")
  return(rc)
  
}

##' Wrapper function for loading spar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' 
##' @return a combined data frame of all R simulations with filters applied pre merge.
##'        - parameter
##'        - value
##'        - location 
##'        - scenario
##'        - pdeath
##'        - date
##'        - lik_type
##'        - is_final
##'        - sim_id
##'        - sim_num
##'
##'
##'@export
load_spar_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "date", "lik_type", "is_final", "sim_id"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    ...
) {
  
  require(tidyverse)
  
  spar <- arrow::open_dataset(file.path(outcome_dir,'spar'), 
                              partitioning = partitions) %>%
    dplyr::filter(is_final=="final",
                  lik_type=="global") %>%
    dplyr::filter(pdeath %in% pdeath_filter) %>%
    pre_process(...)%>%
    dplyr::collect() %>% 
    dplyr::group_by(scenario)%>%
    dplyr::mutate(sim_num = order(sim_id)) %>%
    dplyr::ungroup()
  
  message("Finished loading. Note pdeaths of the same scenario are treated as different simulations.")
  return(spar)
  
}

##' Wrapper function for loading spar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' @param incl_geoids character vector of geoids that are included in the report
##' 
##' @return a combined data frame of all R simulations with filters applied pre merge.
##'        - geoid
##'        - start_date
##'        - end_date
##'        - npi_name
##'        - parameter
##'        - reduction
##'        - location 
##'        - scenario
##'        - pdeath
##'        - sim_id
##'        - sim_num
##'
##'
##'@export
load_snpi_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "date", "lik_type", "is_final", "sim_id"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    incl_geoids,
                                    ...
) {
  
  require(tidyverse)
  
  snpi<- arrow::open_dataset(file.path(outcome_dir,'snpi'), 
                             partitioning = partitions) %>%
    dplyr::filter(is_final=="final",
                  lik_type=="global") %>%
    dplyr::filter(pdeath %in% pdeath_filter) %>%
    dplyr::filter(geoid %in% incl_geoids) %>%
    pre_process(...)%>%
    dplyr::collect() %>%
    dplyr::group_by(geoid, npi_name, scenario)%>%
    dplyr::mutate(sim_num = order(sim_id)) %>%
    dplyr::select(-date, -lik_type, -is_final) %>%
    dplyr::ungroup()
  
  message("Finished loading. Note pdeaths of the same scenario are treated as different simulations.")
  return(snpi)
  
}
