#' Return a function to read files of a specific type (or automatically detected type based on extension)
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



##' Function designed to allow generic filtering and summarization
##' of simulations before merging them together when loading outputs
##' from the infection genertion modeling pipeline.
##' 
##' @param scenario_dir the subdirectory containing this scenario
##' @param post_process function that does processing to sims before loading 
##' @param post_process function that does processing after 
##' @param geoid_len in defined, this we want to make geoids all the same length
##' @param padding_char character to add to the front of geoids if fixed length
##' @param ... additional parameters to pass to pre and/or post process
##' 
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##' 
##' 
##' @author Justin Lessler
##' 
##' @export
load_scenario_sims_filtered <- function(scenario_dir, 
                                        num_files = NA,
                                        post_process = function(x) {x},
                                        pre_process = function(x){x},
                                        geoid_len = 0,
                                        padding_char = "0",
                                        file_extension = 'auto',
                                        ...) {
  
  require(tidyverse)
  require(foreach)
  
  if (is.na(num_files)) {
    files <- dir(sprintf("model_output/%s", scenario_dir), full.names = TRUE)
  } else {
    files <- c()
    for (scenario in scenario_dir) {
      all_files <- dir(sprintf("model_output/%s", scenario), full.names = TRUE)
      if (length(all_files) == num_files) {
        files <- c(files, all_files) 
      } else if (num_files < length(all_files)) {
        warning(paste0("You are only reading in ", num_files, " out of ", length(all_files), " files in ", scenario,
                       ". Check the num_files argument if this is unexpected.\n"))
        files <- c(files, all_files[seq_len(num_files)])
      } else {
        stop(paste0("There were ", length(all_files), " files in ", scenario, " but num_files is ", num_files, "."))
      }
    }
  }
  if (length(files) == 0) {
    stop(paste0("There were no files in ",getwd(), "/", sprintf("model_output/%s", scenario_dir)))
  }

  read_file <- read_file_of_type(file_extension)
  
  if (geoid_len > 0) {
    padfn <- function(x) {x%>% dplyr::mutate(geoid = str_pad(geoid,width =geoid_len,pad=padding_char))}
  } else {
    padfn <- function(x) {x}
  }
  
  rc <- foreach(i = 1:length(files)) %dopar% {
    require(tidyverse)
    
    read_file(files[i]) %>%
      pre_process(...) %>%
      pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>% 
      padfn %>%
      post_process(...) %>%
      mutate(sim_num = i)
  }
  
  rc <- dplyr::bind_rows(rc)
  return(rc)
}

##' Wrapper function for loading multiple hospitalization outcomes with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param model_output folder with hosp outcomes
##' @param partitions used by open_dataset 
##' @param name_filter string that indicates which pdeath to import from outcome_dir
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
                                    partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"),
                                    name_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    post_process=NULL,
                                    ...
) {
  
  require(tidyverse)
  
  rc<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                          partitioning = partitions) %>%
    filter(is_final=="final",
           lik_type=="global") %>%
    filter(death_rate %in% name_filter) %>%
    pre_process(...) %>%
    collect() 
  
  rc <- rc %>%
    rename(pdeath=death_rate) %>%
    mutate(time=as.Date(time))
  
  rc<-rc%>%
    group_by(pdeath, scenario, geoid, location) %>%
    distinct(sim_id)%>%
    mutate(sim_num=seq_along(sim_id)) %>%
    right_join(rc)
    
  if(is.null(post_process)){
  rc<-rc %>%
    group_by(geoid, pdeath, scenario, sim_num, location) %>%
    mutate(cum_hosp=cumsum(incidH)) %>%
    mutate(cum_death=cumsum(incidD)) %>%
    mutate(cum_case=cumsum(incidC)) %>%
    mutate(cum_inf=cumsum(incidI)) %>%
    rename(NhospCurr=hosp_curr,
           NICUCurr=icu_curr,
           NincidDeath=incidD,
           NincidInf=incidI,
           NincidCase=incidC,
           NincidICU=incidICU,
           NincidHosp=incidH,
           NincidVent=incidVent,
           NVentCurr=vent_curr) %>%
    mutate(scenario_name=scenario) 
  
  } else {
    rc <- rc %>%
      post_process(...) %>%
      mutate(scenario_name=scenario)
  }
  
  warning("Finished loading")
  return(rc)
  
}

##' Wrapper function for loading hpar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param model_output folder with hpar outcomes
##' @param partitions used by open_dataset 
##' @param name_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collectio
##' 
##' @return a combined data frame of all hpar simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_hpar_sims_filtered <- function(outcome_dir,
                                    model_output = 'hpar',
                                    partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"),
                                    name_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    ...
) {
  
  require(tidyverse)
  require(foreach)
  
  rc<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                          partitioning = partitions) %>%
    filter(is_final=="final",
           lik_type=="global") %>%
    filter(death_rate %in% name_filter) %>%
    pre_process(...) %>%
    collect() 
  
  rc<-rc%>%
    mutate(time=as.Date(time)) %>%
    group_by(pdeath=death_rate, scenario, geoid, location) %>%
    distinct(sim_id)%>%
    mutate(sim_num=seq_along(sim_id)) %>%
    right_join(rc)
  
  warning("Finished loading")
  return(rc)
  
}

##' Wrapper function for loading spar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param name_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' 
##' @return a combined data frame of all R simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_spar_sims_filtered <- function(outcome_dir,
                                    
                                    partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"),
                                    name_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    ...
) {
  
  require(tidyverse)
  
  spar <- arrow::open_dataset(file.path(outcome_dir,'spar'), 
                              partitioning = partitions) %>%
    filter(parameter=="R0",
           is_final=="final",
           lik_type=="global") %>%
    filter(death_rate %in% name_filter) %>%
    pre_process(...)%>%
    collect() %>% 
    group_by(scenario)%>%
    mutate(sim_num = order(sim_id),
           parameter="r0") %>%
    rename(location_r = value) %>%
    dplyr::select(sim_num, scenario, pdeath=death_rate, location_r, parameter, location)
  
  warning("Finished loading")
  return(spar)
  
}

##' Wrapper function for loading spar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param name_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collectio
##' 
##' @return a combined data frame of all R simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_snpi_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"),
                                    name_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    ...
) {
  
  require(tidyverse)
  
  snpi<- arrow::open_dataset(file.path(outcome_dir,'snpi'), 
                             partitioning = partitions) %>%
    filter(is_final=="final",
           lik_type=="global") %>%
    filter(death_rate %in% name_filter) %>%
    pre_process(...)%>%
    collect() %>%
    group_by(geoid, npi_name, scenario)%>%
    mutate(sim_num = order(sim_id)) %>%
    dplyr::select(-date, -lik_type, -is_final, -sim_id) %>%
    rename(pdeath=death_rate)
  
  warning("Finished loading")
  return(snpi)
  
}