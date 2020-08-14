##' Wrapper function for loading multiple hospitalization outcomes with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param model_output folder with hosp outcomes
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
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
                                    pre_process=function(x) {x},
                                    post_process=NULL,
                                    ...
) {
  if(!is.null(post_process) & class(post_process)!="function"){stop("Post_process must be a function or NULL")}
  
  require(tidyverse)
  
  rc<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                          partitioning = partitions) %>%
    dplyr::filter(is_final=="final",
                  lik_type=="global") %>%
    dplyr::filter(pdeath %in% pdeath_filter) %>%
    pre_process(...) %>%
    dplyr::collect() 
  
  rc <- rc %>%
    dplyr::mutate(time=as.Date(time))
  
  rc<-rc%>%
    dplyr::group_by(pdeath, scenario, geoid, location) %>%
    dplyr::distinct(sim_id)%>%
    dplyr::mutate(sim_num=seq_along(sim_id)) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(rc)
    
  if(is.null(post_process)){
  rc<-rc %>%
    dplyr::group_by(geoid, pdeath, scenario, sim_num, location) %>%
    dplyr::mutate(cum_hosp=cumsum(incidH)) %>%
    dplyr::mutate(cum_death=cumsum(incidD)) %>%
    dplyr::mutate(cum_case=cumsum(incidC)) %>%
    dplyr::mutate(cum_inf=cumsum(incidI)) %>%
    dplyr::rename(NhospCurr=hosp_curr,
                  NICUCurr=icu_curr,
                  NincidDeath=incidD,
                  NincidInf=incidI,
                  NincidCase=incidC,
                  NincidICU=incidICU,
                  NincidHosp=incidH,
                  NincidVent=incidVent,
                  NVentCurr=vent_curr) %>%
    dplyr::ungroup()
  
  } else {
    rc <- rc %>%
      post_process(...)
  }
  
  warning("Finished loading")
  return(rc)
  
}

##' Wrapper function for loading hpar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param model_output folder with hpar outcomes
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collectio
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
                                    ...
) {
  
  require(tidyverse)
  require(foreach)
  
  rc<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                          partitioning = partitions) %>%
    dplyr::filter(is_final=="final",
                  lik_type=="global") %>%
    dplyr::filter(pdeath %in% pdeath_filter) %>%
    pre_process(...) %>%
    dplyr::collect() 
  
  rc<-rc%>%
    dplyr::group_by(pdeath, scenario, geoid, location) %>%
    dplyr::distinct(sim_id)%>%
    dplyr::mutate(sim_num=seq_along(sim_id)) %>%
    dplyr::right_join(rc) %>%
    dplyr::ungroup() 
  
  warning("Finished loading")
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
##' 
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
  
  warning("Finished loading")
  return(spar)
  
}

##' Wrapper function for loading spar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collectio
##' 
##' @return a combined data frame of all R simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_snpi_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "date", "lik_type", "is_final", "sim_id"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    ...
) {
  
  require(tidyverse)
  
  snpi<- arrow::open_dataset(file.path(outcome_dir,'snpi'), 
                             partitioning = partitions) %>%
    dplyr::filter(is_final=="final",
                  lik_type=="global") %>%
    dplyr::filter(pdeath %in% pdeath_filter) %>%
    pre_process(...)%>%
    dplyr::collect() %>%
    dplyr::group_by(geoid, npi_name, scenario)%>%
    dplyr::mutate(sim_num = order(sim_id)) %>%
    dplyr::select(-date, -lik_type, -is_final, -sim_id) %>%
    dplyr::ungroup()
  
  warning("Finished loading")
  return(snpi)
  
}