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
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                    pdeath_filter=c("high", "med", "low"),
                                    incl_geoids,
                                    pre_process=function(x) {x},
                                    post_process=function(x) {x},
                                    inference=TRUE,
                                    ...
) {
  
  require(tidyverse)
  
  if(inference){
    hosp<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                              partitioning = partitions) %>%
      dplyr::filter(!!as.symbol(partitions[5])=="global",
                    !!as.symbol(partitions[6])=="final") %>%
      dplyr::filter(!!as.symbol(partitions[3])  %in% pdeath_filter) %>%
      dplyr::filter(geoid %in% incl_geoids) %>%
      pre_process(...)%>%
      collect()
  } else {
    if(length(partitions)==7){partitions <- partitions[-5:-6]}
    
    hosp<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                              partitioning = partitions) %>%
      dplyr::filter(!!as.symbol(partitions[3])  %in% pdeath_filter) %>%
      dplyr::filter(geoid %in% incl_geoids) %>%
      pre_process(...)%>%
      collect()
  }
  
  hosp<-hosp %>%
    mutate(sim_num=as.numeric(stringr::str_extract(!!as.symbol(partitions[length(partitions)]), "^\\d+")), # removes everything after the first dot
           time=as.Date(time)) 
  
  if(nrow(hosp)==0){stop("Nothing was loaded, confirm filtering values are correct.")}
  
  hosp <- hosp %>%
    post_process(...) 
  
  message("Finished loading")
  return(hosp)
  
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
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    incl_geoids,
                                    ...
) {
  
  require(tidyverse)
  
  hpar<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                            partitioning = partitions) %>%
    dplyr::filter(!!as.symbol(partitions[5])=="global",
                  !!as.symbol(partitions[6])=="final") %>%
    dplyr::filter(!!as.symbol(partitions[3])  %in% pdeath_filter) %>%
    dplyr::filter(geoid %in% incl_geoids)  %>%
    pre_process(...) %>%
    collect() %>%
    dplyr::mutate(sim_num=as.numeric(stringr::str_extract(!!as.symbol(partitions[length(partitions)]), "^\\d+"))) # removes everything after the first dot
  
  message("Finished loading")
  return(hpar)
  
}


##' Wrapper function for loading intermediate hpar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_hpar_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                           pdeath_filter=c("high", "med", "low"),
                                           incl_geoids,
                                           ...
) {
  
  require(tidyverse)
  
  hpar <- arrow::open_dataset(file.path(outcome_dir,'hpar'), 
                              partitioning = partitions) %>%
    #dplyr::filter(!!as.symbol(partitions[5])=="global") %>%
    dplyr::filter(!!as.symbol(partitions[6])=="intermediate") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    collect()
  
  hpar<-hpar %>%
    dplyr::mutate(sim_num=stringr::str_remove(!!as.symbol(partitions[length(partitions)]),paste0(".",runID,'.hpar.parquet'))) %>% # remove runID component
    tidyr::separate(sim_num,c('slot_num','block_num','iter_num'),sep="\\.",convert=TRUE,remove=TRUE) 
  
  message("Finished loading intermediate Outcome parameters.")
  
  return(hpar)
  
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
##'        - filename
##'
##'
##'@export
load_spar_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    ...
) {
  
  require(tidyverse)
  
  spar <- arrow::open_dataset(file.path(outcome_dir,'spar'), 
                              partitioning = partitions) %>%
    dplyr::filter(!!as.symbol(partitions[5])=="global",
                  !!as.symbol(partitions[6])=="final") %>%
    dplyr::filter(!!as.symbol(partitions[3])  %in% pdeath_filter) %>%
    pre_process(...) %>%
    collect() %>%
    dplyr::mutate(sim_num=as.numeric(stringr::str_extract(!!as.symbol(partitions[length(partitions)]), "^\\d+"))) # removes everything after the first dot
  
  message("Finished loading.")
  return(spar)
  
}


##' Wrapper function for loading intermediate spar files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_spar_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                           pdeath_filter=c("high", "med", "low"),
                                           incl_geoids,
                                           ...
) {
  
  require(tidyverse)
  
  spar <- arrow::open_dataset(file.path(outcome_dir,'spar'), 
                              partitioning = partitions) %>%
    #dplyr::filter(!!as.symbol(partitions[5])=="global") %>%
    dplyr::filter(!!as.symbol(partitions[6])=="intermediate") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    collect()
  
  spar<-spar %>%
    dplyr::mutate(sim_num=stringr::str_remove(!!as.symbol(partitions[length(partitions)]),paste0(".",runID,'.spar.parquet'))) %>% # remove runID component
    tidyr::separate(sim_num,c('slot_num','block_num','iter_num'),sep="\\.",convert=TRUE,remove=TRUE) 
  
  message("Finished loading intermediate SEIR parameters.")
  
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_snpi_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    incl_geoids,
                                    ...
) {
  
  require(tidyverse)
  
  snpi<- arrow::open_dataset(file.path(outcome_dir,'snpi'), 
                             partitioning = partitions) %>%
    dplyr::filter(!!as.symbol(partitions[5])=="global",
                  !!as.symbol(partitions[6])=="final") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    dplyr::filter(geoid %in% incl_geoids)  %>%
    pre_process(...)%>%
    collect() %>%
    dplyr::mutate(sim_num=as.numeric(stringr::str_extract(!!as.symbol(partitions[length(partitions)]), "^\\d+"))) # removes everything after the first dot
  
  message("Finished loading.")
  
  return(snpi)
  
}


##' Wrapper function for loading intermediate snpi files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_snpi_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                           pdeath_filter=c("high", "med", "low"),
                                           incl_geoids,
                                           ...
) {
  
  require(tidyverse)
  
  snpi <- arrow::open_dataset(file.path(outcome_dir,'snpi'), 
                              partitioning = partitions) %>%
    #dplyr::filter(!!as.symbol(partitions[5])=="global") %>%
    dplyr::filter(!!as.symbol(partitions[6])=="intermediate") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    collect()
  
  snpi<-snpi %>%
    dplyr::mutate(sim_num=stringr::str_remove(!!as.symbol(partitions[length(partitions)]),paste0(".",runID,'.snpi.parquet'))) %>% # remove runID component
    tidyr::separate(sim_num,c('slot_num','block_num','iter_num'),sep="\\.",convert=TRUE,remove=TRUE) 
  
  message("Finished loading intermediate SNPI  parameters.")
  
  return(snpi)
  
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_hnpi_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    incl_geoids,

                                    ...
) {
  
  require(tidyverse)
  
  hnpi<- arrow::open_dataset(file.path(outcome_dir,'hnpi'), 
                             partitioning = partitions) %>%
    dplyr::filter(!!as.symbol(partitions[5])=="global",
                  !!as.symbol(partitions[6])=="final") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    dplyr::filter(geoid %in% incl_geoids)  %>%
    pre_process(...)%>%
    collect() %>%
    dplyr::mutate(sim_num=as.numeric(stringr::str_extract(!!as.symbol(partitions[length(partitions)]), "^\\d+"))) # removes everything after the first dot
  
  message("Finished loading.")
  
  return(hnpi)
  
}


##' Wrapper function for loading intermediate hnpi files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_hnpi_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                           pdeath_filter=c("high", "med", "low"),
                                           incl_geoids,
                                           ...
) {
  
  require(tidyverse)
  
  hnpi <- arrow::open_dataset(file.path(outcome_dir,'hnpi'), 
                              partitioning = partitions) %>%
    #dplyr::filter(!!as.symbol(partitions[5])=="global") %>%
    dplyr::filter(!!as.symbol(partitions[6])=="intermediate") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    collect()
  
  hnpi<-hnpi %>%
    dplyr::mutate(sim_num=stringr::str_remove(!!as.symbol(partitions[length(partitions)]),paste0(".",runID,'.hnpi.parquet'))) %>% # remove runID component
    tidyr::separate(sim_num,c('slot_num','block_num','iter_num'),sep="\\.",convert=TRUE,remove=TRUE) 
  
  message("Finished loading intermediate hnpi parameters.")
  
  return(hnpi)
  
}


##' Wrapper function for loading final llik files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_llik_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                    pdeath_filter=c("high", "med", "low"),
                                    incl_geoids,
                                    ...
) {
  
  require(tidyverse)
  
  llik <- arrow::open_dataset(file.path(outcome_dir,'llik'), 
                              partitioning = partitions) %>%
    dplyr::filter(!!as.symbol(partitions[5])=="global",
                  !!as.symbol(partitions[6])=="final") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    dplyr::filter(geoid %in% incl_geoids)  %>%
    #pre_process(...)%>%
    collect()
  
  llik<-llik %>%
    mutate(sim_num=as.numeric(stringr::str_extract(!!as.symbol(partitions[length(partitions)]), "^\\d+"))) # removes everything after the first dot
  
  message("Finished loading final log likelihoods.")
  
  return(llik)
  
}

##' Wrapper function for loading intermediate llik files with open_dataset
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
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
##'        - date
##'        - lik_type
##'        - is_final
##'        - filename
##'
##'
##'@export
load_llik_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                           pdeath_filter=c("high", "med", "low"),
                                           incl_geoids,
                                           ...
) {
  
  require(tidyverse)
  
  llik <- arrow::open_dataset(file.path(outcome_dir,'llik'), 
                              partitioning = partitions) %>%
    #dplyr::filter(!!as.symbol(partitions[5])=="global") %>%
    dplyr::filter(!!as.symbol(partitions[6])=="intermediate") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    dplyr::filter(geoid %in% incl_geoids)  %>%
    collect()
  
  llik<-llik %>%
    dplyr::mutate(sim_num=stringr::str_remove(!!as.symbol(partitions[length(partitions)]),paste0(".",runID,'.llik.parquet'))) %>% # remove runID component
    tidyr::separate(sim_num,c('slot_num','block_num','iter_num'),sep="\\.",convert=TRUE,remove=TRUE) # split string
  
  message("Finished loading intermediate log likelihoods.")
  
  return(llik)
  
}

##' Wrapper function for loading seir files with open_dataset
##'
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' @param incl_geoids character vector of geoids that are included in the report
##' 
##' @return a combined data frame of all simulations with filters applied pre merge.
##'        - comp
##'        - pcom
##'        - time
##'        - location 
##'        - scenario
##'        - pdeath
##'        - runID
##'        - sim_num
##'        - geoid
##'        - value
##'
##'
##'@export
load_seir_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "filename"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=function(x) {x},
                                    post_process=function(x) {x},
                                    incl_geoids,
                                    ...
) {
  
  require(tidyverse)
  
  seir<- arrow::open_dataset(file.path(outcome_dir,'seir'), 
                             partitioning = partitions) %>%
    dplyr::filter(!!as.symbol(partitions[5])=="global",
                  !!as.symbol(partitions[6])=="final") %>%
    dplyr::filter(!!as.symbol(partitions[3]) %in% pdeath_filter) %>%
    dplyr::select(comp, p_comp, time, tidyselect::all_of(partitions[c(1:4, 7)]), tidyselect::any_of(incl_geoids)) %>%
    pre_process(...)%>%
    dplyr::collect() %>%
    dplyr::mutate(sim_num=as.numeric(stringr::str_extract(!!as.symbol(partitions[length(partitions)]), "^\\d+")), 
                  time=as.Date(time)) %>%
    dplyr::relocate(comp, p_comp, time, tidyselect::any_of(partitions), sim_num) %>%
    dplyr::select(-partitions[length(partitions)])
  
  geoids <- colnames(seir)[!colnames(seir) %in% c("comp", "p_comp", "time", partitions, "sim_num")] # turn geoID into a single column instead
  temp <- list()
  
  for(i in 1:length(geoids)){ # loop due to memory issues with large datasets
    temp[[i]] <- seir %>%
      dplyr::select(comp, p_comp, time, tidyselect::any_of(partitions), sim_num, geoids[i]) %>%
      tidyr::pivot_longer(cols=-comp:-sim_num, names_to="geoid")
  }
  
  seir <- dplyr::bind_rows(temp) %>%
    dplyr::group_by(across(c(geoid,any_of(partitions)))) %>%  
    dplyr::arrange(sim_num,time,comp,.by_group=TRUE) %>%
    ungroup()
  
  seir <- seir %>%
    post_process()
  
  message("Finished loading seir output.")
  
  return(seir)
  
}
