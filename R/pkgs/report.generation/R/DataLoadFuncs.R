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
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
                                    pdeath_filter=c("high", "med", "low"),
                                    incl_geoids,
                                    pre_process=list(partitions=function(x) {x},
                                                     data=function(x) {x}),
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
    if(length(partitions)==6){partitions <- partitions[-5:-6]}
    
    hosp<-arrow::open_dataset(file.path(outcome_dir,model_output), 
                              partitioning = partitions) %>%
      dplyr::filter(!!as.symbol(partitions[3])  %in% pdeath_filter) %>%
      dplyr::filter(geoid %in% incl_geoids) %>%
      pre_process(...)%>%
      collect()
  }
  
  hosp<-hosp %>%
    mutate(sim_num=as.numeric(str_remove(sim_num, '\\..+$')), # removes everything after the first dot
           time=as.Date(time)) 
  
  # fnames <- hosp$.data$files %>% 
  #   stringr::str_split("\\/") %>% 
  #   dplyr::tibble() %>% 
  #   tidyr::unnest_wider(., col=`.`)
  # 
  # fnames <- fnames[,(ncol(fnames)-length(partitions)):ncol(fnames)]
  # 
  # colnames(fnames) <- c(partitions, "sim_num")
  # 
  # fnames <- fnames %>%
  #   pre_process$partitions() %>%
  #   pull(sim_num)
  # 
  # hosp<-hosp %>%
  #   pre_process$partitions() %>%
  #   pre_process$data() %>%
  #   collect() %>%
  #   group_by(geoid, time, !!as.symbol(partitions[2]), !!as.symbol(partitions[3])) %>%
  #   mutate(sim_num=as.numeric(stringr::str_remove(fnames, '\\..+$')), 
  #          time=as.Date(time)) %>%
  #   dplyr::ungroup()
  
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
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=list(partitions=function(x) {x},
                                                     data=function(x) {x}),
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
    dplyr::mutate(sim_num=as.numeric(str_remove(sim_num, '\\..+$'))) 
  
  
  # fnames <- hpar$.data$files %>% 
  #   stringr::str_split("\\/") %>% 
  #   dplyr::tibble() %>% 
  #   tidyr::unnest_wider(., col=`.`)
  # 
  # fnames <- fnames[,(ncol(fnames)-length(partitions)):ncol(fnames)]
  # 
  # colnames(fnames) <- c(partitions, "sim_num")
  # 
  # fnames <- fnames %>%
  #   pre_process$partitions() %>%
  #   pull(sim_num)
  # 
  # hpar<-hpar %>%
  #   pre_process$partitions() %>%
  #   pre_process$data() %>%
  #   collect() %>%
  #   group_by(quantity, outcome, geoid, !!as.symbol(partitions[2]), !!as.symbol(partitions[3])) %>%
  #   mutate(sim_num=as.numeric(stringr::str_remove(fnames, '\\..+$'))) %>%
  #   dplyr::ungroup()
  
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
##'        - sim_num
##'
##'
##'@export
load_hpar_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
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
    dplyr::mutate(sim_num=str_remove(sim_num,paste0(".",runID,'.hpar.parquet'))) %>% # remove runID component
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
##'        - sim_num
##'
##'
##'@export
load_spar_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=list(partitions=function(x) {x},
                                                     data=function(x) {x}),
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
    dplyr::mutate(sim_num=as.numeric(str_remove(sim_num, '\\..+$'))) 
  
  # fnames <- spar$.data$files %>% 
  #   stringr::str_split("\\/") %>% 
  #   dplyr::tibble() %>% 
  #   tidyr::unnest_wider(., col=`.`)
  # 
  # fnames <- fnames[,(ncol(fnames)-length(partitions)):ncol(fnames)]
  # 
  # colnames(fnames) <- c(partitions, "sim_num")
  # 
  # fnames <- fnames %>%
  #   pre_process$partitions() %>%
  #   pull(sim_num)
  # 
  # spar<-spar %>%
  #   pre_process$partitions() %>%
  #   pre_process$data() %>%
  #   collect() %>%
  #   group_by(parameter, !!as.symbol(partitions[2]), !!as.symbol(partitions[3])) %>%
  #   mutate(sim_num=as.numeric(stringr::str_remove(fnames, '\\..+$'))) %>%
  #   dplyr::ungroup()
  
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
##'        - sim_num
##'
##'
##'@export
load_spar_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
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
    dplyr::mutate(sim_num=str_remove(sim_num,paste0(".",runID,'.spar.parquet'))) %>% # remove runID component
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
##'        - sim_num
##'
##'
##'@export
load_snpi_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=list(partitions=function(x) {x},
                                                     data=function(x) {x}),
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
    dplyr::mutate(sim_num=as.numeric(str_remove(sim_num, '\\..+$'))) 
  
  # fnames <- snpi$.data$files %>% 
  #   stringr::str_split("\\/") %>% 
  #   dplyr::tibble() %>% 
  #   tidyr::unnest_wider(., col=`.`)
  # 
  # fnames <- fnames[,(ncol(fnames)-length(partitions)):ncol(fnames)]
  # 
  # colnames(fnames) <- c(partitions, "sim_num")
  # 
  # fnames <- fnames %>%
  #   pre_process$partitions() %>%
  #   pull(sim_num)
  # 
  # snpi<-snpi %>%
  #   pre_process$partitions() %>%
  #   pre_process$data() %>%
  #   collect() %>%
  #   group_by(geoid, npi_name, !!as.symbol(partitions[2]), !!as.symbol(partitions[3])) %>%
  #   mutate(sim_num=as.numeric(stringr::str_remove(fnames, '\\..+$'))) %>%
  #   dplyr::ungroup()
  
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
##'        - sim_num
##'
##'
##'@export
load_snpi_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
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
    dplyr::mutate(sim_num=str_remove(sim_num,paste0(".",runID,'.snpi.parquet'))) %>% # remove runID component
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
##'        - sim_num
##'
##'
##'@export
load_hnpi_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
                                    pdeath_filter=c("high", "med", "low"),
                                    pre_process=list(partitions=function(x) {x},
                                                     data=function(x) {x}),
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
    dplyr::mutate(sim_num=as.numeric(str_remove(sim_num, '\\..+$'))) 
  
  # fnames <- hnpi$.data$files %>% 
  #   stringr::str_split("\\/") %>% 
  #   dplyr::tibble() %>% 
  #   tidyr::unnest_wider(., col=`.`)
  # 
  # fnames <- fnames[,(ncol(fnames)-length(partitions)):ncol(fnames)]
  # 
  # colnames(fnames) <- c(partitions, "sim_num")
  # 
  # fnames <- fnames %>%
  #   pre_process$partitions() %>%
  #   pull(sim_num)
  # 
  # hnpi<-hnpi %>%
  #   pre_process$partitions() %>%
  #   pre_process$data() %>%
  #   collect() %>%
  #   group_by(geoid, npi_name, !!as.symbol(partitions[2]), !!as.symbol(partitions[3])) %>%
  #   mutate(sim_num=as.numeric(stringr::str_remove(fnames, '\\..+$'))) %>%
  #   dplyr::ungroup()
  
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
##'        - sim_num
##'
##'
##'@export
load_hnpi_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
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
    dplyr::mutate(sim_num=str_remove(sim_num,paste0(".",runID,'.hnpi.parquet'))) %>% # remove runID component
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
##'        - sim_num
##'
##'
##'@export
load_llik_sims_filtered <- function(outcome_dir,
                                    partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
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
    mutate(sim_num=as.numeric(str_remove(sim_num, '\\..+$'))) # removes everything after the first dot
  
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
##'        - sim_num
##'
##'
##'@export
load_llik_sims_filtered_interm <- function(outcome_dir,
                                           partitions=c("location", "scenario", "pdeath", "runID", "lik_type", "is_final", "sim_num"),
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
    dplyr::mutate(sim_num=str_remove(sim_num,paste0(".",runID,'.llik.parquet'))) %>% # remove runID component
    tidyr::separate(sim_num,c('slot_num','block_num','iter_num'),sep="\\.",convert=TRUE,remove=TRUE) # split string
  
  message("Finished loading intermediate log likelihoods.")
  
  return(llik)
  
}
