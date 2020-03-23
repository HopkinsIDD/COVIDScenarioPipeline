##' Function designed to allow generic filtering and summarization
##' of simulations before merging them together when loading outputs
##' from the infection genertion modeling pipeline.
##' 
##' @param scenario_dir the subdirectory containing this scenario
##' @param post_process function that does processing to sims before loading 
##' @param post_process function that does processing after 
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##' 
##' 
##' @author Justin Lessler
load_scenario_sims_filtered <- function(scenario_dir, post_process=function(x) {x},
                                        pre_process=function(x){x}) {
  
  require(tidyverse)
  
  files <- dir(sprintf("model_output/%s", scenario_dir),full.names = TRUE)
  
  rc <- list()
  
  for (i in 1:length(files)) {
    
    file <- files[i]
    
    tmp <- readr::read_csv(file, col_types = cols(.default = col_double(),
                                                  time=col_date(),
                                                  comp=col_character()))  %>%
      pre_process %>%
      pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>% 
      post_process %>%
      mutate(sim_num = i)
    
    rc[[i]] <- tmp
  }
  
  rc<- dplyr::bind_rows(rc)
  
  return(rc)
}


##' Function loads multiple hospital simulations into a combined data frame
##' with pre and post filters
##' 
##' @param scenario_dir the subdirectory containing this scenario
##' @param name_filter function that 
##' @param post_process function that does processing after 
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##' 
##' @author Justin Lessler
##'
load_hosp_sims_filtered <- function(scenario_dir,
                                    name_filter = "",
                                    post_process=function(x) {x}) {
  
  require(tidyverse)
  
  files <- dir(sprintf("hospitalization/model_output/%s", scenario_dir),full.names = TRUE)
  files <- files[grepl(name_filter,files)]
  
  rc <- list()
  
  
  for (i in 1:length(files)) {
    file <- files[i]
    tmp <- readr::read_csv(file, col_types = cols(
      .default = col_double(),
      time=col_date(),
      uid=col_character(),
      comp=col_character()
    )) %>% 
      post_process %>%
      mutate(sim_num = i)
    
    rc[[i]] <- tmp
  }
  
  rc<- dplyr::bind_rows(rc)
  
  return(rc)
  
}

