library(tidyverse)

geoid_params <- readr::read_csv('COVIDScenarioPipeline/sample_data/geoid-params.csv')

ifr_labels <- c("low", "med", "high")
ifr_values <- c(.0025, .005, .01)
hosp_death_ratio <- 7

for(i in 1:length(ifr_labels)){

  ifr_lab <- ifr_labels[i]
  ifr_val <- ifr_values[i]

  rc <- list()
  names <- c('hosp_inf','icu_hosp','vent_icu','death_inf')
  for(name in names){
    p_name <- paste('p',name,sep='_')
    rr_name <- paste('rr',name,sep='_')
    rc[[name]]<- geoid_params[,'geoid']
    rc[[name]]$parameter <- paste('p',name,sep='_')
    if(name == 'hosp_inf'){
      rc[[name]]$value <- geoid_params[[rr_name]] * ifr_val * hosp_death_ratio
    }
    if(name == 'death_inf'){
      rc[[name]]$value <- geoid_params[[rr_name]] * ifr_val
    }
    if(name == 'icu_hosp'){
      rc[[name]]$value <- geoid_params[[p_name]]
    }
    if(name == 'vent_icu'){
      rc[[name]]$value <- geoid_params[[p_name]]
    }
  }

  name <- 'confirmed_inf'
  rc[[name]] <- geoid_params[,'geoid']
  rc[[name]]$parameter <- paste('p',name,sep='_')
  rc[[name]]$value <- .1

  rc <- do.call(rbind,rc) %>%
    separate(parameter, into = c("quantity", "outcome", "source"), sep = "_") %>%
    dplyr::mutate(quantity = recode(quantity, p="probability")) %>%
    dplyr::mutate(outcome = recode(outcome, 
                                   hosp = "incidH",
                                   icu = "incidICU",
                                   vent = "incidVent",
                                   death = "incidD",
                                   confirmed = "incidC")) %>%
    dplyr::mutate(source = recode(source,
                                  inf = "incidI",
                                  hosp = "incidH",
                                  icu = "incidICU"))
  
  arrow::write_parquet(rc, glue::glue("COVIDScenarioPipeline/hospitalization_inference_merge_{ifr_lab}.hpar.parquet"))

}

