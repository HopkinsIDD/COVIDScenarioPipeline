geoid_params <- readr::read_csv('COVIDScenarioPipeline/sample_data/geoid-params.csv')
rc <- list()
names <- c('hosp_inf','icu_hosp','vent_icu','death_inf')
for(name in names){
  p_name <- paste('p',name,sep='_')
  rr_name <- paste('rr',name,sep='_')
  rc[[name]]<- geoid_params[,'geoid']
  rc[[name]]$parameter <- paste('p',name,sep='_')
  if(name == 'hosp_inf'){
    rc[[name]]$value <- geoid_params[[rr_name]] * .05
  }
  if(name == 'death_inf'){
    rc[[name]]$value <- geoid_params[[rr_name]] * .005
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

rc <- do.call(rbind,rc)
##rc <- dplyr::filter(rc,gsub('...$','',geoid) == '36')
arrow::write_parquet(rc,"hospitalization.hpar.parquet")
