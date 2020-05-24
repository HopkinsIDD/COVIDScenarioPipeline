geoid_params <- readr::read_csv('COVIDScenarioPipeline/sample_data/geoid-params.csv')
rc <- list()
names <- c('hosp_inf','icu_hosp','vent_icu','death_inf')
for(name in names){
  rr_name <- paste('rr',name,sep='_')
  p_name <- paste('p',name,sep='_')
  rc[[name]]<- geoid_params[,'geoid']
  rc[[name]]$parameter <- paste('p',name,sep='_')
  if(rr_name %in% names(geoid_params)){
    rc[[name]]$value <- geoid_params[[rr_name]] * geoid_params[[p_name]]
  } else {
    rc[[name]]$value <- geoid_params[[p_name]]
  }
}
name <- 'confirmed_inf'
rc[[name]] <- geoid_params[,'geoid']
rc[[name]]$parameter <- paste('p',name,sep='_')
rc[[name]]$value <- .2

rc <- do.call(rbind,rc)
# rc <- dplyr::filter(rc,gsub('...$','',geoid) == '36')
arrow::write_parquet(rc,"hospitalization.hpar.parquet")
