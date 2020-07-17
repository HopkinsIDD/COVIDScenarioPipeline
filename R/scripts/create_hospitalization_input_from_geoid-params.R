geoid_params <- readr::read_csv('COVIDScenarioPipeline/sample_data/geoid-params.csv')
geoid_params <- tidyr::spread(geoid_params,parameter,value)
rc <- list()
names <- c('hosp_inf','icu_hosp','vent_icu','death_inf')
for(name in names){
  p_name <- paste('p',name,sep='_')
  rr_name <- paste('rr',name,sep='_')
  rc[[name]]<- geoid_params[,'geoid']
  split_name <- strsplit(name,split='_')[[1]]
  rc[[name]]$source <- split_name[[2]]
  rc[[name]]$outcome <- split_name[[1]]
  rc[[name]]$quantity <- "relative_probabily"
  print(names(geoid_params))
  print(rr_name)
  if(name == 'hosp_inf'){
    rc[[name]]$value <- geoid_params[[rr_name]]
  }
  if(name == 'death_inf'){
    rc[[name]]$value <- geoid_params[[rr_name]]
  }
  if(name == 'icu_hosp'){
    rc[[name]]$value <- geoid_params[[rr_name]]
  }
  if(name == 'vent_icu'){
    rc[[name]]$value <- geoid_params[[rr_name]]
  }
  if(!("value" %in% names(rc[[name]] ))){
    print(names(rc[[name]] ))
    rc[[name]]$value <- NA
  }
}

name <- 'confirmed_inf'
rc[[name]] <- geoid_params[,'geoid']
rc[[name]]$quantity <- "relative_probability"
rc[[name]]$source <- "incidI"
rc[[name]]$outcome <- "incidC"
rc[[name]]$value <- .1

print(rc)
rc <- do.call(rbind,rc)
##rc <- dplyr::filter(rc,gsub('...$','',geoid) == '36')
arrow::write_parquet(rc,"hospitalization.hpar.parquet")
