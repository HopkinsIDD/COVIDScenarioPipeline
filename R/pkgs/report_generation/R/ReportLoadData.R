
#' Load cumulative county infections at specific dates
#' 
#' @param included_geoids character vector of geoids that are included in the report
#' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
#' @param config_display_dates config$report$formatting$display_dates character vector of dates to for which cumulative infections should be extracted
#' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels

load_inf_county_cum_dates <- function(included_geoids,
                                      scn_dirs, 
                                      config_display_dates, 
                                      config_scenariolabels){
    
  display_dates <- as.Date(config_display_dates)
  inf_pre_process <- function(x) {
    x %>%
      dplyr::filter(comp == "cumI") %>%
      dplyr::filter(time %in% display_dates)
  }
  
  inf_post_process <- function(x) {
    x %>% 
      ungroup %>%
      dplyr::mutate(geoid=ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>% 
      dplyr::filter(!is.na(time), geoid %in% included_geoids)
  }
  
  inf_county_cum_dates <- NULL
  
  for (i in 1:length(scn_dirs)) {
      inf_county_cum_dates <- inf_county_cum_dates %>% 
        dplyr::bind_rows(load_scenario_sims_filtered(scn_dirs[i],
                                                     pre_process = inf_pre_process,
                                                     post_process = inf_post_process) %>% 
                           mutate(scenario_num=i,
                                  scenario_name=config_scenariolabels[i]) %>% 
                           ungroup
        )
  }

  return(inf_county_cum_dates)
  
}


