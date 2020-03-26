#' @export
setup_testing_environment <- function(config_file = "config.yml"){
  params = list(config_file = config_file)
  library(tidyverse)

  LOADED_CONFIG <<- FALSE
  LOADED_GEOIDS <<- FALSE
  LOADED_HOSP_STATE_TOTALS <<- FALSE
  LOADED_INF_CTY_PEAKS <<- FALSE
  LOADED_HOSP_CTY_PEAKS <<- FALSE
  LOADED_HOSP_CTY_TOTALS <<- FALSE
  LOADED_SHAPEFILES <<- FALSE


  if(is.null(params$config_file)){
    stop("A document parameter `config_file` is required to load the config file")
  }
  config <<- covidcommon::load_config(params$config_file)
  LOADED_CONFIG <- TRUE
  
  if(!LOADED_GEOIDS){
    geodata <<- read.csv(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata))
    included_geoids <<- geodata[["geoid"]][geodata[[config$spatial_setup$include_in_report]]]
    # populations <- geodata[[config$spatial_setup$popnodes]]
    LOADED_GEOIDS <- TRUE
  }
  

  if (!LOADED_CONFIG) { stop("This chunk requires the config to be loaded")}
  ## Code loads the state hospitalization totals if it has not yet been loaded
  if (!LOADED_HOSP_CTY_PEAKS) {
      hosp_post_process <- function(x) {
          x %>% 
              ungroup %>%
              filter(!is.na(time)) %>%
              group_by(geoid) %>% 
              dplyr::slice(which.max(hosp_curr)) %>%
              ungroup
      }
  
      scn_dirs <- paste(config$name,config$interventions$scenarios,sep='_')
      hosp_cty_peaks <- NULL
      
      for (i in 1:length(scn_dirs)) {
          for (pdeath in config$hospitalization$parameters$p_death_names) {
              hosp_cty_peaks <<- dplyr::bind_rows(hosp_cty_peaks, load_hosp_sims_filtered(scn_dirs[i],
                                            name_filter = pdeath,
                                            post_process = hosp_post_process) %>% 
                  mutate(scenario_num=i, scenario_name=params$scenario_labels[i], pdeath=pdeath))
          }
      }
      
      
      LOADED_HOSP_CTY_PEAKS <- TRUE 
  }


  if (!LOADED_CONFIG){stop("This chunk requires the config to be loaded")}
  ## Code loads the state hospitalization totals if it has not yet been loaded
  if (!LOADED_HOSP_CTY_TOTALS) {
      hosp_post_process <- function(x) {
          x %>% 
              dplyr::filter(!is.na(time)) %>%
              dplyr::filter(geoid %in% included_geoids) %>%
              group_by(geoid, time, sim_num) %>% 
              summarize(NhospCurr = sum(hosp_curr),
                        NICUCurr = sum(icu_curr),
                        NincidDeath = sum(incidD),
                        NincidInf = sum(incidI),
                        NincidICU=sum(incidICU),
                        NincidHosp=sum(incidH)) %>% 
              ungroup()
        
      }
  
      scn_dirs <- paste(config$name,config$interventions$scenarios,sep='_')
      hosp_cty_totals <- NULL
      
      for (i in 1:length(scn_dirs)) {
          for (pdeath in config$hospitalization$parameters$p_death_names) {
              hosp_cty_totals <<- dplyr::bind_rows(hosp_cty_totals, load_hosp_sims_filtered(scn_dirs[i],
                                            name_filter = pdeath,
                                            post_process = hosp_post_process) %>% 
                  mutate(scenario_num=i, scenario_name=params$scenario_labels[i], pdeath=pdeath))
          }
      }
      
      
      LOADED_HOSP_CTY_TOTALS <- TRUE
  }
  


  if (!LOADED_CONFIG){stop("This chunk requires the config to be loaded")}
  
  if (!LOADED_HOSP_STATE_TOTALS) {
      hosp_post_process <- function(x) {
          x %>% 
              dplyr::filter(!is.na(time)) %>%
              group_by(time, sim_num) %>% 
              dplyr::summarize(NhospCurr = sum(hosp_curr),
                        NICUCurr = sum(icu_curr),
                        NincidDeath = sum(incidD),
                        NincidInf = sum(incidI),
                        NincidICU=sum(incidICU),
                        NincidHosp=sum(incidH)) %>% 
              ungroup()
      }
  
      scn_dirs <- paste(config$name,config$interventions$scenarios,sep='_')
      hosp_state_totals <- NULL
      
      for (i in 1:length(scn_dirs)) {
          for (pdeath in config$hospitalization$parameters$p_death_names) {
              hosp_state_totals <<- dplyr::bind_rows(hosp_state_totals, load_hosp_sims_filtered(scn_dirs[i],
                                            name_filter = pdeath,
                                            post_process = hosp_post_process) %>% 
                  mutate(scenario_num = i, scenario_name = config$report$formatting$scenario_labels[i], pdeath=pdeath)) 
          }
      }
     
      
      LOADED_HOSP_STATE_TOTALS <- TRUE 
  }



  if(!LOADED_CONFIG){stop("This chunk requires the config to be loaded")}
  ## Code loads the county infection peaks if it has not yet been loaded
  if (!LOADED_INF_CTY_PEAKS) {
    
    inf_pre_process <- function(x) {
      x %>%
        dplyr::filter(comp == "diffI")
    }
    
    inf_post_process <- function(x) {
      x %>% 
        ungroup %>%
        dplyr::filter(!is.na(time)) %>%
        dplyr::filter(geoid %in% included_geoids) %>%
        group_by(geoid) %>%
        dplyr::slice(which.max(N)) %>%
        ungroup
    }
    
    scn_dirs <- paste(config$name,config$interventions$scenarios,sep='_')
    inf_cty_peaks <- NULL
    
    for (i in 1:length(scn_dirs)) {
        inf_cty_peaks <<- dplyr::bind_rows(inf_cty_peaks, load_scenario_sims_filtered(scn_dirs[i],
                                                                                    pre_process = inf_pre_process,
                                                                                    post_process = inf_post_process) %>% 
                                                mutate(scenario_num=i, scenario_name=params$scenario_labels[i]))
  
    }
    
    
    LOADED_INF_CTY_PEAKS <- TRUE 
  }

  shp <<- suppressMessages(sf::st_read(paste(config$spatial_setup$base_path,config$spatial_setup$shapefile_name,sep='/'), quiet=TRUE))
  shp <<- shp[shp[["geoid"]] %in% included_geoids,]
}
