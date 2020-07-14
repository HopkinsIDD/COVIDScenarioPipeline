##' Deprecated convenience function to load cumulative geounit hosp outcomes at a specific date for the given scenario
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param name_filter character string that filenames should match
##' @param display_date character date string for which cumulative infections should be extracted
##' @param incl_geoids character vector of geoids that are included in the report
##' @param geoid_len in defined, this we want to make geoids all the same length
##' @param padding_char character to add to the front of geoids if fixed length
##'
##' @return a data frame with columns
##'          - time
##'          - comp
##'          - geoid
##'          - N
##'          - sim_num
##'          - scenario_num
##'          - scenario_name
##'
##' @export
load_cum_hosp_geounit_date <- function(scn_dirs,
                                    num_files = NA,
                                    scenariolabels = NULL,
                                    name_filter,
                                    display_date=config$end_date,
                                    incl_geoids=NULL,
                                    geoid_len = 0,
                                    padding_char = "0",
                                    file_extension = 'auto'){

    if(is.null(scenariolabels)){
      warning("You have not specified scenario labels for this function. You may encounter future errors.")
    }

    display_date <- as.Date(display_date)
    ##filter to munge the data at the scenario level
    if (!is.null(incl_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time) & geoid %in% incl_geoids, time <= display_date) %>%
                group_by(geoid, sim_num) %>%
                dplyr::summarize(NincidDeath = sum(incidD),
                                 NincidInf = sum(incidI),
                                 NincidICU=sum(incidICU),
                                 NincidHosp=sum(incidH),
                                 NincidVent = sum(incidVent)) %>%
                ungroup()
        }
    } else {
        hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time) & time <= display_date) %>%
                group_by(geoid, sim_num) %>%
                dplyr::summarize(NincidDeath = sum(incidD),
                                 NincidInf = sum(incidI),
                                 NincidICU=sum(incidICU),
                                 NincidHosp=sum(incidH),
                                 NincidVent = sum(incidVent)) %>%
                ungroup()
        }
    }


    rc <- list(length=length(scn_dirs))
    for (i in 1:length(scn_dirs)) {
        rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                           num_files = num_files,
                                           name_filter = name_filter,
                                           post_process = hosp_post_process,
                                           geoid_len = geoid_len,
                                           padding_char = padding_char,
                                           file_extension = file_extension)
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_name <- scenariolabels[[i]]
    }

    return(dplyr::bind_rows(rc))
}



##' Deprecated convenience function to load timeseries current hospital outcomes
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param name_filter character string that filenames should match
##' @param end_date last date to include in timeseries
##' @param incl_geoids character vector of geoids that are included in the report
##' @param geoid_len in defined, this we want to make geoids all the same length
##' @param padding_char character to add to the front of geoids if fixed length
##'
##' @return a data frame with columns
##'          - time
##'          - geoid
##'          - NHospCurr, NICUCurr, NVentCurr
##'          - sim_num
##'          - scenario_num
##'          - scenario_name
##'
##' @export
load_ts_current_hosp_geounit <- function(scn_dirs,
                                         num_files = NA,
                                         scenariolabels = NULL,
                                         name_filter,
                                         end_date,
                                         incl_geoids=NULL,
                                         geoid_len = 0,
                                         padding_char = "0",
                                         qlo = 0.025,
                                         qhi= 0.975,
                                         file_extension = 'auto') {
  
  if(is.null(scenariolabels)){
    warning("You have not specified scenario labels for this function. You may encounter future errors.")
  }
  
  
  ## currently too slow including the quantiles... ##
  end_date <- as.Date(end_date)
  ##filter to munge the data at the scenario level
  if (!is.null(incl_geoids)) {
    hosp_post_process <- function(x) {
      x %>%
        dplyr::filter(!is.na(time) & geoid %in% incl_geoids, time <= end_date) %>%
        dplyr::select(time,
                      geoid,
                      sim_num,
                      NHospCurr = hosp_curr,
                      NICUCurr = icu_curr,
                      NVentCurr = vent_curr) %>%
        group_by(sim_num, time, geoid) %>%
        mutate(#NHospCurrlo = quantile(NHospCurr, qlo),
               #NHospCurrhi = quantile(NHospCurr, qhi),
               NHospCurr = mean(NHospCurr),
               #NICUCurrlo = quantile(NICUCurr, qlo),
               #NICUCurrhi = quantile(NICUCurr, qhi),
               NICUCurr = mean(NICUCurr),
               #NVentCurrlo = quantile(NVentCurr, qlo),
               #NVentCurrhi = quantile(NVentCurr, qhi),
               NVentCurr = mean(NVentCurr)) %>%
        ungroup()
    }
  } else {
    hosp_post_process <- function(x) {
      x %>%
        dplyr::filter(!is.na(time) & time <= end_date) %>%
        dplyr::select(time,
                      geoid,
                      sim_num,
                      NHospCurr = hosp_curr,
                      NICUCurr = icu_curr,
                      NVentCurr = vent_curr) %>%
        group_by(sim_num, time, geoid) %>%
        mutate(#NHospCurrlo = quantile(NHospCurr, qlo),
               #NHospCurrhi = quantile(NHospCurr, qhi),
               NHospCurr = mean(NHospCurr),
               #NICUCurrlo = quantile(NICUCurr, qlo),
               #NICUCurrhi = quantile(NICUCurr, qhi),
               NICUCurr = mean(NICUCurr),
               #NVentCurrlo = quantile(NVentCurr, qlo),
               #NVentCurrhi = quantile(NVentCurr, qhi),
               NVentCurr = mean(NVentCurr)) %>%
        ungroup()
    }
  }
  
  
  rc <- list(length=length(scn_dirs))
  for (i in 1:length(scn_dirs)) {
    rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                       num_files = num_files,
                                       name_filter = name_filter,
                                       post_process = hosp_post_process,
                                       geoid_len = geoid_len,
                                       padding_char = padding_char,
                                       file_extension = file_extension) 
    rc[[i]]$scenario_num <- i
    rc[[i]]$scenario_name <- scenariolabels[[i]]
  }
  
  return(dplyr::bind_rows(rc))
}


##' Deprecated convenience function to load peak geounit infections before a given date for the given scenarios
##' 
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param display_date character string for date before which infection peak should be identified
##' @param scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @param geoid_len required length of geoid
##' @param padding_char padding
##' 
##' @return a data frame with columns
##'          - time
##'          - comp
##'          - geoid
##'          - N
##'          - sim_num
##'          - scenario_num
##'          - scenario_name
##'
##' @export 
load_inf_geounit_peaks_date <- function(scn_dirs,
                                        display_date=config$end_date,
                                        num_files = NA,
                                        scenariolabels=NULL,
                                        incl_geoids=NULL,
                                        geoid_len = 0,
                                        padding_char = "0",
                                        file_extension = 'auto'){

  if(is.null(scenariolabels)){
      warning("You have not specified scenario labels for this function. You may encounter future errors.")  
  }

  display_date <- as.Date(display_date)
  inf_pre_process <- function(x) {
      x %>%
        dplyr::filter(comp == "diffI" & time <= display_date) 
    }
  
  if (!is.null(incl_geoids)) {
        inf_post_process <- function(x) {
          x %>% 
            ungroup %>%
            dplyr::filter(!is.na(time), geoid %in% incl_geoids) %>%
            group_by(geoid) %>%
            dplyr::slice(which.max(N)) %>%
            ungroup()
        } 
  } else{
        inf_post_process <- function(x) {
          x %>% 
            ungroup %>%
            dplyr::filter(!is.na(time)) %>%
            group_by(geoid) %>%
            dplyr::slice(which.max(N)) %>%
            ungroup()
        }
    
  }
    
  rc <- list()
  for (i in 1:length(scn_dirs)) {
      rc[[i]] <- load_scenario_sims_filtered(scn_dirs[i],
                                            num_files = num_files,
                                            pre_process = inf_pre_process,
                                            post_process = inf_post_process,
                                            geoid_len = geoid_len,
                                            padding_char = padding_char,
                                            file_extension = file_extension) 
      rc[[i]]$scenario_num <- i
      rc[[i]]$scenario_name <- scenariolabels[[i]]

  }

  return(dplyr::bind_rows(rc))

}


##' Deprecated convenience function to load peak geounit hospitalizations (or any other variable) by a specific date for the given scenarios
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param max_var character string of variable that will be maximized per geoid
##' @param display_date date before which we should search for peaks
##' @param name_filter character string that filenames should match
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @param scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @param geoid_len required length of geoid
##' @param padding_char padding
##' 
##' @return a data frame with columns
##'         - sim_num
##'         - Pk_[variableName] variable that was maximized by geoid
##'         - NhospCurr number of people in hospital on a day
##'         - NICUCurr number of people in ICU on a day
##'         - NincidDeath  number of incidence deaths on a day
##'         - NincidInf  number of incident infections on a day
##'         - NincidICH number of incident ICUs on a day
##' @export
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_hosp_geounit_peak_date <- function(scn_dirs,
                                  max_var,
                                  display_date = config$end_date,
                                  num_files = NA,
                                  name_filter,
                                  incl_geoids = NULL,
                                  scenariolabels = NULL,
                                  geoid_len = 0,
                                  padding_char = "0",
                                  file_extension = 'auto'){
    
    if(is.null(scenariolabels)){
      warning("You have not specified scenario labels for this function. You may encounter future errors.")  
    }

    display_date <- as.Date(display_date)
    if (!is.null(incl_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::rename(mx_var = !!max_var) %>%
                dplyr::filter(!is.na(time), geoid %in% incl_geoids, time <= display_date) %>%
                group_by(geoid) %>% 
                dplyr::slice(which.max(mx_var)) %>%
                ungroup()
        }
    } else {
        hosp_post_process <- function(x) {
            x %>%
                dplyr::rename(mx_var = !!max_var) %>%
                dplyr::filter(!is.na(time), time <= display_date) %>%
                group_by(geoid) %>% 
                dplyr::slice(which.max(mx_var)) %>%
                ungroup()
        }
    } 
    rc <- list(length=length(scn_dirs))
    for (i in 1:length(scn_dirs)) {
        rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                           num_files = num_files,
                                           name_filter = name_filter,
                                           post_process = hosp_post_process,
                                           geoid_len = geoid_len,
                                           padding_char = padding_char,
                                           file_extension = file_extension) %>%
                        dplyr::select(time, geoid, sim_num, mx_var)
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_name <- scenariolabels[[i]]
    }

    rc %>% 
      dplyr::bind_rows() %>%
      dplyr::rename(!!paste0("Pk_", max_var) := mx_var) %>% ## notate the column that was maximized with "Pk_"
    return()
}
