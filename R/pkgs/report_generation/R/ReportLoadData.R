
##' Convenience function to load cumulative geounit infections at specific dates for the given scenarios
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param config_display_dates config$report$formatting$display_dates character vector of dates to for which cumulative infections should be extracted
##' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids character vector of geoids that are included in the report
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
load_cum_inf_geounit_dates <- function(scn_dirs,
                                      config_display_dates=config$report$formatting$display_dates,
                                      config_scenariolabels=config$report$formatting$scenario_labels,
                                      incl_geoids=NULL){

  display_dates <- as.Date(config_display_dates)
  inf_pre_process <- function(x) {
    x %>%
      dplyr::filter(comp == "cumI") %>%
      dplyr::filter(time %in% display_dates)
  }

  if (!is.null(incl_geoids)) {
      inf_post_process <- function(x) {
          x %>%
              ungroup %>%
              dplyr::mutate(geoid=ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>%
              dplyr::filter(!is.na(time), geoid %in% incl_geoids)
      }
  } else {
      inf_post_process <- function(x) {
          x %>%
              ungroup %>%
              dplyr::mutate(geoid=ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>%
              dplyr::filter(!is.na(time))
      }
  }

  rc <- list()
  for (i in 1:length(scn_dirs)) {
      rc[[i]] <- load_scenario_sims_filtered(scn_dirs[i],
                                             pre_process = inf_pre_process,
                                             post_process = inf_post_process) %>%
          mutate(scenario_num=i,
                 scenario_name=config_scenariolabels[i]) %>%
          ungroup

  }

  return(dplyr::bind_rows(rc))

}




##'
##' Convienence function to allow us to load hospital totals for the combined geounits quickly for
##' the given scenarios.
##'
##' @param scn_dirs the dirctories containing the relevant scenarios
##' @param scenario_labels the scenarios labels
##' @param name_filter character string that filenames should match
##'
##' @return a data frame with columns:
##'    - sim_num
##'    - NhospCurr number of people in hospital on a day
##'    - NICUCurr number of people in ICU on a day
##'    - NincidDeath  number of incidence deaths on a day
##'    - NincidInf  number of incident infections on a day
##'    - NincidICH number of incident ICUs on a day
##'
##'
##'
##' @export
##'
load_hosp_geocombined_totals <- function(scn_dirs,
                                         scenario_labels = config$report$formatting$scenario_labels,
                                         name_filter = "",
                                         incl_geoids = NULL) {

    ##filter to munge the data at the senario level
    if (!is.null(incl_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time), geoid %in% incl_geoids) %>%
                group_by(time, sim_num) %>%
                dplyr::summarize(NhospCurr = sum(hosp_curr),
                                 NICUCurr = sum(icu_curr),
                                 NincidDeath = sum(incidD),
                                 NincidInf = sum(incidI),
                                 NincidICU=sum(incidICU),
                                 NincidHosp=sum(incidH)) %>%
                ungroup()
        }
    } else {
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
    }


    rc <- list(length=length(scn_dirs))
    for (i in 1:length(scn_dirs)) {
        rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                           name_filter = name_filter,
                                           post_process = hosp_post_process) %>%
            mutate(scenario_num = i,
                   scenario_name = config$report$formatting$scenario_labels[i])
    }

    return(dplyr::bind_rows(rc))
}




##' Convenience function to load peak geounit infections for the given scenarios
##' 
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
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
load_inf_geounit_peaks <- function(scn_dirs,
                                  config_scenariolabels=config$report$formatting$scenario_labels,
                                  incl_geoids=NULL){

  inf_pre_process <- function(x) {
      x %>%
        dplyr::filter(comp == "diffI") 
    }
  
  if (!is.null(incl_geoids)) {
        inf_post_process <- function(x) {
          x %>% 
            ungroup %>%
            dplyr::mutate(geoid=ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>% 
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
            dplyr::mutate(geoid=ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>% 
            group_by(geoid) %>%
            dplyr::slice(which.max(N)) %>%
            ungroup()
        }
    
  }
    
  rc <- list()
  for (i in 1:length(scn_dirs)) {
      rc[[i]] <- load_scenario_sims_filtered(scn_dirs[i],
                                            pre_process = inf_pre_process,
                                            post_process = inf_post_process) %>% 
                dplyr::mutate(scenario_num=i,
                              scenario_name=config_scenariolabels[i]) %>%
                ungroup()

  }

  return(dplyr::bind_rows(rc))

}

##' Convenience function to load peak geounit hospitalizations for the given scenarios
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @return a data frame with columns
##'         - sim_num
##'         - NhospCurr number of people in hospital on a day
##'         - NICUCurr number of people in ICU on a day
##'         - NincidDeath  number of incidence deaths on a day
##'         - NincidInf  number of incident infections on a day
##'         - NincidICH number of incident ICUs on a day
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_hosp_geounit_peak <- function(
  scn_dirs,
  name_filter = "",
  incl_geoids = NULL,
  scenario_labels = NULL
){
    if (!is.null(incl_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time), geoid %in% incl_geoids) %>%
                group_by(geoid) %>% 
                dplyr::slice(which.max(hosp_curr)) %>%
                ungroup()
        }
    } else {
        hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time)) %>%
                group_by(geoid) %>% 
                dplyr::slice(which.max(hosp_curr)) %>%
                ungroup()
        }
    } 
    rc <- list(length=length(scn_dirs))
    for (i in 1:length(scn_dirs)) {
        rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                           name_filter = name_filter,
                                           post_process = hosp_post_process)
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_name <- scenario_labels[[i]]
    }

    rc %>% 
      dplyr::bind_rows() %>%
      dplyr::rename(NhospCurr = hosp_curr,
                    NICUCurr = icu_curr,
                    NincidDeath = incidD,
                    NincidInf = incidI,
                    NincidICU = incidICU,
                    NincidHosp = incidH) %>%
      return()
}

##' Convenience function to load the slice for each geoid where the value of an outcome exceeds a given threshold
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param threshold A named numeric vector.  This function will pull the first time slice that meets or exceeds all thresholds
##' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @return a data frame with columns
##'         - sim_num
##'         - NhospCurr number of people in hospital on a day
##'         - NICUCurr number of people in ICU on a day
##'         - NincidDeath  number of incidence deaths on a day
##'         - NincidInf  number of incident infections on a day
##'         - NincidICH number of incident ICUs on a day
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_hosp_geounit_threshold <- function(
  scn_dirs,
  threshold,
  name_filter = "",
  incl_geoids = NULL,
  scenario_labels = NULL
){
    if (!is.null(incl_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time), geoid %in% incl_geoids) %>%
                group_by(geoid) %>% 
                group_map(function(.x,.y){
                  .x <- .x %>% arrange(time)
                  # Take the first element of the arranged data frame that meets the threshold
                  .x <- .x[which(.x[[names(threshold)]] >= threshold)[1], ]
                }) %>%
                do.call(what=dplyr::bind_rows) %>%
                ungroup()
        }
    } else {
        hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time)) %>%
                group_by(geoid) %>% 
                group_map(function(.x,.y){
                  .x <- .x %>% arrange(time)
                  # Take the first element of the arranged data frame that meets the threshold
                  .x <- .x[which(.x[[names(threshold)]] >= threshold)[1], ]
                }, keep = TRUE) %>%
                do.call(what=dplyr::bind_rows) %>%
                ungroup()
        }
    }
    rc <- list(length=length(scn_dirs))
    for (i in 1:length(scn_dirs)) {
        rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                           name_filter = name_filter,
                                           post_process = hosp_post_process)
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_name <- scenario_labels[[i]]
    }

    rc %>% 
      dplyr::bind_rows() %>%
      dplyr::rename(NhospCurr = hosp_curr,
                    NICUCurr = icu_curr,
                    NincidDeath = incidD,
                    NincidInf = incidI,
                    NincidICU = incidICU,
                    NincidHosp = incidH) %>%
      return()
}
