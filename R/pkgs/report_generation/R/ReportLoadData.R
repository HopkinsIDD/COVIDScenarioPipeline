
##' Convenience function to load cumulative geounit infections at specific dates for the given scenarios
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param config_display_dates config$report$formatting$display_dates character vector of dates to for which cumulative infections should be extracted
##' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
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
load_cum_inf_geounit_dates <- function(scn_dirs,
                                      config_display_dates=config$report$formatting$display_dates,
                                      config_scenariolabels=config$report$formatting$scenario_labels,
                                      incl_geoids=NULL,
                                      geoid_len = 0,
                                      padding_char = "0"){

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
              dplyr::filter(!is.na(time), geoid %in% incl_geoids)
      }
  } else {
      inf_post_process <- function(x) {
          x %>%
              ungroup %>%
              dplyr::filter(!is.na(time))
      }
  }

  rc <- list()
  for (i in 1:length(scn_dirs)) {
      rc[[i]] <- load_scenario_sims_filtered(scn_dirs[i],
                                             pre_process = inf_pre_process,
                                             post_process = inf_post_process,
                                             geoid_len = geoid_len,
                                             padding_char = padding_char) %>%
          mutate(scenario_num=i,
                 scenario_name=config_scenariolabels[i]) %>%
          ungroup

  }

  return(dplyr::bind_rows(rc))

}




##'
##' Convenience function to allow us to load hospital totals for the combined geounits quickly for
##' the given scenarios.
##'
##' @param scn_dirs the dirctories containing the relevant scenarios
##' @param scenario_labels the scenarios labels
##' @param name_filter character string that filenames should match
##' @param geoid_len required length of geoid
##' @param padding_char padding
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
                                         incl_geoids = NULL,
                                         geoid_len = 0,
                                         padding_char = "0") {

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
                                 N = sum(incidD),
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
                                           post_process = hosp_post_process,
                                           geoid_len = geoid_len,
                                           padding_char = padding_char) %>%
            mutate(scenario_num = i,
                   scenario_name = config$report$formatting$scenario_labels[i])
    }

    return(dplyr::bind_rows(rc))
}




##' Convenience function to load peak geounit infections before a given date for the given scenarios
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
                                        scenariolabels=config$report$formatting$scenario_labels,
                                        incl_geoids=NULL,
                                        geoid_len = 0,
                                        padding_char = "0"){

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
                                            pre_process = inf_pre_process,
                                            post_process = inf_post_process,
                                            geoid_len = geoid_len,
                                            padding_char = padding_char) %>% 
                dplyr::mutate(scenario_num=i,
                              scenario_name=scenariolabels[i]) %>%
                ungroup()

  }

  return(dplyr::bind_rows(rc))

}

##' Convenience function to load peak geounit hospitalizations for the given scenarios
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @param geoid_len required length of geoid
##' @param padding_char padding
##' 
##' @return a data frame with columns
##'         - sim_num
##'         - NhospCurr number of people in hospital on a day
##'         - NICUCurr number of people in ICU on a day
##'         - NincidDeath  number of incidence deaths on a day
##'         - NincidInf  number of incident infections on a day
##'         - NincidICH number of incident ICUs on a day
##' @export
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_hosp_geounit_peak <- function(scn_dirs,
                                  name_filter = "",
                                  incl_geoids = NULL,
                                  scenario_labels = NULL,
                                  geoid_len = 0,
                                  padding_char = "0"){
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
                                           post_process = hosp_post_process,
                                           geoid_len = geoid_len,
                                           padding_char = padding_char)
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
##' @param geoid_len required length of geoid
##' @param padding_char padding
##' 
##' @return a data frame with columns
##'         - sim_num
##'         - NhospCurr number of people in hospital on a day
##'         - NICUCurr number of people in ICU on a day
##'         - NincidDeath  number of incidence deaths on a day
##'         - NincidInf  number of incident infections on a day
##'         - NincidICH number of incident ICUs on a day
##' @export
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_hosp_geounit_threshold <- function(
  scn_dirs,
  threshold,
  variable,
  name_filter = "",
  incl_geoids = NULL,
  scenario_labels = NULL,
  geoid_len = 0,
  padding_char = "0"
){
    if(sum(names(threshold) == "") > 1){stop("You provided more than one catch all threshold")}
    catch_all_threshold <- Inf
    if(sum(names(threshold) == "") > 0){
      catch_all_threshold <- threshold[names(threshold) == ""]
    }
    if (!is.null(incl_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time), geoid %in% incl_geoids) %>%
                group_by(geoid) %>% 
                group_map(function(.x,.y){
                  .x <- .x %>% arrange(time)
                  # Take the first element of the arranged data frame that meets the threshold for that geoid
                  .x <- .x[which(.x[[variable]] >= threshold[.y$geoid])[1], ]
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
                  if(.y$geoid %in% names(threshold)) {
                    .x <- .x[which(.x[[variable]] >= threshold[.y$geoid])[1], ]
                  } else {
                    .x <- .x[which(.x[[variable]] >= catch_all_threshold)[1], ]
                  }
                  .x$geoid <- .y$geoid
                  return(.x)
                }, keep = TRUE) %>%
                do.call(what=dplyr::bind_rows) %>%
                ungroup()
        }
    }
    rc <- list(length=length(scn_dirs))
    for (i in 1:length(scn_dirs)) {
        rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                           name_filter = name_filter,
                                           post_process = hosp_post_process,
                                           geoid_len = geoid_len,
                                           padding_char = padding_char)
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



##' Convenience function to load geodata.csv
##'
##' @param filename geodata.csv filename
##' @param geoid_len length of geoid character string
##' @param geoid_pad what to pad the geoid character string with 
##' @param to_lower whether to make all column names lowercase
##' 
##' @return a data frame with columns
##'         - 
##' @export
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_geodata_file <- function(
  filename,
  geoid_len = 0,
  geoid_pad = "0",
  to_lower = FALSE
) {
  if(!file.exists(filename)){stop(paste(filename,"does not exist in",getwd()))}
  geodata <- readr::read_csv(filename)

  if(to_lower){
    names(geodata) <- tolower(names(geodata))
  }
  if(!('geoid' %in% names(geodata))){stop(paste(filename,"does not have a column named geoid"))}

  if(geoid_len > 0){
    geodata$geoid <- stringr::str_pad(geodata$geoid,geoid_len, pad = geoid_pad)
  }
  return(geodata)
}



##' Convenience function to load shapefile
##'
##' @param filename shapefile name
##' @param geoid_len length of geoid character string
##' @param geoid_pad what to pad the geoid character string with 
##' @param to_lower whether to make all column names lowercase
##' 
##' @return a data frame with columns
##'         - 
##' @export
load_shape_file<- function(
  filename,
  geoid_len = 0,
  geoid_pad = "0",
  to_lower = FALSE
) {
  if(!file.exists(filename)){stop(paste(filename,"does not exist in",getwd()))}
  shp <- sf::st_read(filename)

  if(to_lower){
    names(shp) <- tolower(names(shp))
  }
  if(!('geoid' %in% names(shp))){stop(paste(filename,"does not have a column named geoid"))}
  if(geoid_len > 0){
    shp$geoid <- stringr::str_pad(shp$geoid,geoid_len, pad = geoid_pad)
  }
  return(shp)
}

