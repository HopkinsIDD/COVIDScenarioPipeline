
##' Convenience function to load cumulative geounit infections at multiple specific dates for the given scenarios
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param display_dates config$report$formatting$display_dates character vector of dates to for which cumulative infections should be extracted
##' @param scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
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
                                      display_dates=config$report$formatting$display_dates,
                                      num_files=NULL,
                                      scenariolabels=NULL,
                                      incl_geoids=NULL,
                                      geoid_len = 0,
                                      padding_char = "0"){

  if(is.null(scenariolabels)){
      warning("You have not specified scenario labels for this function. You may encounter future errors.")  
    }

  display_dates <- as.Date(display_dates)
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
                                             num_files = num_files,
                                             pre_process = inf_pre_process,
                                             post_process = inf_post_process,
                                             geoid_len = geoid_len,
                                             padding_char = padding_char)
      rc[[i]]$scenario_num <- i
      rc[[i]]$scenario_name <- scenariolabels[[i]]
  }

  return(dplyr::bind_rows(rc))

}


##' Convenience function to load cumulative geounit hosp outcomes at a specific date for the given scenario
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
                                    num_files = NULL,
                                    scenariolabels = NULL,
                                    name_filter,
                                    display_date=config$end_date,
                                    incl_geoids=NULL,
                                    geoid_len = 0,
                                    padding_char = "0"){

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
                                           padding_char = padding_char) 
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_name <- scenariolabels[[i]]
    }

    return(dplyr::bind_rows(rc))
}



##'
##' Convenience function to allow us to load hospital totals for the combined geounits quickly for
##' the given scenarios.
##'
##' @param scn_dirs the dirctories containing the relevant scenarios
##' @param scenariolabels the scenarios labels
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
                                        num_files = NULL,
                                        scenariolabels = NULL,
                                        name_filter,
                                        incl_geoids = NULL,
                                        geoid_len = 0,
                                        padding_char = "0") {

    if(is.null(scenariolabels)){
      warning("You have not specified scenario labels for this function. You may encounter future errors.")  
    }

    ##filter to munge the data at the scenario level
    if (!is.null(incl_geoids)) {
        hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time) & (geoid %in% incl_geoids)) %>%
                group_by(time, sim_num) %>%
                dplyr::summarize(NhospCurr = sum(hosp_curr),
                                 NICUCurr = sum(icu_curr),
                                 NincidDeath = sum(incidD),
                                 NincidInf = sum(incidI),
                                 NincidICU = sum(incidICU),
                                 NincidHosp = sum(incidH),
                                 NincidVent = sum(incidVent)) %>%
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
                                 NincidHosp=sum(incidH),
                                 NincidVent = sum(incidVent)) %>%
                ungroup()
        }
    }

    rc <- list(length=length(scn_dirs))
    for (i in 1:length(scn_dirs)) {
        rc[[i]] <- load_hosp_sims_filtered(scn_dirs[i],
                                           name_filter = name_filter,
                                           num_files = num_files,
                                           post_process = hosp_post_process,
                                           geoid_len = geoid_len,
                                           padding_char = padding_char) 
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_name <- scenariolabels[[i]]
    }
    
    rc %>% 
      dplyr::bind_rows() %>%
      return()
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
                                        num_files = NULL,
                                        scenariolabels=NULL,
                                        incl_geoids=NULL,
                                        geoid_len = 0,
                                        padding_char = "0"){

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
                                            padding_char = padding_char) 
      rc[[i]]$scenario_num <- i
      rc[[i]]$scenario_name <- scenariolabels[[i]]

  }

  return(dplyr::bind_rows(rc))

}

##' Convenience function to load peak geounit hospitalizations (or any other variable) by a specific date for the given scenarios
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
                                  name_filter,
                                  num_files = NULL,
                                  incl_geoids = NULL,
                                  scenariolabels = NULL,
                                  geoid_len = 0,
                                  padding_char = "0"){
    
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
                                           padding_char = padding_char) %>%
                        dplyr::select(time, geoid, sim_num, mx_var)
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_name <- scenariolabels[[i]]
    }

    rc %>% 
      dplyr::bind_rows() %>%
      dplyr::rename(!!paste0("Pk_", max_var) := mx_var) %>% ## notate the column that was maximized with "Pk_"
    return()
}

##' Convenience function to load the slice for each geoid where the value of an outcome exceeds a given threshold
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param threshold A named numeric vector.  This function will pull the first time slice that meets or exceeds all thresholds
##' @param variable character string of variable to which to compare threshold
##' @param end_date simulation end date character string
##' @param scenario_labels config$report$formatting$scenario_labels character vector of scenario labels
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
  end_date = config$end_date,
  name_filter,
  num_files = NULL,
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

    if(is.null(scenario_labels)){
      warning("You have not specified scenario labels for this function. You may encounter future errors.")  
    }

    end_date <- as.Date(end_date)
    if (!is.null(incl_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time), geoid %in% incl_geoids, time <= end_date) %>%
                group_by(geoid) %>% 
                group_map(function(.x,.y){
                  .x <- .x %>% arrange(time)
                  # Take the first element of the arranged data frame that meets the threshold
                  if(.y$geoid %in% names(threshold)) {
                    .x <- .x[which(.x[[variable]] >= threshold[.y$geoid])[1], ]
                    .x$threshold_value <- threshold[.y$geoid]
                  } else {
                    .x <- .x[which(.x[[variable]] >= catch_all_threshold)[1], ]
                    .x$threshold_value <- catch_all_threshold
                  }
                  .x$geoid <- .y$geoid
                  return(.x)
                }, keep = TRUE) %>%
                do.call(what=dplyr::bind_rows) %>%
                ungroup()
        }
    } else {
        hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time), time <= end_date) %>%
                group_by(geoid) %>% 
                group_map(function(.x,.y){
                  .x <- .x %>% arrange(time)
                  # Take the first element of the arranged data frame that meets the threshold
                  if(.y$geoid %in% names(threshold)) {
                    .x <- .x[which(.x[[variable]] >= threshold[.y$geoid])[1], ]
                    .x$threshold_value <- threshold[.y$geoid]
                  } else {
                    .x <- .x[which(.x[[variable]] >= catch_all_threshold)[1], ]
                    .x$threshold_value <- catch_all_threshold
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
                                           num_files = num_files,
                                           name_filter = name_filter,
                                           post_process = hosp_post_process,
                                           geoid_len = geoid_len,
                                           padding_char = padding_char)
        rc[[i]]$scenario_num <- i
        rc[[i]]$scenario_label <- scenario_labels[[i]]
    }

    rc %>% 
      dplyr::bind_rows() %>%
      dplyr::rename(NhospCurr = hosp_curr,
                    NICUCurr = icu_curr,
                    NincidDeath = incidD,
                    NincidInf = incidI,
                    NincidICU = incidICU,
                    NincidHosp = incidH,
                    NincidVent = incidVent) %>%
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
  shp <- suppressMessages(sf::st_read(filename, quiet = TRUE))

  if(to_lower){
    names(shp) <- tolower(names(shp))
  }
  if(!('geoid' %in% names(shp))){stop(paste(filename,"does not have a column named geoid"))}
  if(geoid_len > 0){

    if(is.na(geoid_pad) | nchar(geoid_pad)>1){stop(paste("Invalid geoid_pad value. Please provide a character or numeric value"))}
    shp$geoid <- stringr::str_pad(shp$geoid,geoid_len, pad = geoid_pad)
  }
  return(shp)
}


##' Load JHU CSSE data
##'
##' @param jhu_data_dir data directory
##' @param countries character vector of countries
##' @param states character vector of states 
##' @param updateJHUData logical on whether JHU data should be udpated
##' 
##' @return a data frame with columns
##'         - 
##' @export
load_jhu_csse_for_report <- function(jhu_data_dir = "JHU_CSSE_Data",
                                     countries = c("US"),
                                     states,
                                     updateJHUData = TRUE,
                                     ...) {
  if(!dir.exists(jhu_data_dir)) {
    ### Download JHU data
    pull_JHUCSSE_github_data(jhu_data_dir) 
  } else {
    ### Get updated version
    if(updateJHUData) {
      covidImportation:::update_JHUCSSE_github_data(jhu_data_dir) 
    }
  }
  
  ### Read in JHU data
  jhu_dat <- read_JHUCSSE_cases(case_data_dir = jhu_data_dir, ...)
  
  
  jhu_dat <- 
    jhu_dat %>%
    dplyr::mutate(date = as.Date(Update)) %>%
    dplyr::filter(Country_Region %in% countries) %>%
    dplyr::filter(Province_State %in% states) %>%
    group_by(date) %>%
    dplyr::summarize(NcumulConfirmed = sum(Confirmed), NcumulDeathsObs = sum(Deaths, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(NincidConfirmed  = NcumulConfirmed - dplyr::lag(NcumulConfirmed),
                  NincidDeathsObs = NcumulDeathsObs - dplyr::lag(NcumulDeathsObs)) %>%
    na.omit()
  return(jhu_dat)
}

