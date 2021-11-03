
##' Convenience function to load cumulative geounit infections at multiple specific dates for the given scenarios
##'
##' @param outcome_dir paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param varname with variable to download (e.g. 'incidI' or 'incidC')
##' @param display_dates config$report$formatting$display_dates character vector of dates to for which cumulative infections should be extracted
##' @param scenariolevels config$report$formatting$scenario_labels_short character vector of scenario levels
##' @param scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids character vector of geoids that are included in the report
##' @param pdeath_filter string that indicates which pdeath level to import for the source files (from the hosp file name)
##'
##' @return a data frame with columns
##'          - time
##'          - geoid
##'          - sim_num
##'          - N
##'          - scenario_num
##'          - scenario_name
##'          - pdeath
##'
##' @export
load_cum_inf_geounit_dates <- function(outcome_dir,
                                       varname="incidI",
                                       display_dates=config$report$formatting$display_dates,
                                       scenario_levels=NULL,
                                       scenario_labels=NULL,
                                       incl_geoids,
                                       pdeath_filter="med",
                                       inference=TRUE)
{
  warning("This function loads infection or case data from hospitalization outputs. Only one IFR scenario is needed to load these data for a given set of model outputs because infection counts will be the same across IFR scenarios.")

  if (is.null(scenario_labels)) {
    warning("You have not specified scenario labels for this function. You may encounter future errors.")
  }
  
  display_dates <- as.Date(display_dates)
  max_date <- max(display_dates)
  
  hosp_pre_process <- function(x) {
    x %>%
      dplyr::select(geoid, scenario, pdeath, location, time, filename, !!varname)
  }
  ##filter to munge the data at the scenario level
  if (!is.null(incl_geoids)) {
    hosp_post_process <- function(x) {
      x %>%
        dplyr::mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time) & geoid %in% incl_geoids, time <= max_date) %>%
        dplyr::group_by(geoid, sim_num, scenario) %>%
        dplyr::mutate(N = cumsum(!!varname)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(time %in% display_dates)
    }
  } else {
    hosp_post_process <- function(x) {
      x %>%
        dplyr::mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time) & time <= max_date) %>%
        dplyr::group_by(geoid, sim_num, scenario) %>%
        dplyr::mutate(N = cumsum(!!varname)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(time %in% display_dates)
    }
  }
  
  rc<-load_hosp_sims_filtered(outcome_dir=outcome_dir,
                              pdeath_filter=pdeath_filter,
                              pre_process=hosp_pre_process,
                              post_process=hosp_post_process,
                              incl_geoids = incl_geoids,
                              inference=inference)
  
  rc<-rc%>%
    dplyr::mutate(scenario_name = factor(scenario, levels=scenario_levels, labels=scenario_labels), 
                  scenario_num = seq_along(scenario))
  
  return(rc)
  
}


##' Convenience function to load cumulative geounit hosp outcomes at a specific date for the given scenario
##'
##' TODO : FIX ME
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param scenario_labels config$report$formatting$scenario_labels character vector of scenario labels
##' @param pdeath_filter character string that filenames should match
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
##'          - scenario
##'          - scenario_num
##'          - scenario_name
##'
##' @export
load_cum_hosp_geounit_date <- function(outcome_dir,
                                       display_dates=config$report$formatting$display_dates,
                                       scenario_levels=NULL,
                                       scenario_labels=NULL,
                                       incl_geoids,
                                       pdeath_filter="med",
                                       inference=TRUE)
{
  warning("This function loads infection data from hospitalization outputs. Only one IFR scenario is needed to load these data for a given set of model outputs because infection counts will be the same across IFR scenarios.")
  if (is.null(scenario_labels)) {
    warning("You have not specified scenario labels for this function. You may encounter future errors.")
  }
  
  display_dates <- as.Date(display_dates)
  max_date <- max(display_dates)
  
  hosp_pre_process <- function(x) {
    x %>%
      dplyr::select(geoid, scenario, location, pdeath, time, filename, incidI, incidD, incidICU, incidH, incidVent) 
  }
  ##filter to munge the data at the scenario level
  if (!is.null(incl_geoids)) {
    hosp_post_process <- function(x) {
      x %>%
        dplyr::mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time), geoid %in% incl_geoids, time <= max_date) %>%
        dplyr::group_by(geoid, sim_num, scenario, location, pdeath) %>%
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
        dplyr::mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time) & time <= max_date) %>%
        dplyr::group_by(geoid, sim_num, scenario, location, pdeath) %>%
        dplyr::summarize(NincidDeath = sum(incidD),
                         NincidInf = sum(incidI),
                         NincidICU=sum(incidICU),
                         NincidHosp=sum(incidH),
                         NincidVent = sum(incidVent)) %>%
        dplyr::ungroup() 
    }
  }
  
  rc<-load_hosp_sims_filtered(outcome_dir=outcome_dir,
                              pdeath_filter=pdeath_filter,
                              pre_process=hosp_pre_process,
                              post_process=hosp_post_process,
                              incl_geoids=incl_geoids,
                              inference=inference)
  
  rc<-rc%>%
    dplyr::mutate(scenario_name = factor(scenario, levels=scenario_levels, labels=scenario_labels), 
                  scenario_num = seq_along(scenario))
  
  return(rc)
  
}


##'
##' Convenience function to allow us to load hospital totals for the combined geounits quickly for
##' the given scenarios.
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param scenario_levels used to create scenario_name for labelling plots
##' @param scenario_labels used to create scenario_name for labelling plots
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' @param incl_geoids character vector of geoids that are included in the report
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_hosp_geocombined_totals <- function(outcome_dir,
                                         scenario_levels, 
                                         scenario_labels,
                                         pdeath_filter=c("high", "med", "low"),
                                         pre_process=function(x) {x},
                                         incl_geoids,
                                         inference=TRUE,
                                         vacc_compartment=FALSE,
                                         ...
) {
  
  require(tidyverse)
  
  if(vacc_compartment){
    hosp_post_process <- function(x) {
      x %>%
        dplyr::group_by(pdeath, scenario, sim_num, location, p_comp, time) %>%
        dplyr::summarize(NhospCurr=sum(hosp_curr),
                         NICUCurr=sum(icu_curr),
                         NincidDeath=sum(incidD),
                         NincidInf=sum(incidI),
                         NincidCase=sum(incidC),
                         NincidICU=sum(incidICU),
                         NincidHosp=sum(incidH),
                         NincidVent=sum(incidVent),
                         NVentCurr=sum(vent_curr)) %>%
        dplyr::group_by(pdeath, scenario, sim_num, location, p_comp) %>%
        dplyr::mutate(cum_hosp=cumsum(NincidHosp)) %>%
        dplyr::mutate(cum_death=cumsum(NincidDeath)) %>%
        dplyr::mutate(cum_case=cumsum(NincidCase)) %>%
        dplyr::mutate(cum_inf=cumsum(NincidInf)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(scenario_name = factor(scenario,
                                             levels = scenario_levels, 
                                             labels = scenario_labels)) 
    }
  } else{
    hosp_post_process <- function(x) {
      x %>%
        dplyr::group_by(pdeath, scenario, sim_num, location, time) %>%
        dplyr::summarize(NhospCurr=sum(hosp_curr), 
                         NICUCurr=sum(icu_curr),       
                         NincidDeath=sum(incidD),
                         NincidInf=sum(incidI),
                         NincidCase=sum(incidC),
                         NincidICU=sum(incidICU),
                         NincidHosp=sum(incidH),
                         NincidVent=sum(incidVent),
                         NVentCurr=sum(vent_curr)) %>%
        dplyr::group_by(pdeath, scenario, sim_num, location) %>%
        dplyr::mutate(cum_hosp=cumsum(NincidHosp)) %>%
        dplyr::mutate(cum_death=cumsum(NincidDeath)) %>%
        dplyr::mutate(cum_case=cumsum(NincidCase)) %>%
        dplyr::mutate(cum_inf=cumsum(NincidInf)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(scenario_name = factor(scenario,
                                             levels = scenario_levels, 
                                             labels = scenario_labels)) 
    }
  }
  
  
  rc<- load_hosp_sims_filtered(outcome_dir=outcome_dir, 
                               pdeath_filter=pdeath_filter,
                               pre_process=pre_process,
                               post_process=hosp_post_process,
                               incl_geoids=incl_geoids,
                               inference=inference)
  
  if(!unique(rc$scenario) %in% scenario_levels) {warning("Scenario levels were not correctly specified you may encounter errors in the future")}
  
  return(rc)
  
}

##'
##' Convenience function to allow us to load hospital totals for the combined geounits quickly for
##' the given scenarios.
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param scenario_levels used to create scenario_name for labelling plots
##' @param scenario_labels used to create scenario_name for labelling plots
##' @param pdeath_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' @param incl_geoids character vector of geoids that are included in the report
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##'      - geoid
##'      - pdeath
##'      - scenario
##'      - sim_id
##'      - sim_num
##'      - time
##'      - location
##'      - NhospCurr
##'      - NICUCurr
##'      - NincidDeath
##'      - NincidInf
##'      - NincidICU
##'      - NincidHosp
##'      - NincidVent
##'      - NVentCurr
##'      - cum_hosp
##'      - cum_death
##'      - cum_case
##'      - cum_inf
##'      - scenario_name
##'
##'
##'@export
##'
load_hosp_county <- function(outcome_dir,
                             scenario_levels, 
                             scenario_labels,
                             pdeath_filter=c("high", "med", "low"),
                             pre_process=function(x) {x},
                             incl_geoids,
                             inference=TRUE,
                             vacc_compartment = FALSE,
                             ...
) {
  
  require(tidyverse)
  
  if(vacc_compartment){
    hosp_post_process <- function(x) {
      x %>%
        dplyr::group_by(geoid, pdeath, scenario, sim_num, location, p_comp) %>%
        dplyr::mutate(cum_hosp=cumsum(incidH)) %>%
        dplyr::mutate(cum_death=cumsum(incidD)) %>%
        dplyr::mutate(cum_case=cumsum(incidC)) %>%
        dplyr::mutate(cum_inf=cumsum(incidI)) %>%
        dplyr::rename(NhospCurr=hosp_curr,
                      NICUCurr=icu_curr,
                      NincidDeath=incidD,
                      NincidInf=incidI,
                      NincidCase=incidC,
                      NincidICU=incidICU,
                      NincidHosp=incidH,
                      NincidVent=incidVent,
                      NVentCurr=vent_curr) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(scenario_name = factor(scenario,
                                             levels = scenario_levels, 
                                             labels = scenario_labels)) 
    }
  } else{
    hosp_post_process <- function(x) {
      x %>%
        dplyr::group_by(geoid, pdeath, scenario, sim_num, location, time) %>%
        dplyr::summarize(NhospCurr=sum(hosp_curr), 
                         NICUCurr=sum(icu_curr),       
                         NincidDeath=sum(incidD),
                         NincidInf=sum(incidI),
                         NincidCase=sum(incidC),
                         NincidICU=sum(incidICU),
                         NincidHosp=sum(incidH),
                         NincidVent=sum(incidVent),
                         NVentCurr=sum(vent_curr)) %>%
        dplyr::group_by(geoid, pdeath, scenario, sim_num, location) %>%
        dplyr::mutate(cum_hosp=cumsum(NincidHosp)) %>%
        dplyr::mutate(cum_death=cumsum(NincidDeath)) %>%
        dplyr::mutate(cum_case=cumsum(NincidCase)) %>%
        dplyr::mutate(cum_inf=cumsum(NincidInf)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(scenario_name = factor(scenario,
                                             levels = scenario_levels, 
                                             labels = scenario_labels)) 
    }
  }
  
  rc<- load_hosp_sims_filtered(outcome_dir=outcome_dir, 
                               pdeath_filter=pdeath_filter,
                               pre_process=pre_process,
                               post_process=hosp_post_process,
                               incl_geoids=incl_geoids,
                               inference=inference)
  
  if(anyNA(rc$scenario_name)) {warning("Scenario levels were not correctly specified you may encounter errors in the future")}
  
  return(rc)
  
}
##' Convenience function to load the slice for each geoid where the value of an outcome exceeds a given threshold
##'
##' @param threshold A named numeric vector.  This function will pull the first time slice that meets or exceeds all thresholds
##' @param scenario_levels string with scenario names passed on to load_hosp_totals
##' @param scenario_labels string with scenario levels passed on to load_hosp_totals
##' @param pdeath_filter pdeath selection one of: high, med, low
##' @param variable character string of variable to which to compare threshold
##' @param end_date simulation end date character string
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @return a data frame with columns
##'         - scenario_name
##'         - sim_num
##'         - NhospCurr number of people in hospital on a day
##'         - NICUCurr number of people in ICU on a day
##'         - NincidDeath  number of incidence deaths on a day
##'         - NincidInf  number of incident infections on a day
##'         - NincidICH number of incident ICUs on a day
##' @export
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_hosp_geounit_threshold <- function(threshold,
                                        variable,
                                        end_date = config$end_date,
                                        pdeath_filter = "high",
                                        incl_geoids,
                                        scenario_labels, 
                                        scenario_levels,
                                        outcome_dir, 
                                        inference=TRUE
){
    if (is.null(scenario_labels)) {
      warning("You have not specified scenario labels for this function. You may encounter future errors.")
    }
    # TODO : FIX VARIABLE NAMES
    # if (!variable %in% c("incidI", "incidH",  "hosp_curr", "incidICU", "icu_curr", "incidVent", "vent_curr", "incidD")) {
    #   warning("You have specified a variable name that may not be supported in the current output. You may encounter future errors")
    # }

    if(sum(names(threshold) == "") > 1){stop("You provided more than one catch all threshold")}
    catch_all_threshold <- Inf
    if (sum(names(threshold) == "") > 0) {
      catch_all_threshold <- threshold[names(threshold) == ""]
    }

    end_date <- as.Date(end_date)
    
    hosp_pre_process <- function(x) {
      x %>%
        dplyr::filter(geoid %in% incl_geoids) %>%
        dplyr::filter(!is.na(time),
                      time <= end_date)
      } 
    
    rc <- load_hosp_totals(outcome_dir=outcome_dir,
                           pre_process=hosp_pre_process,
                           pdeath_filter=pdeath_filter,, 
                           scenario_levels=scenario_levels,
                           scenario_labels=scenario_labels,
                           incl_geoids=incl_geoids,
                           inference=inference)
    rc <- rc %>%
      dplyr::group_by(geoid, scenario, sim_num) %>%
      dplyr::group_map(function(.x,.y){
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
      }, .keep = TRUE) %>%
      do.call(what=dplyr::bind_rows) %>%
      dplyr::ungroup() 
    
      return(rc)
}



##' Convenience function to load geodata.csv
##'
##' @param filename geodata.csv filename
##' @param geoid_len length of geoid character string
##' @param geoid_pad what to pad the geoid character string with
##' @param to_lower whether to make all column names lowercase
##' @param names whether to add a name column to each geoid (US only)
##' 
##' @return a data frame with columns
##'         -
##' @export
### all of the peak times for each sim and each county so we can make a figure for when things peak
load_geodata_file <- function(filename,
                              geoid_len = 0,
                              geoid_pad = "0",
                              to_lower = FALSE,
                              names = FALSE
) {
  # TODO : FIX ME (either use library or remove entirely and use namespaces)
  require(tigris)
  if(!file.exists(filename)){stop(paste(filename,"does not exist in",getwd()))}
  geodata <- readr::read_csv(filename)

  if (to_lower) {
    names(geodata) <- tolower(names(geodata))
  }
  if (!("geoid" %in% names(geodata))) {
    stop(paste(filename, "does not have a column named geoid"))
  }

  if (geoid_len > 0) {
    geodata$geoid <- stringr::str_pad(geodata$geoid, geoid_len, pad = geoid_pad)
  }
  
  if(names) {
  geodata<-geodata %>%
    dplyr::left_join(fips_codes%>%
                       tidyr::unite(col="geoid", tidyselect::ends_with("_code"), sep="") %>%
                       dplyr::select(-state_name, -state) %>%
                       dplyr::rename(name=county) %>%
                       dplyr::mutate(name=stringr::str_remove(name, " County")))
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
load_shape_file<- function(filename,
                           geoid_len = 0,
                           geoid_pad = "0",
                           to_lower = FALSE
) {
  if (!file.exists(filename)) {
    stop(paste(filename, "does not exist in", getwd()))
  }
  shp <- suppressMessages(sf::st_read(filename, quiet = TRUE))

  if (to_lower) {
    names(shp) <- tolower(names(shp))
  }
  if (!("geoid" %in% names(shp))) {
    stop(paste(filename, "does not have a column named geoid"))
  }
  if (geoid_len > 0) {

    if (is.na(geoid_pad) | nchar(geoid_pad) > 1) {
      stop(paste("Invalid geoid_pad value. Please provide a character or numeric value"))
    }
    shp$geoid <- stringr::str_pad(shp$geoid, geoid_len, pad = geoid_pad)
  }
  return(shp)
}


##' Load JHU CSSE data
##'
##' @param jhu_data_dir data directory
##' @param countries character vector of countries
##' @param states character vector of states (state abbreviations is US-only)
##'
##' @return a data frame with columns
##'         - date
##'         - NcumulConfirmed
##'         - NcumulDeathsObs
##'         - NincidConfirmed
##'         - NincidDeathsObs
##'
##' @export
load_jhu_csse_for_report <- function(jhu_data_dir = "JHU_CSSE_Data",
                                     countries = c("US"),
                                     states) {

  require(magrittr)

  us_data_only <- (countries == c("US"))
  if (us_data_only) {
    message("For US data, consider using load_usafacts_for_report() instead of load_jhu_csse_for_report().")
  }

  jhu_cases <- covidImportation::get_clean_JHUCSSE_data(aggr_level = "UID", 
                                                        case_data_dir = jhu_data_dir,
                                                        save_raw_data=TRUE,
                                                        us_data_only=us_data_only)

  jhu_deaths <- covidImportation::get_clean_JHUCSSE_deaths(aggr_level = "UID", 
                                                           case_data_dir = jhu_data_dir,
                                                           save_raw_data=TRUE,
                                                           us_data_only=us_data_only)

  jhu_dat <- dplyr::full_join(jhu_cases, jhu_deaths)

  if (us_data_only) {
    #' @importFrom magrittr %>%
    jhu_dat <- jhu_dat %>% dplyr::mutate(Province_State = state_abb)
  }

  jhu_dat <-
    jhu_dat %>%
    dplyr::mutate(date = as.Date(Update)) %>%
    dplyr::filter(Country_Region %in% countries) %>%
    dplyr::filter(Province_State %in% states) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(NcumulConfirmed = sum(Confirmed, na.rm = TRUE), NcumulDeathsObs = sum(Deaths, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(NincidConfirmed  = NcumulConfirmed - dplyr::lag(NcumulConfirmed),
                  NincidDeathsObs = NcumulDeathsObs - dplyr::lag(NcumulDeathsObs)) %>%
    na.omit()
  return(jhu_dat)
}

##' Load USAFacts data either summarized or by geoid
##'
##' @param data_dir data directory to download raw USAFacts data to
##' @param aggregate whether to aggregate results by source
##' @param incl_geoids geoids to include if data will not be summarized
##' @param geodat df with geoid, name and population
##' 
##' @return a data frame with columns
##'         - date
##'         - NcumulConfirmed (Confirmed if not summarize)
##'         - NcumulDeathsObs (Deaths if not summarized)
##'         - NincidConfirmed (incidI if not summarized)
##'         - NincidDeathsObs (deaths)
##'         
##' @export
load_USAFacts_for_report <- function(data_dir = "data/case_data",
                                     incl_geoids=NULL,
                                     geodat=geodata,
                                     aggregate=FALSE) {

  require(magrittr)

  usaf_dat <- covidcommon::get_USAFacts_data(case_data_filename = file.path(data_dir,"USAFacts_case_data.csv"),
                                             death_data_filename = file.path(data_dir, "USAFacts_death_data.csv"))
  
  if(is.null(incl_geoids)){
    usaf_dat <- usaf_dat %>%
      dplyr::mutate(date=as.Date(Update),
                    geoid=FIPS) %>%
      dplyr::select(-FIPS, -Update)
  
  } else{
    usaf_dat <- usaf_dat %>%
      dplyr::filter(FIPS %in% incl_geoids) %>%
      dplyr::mutate(date=as.Date(Update),
                    geoid=FIPS) %>%
      dplyr::select(-FIPS, -Update)
    
  }
  
  if(aggregate){
    usaf_dat <- usaf_dat %>%
      dplyr::group_by(date, source) %>%
      dplyr::summarise(dplyr::across(-geoid, ~sum(na.rm=TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::rename(NcumulConfirmed=Confirmed,
                    NcumulDeathsObs=Deaths,
                    NincidConfirmed=incidI,
                    NincidDeathsObs=incidDeath)
      
  } else{
    usaf_dat <- usaf_dat %>%
      dplyr::left_join(geodat)
  }
  
  return(usaf_dat)
}


##' Convenience function to display log proportion  
##'
##' @param outcome_dir df with disaggregated hosp outcomes
##' @param threshold A named numeric vector where the names are geoids and values are thresholds
##' @param variable character string of variable to which to compare threshold
##' @param end_date simulation end date character string
##' @param pdeath_filter pdeath character string one of: high, med, low
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @param scenario_levels config$report$formatting$scenario_labels_short character vector of scenario levels
##' @param scenario_labels config$report$formatting$scenario_labels character vector of scenario labels
##' @param inference passed on to load_hosp_sims
##' @param week will return an estimate for the weekly log_prop_needed peak of needed beds/ventilators
##' 
##' @return a data frame with columns
##'         - scenario_name
##'         - geoid
##'         - time
##'         - NhospCurr number of people in hospital on a day
##'         - NICUCurr number of people in ICU on a day
##'         - NVentCurr  number of ventilators used on a day
##'         - threshold_value numeric values from threshold named vector
##'         - prop_needed ratio of needed beds/ventilators relative to threshold value
##'         - log_prop_needed log ratio of needed beds/ventilators with plotting edits on the borders
##' @export
load_hosp_geounit_relative_to_threshold <- function(outcome_dir,
                                                    threshold,
                                                    variable,
                                                    scenario_levels,
                                                    scenario_labels,
                                                    end_date = config$end_date,
                                                    incl_geoids=NULL,
                                                    pdeath_filter,
                                                    geodat=geodata, 
                                                    inference=TRUE,
                                                    week=FALSE
                                                    ){
  

  if (sum(names(threshold) == "") > 1) {
    stop("You provided more than one catch all threshold")
  }
  catch_all_threshold <- Inf
  if (sum(names(threshold) == "") > 0) {
    catch_all_threshold <- threshold[names(threshold) == ""]
  }  
  if(length(pdeath_filter) > 1) {stop("You provided more than one pdeath value")}
  
  if(is.null(scenario_labels)){
    warning("You have not specified scenario labels for this function. You may encounter future errors.")  
  }

  end_date <- lubridate::as_date(end_date)
  
  if(is.null(incl_geoids)){incl_geoids<-unique(county_dat$geoid)}
  
  county_dat<-load_hosp_sims_filtered(outcome_dir=outcome_dir, 
                                      pdeath_filter=pdeath_filter, 
                                      incl_geoids=incl_geoids,
                                      inference=inference,
                                      pre_process=function(x){x%>%
                                          dplyr::select(geoid, time, pdeath, scenario, filename, tidyselect::ends_with("curr"))})
  
  county_dat<-county_dat %>% 
    dplyr::left_join(geodat) %>%
    dplyr::mutate(name = factor(name, levels = sort(geodat$name, decreasing=TRUE))) %>%
    dplyr::filter(time<=end_date) %>%
    dplyr::filter(pdeath==pdeath_filter) %>%
    dplyr::mutate(scenario_name=factor(scenario, levels= scenario_levels, labels=scenario_labels)) 
  
  if(week){
    county_dat<-county_dat %>%
      dplyr::group_by(scenario_name, geoid, name, time=lubridate::ceiling_date(time, "weeks"), sim_num) %>%
      dplyr::summarize(hosp_curr=max(hosp_curr),
                       icu_curr=max(icu_curr),
                       vent_curr=max(vent_curr))%>%
      dplyr::group_by(scenario_name, geoid, name, time) %>%
      dplyr::summarize(NhospCurr=round(mean(hosp_curr)),
                       NICUCurr=round(mean(icu_curr)),
                       NVentCurr=round(mean(vent_curr)))%>%
      dplyr::ungroup()
    
  } else{
    county_dat<-county_dat %>%
      dplyr::group_by(scenario_name, geoid, name, time) %>%
      dplyr::summarize(NhospCurr=round(mean(hosp_curr)),
                       NICUCurr=round(mean(icu_curr)),
                       NVentCurr=round(mean(vent_curr)))%>%
      dplyr::ungroup()
  }
  
  county_dat %>%
    dplyr::left_join(data.frame(geoid = names(threshold), threshold_value = threshold), by = c("geoid")) %>%
    dplyr::rename(pltVar = !!variable) %>%
    dplyr::mutate(prop_needed = pltVar / threshold_value) %>%
    dplyr::mutate(log_prop_needed = log(prop_needed)) %>%
    dplyr::mutate(log_prop_needed = ifelse(pltVar == 0, 
                                           floor(min(log_prop_needed[which(is.finite(log_prop_needed))])),
                                           log_prop_needed)) %>% ## if numerator is 0, set the value to the floor of the min value among all other log values (for plotting purposes)
    dplyr::mutate(log_prop_needed = ifelse(threshold_value == 0 & pltVar > 0, 
                                           ceiling(max(log_prop_needed[which(is.finite(log_prop_needed))])),
                                           log_prop_needed)) %>% ## if threshold is 0, set the value to the ceiling of the max value among all other logs values (for plotting purposes)
    dplyr::rename(!!variable := pltVar) %>%
    return()

}

##' Convenience function for loading intervention effect and R estimates 
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param pdeath_filter string that indicates which pdeath(s) to import from outcome_dir
##' @param pre_process function that does processing before collection of snpi outputs
##' @param geodat df with geoid and name columns 
##' @param incl_geoids character vector of geoids that are included in the report
##' @param npi_trimmer pattern used by str_remove to edit npi_name column; original 
##' names conserved in npi_group_name col
##' 
##' @return a combined data frame of all R and effectiveness per geoid-intervention across 
##' simulations with filters applied pre merge
##' 
##'
##'
##'@export
load_r_sims_filtered <- function(outcome_dir,
                                 pdeath_filter=c("high", "med", "low"),
                                 pre_process=function(x) {x},
                                 geodat=geodata,
                                 incl_geoids,
                                 npi_trimmer="[[A-Z]].+\\_",
                                 ...
) {
  
  require(tidyverse)
  
  spar <- load_spar_sims_filtered(outcome_dir=outcome_dir, 
                                  pre_process=function(x) {x %>% dplyr::filter(parameter=="R0")}, 
                                  pdeath_filter=pdeath_filter,
                                  ...) %>%
    dplyr::group_by(scenario) %>%
    dplyr::rename(r0=value) %>%
    dplyr::select(sim_num, scenario, pdeath, r0) %>%
    dplyr::ungroup()
  
  snpi<- load_snpi_sims_filtered(outcome_dir=outcome_dir, 
                                 pre_process=pre_process, 
                                 pdeath_filter=pdeath_filter, 
                                 incl_geoids=incl_geoids,
                                 ...) %>%
    dplyr::select(sim_num, scenario, pdeath, geoid, npi_name, start_date, end_date, reduction)
  
  rc <- snpi %>%
    dplyr::filter(npi_name=="local_variance") %>%
    dplyr::right_join(spar)%>%
    dplyr::mutate(local_r = r0*(1-reduction)) %>% # county_r0 ought to be renamed to "geogroup_r0"
    dplyr::select(geoid, sim_num, local_r, scenario, r0) %>%
    dplyr::left_join(snpi) %>%
    dplyr::mutate(r = dplyr::if_else(npi_name=="local_variance",
                       local_r,
                       local_r*(1-reduction))) %>%
    dplyr::left_join(geodat) %>%
    dplyr::rename(npi_group_name=npi_name) %>%
    dplyr::mutate(npi_name = stringr::str_remove(npi_group_name, npi_trimmer)) %>%
    dplyr::ungroup() 
  
  warning("Finished loading")
  return(rc)
  
}

##' Convenience function for loading daily Rt and total effectiveness estimates by geoid
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param pdeath_filter string that indicates which pdeath(s) to import from outcome_dir
##' @param incl_geoids character vector of geoids that are included in the report
##' @param mtr logical to determine if non-contiguous interventions were applied - default TRUE
##' @param n_periods maximum number of non-contiguous periods for any geoid/intervention
##' 
##' @return a combined data frame of daily Rt and total effectiveness estimates per geoid
##' 
##'
##'
##'@export
load_r_daily_sims_filtered <- function(outcome_dir,
                                       pdeath_filter=c("high", "med", "low"),
                                       incl_geoids,
                                       mtr=TRUE,
                                       n_periods=10,
                                       ...
) {
  
  require(tidyverse)
  
  spar <- load_spar_sims_filtered(outcome_dir=outcome_dir, 
                                  pre_process=function(x) {x %>% dplyr::filter(parameter=="R0")}, 
                                  pdeath_filter=pdeath_filter) %>%
    dplyr::select(r0=value, location, scenario, pdeath, sim_num)
  
  snpi<- load_snpi_sims_filtered(outcome_dir=outcome_dir, 
                                 pre_process=function(x) {x %>% dplyr::filter(parameter=="r0")}, 
                                 pdeath_filter=pdeath_filter, 
                                 incl_geoids=incl_geoids) %>%
    dplyr::select(-parameter)
  
  if(mtr){
    npi <- snpi %>%
      dplyr::left_join(spar) %>% 
      mtr_estimates(n_periods=n_periods)
  } else {
    npi <- snpi %>%
      dplyr::left_join(spar) %>% 
      dplyr::mutate(start_date=lubridate::ymd(start_date),
             end_date=lubridate::ymd(end_date))
  }
  
  geoiddate<-tidyr::crossing(geoid=incl_geoids, time=seq(min(as.Date(npi$start_date)), max(as.Date(npi$end_date)), 1))
  
  rc<-list()
  
  for(i in 1:length(incl_geoids)){
    rc[[i]]<-npi %>%
      dplyr::filter(geoid == incl_geoids[i])%>%
      dplyr::left_join(geoiddate)%>%
      dplyr::mutate(geoid=dplyr::if_else(start_date>time | end_date<time, NA_character_, geoid))%>%
      tidyr::drop_na() %>%
      dplyr::group_by(geoid, sim_num, time, pdeath, scenario, location) %>%
      dplyr::mutate(reduction=1-reduction)%>%
      dplyr::summarize(reduction=prod(reduction),
                r0=unique(r0)) %>%
      dplyr::mutate(rt=reduction*r0,
             reduction=1-reduction)
  }
  
  rc<-dplyr::bind_rows(rc) %>%
    dplyr::ungroup()
  
  warning("Finished loading")
  return(rc)
  
}

##' Convenience function to load intervention effectiveness estimates 
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param pdeath_filter string that indicates which pdeath(s) to import from outcome_dir
##' @param incl_geoids character vector of geoids that are included in the report
##' 
##' @return a combined data frame of daily Rt and total effectiveness estimates per geoid/sim 
##' 
##'
##'
##'@export
load_npi_sims_filtered <- function(outcome_dir,
                                       pdeath_filter=c("high", "med", "low"),
                                       incl_geoids,
                                       ...
) {
  
  require(tidyverse)
  
  npi<- load_snpi_sims_filtered(outcome_dir=outcome_dir, 
                                 pre_process=function(x) {x %>% dplyr::filter(parameter=="r0")}, 
                                 pdeath_filter=pdeath_filter, 
                                 incl_geoids=incl_geoids,
                                 ...) %>%
    dplyr::select(-parameter)
  
  warning("Finished loading")
  
  return(npi)
  
}
