
##' Convenience function to load cumulative geounit infections at multiple specific dates for the given scenarios
##'
##' @param outcome_dir paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param display_dates config$report$formatting$display_dates character vector of dates to for which cumulative infections should be extracted
##' @param scenariolevels config$report$formatting$scenario_labels_short character vector of scenario levels
##' @param scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param incl_geoids character vector of geoids that are included in the report
##' @param name_filter string that indicates which pdeath level to import for the source files (from the hosp file name)
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
                                       display_dates=config$report$formatting$display_dates,
                                       scenario_levels=NULL,
                                       scenario_labels=NULL,
                                       incl_geoids=NULL,
                                       name_filter="med",
                                       partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"))
{
  if(is.null(scenario_labels)){
    warning("You have not specified scenario labels for this function. You may encounter future errors.")  
  }
  warning("This function loads infection data from hospitalization outputs. Only one IFR scenario is needed to load these data for a given set of model outputs because infection counts will be the same across IFR scenarios.")
  
  display_dates <- as.Date(display_dates)
  max_date <- max(display_dates)
  
  hosp_pre_process <- function(x) {
    x %>%
      select(geoid, scenario, death_rate, location, time, sim_id, incidI)
  }
  ##filter to munge the data at the scenario level
  if (!is.null(incl_geoids)) {
    hosp_post_process <- function(x) {
      x %>%
        mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time) & geoid %in% incl_geoids, time <= max_date) %>%
        group_by(geoid, sim_num, scenario) %>%
        dplyr::mutate(N = cumsum(incidI)) %>%
        ungroup() %>%
        dplyr::filter(time %in% display_dates)
    }
  } else {
    hosp_post_process <- function(x) {
      x %>%
        mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time) & time <= max_date) %>%
        group_by(geoid, sim_num, scenario) %>%
        dplyr::mutate(N = cumsum(incidI)) %>%
        ungroup() %>%
        dplyr::filter(time %in% display_dates)
    }
  }
  
  rc<-load_hosp_sims_filtered(outcome_dir=outcome_dir,
                              name_filter=name_filter,
                              pre_process=hosp_pre_process,
                              post_process=hosp_post_process)
  
  rc<-rc%>%
    mutate(scenario_name = factor(scenario, levels=scenario_levels, labels=scenario_labels), 
           scenario_num = seq_along(scenario)) %>%
    select(-sim_id)
  
  return(rc)
  
}



##' Convenience function to load cumulative geounit hosp outcomes at a specific date for the given scenario
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param scenario_labels config$report$formatting$scenario_labels character vector of scenario labels
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
load_cum_hosp_geounit_date <- function(outcome_dir,
                                       display_dates=config$report$formatting$display_dates,
                                       scenario_levels=NULL,
                                       scenario_labels=NULL,
                                       incl_geoids=NULL,
                                       name_filter="med",
                                       partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"))
{
  if(is.null(scenario_labels)){
    warning("You have not specified scenario labels for this function. You may encounter future errors.")  
  }
  warning("This function loads infection data from hospitalization outputs. Only one IFR scenario is needed to load these data for a given set of model outputs because infection counts will be the same across IFR scenarios.")
  
  display_dates <- as.Date(display_dates)
  max_date <- max(display_dates)
  
  hosp_pre_process <- function(x) {
    x %>%
      select(geoid, scenario, location, death_rate, time, sim_id, incidI, incidD, incidICU, incidH, incidVent) 
  }
  ##filter to munge the data at the scenario level
  if (!is.null(incl_geoids)) {
    hosp_post_process <- function(x) {
      x %>%
        mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time) & geoid %in% incl_geoids, time <= max_date) %>%
        group_by(geoid, sim_num, scenario, location, pdeath) %>%
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
        mutate(time=as.Date(time))%>%
        dplyr::filter(!is.na(time) & time <= max_date) %>%
        group_by(geoid, sim_num, scenario, location, pdeath) %>%
        dplyr::summarize(NincidDeath = sum(incidD),
                         NincidInf = sum(incidI),
                         NincidICU=sum(incidICU),
                         NincidHosp=sum(incidH),
                         NincidVent = sum(incidVent)) %>%
        ungroup() 
    }
  }
  
  rc<-load_hosp_sims_filtered(outcome_dir=outcome_dir,
                              name_filter=name_filter,
                              pre_process=hosp_pre_process,
                              post_process=hosp_post_process)
  
  rc<-rc%>%
    mutate(scenario_name = factor(scenario, levels=scenario_levels, labels=scenario_labels), 
           scenario_num = seq_along(scenario))
  
  return(rc)
  
}


##'
##' Convenience function to allow us to load hospital totals for the combined geounits quickly for
##' the given scenarios.
##' 
##' @param outcome_dir the subdirectory with all model outputs
##' @param partitions used by open_dataset 
##' @param name_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collection
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_hosp_geocombined_totals <- function(outcome_dir,
                                         partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"),
                                         name_filter=c("high", "med", "low"),
                                         pre_process=function(x) {x},
                                         post_process=NULL,
                                         ...
) {
  
  require(tidyverse)
  
  hosp_post_process <- function(x) {
    x %>%
      group_by(geoid, pdeath, scenario, sim_num, location) %>%
      mutate(cum_hosp=cumsum(incidH)) %>%
      mutate(cum_death=cumsum(incidD)) %>%
      mutate(cum_case=cumsum(incidC)) %>%
      mutate(cum_inf=cumsum(incidI)) %>%
      group_by(pdeath, scenario, time, sim_num) %>%
      summarize(NhospCurr=sum(hosp_curr),
                NICUCurr=sum(icu_curr),
                NincidDeath=sum(incidD),
                NincidInf=sum(incidI),
                NincidCase=sum(incidC),
                NincidICU=sum(incidICU),
                NincidHosp=sum(incidH),
                NincidVent=sum(incidVent),
                NVentCurr=sum(vent_curr),
                cum_hosp=sum(cum_hosp),
                cum_death=sum(cum_death),
                cum_case=sum(cum_case),
                cum_inf=sum(cum_inf)) 
  }
  
  rc<- load_hosp_sims_filtered(outcome_dir=outcome_dir, 
                               partitions=partitions,
                               name_filter=name_filter,
                               pre_process=pre_process,
                               post_process=hosp_post_process)
  
  warning("Finished loading")
  return(rc)
  
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
  num_files = NA,
  incl_geoids = NULL,
  scenario_labels = NULL,
  geoid_len = 0,
  padding_char = "0",
  file_extension = 'auto' 
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
                                           padding_char = padding_char,
                                           file_extension = file_extension)
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
                    NincidVent = incidVent,
                    NVentCurr = vent_curr) %>%
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
  require(tigris)
  if(!file.exists(filename)){stop(paste(filename,"does not exist in",getwd()))}
  geodata <- readr::read_csv(filename)

  if(to_lower){
    names(geodata) <- tolower(names(geodata))
  }
  if(!('geoid' %in% names(geodata))){stop(paste(filename,"does not have a column named geoid"))}

  if(geoid_len > 0){
    geodata$geoid <- stringr::str_pad(geodata$geoid,geoid_len, pad = geoid_pad)
  }
  geodata<-geodata %>%
    left_join(fips_codes%>%
                unite(col="geoid", ends_with("_code"), sep="") %>%
                select(-state_name, -state) %>%
                rename(name=county) %>%
                mutate(name=str_remove(name, " County")))
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
  
  us_data_only = (countries == c("US"))
  if(us_data_only) {
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

  if(us_data_only)
  {
    jhu_dat <- jhu_dat %>% dplyr::mutate(Province_State=state_abb)
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

##' Load USAFacts data
##'
##' @param data_dir data directory to download raw USAFacts data to
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
load_USAFacts_for_report <- function(data_dir = "data/case_data",
                                     states) {

  require(magrittr)

  usaf_dat <- covidcommon::get_USAFacts_data(case_data_filename = file.path(data_dir,"USAFacts_case_data.csv"),
                                              death_data_filename = file.path(data_dir, "USAFacts_death_data.csv"))
  usaf_dat <- 
    usaf_dat %>%
    dplyr::mutate(date = as.Date(Update)) %>%
    dplyr::filter(source %in% states) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(NcumulConfirmed = sum(Confirmed, na.rm = TRUE), NcumulDeathsObs = sum(Deaths, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(NincidConfirmed  = NcumulConfirmed - dplyr::lag(NcumulConfirmed),
                  NincidDeathsObs = NcumulDeathsObs - dplyr::lag(NcumulDeathsObs)) %>%
    na.omit()
  return(usaf_dat)
}


##' Convenience function to display log proportion  
##'
##' @param county_dat df with disaggregated hosp outcomes
##' @param threshold A named numeric vector where the names are geoids and values are thresholds
##' @param variable character string of variable to which to compare threshold
##' @param end_date simulation end date character string
##' @param name_filter pdeath character string one of: high, med, low
##' @param incl_geoids optional character vector of geoids that are included in the report, if not included, all geoids will be used
##' @param scenario_levels config$report$formatting$scenario_labels_short character vector of scenario levels
##' @param scenario_labels config$report$formatting$scenario_labels character vector of scenario labels
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
load_hosp_geounit_relative_to_threshold <- function(county_dat,
                                                    threshold,
                                                    variable,
                                                    scenario_levels,
                                                    scenario_labels,
                                                    end_date = config$end_date,
                                                    incl_geoids=NULL,
                                                    name_filter
                                                    ){

  if(sum(names(threshold) == "") > 1){stop("You provided more than one catch all threshold")}
    catch_all_threshold <- Inf
  if(sum(names(threshold) == "") > 0){
    catch_all_threshold <- threshold[names(threshold) == ""]
  }
  if(is.null(scenario_labels)){
    warning("You have not specified scenario labels for this function. You may encounter future errors.")  
  }

  end_date <- lubridate::as_date(end_date)
  
  if(is.null(incl_geoids)){incl_geoids<-unique(county_dat$geoid)}
  
  county_dat %>% 
    dplyr::filter(time<=end_date) %>%
    dplyr::filter(pdeath==name_filter) %>%
    dplyr::mutate(scenario_label=factor(scenario, levels= scenario_levels, labels=scenario_labels)) %>%
    group_by(scenario_label, geoid, time, name) %>%
    summarize(NhospCurr=round(mean(NhospCurr)),
              NICUCurr=round(mean(NICUCurr)),
              NVentCurr=round(mean(NVentCurr)))%>%
    ungroup %>%
    left_join(data.frame(geoid = names(threshold), threshold_value = threshold), by = c("geoid")) %>%
    dplyr::rename(pltVar = !!variable) %>%
    dplyr::mutate(prop_needed = pltVar/threshold_value) %>%
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
##' @param partitions used by open_dataset 
##' @param name_filter string that indicates which pdeath to import from outcome_dir
##' @param pre_process function that does processing before collectio
##' 
##' @return a combined data frame of all R simulations with filters applied pre merge.
##' 
##'
##'
##'@export
load_r_sims_filtered <- function(outcome_dir,
                                 partitions=c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id"),
                                 name_filter=c("high", "med", "low"),
                                 pre_process=function(x) {x},
                                 geo_dat=geodata,
                                 ...
) {
  
  require(tidyverse)
  
  spar <- load_spar_sims_filtered(outcome_dir=outcome_dir, 
                                  pre_process=pre_process, 
                                  name_filter=name_filter, 
                                  partitions=partitions,
                                  ...)
  
  snpi<- load_snpi_sims_filtered(outcome_dir=outcome_dir, 
                                 pre_process=pre_process, 
                                 name_filter=name_filter, 
                                 partitions=partitions,
                                 ...)
  
  rc <- spar %>%
    right_join(snpi)%>%
    filter(npi_name=="local_variance") %>%
    mutate(local_r = location_r*(1-reduction)) %>% # county_r0 ought to be renamed to "geogroup_r0"
    select(geoid, sim_num, local_r, scenario) %>%
    left_join(snpi) %>%
    mutate(r = if_else(npi_name=="local_variance",
                       local_r,
                       local_r*(1-reduction))) %>%
    left_join(geo_dat)
  
  warning("Finished loading")
  return(rc)
  
}