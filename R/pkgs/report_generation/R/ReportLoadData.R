
##' Load cumulative county infections at specific dates
##'
##' @param scn_dirs paste(config$name, config$interventions$scenarios, sep = "_") character vector of scenario directory names
##' @param config_display_dates config$report$formatting$display_dates character vector of dates to for which cumulative infections should be extracted
##' @param config_scenariolabels config$report$formatting$scenario_labels character vector of scenario labels
##' @param included_geoids character vector of geoids that are included in the report
##'
##' @return a data frame with columns
##'          [TODO: add columns]
##'
##' @export
load_inf_geounit_cum_dates <- function(scn_dirs,
                                      config_display_dates,
                                      config_scenariolabels,
                                      included_geoids=NULL){

  display_dates <- as.Date(config_display_dates)
  inf_pre_process <- function(x) {
    x %>%
      dplyr::filter(comp == "cumI") %>%
      dplyr::filter(time %in% display_dates)
  }

  if (!is.null(included_geoids)) {
      inf_post_process <- function(x) {
          x %>%
              ungroup %>%
              dplyr::mutate(geoid=ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>%
              dplyr::filter(!is.na(time), geoid %in% included_geoids)
      }
  } else {
      inf_post_process <- function(x) {
          x %>%
              ungroup %>%
              dplyr::mutate(geoid=ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>%
              dplyr::filter(!is.na(time))
      }
  }

  inf_county_cum_dates<- list()

  for (i in 1:length(scn_dirs)) {
      inf_county_cum_dates[[i]] <- load_scenario_sims_filtered(scn_dirs[i],
                                                               pre_process = inf_pre_process,
                                                               post_process = inf_post_process) %>%
          mutate(scenario_num=i,
                 scenario_name=config_scenariolabels[i]) %>%
          ungroup

  }

  return(dplyr::bind_rows(inf_county_cum_dates))

}




##'
##' Convienence function to allow us to load hospital totals for the state quickly for
##' the given scenarios.
##'
##' @param scn_dirs the dirctories containing the relevant scenarios
##' @param scenario_labels the scenarios labels
##' @param name_filter filename filter
##' @param fil
##'
##' @return a data frome with columns:
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
                                   included_geoids = NULL) {

    ##filter to munge the data at the senario level
    if (!is.null(included_geoids)) {
         hosp_post_process <- function(x) {
            x %>%
                dplyr::filter(!is.na(time), geoid %in% included_geoids) %>%
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
