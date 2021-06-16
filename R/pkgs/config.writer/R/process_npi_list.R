##' Convenience function to load geodata.csv
##'
##' @param filename geodata.csv filename
##' @param geoid_len length of geoid character string
##' @param geoid_pad what to pad the geoid character string with
##' @param to_lower whether to make all column names lowercase
##' 
##' @return a data frame with columns for state USPS and county geoid and population
##' 
##' @export

load_geodata_file <- function(filename,
                              geoid_len = 0,
                              geoid_pad = "0",
                              to_lower = FALSE
) {
    
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
    
    return(geodata)
}

# ScenarioHub: Recode scenario hub interventions for "ReduceR0" template
#'
#' @param data 
#'
#' @return
#'
#' @examples
npi_recode_scenario <- function(data
                                ){
    
    data %>%
        dplyr::mutate(scenario = action,
                      scenario = dplyr::recode(scenario, 
                                               "stay at home" = "lockdown",
                                               "stay at home_b" = "lockdown2",
                                               "social distancing" = "sd"),
                      scenario = gsub("reopen_phase", "open_p", scenario),
                      scenario = gsub("reclosure", "close", scenario),
                      scenario = gsub(" ", "", scenario))
    
}

#  ScenarioHub: Recode scenario hub interventions for "MultiTimeReduce" template
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
npi_recode_scenario_mult <- function(data
                                     ){
    data %>%
        dplyr::mutate(scenario_mult = action_new, 
                      scenario_mult = ifelse(grepl("stay at home", scenario_mult), "lockdown", scenario_mult),
                      scenario_mult = gsub("paste", "open_p", scenario_mult),
                      scenario_mult = gsub("phase", "open_p", scenario_mult),
                      scenario_mult = ifelse(grepl("open_p", scenario_mult), stringr::str_extract(scenario_mult, "open_p[[:digit:]]"), scenario_mult),
                      scenario_mult = dplyr::recode(scenario_mult, 
                                                    "social distancing" = "sd"))
}

# ScenarioHub: Process scenario hub npi list 
#' Title
#'
#' @param intervention_path path to csv with interventions 
#' @param geodata df with state USPS and geoid from load_geodata_file()
#'
#' @return df with six columns:
#'         - USPS: state abbreviation
#'         - geoid: county ID
#'         - start_date: intervention start date
#'         - end_date: intervention end date
#'         - name: intervention name
#'         - template: intervention template (e.g. ReduceR0, MultiTimeReduce)
#' @export
#'
#' @examples
#' 
#' geodata <- load_geodata_file(filename = "data/geodata_territories_2019_statelevel.csv")
#' npi_dat <- process_npi_shub(intervention_path = "data/intervention_tracking/Shelter-in-place-as-of-04302021.csv", geodata)
#' 
#' npi_dat
process_npi_shub <- function(intervention_path, 
                             geodata
){
    state_cw <- tigris::fips_codes %>%
        dplyr::distinct(state, state_name) %>%
        dplyr::rename(USPS = state) %>%
        dplyr::rename(state = state_name) %>%
        dplyr::mutate(state = dplyr::recode(state, "U.S. Virgin Islands" = "Virgin Islands"))
    
    ## read intervention estimates
    og <- readr::read_csv(intervention_path) %>%
        dplyr::left_join(state_cw, by = c("state"))%>%
        dplyr::left_join(geodata) %>% 
        dplyr::filter(GEOID == "all") %>%
        npi_recode_scenario() %>% # recode action variable into scenario
        npi_recode_scenario_mult() %>%# recode action_new variable into scenario_mult 
        dplyr::mutate(dplyr::across(tidyselect::ends_with("_date"), ~ lubridate::mdy(.x)))
    
    if("template" %in% colnames(og)){
        og <- og %>%
            dplyr::mutate(name = dplyr::if_else(template=="MultiTimeReduce", scenario_mult, scenario)) %>% 
            dplyr::select(USPS, geoid, start_date, end_date, name, template)
    } else{
        og <- og %>%
            dplyr::mutate(template = "MultiTimeReduce") %>%
            dplyr::select(USPS, geoid, start_date, end_date, name=scenario_mult, template)
    }
    
    return(og)
    
}

