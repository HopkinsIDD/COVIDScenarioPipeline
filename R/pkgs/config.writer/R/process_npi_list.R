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

#' ScenarioHub: Recode scenario hub interventions for "ReduceR0" template
#'
#' @param data intervention list for the national forecast or the scenariohub
#'
#' @return recoded npi names
#'
#' @export
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

#'  ScenarioHub: Recode scenario hub interventions for "MultiTimeReduce" template
#'
#' @param data intervention list for the national forecast or the scenariohub
#'
#' @return recoded npi names for use with MultiTimeReduce
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

#' ScenarioHub: Process scenario hub npi list
#'
#' @param intervention_path path to csv with intervention list
#' @param geodata df with state USPS and geoid from load_geodata_file
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

#' Function to process variant data for B117
#'
#' @param variant_path path to variant
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param month_shift shift in variant growth to earlier if <0 or later >0; defaults to 0
#' @param variant_lb
#' @param varian_effect change in transmission for variant default is 50% from Davies et al 2021

#'
#'
#'
#' @return
#' @export
#'
#' @examples
#'
generate_variant_b117 <- function(variant_path,
                                  sim_start_date=as.Date("2020-03-31"),
                                  sim_end_date=Sys.Date()+60,
                                  month_shift=0,
                                  variant_lb = 1.4,
                                  variant_effect = 1.5
){
    if(is.null(month_shift)){
        month_shift <- 0
    }

    sim_start_date <- as.Date(sim_start_date)
    sim_end_date <- as.Date(sim_end_date)

    strains <- readr::read_csv(variant_path) %>% # TODO: separate functions (options in function @params) to generate variant data
        dplyr::mutate(log_prct_b = log(prct_b), log_prct_b_1m = - 0 - log_prct_b)

    strains <- strains %>%
        dplyr::mutate(logit_prct_b = log(prct_b/(1-prct_b))) %>%
        dplyr::select(-month_b) %>%
        dplyr::mutate(month_a = month_a - month_shift) # shift earlier 0 week


    mL <- drc::drm(prct_b ~ month_a, data = strains, fct = drc::L.3(), type = "continuous")

    # Get impact of strain proportions on R0
    pred_month <- predict(mL, data.frame(month_a=seq(1,12,1)))
    pred_month[pred_month>1] <- 1

    prop_strain_b <- dplyr::tibble(month = 1:12, prop_b = pred_month) %>%
        dplyr::mutate(R_ratio = 1*(1-prop_b) + variant_effect*(prop_b))

    pred_week <- predict(mL, data.frame(month_a=seq(1,13,.23)))
    pred_week[pred_week>1] <- 1

    prop_strain_b_week <- dplyr::tibble(prop_b = pred_week) %>%
        dplyr::mutate(week = 1:nrow(.),
                      R_ratio = 1*(1-prop_b) + variant_effect*prop_b,
                      value_sd = 1*(1-prop_b) + variant_lb*prop_b,
                      value_sd = (R_ratio - value_sd)/1.96)


    prop_strain_b_week <- prop_strain_b_week %>%
        dplyr::mutate(start_date = MMWRweek::MMWRweek2Date(MMWRyear=rep(2021, nrow(.)), MMWRweek=week),
                      end_date = (start_date+6)) %>%
        #mutate(month_abb = tolower(month.abb[lubridate::month(start_date)])) %>%
        dplyr::filter(!(start_date>sim_end_date)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(end_date = min(end_date, sim_end_date)) %>%
        dplyr::ungroup()

    max_week <- min(prop_strain_b_week %>% dplyr::filter(prop_b==1) %>% dplyr::pull(week))
    variant_data <- prop_strain_b_week %>%
        dplyr::filter(week<=max_week) %>%
        dplyr::mutate(end_date = lubridate::as_date(ifelse(week==max_week, sim_end_date, end_date))) %>%
        dplyr::filter(end_date > sim_start_date) %>%
        dplyr::mutate(start_date = dplyr::if_else(end_date > sim_start_date & start_date < sim_start_date,
                                                  sim_start_date, start_date))

    return(variant_data)

}

#' Function to process variant data for B117/B1617
#'
#' Generate variant interventions
#'
#' @param variant_path_1 path to B117 variant
#' @param variant_path_2 path to B1617 variant
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param variant_lb
#' @param varian_effect change in transmission for variant default is 50% from Davies et al 2021
#' @param transmission_increase transmission increase in B1617 relative to B117
#'
#'
#' @return
#' @export
#'
#' @examples
#'
generate_multiple_variants <- function(variant_path_1,
                                       variant_path_2,
                                       sim_start_date=as.Date("2020-03-31"),
                                       sim_end_date=Sys.Date()+60,
                                       variant_lb = 1.4,
                                       variant_effect = 1.5,
                                       transmission_increase = 0.2
                                        ){

    if(is.null(transmission_increase)){
        transmission_increase=0.2
    }

    variant_1 <- readr::read_csv(variant_path_1)
    variant_2 <- readr::read_csv(variant_path_2)

    sim_start_date <- as.Date(sim_start_date)
    sim_end_date <- as.Date(sim_end_date)

    b117_week <- b117 %>%
        dplyr::mutate(week = MMWRweek::MMWRweek(date)$MMWRweek,
                      year = MMWRweek::MMWRweek(date)$MMWRyear,
                      start_date = MMWRweek::MMWRweek2Date(MMWRyear=year, MMWRweek=week),
                      end_date = (start_date+6)) %>%
        dplyr::rename(variant_prop = fit) %>%
        dplyr::mutate(variant = "B117") %>%
        dplyr::filter(!(start_date>sim_end_date)) %>%
        dplyr::filter(date==end_date) %>%
        dplyr::mutate(date=start_date) %>%
        dplyr::mutate(param = "ReduceR0") %>%
        dplyr::mutate(R_ratio = 1*(1-variant_prop) + variant_effect*variant_prop,
                      sd_variant = 1*(1-variant_prop) + variant_lb*variant_prop,
                      sd_variant = (R_ratio - sd_variant)/1.96)

    # B.1.617
    b1617_week <- b1617 %>%
        dplyr::mutate(date = lubridate::mdy(date)) %>%
        dplyr::mutate(week = MMWRweek::MMWRweek(date)$MMWRweek,
                      year = MMWRweek::MMWRweek(date)$MMWRyear,
                      start_date = MMWRweek::MMWRweek2Date(MMWRyear=year, MMWRweek=week),
                      end_date = (start_date+6)) %>%
        dplyr::filter(!(start_date>sim_end_date)) %>%
        dplyr::filter(date==start_date) %>%
        dplyr::mutate(param = "ReduceR0") %>%
        dplyr::mutate(R_ratio = variant_b117*(1-variant_prop) + variant_effect*(1+trans_inc)*variant_prop,
                      sd_variant = variant_b117_lb*(1-variant_prop) + variant_lb*(1+trans_inc)*variant_prop,
                      sd_variant = (R_ratio - sd_variant)/1.96) %>%
        dplyr::mutate(variant = "B1617")


    b117_week_ <- b117_week %>%
        dplyr::filter(date < min(b1617_week$date))

    variant_data <- b1617_week %>%
        dplyr::filter(trans_inc==transmission_increase) %>%
        dplyr::bind_rows(b117_week_) %>%
        dplyr::filter(end_date >= sim_start_date) %>%
        dplyr::mutate(start_date = dplyr::if_else(start_date < sim_start_date &
                                                      end_date > sim_start_date, sim_start_date, start_date),
                      end_date = dplyr::if_else(end_date > sim_end_date, sim_end_date, end_date))

    variant_data <- variant_data %>%
        dplyr::mutate(R_ratio = round(R_ratio, 2)) %>%
        dplyr::select(week, start_date, end_date, variant, param, R_ratio, sd_variant) %>%
        dplyr::group_by(R_ratio) %>%
        dplyr::summarise(start_date = min(start_date),
                         end_date = max(end_date),
                         sd_variant = mean(sd_variant),
                         week = ifelse(length(week)==1, as.character(week), paste0(min(week), "-", max(week)))) %>%
        dplyr::filter(R_ratio>1)
}



