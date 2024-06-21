# Process ------

#' Specify parameters for NPIs
#'
#' @param intervention_file df with the location's state and ID and the intervention start and end dates, name, and template - from process_npi_shub
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param npi_cutoff_date only interventions that start before or on npi_cuttof_date are included
#' @param redux_geoids string or vector of characters indicating which geoids will have an intervention with the ReduceIntervention template; it accepts "all". If any values are specified, the intervention in the geoid with the maximum start date will be selected. It defaults to NULL. .
#' @param v_dist type of distribution for reduction
#' @param v_mean reduction mean
#' @param v_sd reduction sd
#' @param v_a reduction a
#' @param v_b reduction b
#' @param inference logical indicating whether inference will be performed on intervention (default is TRUE); perturbation values are replaced with NA if set to FALSE.
#' @param p_dist type of distribution for perturbation
#' @param p_mean perturbation mean
#' @param p_sd perturbation sd
#' @param p_a perturbation a
#' @param p_b perturbation b
#'
#' @return
#'
#' @export
#'
#' @examples
#' geodata <- load_geodata_file(filename = "data/geodata_territories_2019_statelevel.csv")
#' npi_dat <- process_npi_shub(intervention_path = "data/intervention_tracking/Shelter-in-place-as-of-04302021.csv", geodata)
#'
#' npi_dat <- set_npi_params(intervention_file = npi_dat, sim_start_date = "2020-01-15", sim_end_date = "2021-07-30")
#'
set_npi_params <- function(intervention_file,
                           sim_start_date=as.Date("2020-01-31"),
                           sim_end_date=Sys.Date()+60,
                           npi_cutoff_date=Sys.Date()-7,
                           inference = TRUE,
                           redux_geoids = NULL,
                           v_dist = "truncnorm", v_mean=0.6, v_sd=0.05, v_a=0.0, v_b=0.9,
                           p_dist = "truncnorm", p_mean=0, p_sd=0.05, p_a=-1, p_b=1
){


    sim_start_date <- lubridate::ymd(sim_start_date)
    sim_end_date <- lubridate::ymd(sim_end_date)
    npi_cuttoff_date <- lubridate::ymd(npi_cutoff_date)

    npi <- intervention_file %>%
        dplyr::filter(start_date <= npi_cutoff_date) %>%
        dplyr::filter(start_date >= sim_start_date | end_date > sim_start_date) %>% # add warning about npi period <7 days?
        dplyr::group_by(USPS, geoid) %>%
        dplyr::mutate(end_date = dplyr::case_when(is.na(end_date) | end_date == max(end_date) |
                                                      end_date > sim_end_date ~ sim_end_date,
                                                  TRUE ~ end_date),
                      value_dist = v_dist,
                      value_mean = v_mean,
                      value_sd = v_sd,
                      value_a = v_a,
                      value_b = v_b,
                      pert_dist = p_dist,
                      pert_mean = p_mean,
                      pert_sd = p_sd,
                      pert_a = p_a,
                      pert_b = p_b,
                      type = "transmission",
                      category = "NPI",
                      baseline_scenario = "",
                      parameter = dplyr::if_else(template=="MultiTimeReduce", "R0", NA_character_)
        )

    if(any(stringr::str_detect(npi$name, "^\\d$"))) stop("Intervention names must include at least one non-numeric character.")

    npi <- npi %>%
        dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(inference, .x, NA_real_)),
                      pert_dist = ifelse(inference, pert_dist, NA_character_)) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

    if(!is.null(redux_geoids)){
        if(redux_geoids == 'all'){
            redux_geoids <- unique(npi$geoid)
        }

        npi <- npi %>%
            dplyr::filter(geoid %in% redux_geoids) %>%
            dplyr::group_by(geoid) %>%
            dplyr::filter(start_date == max(start_date)) %>%
            dplyr::mutate(category = "base_npi",
                          name = paste0(name, "_last")) %>%
            dplyr::bind_rows(
                npi %>%
                    dplyr::group_by(geoid) %>%
                    dplyr::filter(start_date != max(start_date) |! geoid %in% redux_geoids)
            ) %>%
            dplyr::ungroup()
    }

    npi <- npi %>%
        dplyr::ungroup() %>%
        dplyr::add_count(name) %>%
        dplyr::mutate(template = dplyr::if_else(n==1 & template == "MultiTimeReduce", "Reduce", template),
                      parameter = dplyr::if_else(n==1 & template == "Reduce", "R0", parameter)) %>%
        dplyr::select(-n)

    return(npi)

}



#' Generate seasonality file with params
#'
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param v_dist type of distribution for reduction
#' @param v_mean reduction mean
#' @param v_sd reduction sd
#' @param v_a reduction a
#' @param v_b reduction b
#' @param inference logical indicating whether inference will be performed on intervention (default is TRUE); perturbation values are replaced with NA if set to FALSE.
#' @param p_dist type of distribution for perturbation
#' @param p_mean perturbation mean
#' @param p_sd perturbation sd
#' @param p_a perturbation a
#' @param p_b perturbation b
#'
#' @return data frame with columns seasonal terms and set parameters.
#' @export
#'
#' @examples
#' dat <- set_seasonality_params()
#'
#' dat
#'

set_seasonality_params <- function(sim_start_date=as.Date("2020-03-31"),
                                   sim_end_date=Sys.Date()+60,
                                   inference = TRUE,
                                   template = "MultiTimeReduce",
                                   v_dist="truncnorm",
                                   v_mean = c(-0.2, -0.133, -0.067, 0, 0.067, 0.133, 0.2, 0.133, 0.067, 0, -0.067, -0.133), # TODO function?
                                   v_sd = 0.05, v_a = -1, v_b = 1,
                                   p_dist="truncnorm",
                                   p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1){

    sim_start_date <- as.Date(sim_start_date)
    sim_end_date <- as.Date(sim_end_date)

    years_ <- unique(lubridate::year(seq(sim_start_date, sim_end_date, 1)))

    seas <- tidyr::expand_grid(
        tidyr::tibble(month= tolower(month.abb),
                      month_num = 1:12,
                      value_dist = v_dist,
                      value_mean = v_mean,
                      value_sd = v_sd,
                      value_a = v_a,
                      value_b= v_b,
                      pert_dist = p_dist,
                      pert_mean = p_mean,
                      pert_sd = p_sd,
                      pert_a = p_a,
                      pert_b = p_b)) %>%
        tidyr::expand_grid(year = years_) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(start_date = lubridate::ymd(paste0('"', year, '-', month_num, '-01"')),
                      end_date = lubridate::ceiling_date(start_date, "months")-1,
                      end_date = dplyr::if_else(end_date > sim_end_date, sim_end_date, end_date),
                      USPS = "",
                      type = "transmission",
                      parameter = "R0",
                      category = "seasonal",
                      template = template,
                      baseline_scenario = "",
                      geoid = "all",
                      name = paste0("Seas_", month),
                      pert_dist = ifelse(inference, as.character(pert_dist), NA_character_),
                      dplyr::across(pert_sd:pert_a, ~ifelse(inference, as.numeric(.x), NA_real_))
        ) %>%
        dplyr::filter(start_date <= end_date) %>%
        dplyr::filter(lubridate::ceiling_date(start_date, "months") >= lubridate::ceiling_date(sim_start_date, "months") &
                          lubridate::ceiling_date(end_date, "months") <= lubridate::ceiling_date(sim_end_date, "months")
        ) %>%
        dplyr::add_count(name) %>%
        dplyr::mutate(template = dplyr::if_else(n > 1, template, "ReduceR0"),
                      end_date = dplyr::if_else(end_date > sim_end_date, sim_end_date, end_date),
                      start_date = dplyr::if_else(start_date < sim_start_date, sim_start_date, start_date)
        ) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

    return(seas)
}

#' Generate local variance
#'
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param v_dist type of distribution for reduction
#' @param v_mean reduction mean
#' @param v_sd reduction sd
#' @param v_a reduction a
#' @param v_b reduction b
#' @param inference logical indicating whether inference will be performed on intervention (default is TRUE); perturbation values are replaced with NA if set to FALSE.
#' @param p_dist type of distribution for perturbation
#' @param p_mean perturbation mean
#' @param p_sd perturbation sd
#' @param p_a perturbation a
#' @param p_b perturbation b
#'
#' @return data frame with columns for
#' @export
#'
#' @examples
#' dat <- set_localvar_params()
#'
#' dat
#'
set_localvar_params <- function(sim_start_date=as.Date("2020-03-31"),
                                sim_end_date=Sys.Date()+60,
                                inference = TRUE,
                                v_dist="truncnorm",
                                v_mean =  0, v_sd = 0.05, v_a = -1, v_b = 1, # TODO: add check on limits
                                p_dist="truncnorm",
                                p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1
){
    sim_start_date <- as.Date(sim_start_date)
    sim_end_date <- as.Date(sim_end_date)

    template = "ReduceR0"
    param = NA_character_
    affected_geoids = "all"

    local_var <- dplyr::tibble(USPS = "",
                                geoid = "all",
                                name = "local_variance",
                                type = "transmission",
                                category = "local_variance",
                                parameter = NA_character_,
                                baseline_scenario = "",
                                start_date = sim_start_date,
                                end_date = sim_end_date,
                                template = template,
                                param = param,
                                affected_geoids = affected_geoids,
                                value_dist = v_dist,
                                value_mean = v_mean,
                                value_sd = v_sd,
                                value_a = v_a,
                                value_b= v_b,
                                pert_dist = p_dist,
                                pert_mean = p_mean,
                                pert_sd = p_sd,
                                pert_a = p_a,
                                pert_b = p_b) %>%
        dplyr::mutate(pert_dist = ifelse(inference, as.character(pert_dist), NA_character_),
                      dplyr::across(pert_mean:pert_b, ~ifelse(inference, as.numeric(.x), NA_real_))) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

    return(local_var)
}

#' Generate NPI reduction interventions
#'
#' @param npi_file output from set_npi_params
#' @param incl_geoid vector of geoids to include; NULL will generate interventions for all geographies
#' @param projection_start_date first date without data to fit
#' @param redux_end_date end date for reduction interventions; default NULL uses sim_end_date in npi_file
#' @param redux_level reduction to intervention effectiveness; used to estimate mean value of reduction by month
#' @param v_dist type of distribution for reduction
#' @param v_sd reduction sd
#' @param v_a reduction a
#' @param v_b reduction b
#'
#' @return
#' @export
#'
#' @examples
#'
#'
set_redux_params <- function(npi_file,
                             projection_start_date = Sys.Date(), # baseline npi should have at least 2-3 weeks worth of data
                             redux_end_date=NULL,
                             redux_level = 0.5,
                             v_dist = "truncnorm", # TODO: change to "fixed" and add correct value, remove v_sd-v_b
                             v_mean=0.6,
                             v_sd=0.01,
                             v_a=0,
                             v_b=1
){

    projection_start_date <- as.Date(projection_start_date)

    if(!is.null(redux_end_date)){
        redux_end_date <- as.Date(redux_end_date)

        if(redux_end_date > max(npi_file$end_date)) stop("The end date for reduction interventions should be less than or equal to the sim_end_date in the npi_file.")

    }

    og <- npi_file %>%
        dplyr::filter(category == "base_npi") %>%
        dplyr::group_by(USPS, geoid) %>%
        dplyr::mutate(end_date = dplyr::if_else(is.null(redux_end_date), end_date, redux_end_date))

    if(any(projection_start_date < unique(og$start_date))){warning("Some interventions start after the projection_start_date")}

    months_start <- seq(lubridate::floor_date(projection_start_date, "month"), max(og$end_date), by="month")
    months_start[1] <- projection_start_date

    months_end <- lubridate::ceiling_date(months_start, "months")-1
    months_end[length(months_end)] <- max(og$end_date)

    month_n <- length(months_start)

    reduction <- rep(redux_level/month_n, month_n) %>% cumsum()

    redux <- dplyr::tibble(
        start_date = months_start,
        end_date = months_end,
        month = lubridate::month(months_start, label=TRUE, abbr=TRUE) %>% tolower(),
        value_mean = reduction, # TODO: reduction to value_mean
        type = rep("transmission", month_n),
        geoid = og$geoid %>% paste0(collapse = '", "')) %>%
        mutate(USPS = "",
               category = "NPI_redux",
               name = paste0(category, '_', month),
               baseline_scenario = c("base_npi", paste0("NPI_redux_", month[-length(month)])),
               template = "ReduceIntervention",
               parameter = "R0",
               value_dist = v_dist,
               value_sd = v_sd,
               value_a = v_a,
               value_b = v_b,
               pert_dist = NA_character_,
               pert_mean = NA_real_,
               pert_sd = NA_real_,
               pert_a = NA_real_,
               pert_b = NA_real_) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

    return(redux)
}

#' Generate vaccination rates intervention
#'
#' @param vacc_path path to vaccination rates
#' @param vacc_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param incl_geoid vector of geoids to include
#' @param scenario_num which baseline scenario will be selected from the vaccination rate file
#'
#'
#' @return
#' @export
#'
#' @examples
#'
set_vacc_rates_params <- function(vacc_path,
                                  vacc_start_date="2021-01-01",
                                  sim_end_date=Sys.Date()+60,
                                  # inference = TRUE,
                                  incl_geoid = NULL, # TODO: add scenario filter similar to set_vacc_outcome_params
                                  scenario_num = 1
){
    vacc_start_date <- as.Date(vacc_start_date)
    sim_end_date <- as.Date(sim_end_date)

    vacc <- readr::read_csv(vacc_path) %>%
        dplyr::filter(!is.na(month) & scenario == scenario_num)

    if(!is.null(incl_geoid)){
        vacc<-vacc %>%
            dplyr::filter(geoid %in% incl_geoid)
    }

    vacc <- vacc %>%
        dplyr::filter(start_date <= sim_end_date) %>%
        dplyr::mutate(end_date = lubridate::as_date(ifelse(end_date>sim_end_date, sim_end_date, end_date))) %>%
        dplyr::rename(value_mean = vacc_rate) %>%
        dplyr::mutate(geoid = as.character(geoid),
                      month = lubridate::month(start_date, label=TRUE),
                      type = "transmission",
                      category = "vaccination",
                      name = paste0("Dose1_",tolower(month),lubridate::year(start_date)),
                      template = "Reduce",
                      parameter = "transition_rate 0",
                      baseline_scenario = "",
                      value_dist = "fixed",
                      value_sd = NA_real_,
                      value_a = NA_real_,
                      value_b = NA_real_,
                      pert_dist = NA_character_,
                      pert_mean = NA_real_,
                      pert_sd = NA_real_,
                      pert_a = NA_real_,
                      pert_b = NA_real_) %>%
        # dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(inference, .x, NA_real_)),
        #               pert_dist = ifelse(inference, pert_dist, NA_character_)) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario,tidyselect::starts_with("value_"), tidyselect::starts_with("pert_")) %>%
        dplyr::filter(start_date >= vacc_start_date &
                          value_mean > 0)

    return(vacc)

}

#' Generate variant interventions
#'
#' @param b117_only whether to generate estimates for B117 variant only or both B117 and B1617
#' @param variant_path_1 path to B117 variant
#' @param variant_path_2 path to B1617 variant
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param inference_cutoff_date no inference is applied for interventions that start on or after this day
#' @param variant_lb
#' @param varian_effect change in transmission for variant default is 50% from Davies et al 2021
#' @param month_shift
#' @param geodata file with columns for state/county abbreviation (USPS) and admin code (geoid); only required if state_level is TRUE
#' @param state_level whether there is state-level data on the variant; requires a geodata file
#' @param transmission_increase transmission increase in B1617 relative to B117
#' @param inference logical indicating whether inference will be performed on intervention (default is TRUE); perturbation values are replaced with NA if set to FALSE.
#' @param v_dist type of distribution for reduction
#' @param v_mean reduction mean
#' @param v_sd reduction sd
#' @param v_a reduction a
#' @param v_b reduction b
#' @param p_dist type of distribution for perturbation
#' @param p_mean perturbation mean
#' @param p_sd perturbation sd
#' @param p_a perturbation a
#' @param p_b perturbation b
#'
#'
#' @return
#' @export
#'
#' @examples
#'
set_variant_params <- function(b117_only = FALSE,
                               variant_path,
                               variant_path_2 = NULL,
                               sim_start_date,
                               sim_end_date,
                               inference_cutoff_date=Sys.Date()-7,
                               variant_lb = 1.4,
                               variant_effect = 1.5,
                               month_shift = NULL,
                               state_level = TRUE,
                               geodata = NULL,
                               transmission_increase = NULL,
                               inference = TRUE,
                               v_dist="truncnorm",  v_sd = 0.01, v_a = -1.5, v_b = 0,
                               p_dist="truncnorm",
                               p_mean = 0, p_sd = 0.01, p_a = -1, p_b = 1
){
    inference_cutoff_date <- as.Date(inference_cutoff_date)


    if(b117_only){
        variant_data <- generate_variant_b117(variant_path = variant_path,
                                              sim_start_date = sim_start_date,
                                              sim_end_date = sim_end_date,
                                              variant_lb = variant_lb,
                                              variant_effect= variant_effect,
                                              month_shift = month_shift) %>%
            dplyr::mutate(geoid = "all",
                          USPS = "")
    } else if(state_level) {

        if(is.null(variant_path_2)){stop("You must specify a path for the second variant.")}
        if(is.null(geodata)){stop("You must specify a geodata file")}

        variant_data <- generate_multiple_variants_state(variant_path_1 = variant_path,
                                                         variant_path_2 = variant_path_2,
                                                         sim_start_date = sim_start_date,
                                                         sim_end_date = sim_end_date,
                                                         variant_lb = variant_lb,
                                                         variant_effect= variant_effect,
                                                         transmission_increase = transmission_increase,
                                                         geodata = geodata)
    } else{

        if(is.null(variant_path_2)){stop("You must specify a path for the second variant.")}

        variant_data <- generate_multiple_variants(variant_path_1 = variant_path,
                                                   variant_path_2 = variant_path_2,
                                                   sim_start_date = sim_start_date,
                                                   sim_end_date = sim_end_date,
                                                   variant_lb = variant_lb,
                                                   variant_effect= variant_effect,
                                                   transmission_increase = transmission_increase) %>%
            dplyr::mutate(geoid = "all",
                          USPS = "")
    }

    variant_data <- variant_data %>%
        dplyr::mutate(type = "transmission",
                      category = "variant",
                      name = paste0("variantR0adj_", paste0("Week", week)),
                      template = "ReduceR0",
                      parameter = NA,
                      value_dist = v_dist,
                      value_mean = 1-R_ratio,
                      value_sd = v_sd,
                      value_a = v_a,
                      value_b = v_b,
                      pert_dist = p_dist,
                      pert_mean = p_mean,
                      pert_sd = p_sd, # dont want much perturbation on this if it gets perturbed
                      pert_a = p_a,
                      pert_b = p_b,
                      baseline_scenario = "") %>%
        dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(inference & start_date < inference_cutoff_date, .x, NA_real_)),
                      pert_dist = ifelse(inference & start_date < inference_cutoff_date, pert_dist, NA_character_)) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

}

#' Generate outcome interventions based on vaccination rates
#'
#' @param outcome_path path to vaccination adjusted outcome interventions
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param incl_geoid vector of geoids to include
#' @param scenario which scenario will be selected from the outcome intervention file
#' @param v_dist type of distribution for reduction
#' @param v_sd reduction sd
#' @param v_a reduction a
#' @param v_b reduction b
#' @param inference logical indicating whether inference will be performed on intervention (default is TRUE); perturbation values are replaced with NA if set to FALSE.
#' @param p_dist type of distribution for perturbation
#' @param p_mean perturbation mean
#' @param p_sd perturbation sd
#' @param p_a perturbation a
#' @param p_b perturbation b
#'
#'
#' @return
#' @export
#'
#' @examples
#'
set_vacc_outcome_params <- function(outcome_path,
                                    sim_start_date=as.Date("2020-03-31"),
                                    sim_end_date=Sys.Date()+60,
                                    inference = FALSE,
                                    incl_geoid = NULL,
                                    scenario_num = 1,
                                    v_dist="truncnorm",
                                    v_sd = 0.01, v_a = 0, v_b = 1,
                                    p_dist="truncnorm",
                                    p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1
){
    sim_start_date <- as.Date(sim_start_date)
    sim_end_date <- as.Date(sim_end_date)

    outcome <- readr::read_csv(outcome_path) %>%
        dplyr::filter(!is.na(month) & month!="baseline") %>%
        dplyr::filter(scenario == scenario_num)

    if(!is.null(incl_geoid)){
        outcome<-outcome %>%
            dplyr::filter(geoid %in% incl_geoid)
    }
        outcome <- outcome %>%
        dplyr::mutate(param = dplyr::case_when(var=="rr_death_inf" ~ "incidD",
                                                var=="rr_hosp_inf" ~ "incidH",
                                                TRUE ~ NA_character_))%>%
        dplyr::filter(!is.na(param)) %>%
        dplyr::mutate(month = tolower(month)) %>%
        dplyr::mutate(prob_redux = 1 - prob_redux) %>%
        dplyr::filter(start_date <= sim_end_date) %>%
        dplyr::mutate(end_date = lubridate::as_date(ifelse(end_date>sim_end_date, sim_end_date, end_date)),
                      start_date = lubridate::as_date(ifelse(end_date > start_date & start_date < sim_start_date, sim_start_date, start_date))) %>%
        dplyr::filter(start_date >= sim_start_date) %>%
        dplyr::rename(value_mean = prob_redux) %>%
        dplyr::mutate(geoid = as.character(geoid),
                      type = "outcome",
                      category = "vacc_outcome",
                      name = paste(param, "vaccadj", month, sep="_"),
                      template = "Reduce",
                      parameter = paste0(param, "::probability"),
                      baseline_scenario = "",
                      value_dist = v_dist,
                      value_sd = v_sd,
                      value_a = v_a,
                      value_b = v_b,
                      pert_dist = p_dist,
                      pert_mean = p_mean,
                      pert_sd = p_sd,
                      pert_a = p_a,
                      pert_b = p_b) %>%
        dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(inference, .x, NA_real_)),
                      pert_dist = ifelse(inference, pert_dist, NA_character_)) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

    return(outcome)

}

#' Generate incidC shift interventions
#'
#' @param periods vector of dates that include a shift in incidC
#' @param geodata df with USPS and geoid column for geoids with a shift in incidC
#' @param baseline_ifr assumed true infection fatality rate
#' @param cfr_data optional file with estimates of cfr by state
#' @param epochs character vector with the selection of epochs from the cfr_data file, any of "NoSplit", "MarJun", "JulOct", "NovJan". Required if cfr_data is specified.
#' @param outcomes_parquet_file path to file with geoid-specific adjustments to IFR; required if cfr_data is specified
#' @param inference logical indicating whether inference will be performed on intervention (default is TRUE); perturbation values are replaced with NA if set to FALSE.
#' @param v_dist type of distribution for reduction
#' @param v_mean state-specific initial value. will be taken from empirical CFR estimates if it exists, otherwise this used. If a vector is specified, then each value is added to the corresponding period
#' @param v_sd reduction sd
#' @param v_a reduction a
#' @param v_b reduction b
#' @param p_dist type of distribution for perturbation
#' @param p_mean perturbation mean
#' @param p_sd perturbation sd
#' @param p_a perturbation a
#' @param p_b perturbation b
#' @return
#' @export
#'
#' @examples
set_incidC_shift <- function(periods,
                              geodata,
                              baseline_ifr = 0.005,
                              cfr_data = NULL,
                              epochs = NULL,
                              outcomes_parquet_file = NULL,
                              inference = TRUE,
                              v_dist="truncnorm",
                              v_mean=0.25, v_sd = 0.05, v_a = 0, v_b = 1,
                              p_dist="truncnorm",
                              p_mean = 0, p_sd = 0.01, p_a = -1, p_b = 1
                              ){
    periods <- as.Date(periods)

    if(is.null(cfr_data)){
        epochs <- 1:(length(periods)-1)

        cfr_data <- geodata %>%
            dplyr::select(USPS, geoid) %>%
            tidyr::expand_grid(value_mean = v_mean,
                               epoch=epochs)
    } else{
        if(is.null(epochs) | length(epochs) != (length(periods)-1)){stop("The number of epochs selected should be equal to the number of periods with a shift in incidC")}
        if(any(!epochs %in% c("NoSplit", "MarJun", "JulOct", "NovJan"))){stop('Unknown epoch selected, choose from: "NoSplit", "MarJun", "JulOct", "NovJan"')}
        if(is.null(outcomes_parquet_file)){stop("Must specify a file with the age-adjustments to IFR by state")}

        relative_outcomes <- arrow::read_parquet(outcomes_parquet_file)

        relative_ifr <- relative_outcomes %>%
            dplyr::filter(source == 'incidI' & outcome == "incidD") %>%
            dplyr::filter(geoid %in% geodata$geoid) %>%
            dplyr::select(USPS,geoid,value) %>%
            dplyr::rename(rel_ifr=value) %>%
            dplyr::mutate(ifr=baseline_ifr*rel_ifr)

        cfr_data <- readr::read_csv(cfr_data) %>%
            dplyr::rename(USPS=state, delay=lag) %>%
            dplyr::select(USPS, epoch, delay, cfr) %>%
            dplyr::filter(epoch %in% epochs) %>%
            dplyr::left_join(relative_ifr) %>%
            dplyr::filter(geoid %in% geodata$geoid) %>%
            dplyr::mutate(incidC = pmin(0.99,ifr/cfr),  # get effective case detection rate based in assumed IFR.
                          value_mean = pmax(0,1-incidC),
                          value_mean = signif(value_mean, digits = 2)) %>% # get effective reduction in incidC assuming baseline incidC
            dplyr::select(USPS,geoid, epoch, value_mean)


        no_cfr_data <- relative_ifr %>%
            tidyr::expand_grid(value_mean = v_mean,
                               epoch = epochs) %>%
            dplyr::filter(!geoid %in% cfr_data$geoid) %>%
            dplyr::select(USPS, geoid, epoch, value_mean)

        cfr_data <- dplyr::bind_rows(cfr_data,
                                     no_cfr_data)
    }

    outcome <- list()
    for(i in 1:(length(periods)-1)){
        outcome[[i]] <- cfr_data %>%
            dplyr::filter(epoch == epochs[i]) %>%
            dplyr::select(-epoch) %>%
            dplyr::mutate(
                template = "Reduce",
                name = paste0("incidCshift_", i),
                type = "outcome",
                category = "incidCshift",
                parameter = "incidC::probability",
                baseline_scenario = "",
                start_date = periods[i],
                end_date = periods[i+1]-1,
                value_dist = v_dist,
                value_mean = value_mean,
                value_sd = v_sd,
                value_a = v_a,
                value_b = v_b,
                pert_dist = p_dist,
                pert_mean = p_mean,
                pert_sd = p_sd,
                pert_a = p_a,
                pert_b = p_b
                )

    }

    outcome <- dplyr::bind_rows(outcome) %>%
        dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(inference, .x, NA_real_)),
                      pert_dist = ifelse(inference, pert_dist, NA_character_)) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

    return(outcome)

}

#' Bind interventions and prevents inference on interventions with no data
#'
#' @param ... intervention dfs with config params
#' @param inference_cutoff_date no inference is applied for interventions that start on or after this day
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param save_name directory to save dataframe; NULL if no safe
#'
#' @return
#' @export
#'
#' @examples
#'
bind_interventions <- function(...,
                               inference_cutoff_date = Sys.Date()-7,
                               sim_end_date,
                               sim_start_date,
                               save_name){
    inference_cutoff_date <- as.Date(inference_cutoff_date)
    sim_end_date <- as.Date(sim_end_date)
    sim_start_date <- as.Date(sim_start_date)
    dat <- dplyr::bind_rows(...)

    if(min(dat$start_date) < sim_start_date) stop("At least one intervention has a start date before the sim_start_date.")
    if(max(dat$end_date) > sim_end_date) stop("At least one intervention has an end date after the sim_end_date.")

    check <- dat %>%
        dplyr::filter(category=="NPI") %>%
        dplyr::group_by(USPS, geoid, type, category) %>%
        dplyr::arrange(USPS, geoid, start_date) %>%
        dplyr::mutate(note = dplyr::case_when(end_date >= dplyr::lead(start_date) ~ "Overlap",
                                              dplyr::lead(start_date)-end_date > 1 ~ "Gap",
                                              TRUE ~ NA_character_)) %>%
        dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(start_date < inference_cutoff_date, .x, NA_real_)),
                      pert_dist = ifelse(start_date < inference_cutoff_date, pert_dist, NA_character_)) %>%
        dplyr::filter(!is.na(note))

    if(nrow(check) > 0){

        if(any(check$note=="Overlap")) warning(paste0("There are ", nrow(check[check$note=="Overlap",]), " NPIs of the same category/geoid that overlap in time"))

        if(any(check$note=="Gap")) warning(paste0("There are ", nrow(check[check$note=="Gap",]), " NPIs of the same category/geoid that are discontinuous."))
    }

    if(!is.null(save_name)){
        readr::write_csv(dat, file = save_name)
    }

    return(dat)
}

#' Estimate average reduction in transmission per day per geoid
#'
#' @param dat
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
#'

daily_mean_reduction <- function(dat,
                                 plot = FALSE){

    dat <- dat %>%
        dplyr::filter(type == "transmission") %>%
        dplyr::filter(is.na(parameter) | parameter == "R0") %>%
        dplyr::mutate(mean = dplyr::case_when(value_dist == "truncnorm" ~
                                                  truncnorm::etruncnorm(a=value_a, b=value_b, mean=value_mean, sd=value_sd),
                                              value_dist == "fixed" ~
                                                  value_mean,
                                              value_dist == "uniform" ~
                                                  (value_a+value_b)/2)
        ) %>%
        dplyr::select(USPS, geoid, start_date, end_date, mean)

    timeline <- tidyr::crossing(time = seq(from=min(dat$start_date), to=max(dat$end_date), by = 1),
                                geoid = unique(dat$geoid))

    if(any(stringr::str_detect(dat$geoid, '", "'))){
        mtr_geoid <- dat %>%
            dplyr::filter(stringr::str_detect(geoid, '", "'))

        temp <- list()
        for(i in 1:nrow(mtr_geoid)){
            temp[[i]] <- tidyr::expand_grid(geoid = mtr_geoid$geoid[i] %>% stringr::str_split('", "') %>% unlist(),
                                            mtr_geoid[i,] %>% dplyr::ungroup() %>% dplyr::select(-geoid)) %>%
                dplyr::select(colnames(mtr_geoid))
        }

        dat <- dat %>%
            dplyr::filter(stringr::str_detect(geoid, '", "', negate = TRUE)) %>%
            dplyr::bind_rows(
                dplyr::bind_rows(temp)
            )
    }

    dat <- dat %>%
        dplyr::filter(geoid=="all") %>%
        dplyr::ungroup() %>%
        dplyr::select(-geoid) %>%
        tidyr::crossing(geoid=unique(dat$geoid[dat$geoid!="all"])) %>%
        dplyr::select(geoid, start_date, end_date, mean) %>%
        dplyr::bind_rows(dat %>% dplyr::filter(geoid!="all") %>% dplyr::ungroup() %>% dplyr::select(-USPS)) %>%
        dplyr::left_join(timeline) %>%
        dplyr::filter(time >= start_date & time <= end_date) %>%
        dplyr::group_by(geoid, time) %>%
        dplyr::summarize(mean = prod(1-mean))

    if(plot){
        dat<- ggplot2::ggplot(data= dat, ggplot2::aes(x=time, y=mean))+
            ggplot2::geom_line()+
            ggplot2::facet_wrap(~geoid)+
            ggplot2::theme_bw()+
            ggplot2::ylab("Average reduction")+
            ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b\n%y")+
            ggplot2::scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0))

    }

    return(dat)
}
