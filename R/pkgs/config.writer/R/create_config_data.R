# Process ------

#' Specify parameters for NPIs
#'
#' @param intervention_file df with the location's state and ID and the intervention start and end dates, name, and template - from process_npi_shub
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param npi_cutoff_date only interventions that start before or on npi_cuttof_date are included
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
    npi <- npi %>%
        dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(inference, .x, NA_real_)),
                      pert_dist = ifelse(inference, pert_dist, NA_character_)) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

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
                                   template = "MultiTimeReduce", # TODO: MTR for some, but not all... not critical
                                   v_dist="truncnorm",
                                   v_mean = c(-0.2, -0.133, -0.067, 0, 0.067, 0.133, 0.2, 0.133, 0.067, 0, -0.067, -0.133), # TODO function?
                                   v_sd = 0.05, v_a = -1, v_b = 1,
                                   p_dist="truncnorm",
                                   p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1){

    sim_start_date <- as.Date(sim_start_date)
    sim_end_date <- as.Date(sim_end_date)

    years_ <- unique(lubridate::year(c(sim_start_date, sim_end_date)))

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
#' @param template
#' @param param
#' @param affected_geoids
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
                                template = "ReduceR0",
                                param = NA_character_,
                                affected_geoids = "all",
                                v_dist="truncnorm",
                                v_mean =  0, v_sd = 0.05, v_a = -1, v_b = 1, # TODO: add check on limits
                                p_dist="truncnorm",
                                p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1
){
    sim_start_date <- as.Date(sim_start_date)
    sim_end_date <- as.Date(sim_end_date)

    local_var <- dplyr::tibble(USPS = "",
                                geoid = "all",
                                name = "local_variance",
                                type = "transmission",
                                category = "local_variance",
                                parameter = "",
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
#' @param incl_geoid
#' @param projection_start_date
#' @param redux_end_date end date for reduction interventions; default NULL uses sim_end_date in npi_file
#' @param redux_level reduction to intervention effectiveness
#' @param v_dist type of distribution for reduction
#' @param v_mean reduction mean
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
                             incl_geoid = NULL, # would need some USPS value if running multiple times on different geoids in same config
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

    if(!is.null(incl_geoid)){
        npi_file <- npi_file %>%
            dplyr::filter(geoid %in% incl_geoid)
    }

    og <- npi_file %>%
        dplyr::group_by(USPS, geoid) %>%
        dplyr::filter(start_date == max(start_date)) %>%
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
        reduction = reduction, # TODO: reduction to value_mean
        type = rep("transmission", month_n),
        geoid = npi_file$geoid %>% paste0(collapse = '", "')) %>%
        mutate(USPS = "",
               category = "NPI_redux",
               name = paste0(category, '_', month),
               baseline_scenario = c("base_npi", paste("NPI_redux", month[-length(month)])),
               template = "ReduceIntervention",
               parameter = "R0",
               value_dist = v_dist,
               value_mean = v_mean,
               value_sd = v_sd,
               value_a = v_a,
               value_b = v_b,
               pert_dist = NA_character_,
               pert_mean = NA_real_,
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
        dplyr::mutate(month = lubridate::month(start_date, label=TRUE),
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
#' @param variant_lb
#' @param varian_effect change in transmission for variant default is 50% from Davies et al 2021
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
                               variant_lb = 1.4,
                               variant_effect = 1.5,
                               month_shift = NULL,
                               transmission_increase = NULL,
                               inference = TRUE,
                               v_dist="truncnorm",  v_sd = NULL, v_a = -1.5, v_b = 0,
                               p_dist="truncnorm",
                               p_mean = 0, p_sd = 0.01, p_a = -1, p_b = 1
){


    if(b117_only){
        variant_data <- generate_variant_b117(variant_path = variant_path,
                                              sim_start_date = sim_start_date,
                                              sim_end_date = sim_end_date,
                                              variant_lb = variant_lb,
                                              variant_effect= variant_effect,
                                              month_shift = month_shift)
    } else{

        if(is.null(variant_path_2)){stop("You must specify a path for the second variant.")}

        variant_data <- generate_multiple_variants(variant_path_1 = variant_path,
                                                   variant_path_2 = variant_path_2,
                                                   sim_start_date = sim_start_date,
                                                   sim_end_date = sim_end_date,
                                                   variant_lb = variant_lb,
                                                   variant_effect= variant_effect,
                                                   transmission_increase = transmission_increase)
        }

    variant_data <- variant_data %>%
        dplyr::mutate(type = "transmission",
                      param = "ReduceR0",
                      category = "variant",
                      name = paste("variantR0adj", paste0("Week", week), sep="_"),
                      template = "ReduceR0",
                      geoid = "all",
                      parameter = NA,
                      value_dist = v_dist,
                      value_mean = 1-R_ratio,
                      value_sd = ifelse(is.null(v_sd), round(value_sd,4), v_sd),
                      value_a = v_a,
                      value_b = v_b,
                      pert_dist = p_dist,
                      pert_mean = p_mean,
                      pert_sd = p_sd, # dont want much perturbation on this if it gets perturbed
                      pert_a = p_a,
                      pert_b = p_b,
                      baseline_scenario = "",
                      USPS="") %>% # really dont want to perturb this at the moment
        dplyr::mutate(dplyr::across(pert_mean:pert_b, ~ifelse(inference, .x, NA_real_)),
                      pert_dist = ifelse(inference, pert_dist, NA_character_)) %>%
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
        dplyr::mutate(type = "outcome",
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

#' Generate incidC shift intervention
#'
#' @param startdate vector of start dates for incidC shift
#' @param enddate vector of start dates for incidC shift
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
#'
#' @return data frame with columns for
#' @export
#'
#' @examples
#'
set_incidC_shift <- function(startdate,
                             enddate, # TODO: allow specific geoids
                             inference = TRUE,
                             v_dist="truncnorm",
                             v_mean=0.07, v_sd = 0.05, v_a = 0, v_b = 1,
                             p_dist="truncnorm",
                             p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1
){

    startdate <- as.Date(startdate)
    enddate <- as.Date(enddate)

    outcome <- tidyr::expand_grid(
        template = "Reduce",
        type = "outcome",
        category = "incidCshift",
        parameter = "incidC::probability",
        geoid = "all",
        start_date = startdate,
        end_date = enddate,
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
        baseline_scenario = "",
        USPS = ""
    ) %>%
        dplyr::mutate(name = paste0("incidCshift_", lubridate::month(end_date, label = TRUE)),
                      dplyr::across(pert_mean:pert_b, ~ifelse(inference, .x, NA_real_)),
                      pert_dist = ifelse(inference, pert_dist, NA_character_)) %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, tidyselect::starts_with("value_"), tidyselect::starts_with("pert_"))

    return(outcome)

}

#' Generate incidC shift intervention
#'
#' @param ... intervention dfs with config params
#' @param sim_start_date simulation start date
#' @param sim_end_date simulation end date
#' @param save_name directory to save dataframe
#'
#' @return
#' @export
#'
#' @examples
#'
bind_interventions <- function(...,
                               sim_end_date,
                               sim_start_date,
                               save_name){
    sim_end_date <- as.Date(sim_end_date)
    sim_start_date <- as.Date(sim_start_date)
    dat <- dplyr::bind_rows(...)

    if(min(dat$start_date) < sim_start_date) stop("At least one intervention has a start date before the sim_start_date.")
    if(max(dat$end_date) > sim_end_date) stop("At least one intervention has an end date after the sim_end_date.")

    readr::write_csv(dat, file = save_name)

    return(dat)
}
