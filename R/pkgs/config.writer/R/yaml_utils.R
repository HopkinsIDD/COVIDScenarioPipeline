# Convenience function to print values for different distributions
#
# @param ... object specifying parameter distribution; object name should include param
# @param space_length number of spaces to add when printing
#
# @return string with parameter distribution and values
# @export
#
# @examples
#
# gamma_dist <- "fixed"
# gamma_val <- 0.167
#
#'print_param_val(gamma_dist)
#
# paste_param_val <- function(..., space_length = 6){
#
#     space <- paste0(rep(" ", space_length), collapse = "")
#     param_space <- stringr::str_remove(space, "  ")
#     param <- deparse(substitute(...)) %>% stringr::str_extract(pattern = ".+\\_")
#
#     param_name <- stringr::str_remove(param, "\\_")
#     value <- get(paste0(param, "val"), envir = parent.frame(n=1))
#
#     if(is.null(...)){
#
#         print_val <- paste0(
#             param_space, param_name, ": ", value, "\n"
#         )
#     } else{
#         if(... == "fixed"){
#             print_val <- paste0(
#                 param_space, param_name, ":\n",
#                 space, "distribution: fixed\n",
#                 space, "value: ", value, "\n"
#             )
#         }
#
#         if(... == "uniform"){
#             min_a <- get(paste0(param, "a"), envir = parent.frame(n=1))
#             max_b <- get(paste0(param, "b"), envir = parent.frame(n=1))
#
#             print_val <- paste0(
#                 param_space, param_name, ":\n",
#                 space, "distribution: uniform\n",
#                 space, "low: ", min_a, "\n",
#                 space, "high: ", max_b, "\n"
#             )
#         }
#
#         if(... == "truncnorm"){
#             min_a <- get(paste0(param, "a"), envir = parent.frame(n=1))
#             max_b <- get(paste0(param, "b"), envir = parent.frame(n=1))
#             mean <- get(paste0(param, "val"), envir = parent.frame(n=1))
#             sd <- get(paste0(param, "sd"), envir = parent.frame(n=1))
#
#             print_val <- paste0(
#                 param_space, param_name, ":\n",
#                 space, "distribution: truncnorm\n",
#                 space, "mean: ", mean, "\n",
#                 space, "sd: ", sd, "\n",
#                 space, "a: ", min_a, "\n",
#                 space, "b: ", max_b, "\n"
#             )
#         }
#     }
#
#     return(print_val)
# }

#' Collapse MTR interventions into single row per intervention and generate final intervention names/periods
#'
#' @param dat df with config params from any/all of the set param functions
#'
#' @return
#' @export
#'
#' @examples
#'
collapse_intervention<- function(dat
){
    #TODO: add number to repeated names
    #TODO add a check that all end_dates are the same
    mtr <- dat %>%
        dplyr::filter(template=="MultiTimeReduce") %>%
        dplyr::mutate(end_date=paste0("end_date: ", end_date),
                      start_date=paste0("- start_date: ", start_date)) %>%
        tidyr::unite(col="period", sep="\n              ", start_date:end_date) %>%
        dplyr::group_by(dplyr::across(-period)) %>%
        dplyr::summarize(period = paste0(period, collapse="\n            ")) %>%
        dplyr::group_by(dplyr::across(-geoid)) %>%
        dplyr::summarize(geoid = paste0(geoid, collapse='", "')) %>%
        dplyr::mutate(period = paste0("            ", period))
    
    reduce <- dat %>%
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, baseline_scenario, starts_with("value_"), starts_with("pert_")) %>%
        dplyr::filter(template %in% c("ReduceR0", "Reduce", "ReduceIntervention")) %>%
        dplyr::mutate(end_date=paste0("period_end_date: ", end_date),
                      start_date=paste0("period_start_date: ", start_date)) %>%
        tidyr::unite(col="period", sep="\n      ", start_date:end_date) %>%
        dplyr::mutate(period = paste0("      ", period, "\n")) %>%
        dplyr::ungroup() %>%
        dplyr::add_count(dplyr::across(-USPS)) %>%
        dplyr::mutate(name = dplyr::case_when(category =="local_variance" | USPS %in% c("all", "") | is.na(USPS) ~ name,
                                              n==1 & template=="Reduce" ~ paste0(USPS, "_", name),
                                              template=="Reduce" ~ paste0(geoid, "_", name),
                                              n==1 & template!="ReduceIntervention" ~ paste0(USPS, name),
                                              template!="ReduceIntervention" ~ paste0(geoid, name),
                                              TRUE ~ name),
                      name = stringr::str_remove(name, "^_"))
    
    dat <- dplyr::bind_rows(mtr,
                            reduce) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(category) %>%
        dplyr::arrange(period)  %>%
        dplyr::ungroup() %>%
        dplyr::arrange(category, geoid)
    
    return(dat)
}

#' Print intervention text for MultiTimeReduce interventions
#'
#' @param dat df for an intervention with the MTR template with processed name/period; see collapsed_intervention. All rows in the dataframe should have the same intervention name.
#'
#' @return
#' @export
#'
#' @examples
#'

yaml_mtr_template <- function(dat){
    template <- unique(dat$template)
    geoid_all <- any(unique(dat$geoid)=="all")
    inference <- !any(is.na(dat$pert_dist))
    
    if(template=="MultiTimeReduce" & geoid_all){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: MultiTimeReduce\n",
            "      parameter: ", dat$parameter, "\n",
            "      groups:\n",
            '        - affected_geoids: "all"\n'
        ))
        
        for(j in 1:nrow(dat)){
            cat(paste0('          periods:\n',
                       dat$period[j], '\n'
            ))
        }
    }
    
    if(template=="MultiTimeReduce" & !geoid_all){
        cat(paste0(
            "    ", dat$name[1], ":\n",
            "      template: MultiTimeReduce\n",
            "      parameter: ", dat$parameter[1], "\n",
            "      groups:\n"
        ))
        
        for(j in 1:nrow(dat)){
            cat(paste0(
                '        - affected_geoids: ["', dat$geoid[j], '"]\n',
                '          periods:\n',
                dat$period[j], '\n'
            ))
        }
    }
    
    cat(
        print_value(value_dist = dat$value_dist[1],
                    value_mean = dat$value_mean[1],
                    value_sd = dat$value_sd[1],
                    value_a = dat$value_a[1],
                    value_b = dat$value_b[1])
    )
    
    if(inference){
        cat(
            print_value(value_dist = dat$pert_dist[1],
                        value_mean = dat$pert_mean[1],
                        value_sd = dat$pert_sd[1],
                        value_a = dat$pert_a[1],
                        value_b = dat$pert_b[1],
                        param_name = "perturbation")
        )
    }
}

#' Convenience function to print params based on the specified distribution
#'
#' @param value_dist one of the following distributions: "fixed", "uniform", or "truncnorm"
#' @param value_mean value when value_dist is "fixed" or mean if value_dist is ""truncnorm"
#' @param value_sd standard deviation - required when value_dist is "truncnorm"
#' @param value_a minimum - required when value_dist is "uniform" or "truncnorm"
#' @param value_b maximum - required when value_dist is "uniform" or "truncnorm"
#' @param param_name name of the parameter whose distribution is being specified: "value", "perturbation", "R0s", or "gamma"
#' @param indent_space defaults to 6 for transmission and outcome interventions
#'
#' @return
#' @export
#'
#' @examples
#'
print_value <- function(value_dist,
                        value_mean,
                        value_sd,
                        value_a,
                        value_b,
                        param_name = "value",
                        indent_space = 6){
    
    space <- rep(" ", indent_space) %>% paste0(collapse="")
    space2 <- rep(" ", indent_space+2) %>% paste0(collapse="")
    
    if(value_dist=="fixed"){
        if(is.na(value_mean)){stop('Intervention value must be specified for "fixed" distributions')}
        print_val <- paste0(
            space, param_name, ":\n",
            space2, "distribution: fixed\n",
            space2, "value: ", value_mean, "\n"
        )
    }
    
    if(value_dist=="truncnorm"){
        if(any(is.na(value_mean), is.na(value_sd), is.na(value_a), is.na(value_b))){stop('Intervention mean, sd, a, and b must be specified for "truncnorm" distributions')}
        print_val <- paste0(
            space, "", param_name, ":\n",
            space2, "distribution: truncnorm\n",
            space2, "mean: ", value_mean, "\n",
            space2, "sd: ", value_sd, "\n",
            space2, "a: ", value_a, "\n",
            space2, "b: ", value_b, "\n"
        )
    }
    
    if(value_dist=="uniform"){
        if(any(is.na(value_a), is.na(value_b))){stop('Intervention a and b must be specified for "uniform" distributions')}
        print_val <- paste0(
            space, param_name, ":\n",
            space2, "distribution: uniform\n",
            space2, "low: ", value_a, "\n",
            space2, "high: ", value_b, "\n"
        )
    }
    
    if(is.na(value_dist)){
        print_val = ""
    }
    
    return(print_val)
}

#' Print intervention text for Reduce interventions
#'
#' @param dat df row for an intervention with the Reduce, ReduceR0 or ReduceIntervention template that has been processed name/period; see collapsed_intervention.
#'
#' @return
#' @export
#'
#' @examples
#'
yaml_reduce_template<- function(dat
){
    #if(!dat$template %in% c("ReduceR0", "ReduceIntervention", "Reduce")){stop(paste0("Intervention template should be 'ReduceR0' or 'ReduceIntervention', but is ", dat$template))}
    
    if(dat$template == "ReduceR0" & dat$geoid != "all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            '      affected_geoids: ["', dat$geoid, '"]\n',
            dat$period
        ))
    }
    
    if(dat$template == "ReduceR0" & dat$geoid == "all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            '      affected_geoids: "', dat$geoid, '"\n',
            dat$period
        ))
    }
    
    if(dat$template == "Reduce" & dat$geoid != "all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            "      parameter: ", dat$parameter, "\n",
            '      affected_geoids: ["', dat$geoid, '"]\n',
            dat$period
        ))
    }
    
    if(dat$template == "Reduce" & dat$geoid == "all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            "      parameter: ", dat$parameter, "\n",
            '      affected_geoids: "', dat$geoid, '"\n',
            dat$period
        ))
    }
    
    if(dat$template == "ReduceIntervention" & dat$geoid != "all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            "      parameter: ", dat$parameter, "\n",
            '      affected_geoids: ["', dat$geoid, '"]\n',
            dat$period,
            "      baseline_scenario: ", dat$baseline_scenario, "\n"
        ))
    }
    
    if(dat$template == "ReduceIntervention" & dat$geoid == "all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            "      parameter: ", dat$parameter, "\n",
            '      affected_geoids: "', dat$geoid, '"\n',
            dat$period,
            "      baseline_scenario: ", dat$baseline_scenario, "\n"
        ))
    }
    
    cat(
        print_value(value_dist = dat$value_dist[1],
                    value_mean = dat$value_mean[1],
                    value_sd = dat$value_sd[1],
                    value_a = dat$value_a[1],
                    value_b = dat$value_b[1])
    )
    
    if(!is.na(dat$pert_dist)){
        cat(
            print_value(value_dist = dat$pert_dist[1],
                        value_mean = dat$pert_mean[1],
                        value_sd = dat$pert_sd[1],
                        value_a = dat$pert_a[1],
                        value_b = dat$pert_b[1],
                        param_name = "perturbation")
        )
    }
    
}


#' Print stack interventions at the end of the transmission section
#'
#' @param dat dataframe with processed intervention name/periods; see collapsed_interventions.
#' @param scenario intervention scenario name
#' @param stack Whether to stack interventions; default TRUE
#'
#' @return
#' @export
#'
#' @examples
#'
yaml_stack <- function (dat, scenario = "Inference", stack=TRUE) {
    
    if (stack){
        dat <- dat %>% dplyr::group_by(category, USPS, geoid) %>% 
            dplyr::filter(category == "NPI_redux" & period == max(period)) %>% 
            dplyr::bind_rows(dat %>% dplyr::filter(category != "NPI_redux")) %>% 
            dplyr::distinct(name, category) %>% dplyr::group_by(category) %>% 
            dplyr::summarize(name = paste0(name, collapse = "\", \""))
        duplicate_names <- dat %>% dplyr::count(name) %>% dplyr::filter(n > 1) %>% nrow()
        if (duplicate_names > 1) {
            stop("At least one intervention name is shared by distinct NPIs.")
        }
        for (i in 1:nrow(dat)) {
            if (dat$category[i] %in% c("local_variance", "NPI_redux")) {
                next
            }
            cat(paste0("    ", dat$category[i], ":\n", 
                       "      template: Stacked\n", 
                       "      scenarios: [\"", dat$name[i], "\"]\n"))
        }
        dat <- dat %>% dplyr::filter(category != "base_npi") %>% 
            dplyr::mutate(category = dplyr::if_else(category == "NPI_redux", name, category))
        cat(paste0("    ", scenario, ":\n", 
                   "      template: Stacked\n", 
                   "      scenarios: [\"", paste0(dat$category, collapse = "\", \""), "\"]\n"))
        
    } else {
        dat <- dat %>% dplyr::group_by(category, USPS, geoid) %>% 
            dplyr::filter(category == "NPI_redux" & period == max(period)) %>% 
            dplyr::bind_rows(dat %>% dplyr::filter(category != "NPI_redux")) %>% 
            dplyr::distinct(name, category) %>% as_tibble() %>% dplyr::select(name) %>% 
            pull(name) 
        duplicate_names <- sum(duplicated(dat))
        if (duplicate_names > 1) {
            stop("At least one intervention name is shared by distinct NPIs.")
        } 
        cat(paste0("    ", scenario, ":\n", 
                   "      template: Stacked\n", 
                   "      scenarios: [\"", paste0(dat, collapse = "\", \""), "\"]\n"))
    }
}


#' Print Interventions Section
#'
#' @description Print transmission and outcomes interventions and stack them
#'
#' @param dat dataframe with processed intervention names/periods; see collapsed_interventions
#' @param scenario name of the scenario
#' @param compartment
#' @param stack 
#'
#' @return
#' @export
#'
#' @examples
#'

print_interventions <- function (dat, 
                                 scenario = "Inference", 
                                 stack=TRUE, 
                                 compartment = TRUE) {
    
    cat(paste0("\ninterventions:\n", 
               "  scenarios:\n", 
               "    - ", scenario, "\n", 
               "  settings:\n"))
    
    outcome_dat <- dat %>% collapse_intervention() %>% dplyr::filter(type == "outcome")
    dat <- collapse_intervention(dat) %>% dplyr::filter(type == "transmission")
    for (i in 1:nrow(dat)) {
        if (i > nrow(dat)) 
            break
        if (dat$template[i] == "MultiTimeReduce") {
            dat %>% dplyr::filter(name == dat$name[i]) %>% yaml_mtr_template(.)
            dat <- dat %>% dplyr::filter(name != dat$name[i] | dplyr::row_number() == i)
        }
        else {
            yaml_reduce_template(dat[i, ])
        }
    }
    
    yaml_stack(dat, scenario, stack)
    
    if (nrow(outcome_dat) > 0) {
        if (compartment) {
            yaml_stack(outcome_dat, "outcome_interventions", stack)
            cat(paste0("\n"))
        }
        for (i in 1:nrow(outcome_dat)) {
            if (i > nrow(outcome_dat)) 
                break
            if (outcome_dat$template[i] == "MultiTimeReduce") {
                outcome_dat %>% dplyr::filter(name == outcome_dat$name[i]) %>% yaml_mtr_template(.)
                outcome_dat <- outcome_dat %>% dplyr::filter(name != outcome_dat$name[i] | dplyr::row_number() == i)
            }
            else {
                yaml_reduce_template(outcome_dat[i, ])
            }
        }
        cat(paste0("\n"))
    }
}







#' Print Outcomes Section
#'
#' @param ifr name of ifr scenario
#' @param outcomes_parquet_file path to outcomes parquet file
#' @param incidH_prob_dist distribution for incidH probability
#' @param incidH_prob_value probability of being hospitalized among incident infections
#' @param incidH_delay_dist distribution for incidH delay
#' @param incidH_delay_value time to hospitalization since infection in days
#' @param incidH_duration_dist distribution for incidH duration
#' @param incidH_duration_value duration of hospitalization in days
#' @param incidD_prob_dist distribution for incidD probability
#' @param incidD_prob_value probability of death among incident infections
#' @param incidD_delay_dist distribution for incidD delay in days
#' @param incidD_delay_value time to death since infection in days
#' @param incidICU_prob_dist distribution for incidICU probability
#' @param incidICU_prob_value probability of being admitted to the ICU among incident hospitalizations
#' @param incidICU_delay_dist distribution for incidICU delay
#' @param incidICU_delay_value time to ICU admission since hospitalization
#' @param incidICU_duration_dist distribution for incidICU duration
#' @param incidICU_duration_value duration of ICU stay in days
#' @param incidVent_prob_dist distribution for incidVent probability
#' @param incidVent_prob_value probability of ventilation among incident ICU admissions
#' @param incidVent_delay_dist distribution for incidVent delay
#' @param incidVent_delay_value time to ventilation since ICU admission in days
#' @param incidVent_duration_dist distribution for incidVent duration
#' @param incidVent_duration_value duration of ventilation in days
#' @param incidC_prob_dist distribution for incidC probability
#' @param incidC_prob_value probability of detecting a case among incident infections
#' @param incidC_prob_sd standard deviation for incidC probability
#' @param incidC_prob_a minimum value for incidC probability
#' @param incidC_prob_b maximum value for incidC probability
#' @param incidC_perturbation whether to include perturbation for incidC
#' @param incidC_prob_dist_pert distribution for incidC perturbation
#' @param incidC_prob_value_pert mean perturbation value for incidC
#' @param incidC_prob_sd_pert perturbation sd for incidC
#' @param incidC_prob_a_pert maximum perturbation value for incidC probability
#' @param incidC_prob_b_pert minimum perturbation value for incidC probability
#' @param incidC_delay_value time to case detection since infection in days
#' @param incidC_delay_dist distribution of incidC delay
#' @param compartment
#' @param variant_compartments
#' @param vaccine_compartments
#' @param age_strata
#' @param intervention_params
#' @param outcomes_included which outcomes to include, options: incidH, incidC, incidD, incidICU, incidVent.
#' @param incl_interventions
#' @param dat 
#' @param outcomes_base_data 
#' @param incl_hosp_curr 
#'
#' @details
#' The settings for each scenario correspond to a set of different health outcome risks, most often just differences in the probability of death given infection (Pr(incidD|incidI)) and the probability of hospitalization given infection (Pr(incidH|incidI)). Each health outcome risk is referenced in relation to the outcome indicated in source. For example, the probability and delay in becoming a confirmed case (incidC) is most likely to be indexed off of the number and timing of infection (incidI).
#'
#' Importantly, we note that incidI is automatically defined from the SEIR transmission model outputs, while the other compartment sources must be defined in the config before they are used. These settings are currently hardcoded into the function .
#'
#' Users must specific two metrics for each health outcome, probability and delay, while a duration is optional (e.g., duration of time spent in the hospital). The perturbation section is currently enabled for incidC only.
#'
#' Interventions on the outcomes are printed as a separate block preceding the Outcomes section. This assumes the print_outcomes function is called immediately after the [print_transmission_interventions()]
#' @export
#'
#'
print_outcomes <- function (dat = NULL, ifr = NULL, outcomes_base_data = NULL, 
                            outcomes_parquet_file = "usa-geoid-params-output_statelevel.parquet", 
                            incidH_prob_dist = "fixed", incidH_prob_value = 0.0175, 
                            incidH_delay_dist = "fixed", incidH_delay_value = 7, incidH_duration_dist = "fixed", 
                            incidH_duration_value = 7, incidD_prob_dist = "fixed", incidD_prob_value = 0.005, 
                            incidD_delay_dist = "fixed", incidD_delay_value = 20, incidICU_prob_dist = "fixed", 
                            incidICU_prob_value = 0.167, incidICU_delay_dist = "fixed", 
                            incidICU_delay_value = 3, incidICU_duration_dist = "fixed", 
                            incidICU_duration_value = 8, incidVent_prob_dist = "fixed", 
                            incidVent_prob_value = 0.463, incidVent_delay_dist = "fixed", 
                            incidVent_delay_value = 1, incidVent_duration_dist = "fixed", 
                            incidVent_duration_value = 7, incidC_prob_dist = "truncnorm", 
                            incidC_prob_value = 0.2, incidC_prob_sd = 0.1, incidC_prob_a = 0, 
                            incidC_prob_b = 1, incidC_perturbation = TRUE, incidC_prob_dist_pert = "truncnorm", 
                            incidC_prob_value_pert = 0, incidC_prob_sd_pert = 0.05, 
                            incidC_prob_a_pert = -1, incidC_prob_b_pert = 1, incidC_delay_value = 7, 
                            incidC_delay_dist = "fixed", compartment = TRUE, variant_compartments = c("WILD", "ALPHA", "DELTA"), 
                            vaccine_compartments = c("unvaccinated","1dose", "2dose", "waned"), age_strata = c("0_64", "65_100"), 
                            outcomes_included = c("incidH", "incidD", "incidC", "incidI"), 
                            intervention_params = NULL, 
                            incl_interventions = TRUE,
                            incl_hosp_curr = FALSE) {
    if (is.null(ifr)) {
        stop("You must specify a scenario/IFR name.")
    }
    #age_strata <- dplyr::if_else(stringr::str_detect(age_strata, "^age\\_"), age_strata, paste0("age_", age_strata))
    incidC_pert <- ""
    if (compartment) {
        pert_repeat <- length(variant_compartments) * length(vaccine_compartments) * length(age_strata)
        #incidC_perturbation <- rep(incidC_perturbation, length(incidC_perturbation)/pert_repeat)
    } else {
        pert_repeat <- length(incidC_perturbation)
    }
    for (i in 1:pert_repeat) {
        if (incidC_perturbation) {
            incidC_pert[i] <- print_value(value_dist = incidC_prob_dist_pert, 
                                          value_mean = incidC_prob_value_pert, value_sd = incidC_prob_sd_pert, 
                                          value_a = incidC_prob_a_pert, value_b = incidC_prob_b_pert, 
                                          param_name = "perturbation", indent_space = 10)
        }  else {
            incidC_pert[i] <- ""
        }
    }
    if (compartment) {
        incidH <- ""
        incidD <- ""
        incidC <- ""
        incidI <- ""
        
        outcomes_base_data <- outcomes_base_data %>% 
            dplyr::mutate(age_strata = dplyr::if_else(stringr::str_detect(age_strata, "^age"), age_strata, paste0("age", age_strata)), 
                          var_compartment = paste(vacc, variant, age_strata,  sep = "_"))
        
        if (!("incidD" %in% colnames(outcomes_base_data))){
            outcomes_base_data <- outcomes_base_data %>% mutate(incidD = 1) 
        }
        if (!("incidH" %in% colnames(outcomes_base_data))){
            outcomes_base_data <- outcomes_base_data %>% mutate(incidH = 1) 
        }
        if (!("incidC" %in% colnames(outcomes_base_data))){
            outcomes_base_data <- outcomes_base_data %>% mutate(incidC = 1) 
        }
        if (!("incidI" %in% colnames(outcomes_base_data))){
            outcomes_base_data <- outcomes_base_data %>% mutate(incidI = 1) 
        }
        
        outcomes <- paste0(#"\n",
            "outcomes:\n", 
            "  method: delayframe\n", 
            "  param_from_file: TRUE\n", 
            "  param_place_file: \"", outcomes_parquet_file, "\"\n", 
            "  scenarios:\n", 
            "    - ", ifr, "\n", 
            "  settings:\n", 
            "    ", ifr, ":\n")
        for (i in 1:nrow(outcomes_base_data)) {
            if ("incidH" %in% outcomes_included){
                incidH <- paste0(incidH, 
                                 "      incidH_", outcomes_base_data$var_compartment[i], ":\n", 
                                 "        source: incidI_", outcomes_base_data$var_compartment[i], "\n", 
                                 "        probability:\n", 
                                 if ("incidH" %in% intervention_params) paste0("          intervention_param_name: \"incidH_total\"\n"), 
                                 print_value(value_dist = incidH_prob_dist, 
                                             value_mean = incidH_prob_value * outcomes_base_data$incidH[i], 
                                             indent_space = 10), 
                                 "        delay:\n", print_value(value_dist = incidH_delay_dist, value_mean = incidH_delay_value, indent_space = 10), 
                                 "        duration:\n", print_value(value_dist = incidH_duration_dist, value_mean = incidH_duration_value, indent_space = 10), 
                                 "          name: hosp_curr_", paste0(outcomes_base_data$var_compartment[i]), "\n")
            }
            
            if ("incidI" %in% outcomes_included){
                incidD <- paste0(incidD, 
                                 "      incidD_", outcomes_base_data$var_compartment[i], ":\n", 
                                 "        source: incidI_", outcomes_base_data$var_compartment[i], "\n", 
                                 "        probability:\n", 
                                 if ("incidD" %in% intervention_params) paste0("          intervention_param_name: \"incidD_total\"\n"), 
                                 print_value(value_dist = incidD_prob_dist, 
                                             value_mean = incidD_prob_value * outcomes_base_data$incidD[i], 
                                             indent_space = 10), 
                                 "        delay:\n", print_value(value_dist = incidD_delay_dist, value_mean = incidD_delay_value, indent_space = 10))
            } else {
                incidD <- paste0(incidD,
                                 "      incidD_", outcomes_base_data$var_compartment[i],
                                 ":\n", "        source:\n", "          incidence:\n",
                                 "            infection_stage: \"I1\"\n",
                                 "            vaccination_stage: \"", paste0(outcomes_base_data$vacc[i], collapse = "\", \""), "\"\n",
                                 "            variant_type: \"", paste0(outcomes_base_data$variant[i], collapse = "\", \""), "\"\n",
                                 "            age_strata: \"", paste0(outcomes_base_data$age_strata[i]), "\"\n",
                                 "        probability:\n",
                                 if ("incidD" %in% intervention_params) paste0("          intervention_param_name: \"incidD_total\"\n"),
                                 print_value(value_dist = incidD_prob_dist,
                                             value_mean = incidD_prob_value * outcomes_base_data$incidD[i],
                                             indent_space = 10),
                                 "        delay:\n", print_value(value_dist = incidD_delay_dist, value_mean = incidD_delay_value, indent_space = 10))
            }
            
            
            if ("incidI" %in% outcomes_included){
                incidC <- paste0(incidC, 
                                 "      incidC_", outcomes_base_data$var_compartment[i], ":\n", 
                                 "        source: incidI_", outcomes_base_data$var_compartment[i],  "\n", 
                                 "        probability:\n", 
                                 if ("incidC" %in% intervention_params) paste0("          intervention_param_name: \"incidItoC_all\"\n"), 
                                 print_value(value_dist = incidC_prob_dist, 
                                             value_mean = incidC_prob_value * outcomes_base_data$incidC[i], 
                                             value_sd = incidC_prob_sd, 
                                             value_a = incidC_prob_a, 
                                             value_b = incidC_prob_b, 
                                             indent_space = 10), 
                                 incidC_pert[i], 
                                 "        delay:\n", print_value(value_dist = incidC_delay_dist, value_mean = incidC_delay_value, indent_space = 10))
            } else {
                incidC <- paste0(incidC,
                                 "      incidC_", outcomes_base_data$var_compartment[i], ":\n",
                                 "        source:\n",
                                 "          incidence:\n",
                                 "            infection_stage: \"I1\"\n",
                                 "            vaccination_stage: \"", paste0(outcomes_base_data$vacc[i], collapse = "\", \""), "\"\n",
                                 "            variant_type: \"", paste0(outcomes_base_data$variant[i], collapse = "\", \""), "\"\n",
                                 "            age_strata: \"", paste0(outcomes_base_data$age_strata[i]), "\"\n",
                                 "        probability:\n",
                                 if ("incidC" %in% intervention_params) paste0("          intervention_param_name: \"incidItoC_all\"\n"),
                                 print_value(value_dist = incidC_prob_dist,
                                             value_mean = incidC_prob_value * outcomes_base_data$incidC[i],
                                             value_sd = incidC_prob_sd,
                                             value_a = incidC_prob_a,
                                             value_b = incidC_prob_b,
                                             indent_space = 10),
                                 incidC_pert[i],
                                 "        delay:\n", print_value(value_dist = incidC_delay_dist, value_mean = incidC_delay_value, indent_space = 10))
            }
            
            if ("incidI" %in% outcomes_included){
                incidI <- paste0(incidI, 
                                 "      incidI_", outcomes_base_data$var_compartment[i], ":\n", 
                                 "        source:\n", 
                                 "          incidence:\n", 
                                 "            infection_stage: \"I1\"\n", 
                                 "            vaccination_stage: \"", paste0(outcomes_base_data$vacc[i], collapse = "\", \""), "\"\n", 
                                 "            variant_type: \"", paste0(outcomes_base_data$variant[i], collapse = "\", \""), "\"\n", 
                                 "            age_strata: \"", paste0(outcomes_base_data$age_strata[i]), "\"\n", 
                                 "        probability:\n",                           
                                 if("incidI" %in% intervention_params) paste0('          intervention_param_name: "incidI_total"\n'),
                                 print_value(value_dist = "fixed", 
                                             value_mean = 1, 
                                             indent_space = 10),
                                 "        delay:\n", print_value(value_dist = "fixed", value_mean = 0, indent_space = 10))
            }
        }
        
        
        for (i in 1:length(variant_compartments)) {
            if ("incidH" %in% outcomes_included){
                incidH <- paste0(incidH, 
                                 "      ", paste0("incidH_", variant_compartments[i]), ":\n", 
                                 "        sum: [\n", 
                                 paste0(paste0("          incidH_", outcomes_base_data$var_compartment[outcomes_base_data$variant == variant_compartments[i]]), collapse = ",\n"), "\n", 
                                 "        ]\n")
                
                if (incl_hosp_curr){
                    incidH <- paste0(incidH, 
                                     "      ", paste0("hosp_curr_", variant_compartments[i]), ":\n", 
                                     "        sum: [\n", 
                                     paste0(paste0("          hosp_curr_", outcomes_base_data$var_compartment[outcomes_base_data$variant == variant_compartments[i]]), collapse = ",\n"), "\n", 
                                     "        ]\n")
                }
            }
            incidD <- paste0(incidD, 
                             "      ", paste0("incidD_", variant_compartments[i]), ":\n", 
                             "        sum: [\n", 
                             paste0(paste0("          incidD_", outcomes_base_data$var_compartment[outcomes_base_data$variant == variant_compartments[i]]), collapse = ",\n"), "\n", 
                             "        ]\n")
            incidC <- paste0(incidC, 
                             "      ", paste0("incidC_", variant_compartments[i]), ":\n", 
                             "        sum: [\n", 
                             paste0(paste0("          incidC_", outcomes_base_data$var_compartment[outcomes_base_data$variant == variant_compartments[i]]), collapse = ",\n"), "\n", 
                             "        ]\n")
            if ("incidI" %in% outcomes_included){
                incidI <- paste0(incidI, 
                                 "      ", paste0("incidI_", variant_compartments[i]), ":\n", 
                                 "        sum: [\n", 
                                 paste0(paste0("          incidI_", outcomes_base_data$var_compartment[outcomes_base_data$variant == variant_compartments[i]]), collapse = ",\n"), "\n", 
                                 "        ]\n")
            }
            
            if (i == length(variant_compartments)) {
                if ("incidH" %in% outcomes_included){
                    incidH <- paste0(incidH, 
                                     "      incidH:\n", 
                                     "        sum: ['", paste0("incidH_", variant_compartments, collapse = "', '"), "']\n")
                    if (incl_hosp_curr){
                        incidH <- paste0(incidH, 
                                         "      hosp_curr:\n", 
                                         "        sum: ['", paste0("hosp_curr_", variant_compartments, collapse = "', '"), "']\n")
                    }
                }
                incidD <- paste0(incidD, 
                                 "      incidD:\n", 
                                 "        sum: ['", paste0("incidD_", variant_compartments, collapse = "', '"), "']\n")
                incidC <- paste0(incidC, "      incidC:\n", 
                                 "        sum: ['", paste0("incidC_", variant_compartments, collapse = "', '"), "']\n")
                if ("incidI" %in% outcomes_included){
                    incidI <- paste0(incidI, 
                                     "      incidI:\n", 
                                     "        sum: ['", paste0("incidI_", variant_compartments, collapse = "', '"), "']\n")
                }
            }
        }
        
        if (any(outcomes_included == "incidICU")) {
            if (any(outcomes_included == "incidVent")) {
                order_outcomes <- c("incidH", "incidICU", "incidVent")
                outcomes_included <- c(order_outcomes, outcomes_included[!outcomes_included %in% order_outcomes])
            }
            else {
                order_outcomes <- c("incidH", "incidICU")
                outcomes_included <- c(order_outcomes, outcomes_included[!outcomes_included %in% order_outcomes])
            }
        }
        cat(paste0(outcomes, mget(outcomes_included) %>% unlist() %>% paste0(collapse = "")))
        
        if (incl_interventions) {
            cat(paste0("  interventions:\n", 
                       "    settings:\n", 
                       "      ", ifr, ":\n", 
                       "        template: Stacked\n", 
                       "        scenarios: [\"outcome_interventions\"]\n"))
        }
        
        
    }    else {
        cat(paste0("\n", "outcomes:\n", 
                   "  method: delayframe\n", 
                   "  param_from_file: TRUE\n", 
                   "  param_place_file: \"", outcomes_parquet_file, "\"\n", 
                   "  scenarios:\n", 
                   "    - ", ifr, "\n", 
                   "  settings:\n", 
                   "    ", ifr, ":\n", 
                   "      incidH:\n", 
                   "        source: incidI\n", 
                   "        probability:\n", print_value(value_dist = incidH_prob_dist[1], value_mean = incidH_prob_value[1], indent_space = 10), 
                   "        delay:\n", print_value(value_dist = incidH_delay_dist[1], value_mean = incidH_delay_value[1], indent_space = 10), 
                   "        duration:\n", print_value(value_dist = incidH_duration_dist[1], value_mean = incidH_duration_value[1], indent_space = 10), 
                   "          name: hosp_curr\n", 
                   "      incidD:\n", 
                   "        source: incidI\n", 
                   "        probability:\n", print_value(value_dist = incidD_prob_dist[1], value_mean = incidD_prob_value[1], indent_space = 10), 
                   "        delay:\n", print_value(value_dist = incidD_delay_dist[1], value_mean = incidD_delay_value[1], indent_space = 10), 
                   "      incidICU:\n", 
                   "        source: incidH\n", 
                   "        probability:\n", print_value(value_dist = incidICU_prob_dist[1], value_mean = incidICU_prob_value[1], indent_space = 10), 
                   "        delay:\n", print_value(value_dist = incidICU_delay_dist[1], value_mean = incidICU_delay_value[1], indent_space = 10), 
                   "        duration:\n", print_value(value_dist = incidICU_duration_dist[1], value_mean = incidICU_duration_value[1], indent_space = 10), 
                   "          name: icu_curr\n", 
                   "      incidVent:\n", 
                   "        source: incidICU\n", 
                   "        probability: \n", print_value(value_dist = incidVent_prob_dist[1], value_mean = incidVent_prob_value[1], indent_space = 10), 
                   "        delay:\n", print_value(value_dist = incidVent_delay_dist[1], value_mean = incidVent_delay_value[1], indent_space = 10), 
                   "        duration:\n", print_value(value_dist = incidVent_duration_dist[1], value_mean = incidVent_duration_value[1], indent_space = 10), 
                   "          name: vent_curr\n", 
                   "      incidC:\n", 
                   "        source: incidI\n", 
                   "        probability:\n", print_value(value_dist = incidC_prob_dist[1], value_mean = incidC_prob_value[1], 
                                                         value_sd = incidC_prob_sd[1], value_a = incidC_prob_a[1], value_b = incidC_prob_b[1], indent_space = 10), incidC_pert[1], 
                   "        delay:\n", print_value(value_dist = incidC_delay_dist[1], value_mean = incidC_delay_value[1], indent_space = 10)))
        
        if (!is.null(dat)) {
            dat <- dat %>% collapse_intervention() %>% dplyr::filter(type == "outcome")
            if (nrow(dat) > 0) {
                outcome_interventions <- paste0(unique(dat$name), collapse = "\", \"")
                
                cat(paste0("  interventions:\n", 
                           "    settings:\n", 
                           "      ", ifr, ":\n", 
                           "        template: Stacked\n", 
                           "        scenarios: [\"", outcome_interventions, "\"]\n"))
            }
        } 
    }
}






#' Print SEIR Section
#' @description Print seir section with specified parameters.
#'
#' @param sigma_val inverse of the incubation period in days - fraction or probability
#' @param gamma_dist specify if gamma is fixed or distributional
#' @param gamma_val inverse of the infectious period in days - fraction or probability
#' @param gamma_a minimum value of gamma - required if distribution is not "fixed"
#' @param gamma_b maximum value of gamma - required if distribution is not "fixed"
#' @param alpha_val transmission dampening parameter; reasonable values for respiratory viruses range from 0.88-0.99
#' @param R0s_val basic reproduction number
#' @param R0s_dist specify if R0 is fixed or distributional
#' @param R0s_a minimum value of R0 - required if distribution is not "fixed"
#' @param R0s_b maximum value of R0 - required if distribution is not "fixed"
#' @param nu_1_val value of transition from the unvaccinated to the 1st vaccine compartment
#' @param nu_1_dist distribution of nu_1
#' @param nu_1_overlap_operation
#' @param nu_2_val value of transition from the 1st to the 2nd vaccine compartment
#' @param nu_2_dist distribution of nu_2
#' @param nu_2_overlap_operation
#' @param epsilon_val rate at which immunity wanes (i.e. movement from R --> S)
#' @param epsilon_dist distribution of epsilon
#' @param seir_csv 
#' @param gamma_sd 
#' @param R0s_sd 
#' @param ve_data 
#' @param theta_dist 
#' @param nu_3_val 
#' @param nu_3_dist 
#' @param nu_3_overlap_operation 
#' @param chi_vals 
#' @param chi_dist 
#' @param variant_compartments 
#' @param age_strata 
#' @param age_strata_boosters 
#' @param inf_stages 
#' @param use_descriptions 
#' @param vaccine_compartments names of vaccination compartments: defaults to "unvaccinated", "first dose" and "second dose"
#'
#' @export
#'
#' @examples
#' print_seir(seir_csv = "seir_R11.csv",
#'            alpha_val=0.99,
#'            sigma_val = 1/5.2,
#'            gamma_dist = "uniform",
#'            gamma_a = 1/6,
#'            gamma_b = 1/2.6,
#'            r0_dist = "fixed",
#'            r0_val = 2.3,
#'            incl_vacc = FALSE)
#'
print_seir <- function (seir_csv = "seir_R11.csv", 
                        sigma_val = 1/5.2, 
                        gamma_dist = "fixed", gamma_val = 1/3.83, gamma_sd = NULL, gamma_a = 1/4.5, gamma_b = 1/3, 
                        alpha_val = 0.99,
                        R0s_val = 2.3, R0s_dist = "uniform", R0s_sd = NULL, R0s_a = 2, R0s_b = 3,
                        ve_data = ve_data,
                        theta_dist = "fixed",
                        nu_1_val = 0, nu_1_dist = "fixed", nu_1_overlap_operation = "sum",
                        nu_2_val = 1/(3.5 * 7), nu_2_dist = "fixed", nu_2_overlap_operation = "sum",
                        nu_3_val = 0, nu_3_dist = "fixed", nu_3_overlap_operation = "sum",
                        chi_vals = c(1.5, 1.5*1.6, 1.5*1.6*1.25), chi_dist = "fixed",
                        epsilon_val = 0 * 1/(365 * 1.5), epsilon_dist = "fixed",
                        vaccine_compartments = c("unvaccinated", "1dose", "2dose", "waned"),
                        variant_compartments = c("WILD", "ALPHA", "DELTA", "OMICRON"),
                        age_strata = c("age0to17", "age18to64", "age65to100"),
                        age_strata_boosters = c("age0to17", "age18to64","age65to100"),
                        inf_stages = c("S", "E", "I1", "I2", "I3", "R", "W"),
                        use_descriptions = TRUE) {
    
      # get csv of transitions and remove errors (remove quotations and spaces, which screw up parsing)
    seir_dat <- suppressWarnings(suppressMessages(read_csv(seir_csv, lazy = FALSE, progress = FALSE)))
    seir_dat[colnames(seir_dat!="description")] <- apply(seir_dat[colnames(seir_dat!="description")], 2, gsub, pattern=" ", replacement="")
    seir_dat[colnames(seir_dat!="description")] <- apply(seir_dat[colnames(seir_dat!="description")], 2, gsub, pattern='"', replacement='')
    
    
    seir <- ""
    
    seir <- paste0("seir:\n", 
                   "  parameters:\n", 
                   "    sigma: \n", 
                   print_value(value_dist = "fixed", value_mean = sigma_val), 
                   "    alpha: \n", 
                   print_value(value_dist = "fixed", 
                               value_mean = alpha_val), 
                   "    r0: \n", 
                   print_value(value_dist = R0s_dist, 
                               value_mean = R0s_val, 
                               value_sd = R0s_sd, 
                               value_a = R0s_a, 
                               value_b = R0s_b), 
                   "    gamma: \n", 
                   print_value(value_dist = gamma_dist, value_mean = gamma_val, 
                               value_sd = gamma_sd, value_a = gamma_a, value_b = gamma_b))
    
    thetas <- ve_data %>% pull(theta_name)
    theta_vals <- ve_data %>% pull(VE)
    
    
    if (!(length(theta_dist) %in% c(1, length(thetas)))) {
        stop("theta_dist must be assigned 1 value only or a vector of values corresponding to the number of thetas.")
    }
    if (length(theta_dist)==1){
        theta_dist <- rep(theta_dist, length(thetas))
    }
    
    # Add thetas (VEs)
    for (i in 1:length(thetas)) {
        seir <- paste0(seir, 
                       "    ", thetas[i], ":\n", 
                       print_value(value_dist = theta_dist[i], value_mean = paste0(1, " - ", theta_vals[i])))
    }
    
    # Add nus (vaccination rates)
    for (i in 1:length(age_strata)) {
        seir <- paste0(seir, 
                       "    nu1", age_strata[i], ": \n", 
                       "      intervention_overlap_operation: ", nu_1_overlap_operation, "\n", 
                       print_value(value_dis = nu_1_dist, value_mean = nu_1_val))
        
        if (all(nu_2_val==0)){
            seir <- paste0(seir, 
                           "    nu2", age_strata[i], ": \n", 
                           "      intervention_overlap_operation: ", nu_2_overlap_operation, "\n", 
                           print_value(value_dis = nu_2_dist, value_mean = nu_2_val))
        }
        if (any(!is.na(nu_3_val))){
            seir <- paste0(seir, 
                           "    nu3", age_strata[i], ": \n", 
                           "      intervention_overlap_operation: ", nu_3_overlap_operation, "\n", 
                           print_value(value_dis = nu_3_dist, value_mean = nu_3_val))
        }
    }
    if (any(nu_2_val!=0)){
        seir <- paste0(seir, 
                       "    nu2: \n", 
                       "      intervention_overlap_operation: ", nu_2_overlap_operation, "\n", 
                       print_value(value_dis = nu_2_dist, value_mean = nu_2_val))
    }
    
    
    for (i in 1:(length(variant_compartments)-1)) {
        seir <- paste0(seir, 
                       paste0("    chi",i), ": \n", 
                       print_value(value_dist = chi_dist, value_mean = chi_vals[i]))
    }
    seir <- paste0(seir, 
                   "    epsilon: \n", 
                   print_value(value_dist = epsilon_dist, value_mean = epsilon_val))
    
    
    
    # Define Compartments ...........................................
    seir <- paste0(seir, 
                   "  compartments:\n", 
                   "    infection_stage: [", cmprt_list(inf_stages),"] \n", 
                   "    vaccination_stage: [", cmprt_list(vaccine_compartments),"] \n", 
                   "    variant_type: [", cmprt_list(variant_compartments),"] \n", 
                   "    age_strata: [", cmprt_list(age_strata),"]\n", 
                   "  transitions:\n")
    
    
    # Define Transitions .............................................
    
    for (i in 1:nrow(seir_dat)){
        
        seir <- paste0(seir, 
                       ifelse(use_descriptions & 
                                  !(is.na(seir_dat$description[i]) | is.null(seir_dat$description[i]) | seir_dat$description[i]==""), 
                              paste0("# ", seir_dat$transition[i], " - ", seir_dat$description[i], "\n"),""),
                       
                       seir_chunk(SEIR_source = strsplit(seir_dat$SEIR_source[i],",")[[1]],
                                  SEIR_dest = strsplit(seir_dat$SEIR_dest[i],",")[[1]],
                                  vaccine_compartments_source = strsplit(seir_dat$vaccine_compartments_source[i],",")[[1]],
                                  vaccine_compartments_dest = strsplit(seir_dat$vaccine_compartments_dest[i],",")[[1]],
                                  vaccine_infector = strsplit(seir_dat$vaccine_infector[i],",")[[1]],
                                  variant_compartments_source = strsplit(seir_dat$variant_compartments_source[i],",")[[1]],
                                  variant_compartments_dest = strsplit(seir_dat$variant_compartments_dest[i],",")[[1]],
                                  age_strata = strsplit(seir_dat$age_strata[i],",")[[1]],
                                  rate_seir = strsplit(seir_dat$rate_seir[i],",")[[1]],
                                  rate_vacc = strsplit(seir_dat$rate_vacc[i],",")[[1]], 
                                  rate_var = strsplit(seir_dat$rate_var[i],",")[[1]],
                                  rate_age = strsplit(seir_dat$rate_age[i],",")[[1]]))
    }
    
    cat(seir)
}





#' Print Header Section
#' @description Prints the global options and the spatial setup section of the configuration files. These typically sit at the top of the configuration file.
#'
#' @param sim_name name of simulation, typically named after the region/location you are modeling
#' @param smh_round round of Scenario Modeling Hub, for special adjustments. NA if not SMH.
#' @param sim_start_date simulation start date, should match that of interventions, with format YYYY-MM-DD (e.g., 2020-01-31)
#' @param sim_end_date simulation end date with format YYYY-MM-DD (e.g., 2020-01-31)
#' @param end_date_groundtruth end date of the ground truth that is fit to. NA if not limiting ground truth date
#' @param n_simulations number of simulations to run
#' @param dt simulation time step in days
#' @param census_year integer(year)
#' @param base_path base path for spatial files
#' @param sim_states vector of locations that will be modeled
#' @param setup_name spatial folder name
#' @param geodata_file path to file relative to base_path. Geodata is a .csv with column headers, with at least two columns: nodenames and popnodes
#' @param popnodes is the name of a column in geodata that specifies the population of the nodenames column
#' @param nodenames is the name of a column in geodata that specifies the geo IDs of an area. This column must be unique.
#' @param include_in_report is the name of an optional, boolean column in geodata that specifies which nodenames are included in the report. Models may include more locations than simply the location of interest.
#' @param mobility_file path to file relative to base_path. The mobility file is a .csv file (it has to contains .csv as extension) with long form comma separated values. Columns have to be named ori, dest, amount with amount being the amount of individual going from place ori to place dest. Unassigned relations are assumed to be zero. ori and dest should match exactly the nodenames column in geodata.csv. It is also possible, but NOT RECOMMENDED to specify the mobility file as a .txt with space-separated values in the shape of a matrix. This matrix is symmetric and of size K x K, with K being the number of rows in geodata.
#' @param state_level whether this is a state-level run
#'
#' @return
#' @export
#'
#' @examples
#'
print_header <- function (sim_name, 
                          smh_round = NA,
                          sim_start_date, sim_end_date, 
                          end_date_groundtruth = NA, 
                          n_simulations, 
                          dt = 0.025, 
                          census_year = 2019, 
                          base_path = "data",
                          sim_states, 
                          setup_name, 
                          geodata_file = "geodata.csv", 
                          mobility_file = "mobility.csv", 
                          popnodes = "pop2019est", 
                          nodenames = "geoid",
                          include_in_report = "include_in_report", 
                          state_level = TRUE) 
{
    cat(paste0("name: ", sim_name, "\n", 
               ifelse(!is.na(smh_round), paste0("smh_round: ", smh_round, "\n"), ""),
               "start_date: ", sim_start_date, "\n", 
               "end_date: ", sim_end_date, "\n", 
               ifelse(!is.na(end_date_groundtruth), paste0("end_date_groundtruth: ", end_date_groundtruth, "\n"), ""),
               "nsimulations: ", n_simulations, "\n", 
               "dt: ", sprintf(fmt="%#.3f",as.numeric(dt)), "\n", 
               "\n", 
               "spatial_setup:\n", 
               "  census_year: ", census_year, "\n", 
               "  base_path: ", base_path, "\n", 
               "  modeled_states:\n"), 
        paste0("   - ", sim_states, "\n"), 
        paste0(" setup_name: ", setup_name, "\n", 
               "  geodata: ", geodata_file, "\n", 
               "  mobility: ", mobility_file, "\n", 
               "  popnodes: ", popnodes, "\n", 
               "  nodenames: ", nodenames, "\n", 
               "  include_in_report: ", include_in_report, "\n", 
               "  state_level: ", state_level, "\n"))
}



#' Print seeding section
#' @description Prints the seeding section of the configuration file
#' @param method There are two different seeding methods: 1) based on air importation (FolderDraw) and 2) based on earliest identified cases (PoissonDistributed). FolderDraw is required if the importation section is present and requires folder_path. Otherwise, put PoissonDistributed, which requires lambda_file.
#' @param seeding_file_type indicates which seeding file type the SEIR model will look for, "seed", which is generated from inference::create_seeding.R, or "impa", which refers to importation
#' @param folder_path path to folder where importation inference files will be saved
#' @param lambda_file path to seeding file
#' @param date_sd standard deviation for the proposal value of the seeding date, in number of days (date_sd )
#' @param amount_sd
#' @param variant_filename path to file with variant proportions per day per variant. Variant names: 'wild', 'alpha', 'delta'
#' @param compartment whether to print config with compartments
#' @param variant_compartments vector of variant compartment names
#'
#' @details
#' ## The model performns inference on the seeding date and initial number of seeding infections in each geoid with the default settings
#' ## The method for determining the proposal distribution for the seeding amount is hard-coded in the inference package (R/pkgs/inference/R/functions/perturb_seeding.R). It is pertubed with a normal distribution where the mean of the distribution 10 times the number of confirmed cases on a given date and the standard deviation is 1.
#'
#' @export
#'
#' @examples
#'
print_seeding <- function (method = "FolderDraw", 
                           seeding_file_type = "seed", 
                           folder_path = "importation/minimal/", 
                           lambda_file = "data/minimal/seeding.csv", 
                           population_file = "data/seeding_agestrat.csv", 
                           date_sd = 1, 
                           amount_sd = 1, 
                           variant_filename = "data/variant/variant_props_long.csv", 
                           compartment = TRUE, 
                           variant_compartments = c("WILD", "ALPHA", "DELTA"), 
                           vaccine_compartments = c("unvaccinated", "1dose", "2dose", "waned"), 
                           age_strata_seed = "0_64"){
    
    variant_compartments <- stringr::str_to_upper(variant_compartments)
    seeding_comp <- "\nseeding:\n"
    if (compartment) {
        age_strata_seed <- paste0("age", age_strata_seed)
        seeding_comp <- paste0(seeding_comp, 
                               "  variant_filename: ", variant_filename, "\n", 
                               "  seeding_compartments:\n")
        for (i in 1:length(variant_compartments)) {
            seeding_comp <- paste0(seeding_comp, "    ", variant_compartments[i], ":\n", 
                                   "      source_compartment: [\"S\", \"unvaccinated\", \"", stringr::str_to_upper(variant_compartments[1]), "\", \"", age_strata_seed, "\"]\n", 
                                   "      destination_compartment: [\"E\", \"unvaccinated\", \"", stringr::str_to_upper(variant_compartments[i]), "\", \"", age_strata_seed, "\"]\n")
        }
    }
    seeding <- paste0(seeding_comp, 
                      "  method: ", method, "\n", 
                      "  seeding_file_type: ", seeding_file_type, "\n", 
                      "  folder_path: ", folder_path, "\n", 
                      "  lambda_file: ", lambda_file, "\n", 
                      if (compartment) paste0("  pop_seed_file: ", population_file, "\n"),
                      "  date_sd: ", date_sd, "\n", 
                      "  amount_sd: ", amount_sd, "\n", 
                      "\n")
    cat(seeding)
}



#' Print filtering and filtering::statistics
#' @description Set settings for the filtering section and its statistics component
#'
#' @param sims_per_slot number of iterations in a single MCMC inference chain With inference model runs, the number of simulations nsimulations refers to the number of final model simulations that will be produced. The sims_per_slot setting refers to the number of iterative simulations that will be run in order to produce a single final simulation (i.e., number of simulations in a single MCMC chain).
#' @param do_filtering whether to perform inference
#' @param data_path file path where observed data are saved
#' @param gt_source source of data
#' @param gt_source_statistics
#' @param misc_data_filename
#' @param stat_names the names of the statistics used to calibrate the model to empirical data
#' @param aggregator function used to aggregate data over the period, usually sum or mean
#' @param period duration over which data should be aggregated prior to use in the likelihood, may be specified in any number of days, weeks, months (e.g., "1 weeks")
#' @param sim_var column name where model data can be found, from the hospitalization outcomes files
#' @param data_var column where data can be found in data_path file; used in inference
#' @param remove_na logical
#' @param add_one logical, TRUE if evaluating the log likelihood
#' @param ll_dist distribution of the likelihood: "sqrtnorm" or "pois"
#' @param ll_param parameter value(s) for the likelihood distribution; not used if ll_dist = "pois". See [inference::logLikStat()]
#' @param final_print whether this is the final section of the config to print an empty space; set to FALSE if running [print_hierarchical()] and/or [print_prior()]
#' @param compartment
#' @param stat_names_compartment 
#' @param sim_var_compartment 
#' @param data_var_compartment 
#' @param variant_compartments 
#'
#' @details
#' The filtering section configures the settings for the inference algorithm, while the statistics component determines how the model is calibrated.
#' With inference model runs, the number of simulations n_simulations in [print_header()] refers to the number of final model simulations that will be produced. The sims_per_slot setting refers to the number of iterative simulations that will be run in order to produce a single final simulation (i.e., number of simulations in a single MCMC chain).
#' The statistics specified here are used to calibrate the model to empirical data. If multiple statistics are specified, this inference is performed jointly and they are weighted in the likelihood according to the number of data points and the variance of the proposal distribution.
#' @export
#'
#' @examples
#' print_filtering_statistics()
#'
print_filtering_statistics <- function(sims_per_slot = 300, 
                                       do_filtering = TRUE, 
                                       data_path = "data/us_data.csv", 
                                       gt_source = "csse", 
                                       gt_source_statistics = NULL, misc_data_filename = NULL, 
                                       aggregator = "sum", 
                                       period = "1 weeks", 
                                       stat_names = c("sum_deaths", "sum_confirmed"), 
                                       stat_names_compartment = c("sum_confirmed"),
                                       sim_var = c("incidD", "incidC"), 
                                       sim_var_compartment = c("incidC"), 
                                       data_var = c("incidDeath","incidI"), 
                                       data_var_compartment = c("incidI"), 
                                       remove_na = FALSE, add_one = c(FALSE, TRUE), 
                                       ll_dist = c("sqrtnorm", "pois"), 
                                       ll_param = 0.4, final_print = FALSE, 
                                       compartment = TRUE, 
                                       variant_compartments = c("WILD", "ALPHA", "DELTA")) {
    
    if (length(stat_names) != length(data_var)) stop("stat_names and data_var must be the same length")
    
    cat(paste0("\n", 
               "filtering:\n", 
               "  simulations_per_slot: ", sims_per_slot, "\n", 
               "  do_filtering: ", do_filtering, "\n", 
               "  data_path: ", data_path, "\n", 
               "  gt_source: \"", gt_source, "\"\n", {
                   if (!is.null(misc_data_filename)) 
                       paste0("  misc_data_filename: ", misc_data_filename, "\n")
                   else (paste0(""))
               }, "  statistics:\n"))
    
    if (compartment) {
        
        #which outcome is not compartment disaggregated
        not_compartment <- which(!(stat_names %in% stat_names_compartment))
        yes_compartment <- which((stat_names %in% stat_names_compartment))
        
        stat_names_orig <- stat_names
        
        stat_names <- stat_names[!(stat_names %in% stat_names_compartment)]
        sim_var <- sim_var[!(sim_var %in% sim_var_compartment)]
        data_var <- data_var[!(data_var %in% data_var_compartment)]
        
        variant_compartments <- stringr::str_to_upper(variant_compartments)
        
        if (!(any(c(is.null(stat_names_compartment), is.na(stat_names_compartment))))){
            stat_names_compartment <- paste(rep(stat_names_compartment, each = length(variant_compartments)), variant_compartments, sep = "_")
            sim_var_compartment <- paste(rep(sim_var_compartment, each = length(variant_compartments)), stringr::str_to_upper(variant_compartments), sep = "_")
            data_var_compartment <- paste(rep(data_var_compartment, each = length(variant_compartments)), variant_compartments, sep = "_")
            for (i in 1:length(variant_compartments)) {
                stat_names_compartment <- c(stat_names_compartment[stringr::str_detect(stat_names_compartment, variant_compartments[i], negate = TRUE)], 
                                            stat_names_compartment[stringr::str_detect(stat_names_compartment, variant_compartments[i], negate = FALSE)])
                sim_var_compartment <- c(sim_var_compartment[stringr::str_detect(sim_var_compartment, stringr::str_to_upper(variant_compartments[i]), negate = TRUE)], 
                                         sim_var_compartment[stringr::str_detect(sim_var_compartment, stringr::str_to_upper(variant_compartments[i]), negate = FALSE)])
                data_var_compartment <- c(data_var_compartment[stringr::str_detect(data_var_compartment, variant_compartments[i], negate = TRUE)], 
                                          data_var_compartment[stringr::str_detect(data_var_compartment, variant_compartments[i], negate = FALSE)])
            }
        }
        
        n_vars <- length(stat_names)
        aggregator_nocomp <- rep(aggregator, n_vars)
        period_nocomp <- rep(period, n_vars)
        remove_na_nocomp <- rep(remove_na, n_vars)
        add_one_nocomp <- rep(add_one[not_compartment], n_vars)
        ll_dist_nocomp <- rep(ll_dist[not_compartment], n_vars)
        ll_param_nocomp <- rep(ll_param[not_compartment], n_vars)
        gt_source_statistics_nocomp <- rep(gt_source_statistics, n_vars)
        
        n_vars <- length(stat_names_compartment)
        aggregator_comp <- rep(aggregator, n_vars)
        period_comp <- rep(period, n_vars)
        remove_na_comp <- rep(remove_na, n_vars)
        add_one_comp <- rep(add_one[yes_compartment], n_vars)
        ll_dist_comp <- rep(ll_dist[yes_compartment], n_vars)
        ll_param_comp <- rep(ll_param[yes_compartment], n_vars)
        gt_source_statistics_comp <- rep(gt_source_statistics, n_vars)
        
        if (is.null(gt_source_statistics)) {
            gt_source_statistics <- gt_source
        }
        
        aggregator <- c(aggregator_nocomp, aggregator_comp)
        period <- c(period_nocomp, period_comp)
        remove_na <- c(remove_na_nocomp, remove_na_comp)
        add_one <- c(add_one_nocomp, add_one_comp)
        ll_dist <- c(ll_dist_nocomp, ll_dist_comp)
        ll_param <- c(ll_param_nocomp, ll_param_comp)
        gt_source_statistics <- c(gt_source_statistics_nocomp, gt_source_statistics_comp)
        
        stat_names <- c(stat_names, stat_names_compartment)
        sim_var <- c(sim_var, sim_var_compartment)
        data_var <- c(data_var, data_var_compartment)
        
    }
    
    
    n_vars <- length(stat_names)
    if (length(aggregator) != n_vars) {
        aggregator <- rep(aggregator, n_vars)
    }
    if (length(period) != n_vars) {
        period <- rep(period, n_vars)
    }
    if (length(remove_na) != n_vars) {
        remove_na <- rep(remove_na, n_vars)
    }
    if (length(add_one) != n_vars) {
        add_one <- rep(add_one, n_vars)
    }
    if (length(ll_dist) != n_vars) {
        ll_dist <- rep(ll_dist, n_vars)
    }
    if (length(ll_param) != n_vars) {
        ll_param <- rep(ll_param, n_vars)
    }
    if (is.null(gt_source_statistics)) {
        gt_source_statistics <- gt_source
    }
    if (length(gt_source_statistics) != n_vars) {
        gt_source_statistics <- rep(gt_source_statistics, n_vars)
    }
    for (i in 1:length(stat_names)) {
        cat(paste0("    ", stat_names[i], ":\n", 
                   "      name: ", stat_names[i], "\n", 
                   "      aggregator: ", aggregator[i], "\n", 
                   "      period: \"", period[i], "\"\n", 
                   "      gt_source: \"", gt_source_statistics[i], "\"\n", 
                   "      sim_var: ", sim_var[i], "\n", 
                   "      data_var: ", data_var[i], "\n", 
                   "      remove_na: ", remove_na[i], "\n", 
                   "      add_one: ", add_one[i], "\n", 
                   "      likelihood:\n", 
                   "        dist: ", ll_dist[i], "\n"))
        if (ll_dist[i] != "pois") {
            cat(paste0("        param: [", ll_param[i], "]\n"))
        }
    }
    if (final_print) {
        cat(paste0("\n"))
    }
}





#' Print filtering::hierarchical_stats_geo
#' @description Specify and print hierarchical settings as part of the filtering section of the configuration file.
#' @param npi_name vector of names of the estimated parameters that will be grouped (e.g., the NPI scenario name or a standardized, combined health outcome name like probability_incidI_incidC)
#' @param module vector of names of the module where this parameter is estimated (important for finding the appropriate files)
#' @param geo_group_col geodata column name that should be used to group parameter estimation
#' @param transform type of transform that should be applied to the likelihood: "none" or "logit"
#' @param final_print whether this is the final section of the config to print an empty space; set to FALSE if running [print_hierarchical()] and/or [print_prior()]
#' @param compartment
#' @param variant_compartments
#'
#' @details
#' This function should only be called after [print_hierarchical()].
#'
#' The hierarchical settings specified here are used to group the inference of certain parameters together (similar to inference in "hierarchical" or "fixed/group effects" models). For example, users may desire to group all counties in a given state because they are geograhically proximate and impacted by the same statewide policies. The effect should be to make these inferred parameters follow a normal distribution and to observe shrinkage among the variance in these grouped estimates.
#' @export
#'
#' @examples
#' print_filtering_hierarchical()

print_filtering_hierarchical <- function(npi_name = c("local_variance", "probability_incidI_incidC"),
                                         compartment = TRUE,
                                         variant_compartments = c("WILD", "ALPHA", "DELTA"),
                                         module = c("seir", "hospitalization"),
                                         geo_group_col = "USPS",
                                         transform = c("none", "logit"),
                                         empty_print = FALSE,
                                         final_print = FALSE){
    
    variant_compartments <- stringr::str_to_upper(variant_compartments)
    
    if(compartment){
        not_variance <- npi_name!="local_variance"
        
        npi_name <- c(npi_name[!not_variance], paste0(rep(npi_name[not_variance], each = length(variant_compartments)), "_", variant_compartments))
        
        transform <- c(transform[!not_variance], rep(transform[not_variance], each = length(variant_compartments)))
        
        module <- c(module[!not_variance], rep(module[not_variance], each = length(variant_compartments)))
        
    }
    
    cat(paste0(
        "  hierarchical_stats_geo:\n"
    ))
    
    n_vars <- length(npi_name)
    
    if(length(module)!=n_vars & length(module==1)){
        module <- rep(module, n_vars)
    }
    
    if(length(geo_group_col)!=n_vars & length(geo_group_col)==1){
        geo_group_col <- rep(geo_group_col, n_vars)
    }
    
    if(length(transform)!=n_vars & length(transform)==1){
        transform <- rep(transform, n_vars)
    }
    
    if(!empty_print){
        for(i in 1:n_vars){
            cat(paste0(
                "    ", paste0(npi_name[i], "_hierarchy:\n"),
                "      name: ", npi_name[i], "\n",
                "      module: ", module[i], "\n",
                "      geo_group_col: ", geo_group_col[i], "\n",
                "      transform: ", transform[i], "\n"
            ))
        }
    }
    
    if(final_print){
        cat(paste0("\n"))
    }
    
}
#' print_filtering_prior
#'
#' @description Set and print prior values for inferred parameters
#'
#' @param npi_name vector of names of NPI scenario or parameter that will have the prior
#' @param module name of the module where this parameter is estimated: "seir", "outcomes_interventions", "outcomes_parameters" or "hospitalization".
#' @param dist string or vector of distribution of priors, "normal" or "logit_normal" are supported. If string is provided, then the same distribution is used across all priors.
#' @param param_mean string or vector of prior means. Defaults to NULL and takes intervention mean for the corresponding NPI scenario from dat. If string is provided, then the same mean value is used across all priors.
#' @param param_sd string or vector of prior sd. If string is provided, then the same distribution is used across all priors.
#' @param dat dataframe with intervention names (npi_name) and means (value_mean); required if param_mean is NULL
#'
#' @details
#' Specifying prior values for inferred parameters will speed up model convergence
#' @export
#'
#' @examples
#' print_filtering_prior()
#' 
print_filtering_prior <- function(npi_name = c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar", "Seas_apr",
                                               "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
                                               "Seas_oct", "Seas_nov", "Seas_dec"),
                                  module = "seir",
                                  dist = "normal",
                                  param_mean = c(0.000, -0.200, -0.133, -0.067, 0.00,  0.067, 0.133,  0.200, 0.133,  0.067,  0.000, -0.067, -0.133),
                                  param_sd = 1,
                                  empty_print = FALSE, 
                                  dat=NULL
){
    module <- repeat_string(module, npi_name)
    dist <- repeat_string(dist, npi_name)
    param_sd <- repeat_string(param_sd, npi_name)
    param_mean <- repeat_string(param_mean, npi_name)
    
    if(is.null(param_mean)){ # TODO: allow to specify priors for some, take NPI means for others
        if(is.null(dat)) stop("Dataframe with intervention names (npi_name) and means (value_mean) must be provided if param_mean is NULL")
        
        dat <- dat %>%
            collapse_intervention() %>%
            dplyr::filter(name %in% npi_name) %>%
            dplyr::mutate(value_mean = dplyr::if_else(is.na(value_mean), 0, value_mean))
        
        for(i in 1:length(npi_name)){
            param_mean[i] <- dat %>%
                dplyr::filter(name == npi_name[i]) %>%
                dplyr::pull(value_mean)
            
        }
    }
    
    cat(paste0(
        "  priors:\n"))
    
    if(!empty_print){
        for(i in 1:length(npi_name)){
            cat(paste0(
                "    ", paste0(npi_name[i], "_prior"),":\n",
                "      name: ", npi_name[i], "\n",
                "      module: ", module[i], "\n",
                "      likelihood:\n",
                "        dist: ", dist[i], "\n",
                "        param:\n",
                "        - ", param_mean[i], "\n",
                "        - ", param_sd[i], "\n"
            ))
        }
    } else{
        cat(paste0("\n"))
    }
}

#' Convenience function to repeat string x to match length of vector y
#'
#' @param x string
#' @param y vector
#'
#' @return vector of length y with values x
#' @export
#'
#' @examples
#'
repeat_string <- function(x,
                          y){
    if(length(x)==length(y) | is.null(x)){
        z <- x
    } else if(length(x)!=length(y) & length(x)==1){
        z <- rep(x, length(y))
    } else {
        stop(paste0("x must be of length 1 or a vector of equal length as y"))
    }
    
    return(z)
}




#' Title
#'
#' @param compartments 
#'
#' @return
#' @export
#'
#' @examples
cmprt_list <- function(compartments = c("S","E","I","R")){
    return(paste0("\"", paste(compartments, collapse = "\", \""),"\""))
}


#' Title
#'
#' @param source 
#' @param dest 
#'
#' @return
#' @export
#'
#' @examples
cmprt_bracketing <- function(source, dest){
    
    source_part <- cmprt_list(source)
    dest_part <- cmprt_list(dest)
    if(length(source)>=length(dest)){
        source_part <- paste0("[", source_part,"]")
    }
    if(length(dest)>=length(source)){
        dest_part <- paste0("[", dest_part,"]")
    }
    return(list(source_part, dest_part))
}


#' Title
#'
#' @param rate 
#' @param length_comprt 
#'
#' @return
#' @export
#'
#' @examples
cmprt_rate_bracketing <- function(rate, length_comprt){
    rate_part <- cmprt_list(rate)
    if(length(rate)==length_comprt){
        rate_part <- paste0("[", rate_part,"]")
    }
    return(rate_part)
}


#' Title
#'
#' @param SEIR_source 
#' @param SEIR_dest 
#' @param vaccine_compartments_source 
#' @param vaccine_compartments_dest 
#' @param variant_compartments_source 
#' @param variant_compartments_dest 
#' @param age_strata 
#' @param vaccine_infector 
#' @param rate_seir 
#' @param rate_vacc 
#' @param rate_var 
#' @param rate_age 
#'
#' @return
#' @export
#'
#' @examples
seir_chunk <- function(SEIR_source = c("R","W"),
                       SEIR_dest = "E",
                       vaccine_compartments_source = c("2dose", "previousinfection"),
                       vaccine_compartments_dest = c("2dose", "previousinfection"),
                       variant_compartments_source = c("WILD", "ALPHA", "DELTA"),
                       variant_compartments_dest = c("OMICRON"),
                       age_strata=c("age0to17", "age18to64", "age65to100"),
                       vaccine_infector = c("unvaccinated", "1dose", "2dose", "waned", "previousinfection"),
                       rate_seir=rep("r0 * gamma", 2),
                       rate_vacc=c("theta2_OMICRON", "theta2_OMICRON"), 
                       rate_var=c("chi3","chi3","chi3"),
                       rate_age=rep(1,3)){
    
    seir_parts <- cmprt_bracketing(SEIR_source, SEIR_dest)
    vacc_parts <- cmprt_bracketing(vaccine_compartments_source, vaccine_compartments_dest)
    variant_parts <- cmprt_bracketing(variant_compartments_source, variant_compartments_dest)
    
    rate_propexp_parts <- cmprt_rate_bracketing(rep("1",length(SEIR_source)), max(length(SEIR_source), length(SEIR_dest)))
    rate_alpha_parts <- cmprt_rate_bracketing(rep("alpha",length(SEIR_source)), max(length(SEIR_source), length(SEIR_dest)))
    
    rate_seir_parts <- cmprt_rate_bracketing(rate_seir, max(length(SEIR_source), length(SEIR_dest)))
    rate_vacc_parts <- cmprt_rate_bracketing(rate_vacc, max(length(vaccine_compartments_source), length(vaccine_compartments_dest)))
    rate_var_parts <- cmprt_rate_bracketing(rate_var, max(length(variant_compartments_source), length(variant_compartments_dest)))
    rate_age_parts <- cmprt_rate_bracketing(rate_age, max(length(age_strata), length(1)))
    
    incl_agestrat <- any(!is.na(age_strata) & !is.null(age_strata))
    
    # check rates
    if(!(length(rate_vacc) %in% c(1, length(vaccine_compartments_source), length(vaccine_compartments_dest)))){
        stop("rate_vacc needs to be length==1 or the same length\nas vaccine_compartments_source or vaccine_compartments_dest")}
    if(!(length(rate_var) %in% c(1, length(variant_compartments_source), length(variant_compartments_dest)))){
        stop("rate_var needs to be length==1 or the same length\nas variant_compartments_source or variant_compartments_dest")}
    if(!(length(rate_age) %in% c(1, length(age_strata)))){
        stop("rate_age needs to be length==1 or the same length as age_strata")}
    
    tmp <- paste0(
        "    - source: [", seir_parts[[1]], ", ", vacc_parts[[1]], ", ", variant_parts[[1]], ifelse(incl_agestrat, paste0(", ", "[", cmprt_list(age_strata),"]"), ""), "] \n", 
        "      destination: [", seir_parts[[2]], ", ", vacc_parts[[2]], ", ", variant_parts[[2]], ifelse(incl_agestrat, paste0(", ", "[", cmprt_list(age_strata),"]"), ""), "] \n", 
        ifelse(any(!is.null(vaccine_infector) & !is.na(vaccine_infector)),
               paste0(
                   "      proportional_to: [\n", 
                   "        \"source\",\n", 
                   "        [\n",
                   "          [",paste(rep("[\"I1\",\"I2\",\"I3\"]", length(SEIR_source)), collapse = ",\n           "),"],\n", 
                   "          [",paste(rep(paste0("[",cmprt_list(vaccine_infector),"]"), length(vaccine_compartments_source)), collapse=",\n           "),"],\n", 
                   "          [",paste(rep(paste0("[\"",paste(variant_compartments_dest, collapse = "\"], [\""),"\"]"), 
                                           ifelse(length(variant_compartments_dest)>=length(variant_compartments_source), 
                                                  1, max(c(length(variant_compartments_source),length(variant_compartments_dest))))), collapse=", "),"],\n", 
                   "          [",paste(rep(paste0("[",cmprt_list(age_strata),"]"), length(age_strata)), collapse=",\n           "),"]\n", 
                   "        ]\n", 
                   "      ]\n", 
                   "      proportion_exponent: [\n",
                   "        [",rate_propexp_parts, "\"1\",\"1\",\"1\"],\n",
                   "        [",rate_alpha_parts, "\"1\",\"1\",\"1\"]]\n", 
                   "      rate: [\n",
                   "        ",rate_seir_parts,",\n",
                   "        ",rate_vacc_parts,",\n",
                   "        ",rate_var_parts,",\n",
                   "        ",rate_age_parts,"\n",
                   "      ]\n"),
               paste0(
                   "      proportional_to: [\"source\"],\n", 
                   "      proportion_exponent: [[\"1\",\"1\",\"1\",\"1\"]],\n",
                   "      rate: [",rate_seir_parts,", ", rate_vacc_parts,", ", rate_var_parts,", ", rate_age_parts,"]\n")), 
        "\n")
    
    return(tmp)
}
