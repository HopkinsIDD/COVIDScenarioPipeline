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
#'
#' @return
#' @export
#'
#' @examples
#'
yaml_stack <- function(dat,
                       scenario = "Inference"
){

    dat<-dat %>%
        dplyr::group_by(category, USPS, geoid) %>%
        dplyr::filter(category == "NPI_redux" & period == max(period)) %>%
        dplyr::bind_rows(dat %>%
                             dplyr::filter(category != "NPI_redux")) %>%
        dplyr::distinct(name, category) %>%
        dplyr::group_by(category) %>%
        dplyr::summarize(name = paste0(name, collapse = '", "'))

    for(i in 1:nrow(dat)){
        if(dat$category[i] %in% c("local_variance", "NPI_redux")){next}
        cat(paste0(
            "    ", dat$category[i], ":\n",
            "      template: Stacked\n",
            '      scenarios: ["', dat$name[i], '"]\n'
        ))

    }

    dat <- dat %>%
        dplyr::filter(category != "base_npi") %>%
        dplyr::mutate(category = dplyr::if_else(category == "NPI_redux", name, category))

    cat(paste0(
        "    ", scenario, ":\n",
        "      template: Stacked\n",
        '      scenarios: ["', paste0(dat$category, collapse='", "'), '"]\n'
    ))
}

#' Print Interventions Section
#'
#' @description Print transmission and outcomes interventions and stack them
#'
#' @param dat dataframe with processed intervention names/periods; see collapsed_interventions
#' @param scenario name of the scenario
#' @param compartment
#'
#' @return
#' @export
#'
#' @examples
#'

print_interventions <- function(dat,
                                scenario = "Inference",
                                compartment = TRUE
){

    cat(paste0(
        '\ninterventions:\n',
        '  scenarios:\n',
        '    - ', scenario, '\n',
        '  settings:\n'
    ))

    outcome_dat <- dat %>%
        collapse_intervention() %>%
        dplyr::filter(type=="outcome")

    dat <- collapse_intervention(dat) %>%
        dplyr::filter(type == "transmission")


    for(i in 1:nrow(dat)){

        if(i > nrow(dat)) break

        if(dat$template[i]=="MultiTimeReduce"){
            dat %>%
                dplyr::filter(name == dat$name[i]) %>%
                yaml_mtr_template(.)

            dat <- dat %>%
                dplyr::filter(name != dat$name[i] | dplyr::row_number() == i)
        } else{
            yaml_reduce_template(dat[i,])
        }


    }

    yaml_stack(dat,
               scenario)

    if(nrow(outcome_dat) > 0){
        if(compartment){
            yaml_stack(outcome_dat,
                       "outcome_interventions")

            cat(paste0("\n"))
        }

        for(i in 1:nrow(outcome_dat)){
            if(outcome_dat$template[i]=="Reduce"){
                yaml_reduce_template(outcome_dat[i,])
            }
        }
        cat(paste0("\n"))
    }
}

#' Print Outcomes Section
#'
#' @param dat=NULL df with outcome interventions
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
#' @param outcomes_included which outcomes to include, options: incidH, incidC, incidD, incidICU, incidVent.
#' @param incl_interventions
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

print_outcomes <- function(dat=NULL,
                           ifr=NULL,
                          outcomes_parquet_file="usa-geoid-params-output_statelevel.parquet",
                          incidH_prob_dist=c("fixed", "fixed", "fixed"),
                          incidH_prob_value=c(0.0175, 0.0175, 0.0175),
                          incidH_delay_dist= c("fixed", "fixed", "fixed"),
                          incidH_delay_value= c(7, 7, 7),
                          incidH_duration_dist=c("fixed", "fixed", "fixed"),
                          incidH_duration_value=c(7, 7, 7),
                          incidD_prob_dist=c("fixed", "fixed", "fixed"),
                          incidD_prob_value=c(0.005, 0.005, 0.005),
                          incidD_delay_dist=c("fixed", "fixed", "fixed"),
                          incidD_delay_value=c(20, 20, 20),
                          incidICU_prob_dist="fixed",
                          incidICU_prob_value=0.167,
                          incidICU_delay_dist="fixed",
                          incidICU_delay_value=3,
                          incidICU_duration_dist="fixed",
                          incidICU_duration_value=8,
                          incidVent_prob_dist="fixed",
                          incidVent_prob_value=0.463,
                          incidVent_delay_dist="fixed",
                          incidVent_delay_value= 1,
                          incidVent_duration_dist="fixed",
                          incidVent_duration_value=7,
                          incidC_prob_dist=c("truncnorm", "truncnorm", "truncnorm"),
                          incidC_prob_value=c(0.2, 0.2, 0.2),
                          incidC_prob_sd=c(.1, .1, .1),
                          incidC_prob_a=c(0, 0, 0),
                          incidC_prob_b=c(1, 1, 1),
                          incidC_perturbation = c(TRUE, TRUE, TRUE),
                          incidC_prob_dist_pert=c("truncnorm", "truncnorm", "truncnorm"),
                          incidC_prob_value_pert=c(0, 0, 0),
                          incidC_prob_sd_pert=c(0.05, 0.05, 0.05),
                          incidC_prob_a_pert=c(-1, -1, -1),
                          incidC_prob_b_pert=c(1, 1, 1),
                          incidC_delay_value=c(7, 7, 7),
                          incidC_delay_dist=c("fixed","fixed", "fixed"),
                          compartment = TRUE,
                          variant_compartments = c("wild", "alpha", "delta"),
                          vaccine_compartments = c("unvaccinated", "1dose", "2dose"),
                          outcomes_included = c("incidH", "incidD", "incidC", "incidICU", "incidVent"),
                          incl_interventions = TRUE
){

    if(is.null(ifr)){stop("You must specify a scenario/IFR name.")}

    incidC_pert <- ""

    for(i in 1:length(variant_compartments)){
        if(length(incidC_perturbation)==1){
            incidC_perturbation <- rep(incidC_perturbation, length(variant_compartments))
        }
        if(incidC_perturbation[i]){
            incidC_pert[i] <- print_value(value_dist = incidC_prob_dist_pert[i],
                                          value_mean = incidC_prob_value_pert[i],
                                          value_sd = incidC_prob_sd_pert[i],
                                          value_a = incidC_prob_a_pert[i],
                                          value_b = incidC_prob_b_pert[i],
                                          param_name = "perturbation",
                                          indent_space=10)
        } else{
            incidC_pert[i] <- ""
        }

    }

    if(compartment){

        outcomes <- paste0('\n',
                           'outcomes:\n',
                           '  method: delayframe\n',
                           '  param_from_file: TRUE\n',
                           '  param_place_file: "', outcomes_parquet_file, '"\n',
                           '  scenarios:\n',
                           '    - ',ifr,'\n',
                           '  settings:\n',
                           '    ',ifr,':\n')

        incidH <- ""
        incidD <- ""
        incidC <- ""

        for(i in 1:length(variant_compartments)){
            incidH <- paste0(incidH,
                             '      incidH_', stringr::str_to_upper(variant_compartments[i]), ':\n',
                             '        source:\n',
                             '          incidence:\n',
                             '            infection_stage: ["I1"]\n',
                             '            vaccination_stage: ["', paste0(vaccine_compartments, collapse = '", "'),'"]\n',
                             '            variant_type: ["',stringr::str_to_upper(variant_compartments[i]),'"]\n',
                             '        probability: \n',
                             print_value(value_dist = incidH_prob_dist[i],
                                         value_mean = incidH_prob_value[i],
                                         indent_space = 10),
                             '        delay:\n',
                             print_value(value_dist = incidH_delay_dist[i],
                                         value_mean = incidH_delay_value[i],
                                         indent_space = 10),
                             '        duration:\n',
                             print_value(value_dist = incidH_duration_dist[i],
                                         value_mean = incidH_duration_value[i],
                                         indent_space = 10),
                             '          name: hosp_curr_', stringr::str_to_upper(variant_compartments[i]),'\n'
            )

            incidD <- paste0(incidD,
                             '      incidD_', stringr::str_to_upper(variant_compartments[i]), ':\n',
                             '        source:\n',
                             '          incidence:\n',
                             '            infection_stage: ["I1"]\n',
                             '            vaccination_stage: ["', paste0(vaccine_compartments, collapse = '", "'),'"]\n',
                             '            variant_type: ["',stringr::str_to_upper(variant_compartments[i]),'"]\n',
                             '        probability:\n',
                             print_value(value_dist = incidD_prob_dist[i],
                                         value_mean = incidD_prob_value[i],
                                         indent_space = 10),
                             '        delay:\n',
                             print_value(value_dist = incidD_delay_dist[i],
                                         value_mean = incidD_delay_value[i],
                                         indent_space = 10)
            )

            incidC <- paste0(incidC,
                             '      incidC_', stringr::str_to_upper(variant_compartments[i]), ':\n',
                             '        source:\n',
                             '          incidence:\n',
                             '            infection_stage: ["I1"]\n',
                             '            vaccination_stage: ["', paste0(vaccine_compartments, collapse = '", "'),'"]\n',
                             '            variant_type: ["',stringr::str_to_upper(variant_compartments[i]),'"]\n',
                             '        probability:\n',
                             print_value(value_dist = incidC_prob_dist[i],
                                         value_mean = incidC_prob_value[i],
                                         value_sd = incidC_prob_sd[i],
                                         value_a = incidC_prob_a[i],
                                         value_b = incidC_prob_b[i],
                                         indent_space=10),
                             incidC_pert[i],
                             '        delay:\n',
                             print_value(value_dist = incidC_delay_dist[i],
                                         value_mean = incidC_delay_value[i],
                                         indent_space = 10)
            )
        }

        incidICU_source <- paste0("'incidH_", stringr::str_to_upper(variant_compartments), "'") %>% paste0(collapse = ', ')

        incidICU <- paste0('      incidH:\n',
                           '        sum: [', incidICU_source, ']\n',
                           '      incidICU:\n',
                           '        source: incidH\n',
                           '        probability:\n',
                           print_value(value_dist = incidICU_prob_dist,
                                       value_mean = incidICU_prob_value,
                                       indent_space = 10),
                           '        delay:\n',
                           print_value(value_dist = incidICU_delay_dist,
                                       value_mean = incidICU_delay_value,
                                       indent_space = 10),
                           '        duration:\n',
                           print_value(value_dist = incidICU_duration_dist,
                                       value_mean = incidICU_duration_value,
                                       indent_space = 10),
                           '          name: icu_curr\n')

        incidVent <- paste0('      incidVent:\n',
                            '        source: incidICU\n',
                            '        probability: \n',
                            print_value(value_dist = incidVent_prob_dist,
                                        value_mean = incidVent_prob_value,
                                        indent_space = 10),
                            '        delay:\n',
                            print_value(value_dist = incidVent_delay_dist,
                                        value_mean = incidVent_delay_value,
                                        indent_space = 10),
                            '        duration:\n', # TODO: optional
                            print_value(value_dist = incidVent_duration_dist,
                                        value_mean = incidVent_duration_value,
                                        indent_space = 10),
                            '          name: vent_curr\n'
        )

        if(any(outcomes_included=="incidICU")){
            if(any(outcomes_included=="incidVent")){
                order_outcomes <- c("incidH", "incidICU", "incidVent")
                outcomes_included <- c(order_outcomes, outcomes_included[!outcomes_included %in% order_outcomes])
            } else{
                order_outcomes <- c("incidH", "incidICU")
                outcomes_included <- c(order_outcomes, outcomes_included[!outcomes_included %in% order_outcomes])
            }
        }

        cat(paste0(
            outcomes,
            mget(outcomes_included) %>% unlist() %>% paste0(collapse = "")
        ))

        if(incl_interventions){
            cat(paste0(
                '  interventions:\n',
                '    settings:\n',
                '      ', ifr, ':\n',
                '        template: Stacked\n',
                '        scenarios: ["outcome_interventions"]\n'
            ))
        }
    } else{

        cat(paste0(
            '\n',
            'outcomes:\n',
            '  method: delayframe\n',
            '  param_from_file: TRUE\n',
            '  param_place_file: "', outcomes_parquet_file, '"\n',
            '  scenarios:\n',
            '    - ',ifr,'\n',
            '  settings:\n',
            '    ',ifr,':\n',
            '      incidH:\n',
            '        source: incidI\n',
            '        probability:\n',
            print_value(value_dist = incidH_prob_dist[1],
                        value_mean = incidH_prob_value[1],
                        indent_space = 10),
            '        delay:\n',
            print_value(value_dist = incidH_delay_dist[1],
                        value_mean = incidH_delay_value[1],
                        indent_space = 10),
            '        duration:\n', # TODO: optional
            print_value(value_dist = incidH_duration_dist[1],
                        value_mean = incidH_duration_value[1],
                        indent_space = 10),
            '          name: hosp_curr\n',
            '      incidD:\n',
            '        source: incidI\n',
            '        probability:\n',
            print_value(value_dist = incidD_prob_dist[1],
                        value_mean = incidD_prob_value[1],
                        indent_space = 10),
            '        delay:\n',
            print_value(value_dist = incidD_delay_dist[1],
                        value_mean = incidD_delay_value[1],
                        indent_space = 10),
            '      incidICU:\n',
            '        source: incidH\n',
            '        probability:\n',
            print_value(value_dist = incidICU_prob_dist[1],
                        value_mean = incidICU_prob_value[1],
                        indent_space = 10),
            '        delay:\n',
            print_value(value_dist = incidICU_delay_dist[1],
                        value_mean = incidICU_delay_value[1],
                        indent_space = 10),
            '        duration:\n', # TODO: optional
            print_value(value_dist = incidICU_duration_dist[1],
                        value_mean = incidICU_duration_value[1],
                        indent_space = 10),
            '          name: icu_curr\n',
            '      incidVent:\n',
            '        source: incidICU\n',
            '        probability: \n',
            print_value(value_dist = incidVent_prob_dist[1],
                        value_mean = incidVent_prob_value[1],
                        indent_space = 10),
            '        delay:\n',
            print_value(value_dist = incidVent_delay_dist[1],
                        value_mean = incidVent_delay_value[1],
                        indent_space = 10),
            '        duration:\n',
            print_value(value_dist = incidVent_duration_dist[1],
                        value_mean = incidVent_duration_value[1],
                        indent_space = 10),
            '          name: vent_curr\n',
            '      incidC:\n',
            '        source: incidI\n',
            '        probability:\n',
            print_value(value_dist = incidC_prob_dist[1],
                        value_mean = incidC_prob_value[1],
                        value_sd = incidC_prob_sd[1],
                        value_a = incidC_prob_a[1],
                        value_b = incidC_prob_b[1],
                        indent_space=10),
            incidC_pert[1],
            '        delay:\n',
            print_value(value_dist = incidC_delay_dist[1],
                        value_mean = incidC_delay_value[1],
                        indent_space = 10)
        ))

        if(!is.null(dat)){
            dat <- dat %>%
                collapse_intervention() %>%
                dplyr::filter(type=="outcome")

            if(nrow(dat)==0){
                outcome_interventions <- "local_variance"
            } else{

                outcome_interventions <- paste0(unique(dat$name), collapse = '", "')

            }

            cat(paste0(
                '  interventions:\n',
                '    settings:\n',
                '      ', ifr, ':\n',
                '        template: Stacked\n',
                '        scenarios: ["', outcome_interventions, '"]\n'
            ))

        } else{
            cat(paste0(
                '  interventions:\n',
                '    settings:\n',
                '      ', ifr, ':\n',
                '        template: Stacked\n',
                '        scenarios: ["local_variance"]\n'
            ))
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
#' @param theta_1A_val reduction in susceptibility for the 1st vaccine compartment (i.e. 1-dose)
#' @param theta_1A_dist distribution of theta_1A
#' @param theta_2A_val reduction in susceptibility for the 2nd vaccine compartment (i.e. 2-doses)
#' @param theta_2A_dist distribution of theta_2A
#' @param theta_1B_val reduction in susceptibility to the third variant for the 1st vaccine compartment (i.e. 1-dose)
#' @param theta_1B_dist distribution of theta_1B
#' @param theta_2B_val reduction in susceptibility to the third variant for the 2nd vaccine compartment (i.e. 2-doses)
#' @param theta_2B_dist distribution of theta_2B
#' @param nu_1_val value of transition from the unvaccinated to the 1st vaccine compartment
#' @param nu_1_dist distribution of nu_1
#' @param nu_1_overlap_operation
#' @param nu_2_val value of transition from the 1st to the 2nd vaccine compartment
#' @param nu_2_dist distribution of nu_2
#' @param nu_2_overlap_operation
#' @param chi_1_val relative change in transmission for the 2nd vs 1st variant compartment (e.g. alpha vs wild)
#' @param chi_1_dist distribution of chi_1
#' @param chi_2_val relative change in transmission for the 3rd vs 1st variant compartment (e.g. delta vs wild)
#' @param chi_2_dist distribution of chi_2
#' @param epsilon_val rate at which immunity wanes (i.e. movement from R --> S)
#' @param epsilon_dist distribution of epsilon
#' @param incl_vacc specify whether to include vaccination compartments. If TRUE, must specify dose transmission/susceptibility parameters for each compartment and transition across compartments
#' @param transitions_dist vector specifying whether transition rate between compartments is fixed; distributional is not yet supported
#' @param transitions_val vector specifying base transition rate between compartments
#' @param vaccine_compartments names of vaccination compartments: defaults to "unvaccinated", "first dose" and "second dose"
#' @export
#'
#' @examples
#' print_seir(alpha_val=0.99,
#'            sigma_val = 1/5.2,
#'            gamma_dist = "uniform",
#'            gamma_a = 1/6,
#'            gamma_b = 1/2.6,
#'            r0_dist = "fixed",
#'            r0_val = 2.3,
#'            incl_vacc = FALSE)
#'

print_seir <- function(sigma_val = 1/5.2,
                       gamma_dist = "fixed",
                       gamma_val = 1/3.83,
                       gamma_sd = NULL,
                       gamma_a = 1/4.5,
                       gamma_b = 1/3,
                       alpha_val = 0.99,
                       R0s_val = 2.3,
                       R0s_dist = "uniform",
                       R0s_sd = NULL,
                       R0s_a = 2,
                       R0s_b = 3,
                       theta_1A_val = 0.5,
                       theta_1A_dist = "fixed",
                       theta_2A_val = 0.9,
                       theta_2A_dist = "fixed",
                       theta_1B_val = 0.35,
                       theta_1B_dist = "fixed",
                       theta_2B_val = 0.85,
                       theta_2B_dist = "fixed",
                       nu_1_val = 0,
                       nu_1_dist = "fixed",
                       nu_1_overlap_operation = "sum",
                       nu_2_val = 1/(3.5*7),
                       nu_2_dist = "fixed",
                       nu_2_overlap_operation = "sum",
                       chi_1_val = 1.5,
                       chi_1_dist = "fixed",
                       chi_2_val = 1.6*1.5,
                       chi_2_dist = "fixed",
                       epsilon_val = 0*1/(365*1.5),
                       epsilon_dist = "fixed",
                       incl_vacc = TRUE,
                       vaccine_compartments = c("unvaccinated", "1dose", "2dose"),
                       compartment = TRUE
){
    # TODO: add checks to compartment length

        if(compartment){
            seir <- paste0("seir:\n",
                           "  parameters:\n",
                           "    sigma: \n", print_value(value_dist = "fixed",
                                                        value_mean = sigma_val),
                           "    alpha: \n", print_value(value_dist = "fixed",
                                                        value_mean = alpha_val),
                           "    r0: \n", print_value(value_dist = R0s_dist,
                                                     value_mean = R0s_val,
                                                     value_sd = R0s_sd,
                                                     value_a = R0s_a,
                                                     value_b = R0s_b),
                           "    gamma: \n", print_value(value_dist = gamma_dist,
                                                        value_mean = gamma_val,
                                                        value_sd = gamma_sd,
                                                        value_a = gamma_a,
                                                        value_b = gamma_b))

            seir <- paste0(seir,
                           "    theta_1A: \n", print_value(value_dis = theta_1A_dist,
                                                           value_mean = paste0(1, " - ", theta_1A_val)),
                           "    theta_2A: \n", print_value(value_dis = theta_2A_dist,
                                                           value_mean = paste0(1, " - ", theta_2A_val)),
                           "    theta_1B: \n", print_value(value_dis = theta_1B_dist,
                                                           value_mean = paste0(1, " - ", theta_1B_val)),
                           "    theta_2B: \n", print_value(value_dis = theta_2B_dist,
                                                           value_mean = paste0(1, " - ", theta_2B_val)),
                           "    nu_1: \n",
                           "      intervention_overlap_operation: ", nu_1_overlap_operation, "\n",
                           print_value(value_dis = nu_1_dist,
                                       value_mean = nu_1_val),
                           "    nu_2: \n",
                           "      intervention_overlap_operation: ", nu_2_overlap_operation, "\n",
                           print_value(value_dis = nu_2_dist,
                                       value_mean = nu_2_val),
                           "    chi_1: \n", print_value(value_dist = chi_1_dist,
                                                        value_mean = chi_1_val),
                           "    chi_2: \n", print_value(value_dist = chi_2_dist,
                                                        value_mean = chi_2_val),
                           "    epsilon: \n", print_value(value_dist = epsilon_dist,
                                                          value_mean = epsilon_val),
                           "  compartments:\n",
                           '    infection_stage: ["S", "E", "I1", "I2", "I3", "R"] \n',
                           '    vaccination_stage: ["', paste0(vaccine_compartments, collapse = '", "'), '"] \n',
                           '    variant_type: ["WILD", "ALPHA", "DELTA"] \n',
                           '  transitions:\n',
                           '    - source: [["S"],["', paste0(vaccine_compartments, collapse = '", "'), '"],"WILD"] \n',
                           '      destination: [["E"],["', paste0(vaccine_compartments, collapse = '", "'), '"],["WILD", "ALPHA"]]\n',
                           '      proportional_to: [\n',
                           '        "source",\n',
                           '        [[["I1","I2","I3"]],[["', paste0(vaccine_compartments, collapse = '", "'), '"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["', paste0(vaccine_compartments, collapse = '", "'), '"]],[["WILD"],["ALPHA"]]]\n',
                           '      ]\n',
                           '      proportion_exponent: [[["1"],"1","1"],[["alpha"],"1","1"]]\n',
                           '      rate: [["r0 * gamma"],["1", "theta_1A", "theta_2A"],["1", "chi_1"]]\n',

                           '    - source: [["S"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD"]] \n',
                           '      destination: [["E"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["DELTA"]]\n',
                           '      proportional_to: [\n',
                           '        "source",\n',
                           '        [[["I1","I2","I3"]], [["', paste0(vaccine_compartments, collapse = '", "'), '"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["', paste0(vaccine_compartments, collapse = '", "'), '"]], ["DELTA"]]\n',
                           '      ]\n',
                           '      proportion_exponent: [\n',
                           '        [["1"],"1","1"],\n',
                           '        [["alpha"],"1","1"]\n',
                           '      ]\n',
                           '      rate: [["r0 * gamma"], ["1", "theta_1B", "theta_2B"], ["chi_2"]]\n',

                           '    - source: [["E"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]]\n',
                           '      destination: [["I1"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]]\n',
                           '      proportional_to: ["source"]\n',
                           '      proportion_exponent:  [["1","1","1"]]\n',
                           '      rate: [["sigma"], "1", "1"]\n',

                           '    - source: [["I1"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      destination: [["I2"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      proportional_to: ["source"] \n',
                           '      proportion_exponent: [["1","1","1"]] \n',
                           '      rate: ["3 * gamma", 1, 1] \n',

                           '    - source: [["I2"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      destination: [["I3"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      proportional_to: ["source"] \n',
                           '      proportion_exponent: [["1","1","1"]] \n',
                           '      rate: ["3 * gamma", 1, 1] \n',

                           '    - source: [["I3"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      destination: [["R"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      proportional_to: ["source"] \n',
                           '      proportion_exponent: [["1","1","1"]] \n',
                           '      rate: ["3 * gamma", 1, 1] \n',

                           '    - source: [ ["S","E","I1","I2","I3","R"], ["unvaccinated", "1dose"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      destination: [["S","E","I1","I2","I3","R"], ["1dose", "2dose"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      proportional_to: ["source"] \n',
                           '      proportion_exponent: [["1","1","1"]] \n',
                           '      rate: ["1", ["nu_1","nu_2"], "1"] \n',

                           '    - source: [["R"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], ["WILD", "ALPHA", "DELTA"]] \n',
                           '      destination: [["S"], ["', paste0(vaccine_compartments, collapse = '", "'), '"], "WILD"] \n',
                           '      proportional_to: ["source"] \n',
                           '      proportion_exponent: [["1","1","1"]] \n',
                           '      rate: ["epsilon", 1, 1] \n'
                           )
        } else{

            seir <- paste0("seir:\n",
                           "  parameters:\n",
                           print_value(value_dist = R0s_dist,
                                       value_mean = R0s_val,
                                       value_sd = R0s_sd,
                                       value_a = R0s_a,
                                       value_b = R0s_b,
                                       indent_space = 4,
                                       param_name = "R0s"),
                           print_value(value_dist = gamma_dist,
                                       value_mean = gamma_val,
                                       value_sd = gamma_sd,
                                       value_a = gamma_a,
                                       value_b = gamma_b,
                                       indent_space = 4,
                                       param_name = "gamma"),
                           "    sigma: ", sigma_val, "\n",
                           "    alpha: ", alpha_val, "\n"
            )

            if(incl_vacc){
                vacc <- paste0("    parallel_structure:\n",
                               "      compartments:\n",
                               "        ", vaccine_compartments[1], ":\n",
                               print_value(value_dist = "fixed",
                                           value_mean = 0,
                                           indent_space = 10,
                                           param_name = "transmissibility_reduction"),
                               print_value(value_dist = "fixed",
                                           value_mean = 0,
                                           indent_space = 10,
                                           param_name = "susceptibility_reduction"))

                dose_transmission_dist <- c("fixed", "fixed")
                dose_transmission_val <- c(0, 0)

                dose_susceptibility_dist <- c(theta_1A_dist, theta_2A_dist)
                dose_susceptibility_val <- c(theta_1A_val, theta_2A_val)

                for(i in 2:length(vaccine_compartments)){
                    vacc <- paste0(vacc,
                                   "        ", vaccine_compartments[i], ":\n",
                                   print_value(value_dist = dose_transmission_dist[i-1],
                                               value_mean = dose_transmission_val[i-1],
                                               indent_space = 10,
                                               param_name = "transmissibility_reduction"),
                                   print_value(value_dist = dose_susceptibility_dist[i-1],
                                               value_mean = dose_susceptibility_val[i-1],
                                               indent_space = 10,
                                               param_name = "susceptibility_reduction")
                    )
                }
                vacc <- paste0(vacc,
                               "      transitions:\n")

                transitions_dist <- c(nu_1_dist, nu_2_dist)
                transitions_val <- c(nu_1_val, nu_2_val)

                for(i in 2:length(vaccine_compartments)){
                    vacc <- paste0(vacc,
                                   "        - from: ", vaccine_compartments[i-1], "\n",
                                   "          to: ", vaccine_compartments[i], "\n",
                                   print_value(value_dist = transitions_dist[i-1],
                                               value_mean = transitions_val[i-1],
                                               indent_space = 10,
                                               param_name = "rate"))
                }
                vacc
            } else{ vacc <- ""}

            seir <- paste0(seir, vacc, "\n")
        }

        cat(seir)

}

#' Print Header Section
#' @description Prints the global options and the spatial setup section of the configuration files. These typically sit at the top of the configuration file.
#'
#' @param sim_name name of simulation, typically named after the region/location you are modeling
#' @param sim_start_date simulation start date, should match that of interventions, with format YYYY-MM-DD (e.g., 2020-01-31)
#' @param sim_end_date simulation end date with format YYYY-MM-DD (e.g., 2020-01-31)
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
print_header <- function(sim_name,
                        sim_start_date,
                        sim_end_date,
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
                        state_level = TRUE
){
    cat(paste0(
        "name: ", sim_name,"\n",
        "start_date: ", sim_start_date,"\n",
        "end_date: ", sim_end_date, "\n",
        "nsimulations: ", n_simulations,"\n",
        "dt: ", dt, "\n",
        "\n",
        "spatial_setup:\n",
        "  census_year: ", census_year,"\n",
        "  base_path: ", base_path,"\n",
        "  modeled_states:\n"),
        paste0('   - ',sim_states, '\n'),
        paste0(
            " setup_name: ", setup_name, "\n",
            "  geodata: ",geodata_file,"\n",
            "  mobility: ", mobility_file,"\n",
            "  popnodes: ", popnodes,"\n",
            "  nodenames: ", nodenames,"\n",
            "  include_in_report: ",include_in_report,"\n",
            "  state_level: ", state_level,"\n")
    )
}

#' Print seeding section
#' @description Prints the seeding section of the configuration file
#' @param method There are two different seeding methods: 1) based on air importation (FolderDraw) and 2) based on earliest identified cases (PoissonDistributed). FolderDraw is required if the importation section is present and requires folder_path. Otherwise, put PoissonDistributed, which requires lambda_file.
#' @param seeding_file_type indicates which seeding file type the SEIR model will look for, "seed", which is generated from inference::create_seeding.R, or "impa", which refers to importation
#' @param folder_path path to folder where importation inference files will be saved
#' @param lambda_file path to seeding file
#' @param perturbation_sd standard deviation for the proposal value of the seeding date, in number of days
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
print_seeding <- function(method = "FolderDraw",
                         seeding_file_type = "seed",
                         folder_path = "importation/minimal/",
                         lambda_file = "data/minimal/seeding.csv",
                         perturbation_sd = 1,
                         variant_filename = "data/variant/variant_props_long.csv",
                         compartment = TRUE,
                         variant_compartments = c("wild", "alpha", "delta")
){
    variant_compartments <- stringr::str_to_upper(variant_compartments)

    seeding_comp <- "\nseeding:\n"

    if(compartment){
        seeding_comp <- paste0(seeding_comp,
                               '  variant_filename: ', variant_filename, '\n',
                               '  seeding_compartments:\n')

        for(i in 1:length(variant_compartments)){
            seeding_comp <- paste0(seeding_comp,
                                   '    ', variant_compartments[i], ':\n',
                                   '      source_compartment: ["S", "unvaccinated","', stringr::str_to_upper(variant_compartments[1]),'"]\n',
                                   '      destination_compartment: ["E", "unvaccinated", "',stringr::str_to_upper(variant_compartments[i]),'"]\n')
        }
    }

    cat(paste0(
        seeding_comp,
        "  method: ", method,"\n",
        "  seeding_file_type: ", seeding_file_type,"\n",
        "  folder_path: ", folder_path,"\n",
        "  lambda_file: ", lambda_file,"\n",
        "  perturbation_sd: ", perturbation_sd, "\n",
        "\n"
    ))
}

#' Print filtering and filtering::statistics
#' @description Set settings for the filtering section and its statistics component
#'
#' @param sims_per_slot number of iterations in a single MCMC inference chain With inference model runs, the number of simulations nsimulations refers to the number of final model simulations that will be produced. The sims_per_slot setting refers to the number of iterative simulations that will be run in order to produce a single final simulation (i.e., number of simulations in a single MCMC chain).
#' @param do_filtering whether to perform inference
#' @param data_path file path where observed data are saved
#' @param gt_source source of data
#' @param stat_names the names of the statistics used to calibrate the model to empirical data
#' @param aggregator function used to aggregate data over the period, usually sum or mean
#' @param period duration over which data should be aggregated prior to use in the likelihood, may be specified in any number of days, weeks, months (e.g., "1 weeks")
#' @param sim_var column name where model data can be found, from the hospitalization outcomes files
#' @param data_var column where data can be found in data_path file
#' @param remove_na logical
#' @param add_one logical, TRUE if evaluating the log likelihood
#' @param ll_dist distribution of the likelihood: "sqrtnorm" or "pois"
#' @param ll_param parameter value(s) for the likelihood distribution; not used if ll_dist = "pois". See [inference::logLikStat()]
#' @param final_print whether this is the final section of the config to print an empty space; set to FALSE if running [print_hierarchical()] and/or [print_prior()]
#' @param compartment
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
                                       stat_names = c("sum_deaths", "sum_confirmed"),
                                       variant_compartments = c("wild", "alpha", "delta"),
                                       aggregator = "sum",
                                       period = "1 weeks",
                                       sim_var = c("incidD", "incidC"),
                                       data_var = c("incidDeath", "incidI"),
                                       remove_na = FALSE,
                                       add_one = c(FALSE, TRUE),
                                       ll_dist = c("sqrtnorm", "pois"),
                                       ll_param = .4,
                                       final_print = FALSE,
                                       compartment = TRUE){

    if(length(stat_names)!=length(data_var)) stop("stat_names and data_var must be the same length")

    cat(paste0(
        "\n",
        "filtering:\n",
        "  simulations_per_slot: ", sims_per_slot, "\n",
        "  do_filtering: ", do_filtering,"\n",
        "  data_path: ", data_path, "\n",
        '  gt_source: "', gt_source, '"\n',
        "  statistics:\n"
    ))

    if(compartment){

        variant_compartments <- stringr::str_to_upper(variant_compartments)

        stat_names <- paste(rep(stat_names, each = length(variant_compartments)), variant_compartments, sep ="_")
        sim_var <- paste(rep(sim_var, each = length(variant_compartments)), stringr::str_to_upper(variant_compartments), sep="_")
        data_var <- paste(rep(data_var, each = length(variant_compartments)), variant_compartments, sep ="_")

        for(i in 1:length(variant_compartments)){
            stat_names <- c(stat_names[stringr::str_detect(stat_names, variant_compartments[i], negate = TRUE)], stat_names[stringr::str_detect(stat_names, variant_compartments[i], negate = FALSE)])
            sim_var <- c(sim_var[stringr::str_detect(sim_var, stringr::str_to_upper(variant_compartments[i]), negate = TRUE)], sim_var[stringr::str_detect(sim_var, stringr::str_to_upper(variant_compartments[i]), negate = FALSE)])
            data_var <- c(data_var[stringr::str_detect(data_var, variant_compartments[i], negate = TRUE)], data_var[stringr::str_detect(data_var, variant_compartments[i], negate = FALSE)])
        }
    }

    n_vars <- length(stat_names)

    if(length(aggregator)!=n_vars){
        aggregator <- rep(aggregator, n_vars)
    }

    if(length(period)!=n_vars){
        period <- rep(period, n_vars)
    }

    if(length(remove_na)!=n_vars){
        remove_na <- rep(remove_na, n_vars)
    }

    if(length(add_one)!=n_vars){
        add_one <- rep(add_one, n_vars)
    }

    if(length(ll_dist)!=n_vars){
        ll_dist <- rep(ll_dist, n_vars)
    }

    if(length(ll_param)!=n_vars){
        ll_param <- rep(ll_param, n_vars)

    }


    for(i in 1:length(stat_names)){
        cat(paste0(
            "    ", stat_names[i], ":\n",
            "      name: ", stat_names[i], "\n",
            "      aggregator: ", aggregator[i], "\n",
            '      period: "', period[i], '"\n',
            "      sim_var: ", sim_var[i], "\n",
            "      data_var: ", data_var[i], "\n",
            "      remove_na: ", remove_na[i], "\n",
            "      add_one: ", add_one[i], "\n",
            "      likelihood:\n",
            "        dist: ", ll_dist[i], "\n"
        ))

        if(ll_dist[i]!="pois"){
            cat(paste0(
                "        param: [", ll_param[i], "]\n"
            ))
        }
    }

    if(final_print){
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
                                         variant_compartments = c("wild", "alpha", "delta"),
                                         module = c("seir", "hospitalization"),
                                         geo_group_col = "USPS",
                                         transform = c("none", "logit"),
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

    for(i in 1:n_vars){
        cat(paste0(
            "    ", paste0(npi_name[i], "_hierarchy:\n"),
            "      name: ", npi_name[i], "\n",
            "      module: ", module[i], "\n",
            "      geo_group_col: ", geo_group_col[i], "\n",
            "      transform: ", transform[i], "\n"
        ))
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

print_filtering_prior <- function(npi_name = c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar", "Seas_apr",
                                               "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
                                               "Seas_oct", "Seas_nov", "Seas_dec"),
                                  module = "seir",
                                  dist = "normal",
                                  param_mean = c(0.000, -0.200, -0.133, -0.067, 0.00,  0.067, 0.133,  0.200, 0.133,  0.067,  0.000, -0.067, -0.133),
                                  param_sd = 1,
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

#' #' Convenience function to generate/save config
#' #'
#' generate_config <- function(intervention_path,
#'                             config_name = "config.yml",
#'                             save_path = NULL,
#'                             header_sim_name = "USA",
#'                             header_sim_start_date = "2020-01-01",
#'                             header_sim_end_date = "2021-08-07",
#'                             header_n_simulations = 300,
#'                             header_dt = 0.25,
#'                             header_census_year = 2019,
#'                             header_base_path = "data",
#'                             header_setup_name = "USA",
#'                             header_geodata = "geodata.csv",
#'                             header_mobility = "mobility.csv",
#'                             header_popnodes = "pop2019est",
#'                             header_nodenames = "geoid",
#'                             header_include_in_report = "include_in_report",
#'                             header_state_level = TRUE,
#'                             seeding_method = "FolderDraw",
#'                             seeding_file_type = "seed",
#'                             seeding_folder_path = "importation/minimal/",
#'                             seeding_lambda_file = "data/minimal/seeding.csv",
#'                             seeding_perturbation_sd = 1,
#'                             seir_alpha_val = 0.99,
#'                             seir_sigma_val = 1/5.2 ,
#'                             seir_gamma_dist = "fixed",
#'                             seir_gamma_val = 1/3.83,
#'                             seir_gamma_sd = NULL,
#'                             seir_gamma_a = 1/4.5,
#'                             seir_gamma_b = 1/3,
#'                             seir_R0s_dist = "uniform",
#'                             seir_R0s_val = 2.3,
#'                             seir_R0s_sd = NULL,
#'                             seir_R0s_a = 2,
#'                             seir_R0s_b = 3,
#'                             seir_incl_vacc = TRUE,
#'                             seir_dose_transmission_dist = c("fixed", "fixed", "fixed"),
#'                             seir_dose_transmission_val = c(0, 0, 0),
#'                             seir_dose_susceptibility_dist = c("fixed", "fixed", "fixed"),
#'                             seir_dose_susceptibility_val = c(0, 0.75, 0.90),
#'                             seir_transitions_dist = c("fixed", "fixed"),
#'                             seir_transitions_val = c(0, 0.04),
#'                             transmission_scenario = "inference",
#'                             outcomes_ifr="med",
#'                             outcomes_parquet_file="usa-geoid-params-output_statelevel.parquet",
#'                             outcomes_incidH_prob_dist="fixed",
#'                             outcomes_incidH_prob_value=0.0175,
#'                             outcomes_incidH_delay_dist="fixed",
#'                             outcomes_incidH_delay_value=7,
#'                             outcomes_incidH_duration_dist="fixed",
#'                             outcomes_incidH_duration_value=7,
#'                             outcomes_incidD_prob_dist="fixed",
#'                             outcomes_incidD_prob_value=0.005,
#'                             outcomes_incidD_delay_dist="fixed",
#'                             outcomes_incidD_delay_value=20,
#'                             outcomes_incidICU_prob_dist="fixed",
#'                             outcomes_incidICU_prob_value=0.167,
#'                             outcomes_incidICU_delay_dist="fixed",
#'                             outcomes_incidICU_delay_value=3,
#'                             outcomes_incidICU_duration_dist="fixed",
#'                             outcomes_incidICU_duration_value=8,
#'                             outcomes_incidVent_prob_dist="fixed",
#'                             outcomes_incidVent_prob_value=0.463,
#'                             outcomes_incidVent_delay_dist="fixed",
#'                             outcomes_incidVent_delay_value=1,
#'                             outcomes_incidVent_duration_dist="fixed",
#'                             outcomes_incidVent_duration_value=7,
#'                             outcomes_incidC_prob_dist="truncnorm",
#'                             outcomes_incidC_prob_mean=0.2,
#'                             outcomes_incidC_prob_sd=.1,
#'                             outcomes_incidC_prob_a=0,
#'                             outcomes_incidC_prob_b=1,
#'                             outcomes_incidC_prob_dist_pert="truncnorm",
#'                             outcomes_incidC_prob_mean_pert=0,
#'                             outcomes_incidC_prob_sd_pert=0.05,
#'                             outcomes_incidC_prob_a_pert=-1,
#'                             outcomes_incidC_prob_b_pert=1,
#'                             outcomes_incidC_delay_value=7,
#'                             outcomes_incidC_delay_dist="fixed",
#'                             filtering_sims_per_slot = 300,
#'                             filtering_data_path = "data/us_data.csv",
#'                             filtering_gt_source = "csse",
#'                             filtering_stat_names = c("sum_deaths", "sum_confirmed"),
#'                             filtering_aggregator = "sum",
#'                             filtering_period = "1 weeks",
#'                             filtering_sim_var = c("incidD", "incidC"),
#'                             filtering_data_var = c("death_incid", "confirmed_incid"),
#'                             filtering_remove_na = FALSE,
#'                             filtering_add_one = c(FALSE, TRUE),
#'                             filtering_ll_dist = c("sqrtnorm", "pois"),
#'                             filtering_ll_param = .4,
#'                             filtering_final_print = FALSE,
#'                             hierarchical_npi_name = c("local_variance", "probability_incidI_incidC"),
#'                             hierarchical_module = c("seir", "hospitalization"),
#'                             hierarchical_geo_group_col = "USPS",
#'                             hierarchical_transform = c("none", "logit"),
#'                             hierarchical_final_print = FALSE,
#'                             prior_npi_name = c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar", "Seas_apr",
#'                                                "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
#'                                                "Seas_oct", "Seas_nov", "Seas_dec"),
#'                             prior_module = "seir",
#'                             prior_dist = "normal",
#'                             prior_param_low = NULL,
#'                             prior_param_high = 1){
#'
#'     interventions <- readr::read_csv(intervention_path)
#'
#'     if(is.null(save_path)){
#'         config_name <- file.path(tempdir(), config_name)
#'     } else{
#'         config_name <- file.path(save_path, config_name)
#'         print(paste0("Config saved in ", config_name))
#'     }
#'
#'     sink(config_name)
#'
#'     print_header(sim_name = header_sim_name,
#'                  sim_start_date = header_sim_start_date,
#'                  sim_end_date = header_sim_end_date,
#'                  n_simulations = header_n_simulations,
#'                  dt = header_dt,
#'                  census_year = header_census_year,
#'                  base_path = header_census_year,
#'                  sim_states = unique(interventions$USPS[!interventions$USPS %in% c("", "all") & !is.na(interventions$USPS)]),
#'                  setup_name = header_setup_name,
#'                  geodata = header_geodata,
#'                  mobility = header_mobility,
#'                  popnodes = header_popnodes,
#'                  nodenames = header_nodenames,
#'                  include_in_report = header_include_in_report,
#'                  state_level = header_state_level)
#'
#'     print_seeding(method = seeding_method,
#'                   seeding_file_type = seeding_file_type,
#'                   folder_path = seeding_folder_path,
#'                   lambda_file = seeding_lambda_file ,
#'                   perturbation_sd = seeding_perturbation_sd)
#'
#'     print_seir(alpha_val = seir_alpha_val,
#'                sigma_val = seir_sigma_val,
#'                gamma_dist = seir_gamma_dist,
#'                gamma_val = seir_gamma_val,
#'                gamma_sd = seir_gamma_sd,
#'                gamma_a = seir_gamma_a,
#'                gamma_b = seir_gamma_b,
#'                R0s_dist = seir_R0s_dist,
#'                R0s_val = seir_R0s_val,
#'                R0s_sd = seir_R0s_sd,
#'                R0s_a = seir_R0s_a,
#'                R0s_b = seir_R0s_b,
#'                incl_vacc = seir_incl_vacc,
#'                dose_transmission_dist = seir_dose_transmission_dist,
#'                dose_transmission_val = seir_dose_transmission_val,
#'                dose_susceptibility_dist = seir_dose_susceptibility_dist,
#'                dose_susceptibility_val = seir_dose_susceptibility_val,
#'                transitions_dist = seir_transitions_dist,
#'                transitions_val = seir_transitions_val)
#'
#'     print_transmission_interventions(dat = interventions,
#'                                      scenario = transmission_scenario)
#'
#'     print_outcomes(dat = interventions,
#'                    ifr = outcomes_ifr,
#'                    outcomes_parquet_file = outcomes_parquet_file,
#'                    incidH_prob_dist= outcomes_incidH_prob_dist,
#'                    incidH_prob_value= outcomes_incidH_prob_value,
#'                    incidH_delay_dist= outcomes_incidH_delay_dist,
#'                    incidH_delay_value= outcomes_incidH_delay_value,
#'                    incidH_duration_dist= outcomes_incidH_duration_dist,
#'                    incidH_duration_value= outcomes_incidH_duration_value,
#'                    incidD_prob_dist= outcomes_incidD_prob_dist,
#'                    incidD_prob_value= outcomes_incidD_prob_value,
#'                    incidD_delay_dist= outcomes_incidD_delay_dist,
#'                    incidD_delay_value= outcomes_incidD_delay_value,
#'                    incidICU_prob_dist= outcomes_incidICU_prob_dist,
#'                    incidICU_prob_value= outcomes_incidICU_prob_value,
#'                    incidICU_delay_dist= outcomes_incidICU_delay_dist,
#'                    incidICU_delay_value= outcomes_incidICU_delay_value,
#'                    incidICU_duration_dist= outcomes_incidICU_duration_dist,
#'                    incidICU_duration_value= outcomes_incidICU_duration_value,
#'                    incidVent_prob_dist= outcomes_incidVent_prob_dist,
#'                    incidVent_prob_value= outcomes_incidVent_prob_value,
#'                    incidVent_delay_dist= outcomes_incidVent_delay_dist,
#'                    incidVent_delay_value= outcomes_incidVent_delay_value,
#'                    incidVent_duration_dist= outcomes_incidVent_duration_dist,
#'                    incidVent_duration_value= outcomes_incidVent_duration_value,
#'                    incidC_prob_dist= outcomes_incidC_prob_dist,
#'                    incidC_prob_mean= outcomes_incidC_prob_mean,
#'                    incidC_prob_sd= outcomes_incidC_prob_sd,
#'                    incidC_prob_a= outcomes_incidC_prob_a,
#'                    incidC_prob_b= outcomes_incidC_prob_b,
#'                    incidC_prob_dist_pert= outcomes_incidC_prob_dist_pert,
#'                    incidC_prob_mean_pert= outcomes_incidC_prob_mean_pert,
#'                    incidC_prob_sd_pert= outcomes_incidC_prob_sd_pert,
#'                    incidC_prob_a_pert = outcomes_incidC_prob_a_pert,
#'                    incidC_prob_b_pert= outcomes_incidC_prob_b_pert,
#'                    incidC_delay_value= outcomes_incidC_delay_value,
#'                    incidC_delay_dist= outcomes_incidC_delay_dist)
#'
#'     print_filtering(sims_per_slot = filtering_sims_per_slot,
#'                     data_path = filtering_data_path,
#'                     gt_source = filtering_gt_source,
#'                     stat_names = filtering_stat_names,
#'                     aggregator = filtering_aggregator,
#'                     period = filtering_period,
#'                     sim_var = filtering_sim_var,
#'                     data_var = filtering_data_var,
#'                     remove_na = filtering_remove_na,
#'                     add_one = filtering_add_one,
#'                     ll_dist = filtering_ll_dist,
#'                     ll_param = filtering_ll_param,
#'                     final_print = filtering_final_print)
#'
#'     print_hierarchical(npi_name = hierarchical_npi_name,
#'                        module = hierarchical_module,
#'                        geo_group_col = hierarchical_geo_group_col,
#'                        transform = hierarchical_transform,
#'                        final_print = hierarchical_final_print)
#'
#'     print_prior(dat = interventions,
#'                 npi_name = prior_npi_name,
#'                 module = prior_module,
#'                 dist = prior_dist,
#'                 param_low = prior_param_low,
#'                 param_high = prior_param_high)
#'
#'     sink()
#'
#'     config <- yaml::read_yaml(config_name)
#'
#'     if(is.null(save_path)){
#'         unlink(config_name)
#'     }
#'
#'     return(config)
#' }

