#' Convenience function to print values for different distributions
#'
#' @param ... object specifying parameter distribution; object name should include param
#' @param space_length number of spaces to add when printing
#'
#' @return string with parameter distribution and values
#' @export
#'
#' @examples
#'
#' gamma_dist <- "fixed"
#' gamma_val <- 0.167
#'
#' print_param_val(gamma_dist)
#'
paste_param_val <- function(..., space_length = 6){

    space <- paste0(rep(" ", space_length), collapse = "")
    param_space <- stringr::str_remove(space, "  ")
    param <- deparse(substitute(...)) %>% stringr::str_extract(pattern = ".+\\_")

    param_name <- stringr::str_remove(param, "\\_")
    value <- get(paste0(param, "val"), envir = parent.frame(n=1))

    if(is.null(...)){

        print_val <- paste0(
            param_space, param_name, ": ", value, "\n"
        )
    } else{
        if(... == "fixed"){
            print_val <- paste0(
                param_space, param_name, ":\n",
                space, "distribution: fixed\n",
                space, "value: ", value, "\n"
            )
        }

        if(... == "uniform"){
            min_a <- get(paste0(param, "a"), envir = parent.frame(n=1))
            max_b <- get(paste0(param, "b"), envir = parent.frame(n=1))

            print_val <- paste0(
                param_space, param_name, ":\n",
                space, "distribution: uniform\n",
                space, "low: ", min_a, "\n",
                space, "high: ", max_b, "\n"
            )
        }

        if(! ... %in% c("fixed", "uniform")){
            min_a <- get(paste0(param, "a"), envir = parent.frame(n=1))
            max_b <- get(paste0(param, "b"), envir = parent.frame(n=1))
            mean <- get(paste0(param, "val"), envir = parent.frame(n=1))
            sd <- get(paste0(param, "sd"), envir = parent.frame(n=1))

            print_val <- paste0(
                param_space, param_name, ":\n",
                space, "distribution: uniform\n",
                space, "mean: ", mean, "\n",
                space, "sd: ", sd, "\n",
                space, "a: ", min_a, "\n",
                space, "b: ", max_b, "\n"
            )
        }
    }

    return(print_val)
}

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
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, starts_with("value_"), starts_with("pert_")) %>%
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

    if(dat$template=="MultiTimeReduce" & dat$geoid=="all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: MultiTimeReduce\n",
            "      parameter: ", dat$parameter, "\n",
            "      groups:\n",
            '        - affected_geoids: "all"\n'
        ))

        for(j in 1:nrow(dat)){
            cat(paste0(
                '          periods:\n',
                dat$period[j]
            ))
        }
    }

    if(dat$template=="MultiTimeReduce" & dat$geoid!="all"){
        cat(paste0(
            "    ", dat$name[1], ":\n",
            "      template: MultiTimeReduce\n",
            "      parameter: ", dat$parameter[1], "\n",
            "      groups:"
            ))

            for(j in 1:nrow(dat)){
                cat(paste0(
                    "\n",
                    '        - affected_geoids: ["', dat$geoid[j], '"]\n',
                    '          periods:\n',
                    dat$period[j]
                ))
            }
    }

    cat(paste0(
        "\n",
        '      value:\n',
        "        distribution: ", dat$value_dist[1],"\n",
        "        mean: ", dat$value_mean[1],"\n",
        "        sd: ",dat$value_sd[1],"\n",
        "        a: ",dat$value_a[1],"\n",
        "        b: ",dat$value_b[1]
        ))

    if(!is.na(dat$pert_dist)){
        cat(paste0(
            "\n",
            "      perturbation:\n",
            "        distribution: ", dat$pert_dist[1],"\n",
            "        mean: ", dat$pert_mean[1],"\n",
            "        sd: ", dat$pert_sd[1],"\n",
            "        a: ", dat$pert_a[1],"\n",
            "        b: ", dat$pert_b[1],"\n"
        ))
    }
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
            dat$period,
            '      value:\n',
            "        distribution: ", dat$value_dist,"\n",
            "        mean: ", dat$value_mean,"\n",
            "        sd: ",dat$value_sd,"\n",
            "        a: ",dat$value_a,"\n",
            "        b: ",dat$value_b,"\n"
        ))
    }

    if(dat$template == "ReduceR0" & dat$geoid == "all"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            '      affected_geoids: "', dat$geoid, '"\n',
            dat$period,
            '      value:\n',
            "        distribution: ", dat$value_dist,"\n",
            "        mean: ", dat$value_mean,"\n",
            "        sd: ",dat$value_sd,"\n",
            "        a: ",dat$value_a,"\n",
            "        b: ",dat$value_b,"\n"
        ))
    }

    if(dat$template == "Reduce" & is.na(dat$value_sd)){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            "      parameter: ", dat$parameter, "\n",
            '      affected_geoids: ["', dat$geoid, '"]\n',
            dat$period,
            '      value:\n',
            "        distribution: ", dat$value_dist,"\n",
            "        value: ", dat$value_mean,"\n"
        ))
    }

    if(dat$template == "Reduce" & !is.na(dat$value_sd)){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            "      parameter: ", dat$parameter, "\n",
            '      affected_geoids: ["', dat$geoid, '"]\n',
            dat$period,
            '      value:\n',
            "        distribution: ", dat$value_dist,"\n",
            "        mean: ", dat$value_mean,"\n",
            "        sd: ", dat$value_sd, "\n",
            "        a: ", dat$value_a, "\n",
            "        b: ", dat$value_b, "\n"
        ))
    }

    if(dat$template == "ReduceIntervention"){
        cat(paste0(
            "    ", dat$name, ":\n",
            "      template: ", dat$template,"\n",
            "      parameter: ", dat$parameter, "\n",
            '      affected_geoids: ["', dat$geoid, '"]\n',
            dat$period,
            "      baseline_scenario: ", dat$baseline_scenario, "\n",
            '      value:\n',
            "        distribution: ", dat$value_dist,"\n",
            "        value: ", dat$value_mean,"\n"
        ))
    }

    if(!is.na(dat$pert_dist)){
        cat(paste0(
            "      perturbation:\n",
            "        distribution: ", dat$pert_dist,"\n",
            "        mean: ", dat$pert_mean,"\n",
            "        sd: ", dat$pert_sd,"\n",
            "        a: ", dat$pert_a,"\n",
            "        b: ", dat$pert_b,"\n"
        ))
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
        dplyr::distinct(name, category) %>%
        dplyr::group_by(category) %>%
        dplyr::summarize(name = paste0(name, collapse = '", "'))

    for(i in 1:nrow(dat)){
        if(dat$category[i]=="local_variance"){next}
        cat(paste0(
            "    ", dat$category[i], ":\n",
            "      template: Stacked\n",
            '      scenarios: ["', dat$name[i], '"]\n'
        ))

    }

    cat(paste0(
        "    ", scenario, ":\n",
        "      template: Stacked\n",
        '      scenarios: ["', paste0(dat$category, collapse='", "'), '"]\n'
    ))
}

#' Convenience function to print transmission interventions and stack them
#'
#' @param dat dataframe with processed intervention names/periods; see collapsed_interventions
#' @param scenario name of the scenario
#'
#' @return
#' @export
#'
#' @examples
#'

print_transmission_interventions <- function(dat,
                                             scenario = "Inference"
){

    cat(paste0(
        'interventions:\n',
        '  scenarios:\n',
        '    - ', scenario, '\n',
        '  settings:\n'
    ))

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

    # reduce <- dat %>%
    #     dplyr::filter(template != "MultiTimeReduce")
    #
    # if(nrow(reduce) > 0 ){
    #     for(i in 1:nrow(reduce)){
    #
    #         yaml_reduce_template(reduce[i,])
    #
    #     }
    # }
    #
    # mtr <- dat %>%
    #     dplyr::filter(template=="MultiTimeReduce")
    #
    # npi_names <- mtr %>% dplyr::distinct(name) %>% dplyr::pull()
    # if(nrow(mtr) > 0){
    #     for(i in 1:length(npi_names)){
    #         npi <- mtr %>%
    #             dplyr::filter(name == npi_names[i])
    #
    #         yaml_mtr_template(npi)
    #     }
    #
    # }

    yaml_stack(dat,
               scenario)
}

#' Print outcomes section
#'
#' @param dat=NULL df with processed outcome interventions
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
#' @param incidC_prob_mean probability of detecting a case among incident infections
#' @param incidC_prob_sd standard deviation for incidC probability
#' @param incidC_prob_a minimum value for incidC probability
#' @param incidC_prob_b maximum value for incidC probability
#' @param incidC_prob_dist_pert distribution for incidC perturbation
#' @param incidC_prob_mean_pert mean perturbation value for incidC
#' @param incidC_prob_sd_pert perturbation sd for incidC
#' @param incidC_prob_a_pert maximum perturbation value for incidC probability
#' @param incidC_prob_b_pert minimum perturbation value for incidC probability
#' @param incidC_delay_value time to case detection since infection in days
#' @param incidC_delay_dist distribution of incidC delay
#'
#' @return
#' @export
#'
#' @examples
#'

print_outcomes <- function(dat=NULL,
                          ifr=NULL,
                          outcomes_parquet_file="usa-geoid-params-output_statelevel.parquet",
                          incidH_prob_dist="fixed",
                          incidH_prob_value=0.0175,
                          incidH_delay_dist="fixed",
                          incidH_delay_value=7,
                          incidH_duration_dist="fixed",
                          incidH_duration_value=7,
                          incidD_prob_dist="fixed",
                          incidD_prob_value=0.005,
                          incidD_delay_dist="fixed",
                          incidD_delay_value=20,
                          incidICU_prob_dist="fixed",
                          incidICU_prob_value=0.167,
                          incidICU_delay_dist="fixed",
                          incidICU_delay_value=3,
                          incidICU_duration_dist="fixed",
                          incidICU_duration_value=8,
                          incidVent_prob_dist="fixed",
                          incidVent_prob_value=0.463,
                          incidVent_delay_dist="fixed",
                          incidVent_delay_value=1,
                          incidVent_duration_dist="fixed",
                          incidVent_duration_value=7,
                          incidC_prob_dist="truncnorm",
                          incidC_prob_mean=0.2,
                          incidC_prob_sd=.1,
                          incidC_prob_a=0,
                          incidC_prob_b=1,
                          incidC_prob_dist_pert="truncnorm",
                          incidC_prob_mean_pert=0,
                          incidC_prob_sd_pert=0.05,
                          incidC_prob_a_pert=-1,
                          incidC_prob_b_pert=1,
                          incidC_delay_value=7,
                          incidC_delay_dist="fixed"
){

    if(is.null(ifr)){stop("You must specify a scenario/IFR name.")}

    if(!is.null(dat)){
        dat <- dat %>%
            collapse_intervention() %>%
            dplyr::filter(type=="outcome")

        if(nrow(dat) > 0){
            cat(paste0("\n"))
            for(i in 1:nrow(dat)){
                if(dat$template[i]=="Reduce"){
                    yaml_reduce_template(dat[i,])
                }
            }
        }
    }

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
        '          value:\n',
        '            distribution: ',incidH_prob_dist,'\n',
        '            value: ',incidH_prob_value,'\n',
        '        delay:\n',
        '          value:\n',
        '            distribution: ', incidH_delay_dist,'\n',
        '            value: ',incidH_delay_value,'\n',
        '        duration:\n',
        '          value:\n',
        '            distribution: ',incidH_duration_dist,'\n',
        '            value: ',incidH_duration_value,'\n',
        '          name: hosp_curr\n',
        '      incidD:\n',
        '        source: incidI\n',
        '        probability:\n',
        '          value:\n',
        '            distribution: ',incidD_prob_dist,'\n',
        '            value: ',incidD_prob_value,'\n',
        '        delay:\n',
        '          value:\n',
        '            distribution: ',incidD_delay_dist,'\n',
        '            value: ',incidD_delay_value,'\n',
        '      incidICU:\n',
        '        source: incidH\n',
        '        probability:\n',
        '          value:\n',
        '            distribution: ',incidICU_prob_dist,'\n',
        '            value: ',incidICU_prob_value,'\n',
        '        delay:\n',
        '          value:\n',
        '            distribution: ',incidICU_delay_dist,'\n',
        '            value: ',incidICU_delay_value,'\n',
        '        duration:\n',
        '          value:\n',
        '            distribution: ',incidICU_duration_dist,'\n',
        '            value: ',incidICU_duration_value,'\n',
        '          name: icu_curr\n',
        '      incidVent:\n',
        '        source: incidICU\n',
        '        probability: \n',
        '          value:\n',
        '            distribution: ',incidVent_prob_dist,'\n',
        '            value: ',incidVent_prob_value,'\n',
        '        delay:\n',
        '          value:\n',
        '            distribution: ',incidVent_delay_dist,'\n',
        '            value: ',incidVent_delay_value,'\n',
        '        duration:\n',
        '          value:\n',
        '            distribution: ',incidVent_duration_dist,'\n',
        '            value: ',incidVent_duration_value,'\n',
        '          name: vent_curr\n',
        '      incidC:\n',
        '        source: incidI\n',
        '        probability:\n',
        '          value:\n',
        '            distribution: ',incidC_prob_dist,'\n',
        '            mean: ', incidC_prob_mean, '\n',
        '            sd: ',incidC_prob_sd,'\n',
        '            a: ',incidC_prob_a,'\n',
        '            b: ', incidC_prob_b, '\n',
        '          perturbation:\n',
        '            distribution: ',incidC_prob_dist_pert,'\n',
        '            mean: ',incidC_prob_mean_pert,'\n',
        '            sd: ',incidC_prob_sd_pert,'\n',
        '            a: ',incidC_prob_a_pert,'\n',
        '            b: ',incidC_prob_b_pert,'\n',
        '        delay:\n',
        '          value:\n',
        '            distribution: ',incidC_delay_dist,'\n',
        '            value: ',incidC_delay_value,'\n'
    ))

    if(!is.null(dat)){
        cat(paste0(
            '  interventions:\n',
            '    settings:\n',
            '      ', ifr, ':\n',
            '        template: Stacked\n',
            '        scenarios: ["', paste0(unique(dat$name), collapse = '", "'), '"]\n'
        ))

    }
}

#' Print seir section
#'
#' @param sigma inverse of the incubation period in days - fraction or probability
#' @param gamma_dist specify if gamma is fixed or distributional
#' @param gamma_val inverse of the infectious period in days - fraction or probability
#' @param gamma_a minimum value of gamma - required if distribution is not "fixed"
#' @param gamma_b maximum value of gamma - required if distribution is not "fixed"
#' @param alpha transmission dampening parameter; reasonable values for respiratory viruses range from 0.88-0.99
#' @param R0_val basic reproduction number
#' @param R0_dist specify if R0 is fixed or distributional
#' @param R0_a minimum value of R0 - required if distribution is not "fixed"
#' @param R0_b maximum value of R0 - required if distribution is not "fixed"
#' @param incl_vacc specify whether to include vaccination compartments. If TRUE, must specify dose transmission/susceptibility parameters for each compartment and transition across compartments
#' @param dose_transmission_dist vector specifying whether transmission in each compartment is fixed; distributional is not yet supported
#' @param dose_transmission_val vector specifying transmission rates per compartment
#' @param dose_susceptibility_dist vector specifying whether susceptibility in each compartment is fixed; distributional is not yet supported
#' @param dose_susceptibility_val vector specifying reduction in risk per compartment
#' @param transitions_dist vector specifying whether transition rate between compartments is fixed; distributional is not yet supported
#' @param transitions_val vector specifying base transition rate between compartments
#'
#' @return
#' @export
#'
#' @examples
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
                       incl_vacc = TRUE,
                       dose_transmission_dist = c("fixed","fixed", "fixed"),
                       dose_transmission_val = c(0, 0, 0),
                       dose_susceptibility_dist = c("fixed","fixed", "fixed"),
                       dose_susceptibility_val = c(0, 0.75, 0.90),
                       transitions_dist = c("fixed", "fixed"),
                       transitions_val = c(0, 0.04)
){
    # TODO: add checks to compartment length

    sigma_dist = NULL
    alpha_dist = NULL

    cat(
        paste0("seir:\n",
               "  parameters:\n",
        paste_param_val(R0s_dist, space_length = 6),
        paste_param_val(gamma_dist, space_length = 6),
        paste_param_val(sigma_dist, space_length = 6),
        paste_param_val(alpha_dist, space_length = 6),
        if(incl_vacc){
            paste0(
                "    parallel_structure:\n",
                "      compartments:\n",
                "        unvaccinated:\n",
                "          transmissibility_reduction:\n",
                "            distribution: ", dose_transmission_dist[1],"\n",
                "            value: ", dose_transmission_val[1],"\n",
                "          susceptibility_reduction:\n",
                "            distribution: ", dose_susceptibility_dist[1],"\n",
                "            value: ", dose_susceptibility_val[1],"\n",
                "        first_dose:\n",
                "          transmissibility_reduction:\n",
                "            distribution: ", dose_transmission_dist[2],"\n",
                "            value: ", dose_transmission_val[2],"\n",
                "          susceptibility_reduction:\n",
                "            distribution: ", dose_susceptibility_dist[2],"\n",
                "            value: ", dose_susceptibility_val[2],"\n",
                "        second_dose:\n",
                "          transmissibility_reduction:\n",
                "            distribution: ", dose_transmission_dist[3],"\n",
                "            value: ", dose_transmission_val[3],"\n",
                "          susceptibility_reduction:\n",
                "            distribution: ", dose_susceptibility_dist[3],"\n",
                "            value: ", dose_susceptibility_val[3], "\n",
                "      transitions:\n",
                "        - from: unvaccinated\n",
                "          to: first_dose\n",
                "          rate:\n",
                "            distribution: ", transitions_dist[1],"\n",
                "            value: ", transitions_val[1],"\n",
                "        - from: first_dose\n",
                "          to: second_dose\n",
                "          rate:\n",
                "            distribution: ", transitions_dist[2],"\n",
                "            value: ", transitions_val[2],"\n"
                )
        } else{""},
        "\n"
        )
    )

}

#' Print header section
#'
#' @param sim_name
#' @param sim_start_date
#' @param sim_end_date
#' @param n_simulations
#' @param dt
#' @param census_year
#' @param base_path
#' @param sim_states
#' @param setup_name
#' @param geodata_file
#' @param mobility_file
#' @param popnodes
#' @param nodenames
#' @param include_in_report
#' @param state_level
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
                        dt = 0.25,
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
        "dt: 0.25","\n",
        "\n",
        "spatial_setup:\n",
        "  census_year: 2019\n",
        "  base_path: data\n",
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
#'
#' @param method
#' @param seeding_file_type
#' @param folder_path
#' @param lambda_file
#' @param perturbation_sd
#'
#' @return
#' @export
#'
#' @examples
#'
print_seeding <- function(method = "FolderDraw",
                         seeding_file_type = "seed",
                         folder_path = "importation/minimal/",
                         lambda_file = "data/minimal/seeding.csv",
                         perturbation_sd = 1
){

    cat(paste0(
        "\n",
        "seeding:\n",
        "  method: ", method,"\n",
        "  seeding_file_type: ", seeding_file_type,"\n",
        "  folder_path: ", folder_path,"\n",
        "  lambda_file: ", lambda_file,"\n",
        "  perturbation_sd: ", perturbation_sd, "\n",
        "\n"
    ))
}

#' Print filtering section
#'
#' @param method
#' @param seeding_file_type
#' @param folder_path
#' @param lambda_file
#' @param perturbation_sd
#'
#' @return
#' @export
#'
#' @examples
#'
print_filtering <- function(sims_per_slot = 300,
                            data_path = "data/us_data.csv",
                            gt_source = "csse",
                            stat_names = c("sum_deaths", "sum_confirmed"),
                            aggregator = "sum",
                            period = "1 weeks",
                            sim_var = c("incidD", "incidC"),
                            data_var = c("death_incid", "confirmed_incid"),
                            remove_na = FALSE,
                            add_one = c(FALSE, TRUE),
                            ll_dist = c("sqrtnorm", "pois"),
                            ll_param = .4,
                            final_print = FALSE){

    if(length(stat_names)!=length(data_var)) stop("stat_names and data_var must be the same length")

    cat(paste0(
        "\n",
        "filtering:\n",
        "  simulations_per_slot: ", sims_per_slot, "\n",
        "  data_path: ", data_path, "\n",
        '  gt_source: "', gt_source, '"\n',
        "  statistics:\n"
    ))

    n_vars <- length(stat_names)

    if(length(aggregator)!=n_vars & length(aggregator)==1){
        aggregator <- rep(aggregator, n_vars)
    }

    if(length(period)!=n_vars & length(period)==1){
        period <- rep(period, n_vars)
    }

    if(length(remove_na)!=n_vars & length(remove_na)==1){
        remove_na <- rep(remove_na, n_vars)
    }

    if(length(add_one)!=n_vars & length(add_one)==1){
        add_one <- rep(add_one, n_vars)
    }

    if(length(ll_dist)!=n_vars & length(ll_dist)==1){
        ll_dist <- rep(ll_dist, n_vars)
    }

    if(length(ll_param)!=n_vars & length(ll_param)==1){
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

#' Create hierarchical terms
#'
#' @param npi_name
#' @param module
#' @param geo_group_col
#' @param transform
#'
#' @return
#' @export
#'
#' @examples
#'

print_hierarchical <- function(npi_name = c("local_variance", "probability_incidI_incidC"),
                               module = c("seir", "hospitalization"),
                               geo_group_col = "USPS",
                               transform = c("none", "logit"),
                               final_print = FALSE){

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
#' Create priors
#'
#' @param dat
#' @param npi_name
#' @param module
#' @param dist
#' @param param_low
#' @param param_high
#'
#' @return
#' @export
#'
#' @examples
#'

print_prior <- function(dat,
                        npi_name = c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar", "Seas_apr",
                                     "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
                                     "Seas_oct", "Seas_nov", "Seas_dec"),
                        module = "seir",
                        dist = "normal",
                        param_low = NULL,
                        param_high = 1
){

    dat <- dat %>%
        collapse_intervention() %>%
        dplyr::filter(name %in% npi_name) %>%
        dplyr::mutate(value_mean = dplyr::if_else(is.na(value_mean), 0, value_mean))

    if(length(module)!=length(npi_name) & length(module)==1){
        module <- rep(module, length(npi_name))
    }

    if(length(dist)!=length(npi_name) & length(dist)==1){
        dist <- rep(dist, length(npi_name))
    }


    if(length(param_high)!=length(npi_name) & length(param_high)==1){
        param_high <- rep(param_high, length(npi_name))
    }

    if(length(param_low)!=length(npi_name) & length(param_low)==1){
        param_low <- rep(param_low, length(npi_name))
    }

    if(is.null(param_low)){
        for(i in 1:length(npi_name)){
            param_low[i] <- dat %>%
                dplyr::filter(name == npi_name[i]) %>%
                dplyr::pull(value_mean)

        }
    }

    cat(paste0(
        "  priors:\n"))

    for(i in 1:nrow(dat)){
        cat(paste0(
            "    ", paste0(npi_name[i], "_prior"),":\n",
            "      name: ", npi_name[i], "\n",
            "      module: ", module[i], "\n",
            "      likelihood:\n",
            "        dist: ", dist[i], "\n",
            "        param:\n",
            "        - ", param_low[i], "\n",
            "        - ", param_high[i], "\n"
        ))
    }
}

#' Convenience function to generate/save config
#'
#' @param intervention_path
#' @param config_name
#' @param save_path whether to save it in a specific directory; default NULL saves it in a temporary directory
#'
#' @return
#' @export
#'
#' @examples
#'
generate_config <- function(intervention_path,
                            config_name = "config.yml",
                            save_path = NULL,
                            header_sim_name = "USA",
                            header_sim_start_date = "2020-01-01",
                            header_sim_end_date = "2021-08-07",
                            header_n_simulations = 300,
                            header_dt = 0.25,
                            header_census_year = 2019,
                            header_base_path = "data",
                            header_setup_name = "USA",
                            header_geodata = "geodata.csv",
                            header_mobility = "mobility.csv",
                            header_popnodes = "pop2019est",
                            header_nodenames = "geoid",
                            header_include_in_report = "include_in_report",
                            header_state_level = TRUE,
                            seeding_method = "FolderDraw",
                            seeding_file_type = "seed",
                            seeding_folder_path = "importation/minimal/",
                            seeding_lambda_file = "data/minimal/seeding.csv",
                            seeding_perturbation_sd = 1,
                            seir_alpha_val = 0.99,
                            seir_sigma_val = 1/5.2 ,
                            seir_gamma_dist = "fixed",
                            seir_gamma_val = 1/3.83,
                            seir_gamma_sd = NULL,
                            seir_gamma_a = 1/4.5,
                            seir_gamma_b = 1/3,
                            seir_R0s_dist = "uniform",
                            seir_R0s_val = 2.3,
                            seir_R0s_sd = NULL,
                            seir_R0s_a = 2,
                            seir_R0s_b = 3,
                            seir_incl_vacc = TRUE,
                            seir_dose_transmission_dist = c("fixed", "fixed", "fixed"),
                            seir_dose_transmission_val = c(0, 0, 0),
                            seir_dose_susceptibility_dist = c("fixed", "fixed", "fixed"),
                            seir_dose_susceptibility_val = c(0, 0.75, 0.90),
                            seir_transitions_dist = c("fixed", "fixed"),
                            seir_transitions_val = c(0, 0.04),
                            transmission_scenario = "inference",
                            outcomes_ifr="med",
                            outcomes_parquet_file="usa-geoid-params-output_statelevel.parquet",
                            outcomes_incidH_prob_dist="fixed",
                            outcomes_incidH_prob_value=0.0175,
                            outcomes_incidH_delay_dist="fixed",
                            outcomes_incidH_delay_value=7,
                            outcomes_incidH_duration_dist="fixed",
                            outcomes_incidH_duration_value=7,
                            outcomes_incidD_prob_dist="fixed",
                            outcomes_incidD_prob_value=0.005,
                            outcomes_incidD_delay_dist="fixed",
                            outcomes_incidD_delay_value=20,
                            outcomes_incidICU_prob_dist="fixed",
                            outcomes_incidICU_prob_value=0.167,
                            outcomes_incidICU_delay_dist="fixed",
                            outcomes_incidICU_delay_value=3,
                            outcomes_incidICU_duration_dist="fixed",
                            outcomes_incidICU_duration_value=8,
                            outcomes_incidVent_prob_dist="fixed",
                            outcomes_incidVent_prob_value=0.463,
                            outcomes_incidVent_delay_dist="fixed",
                            outcomes_incidVent_delay_value=1,
                            outcomes_incidVent_duration_dist="fixed",
                            outcomes_incidVent_duration_value=7,
                            outcomes_incidC_prob_dist="truncnorm",
                            outcomes_incidC_prob_mean=0.2,
                            outcomes_incidC_prob_sd=.1,
                            outcomes_incidC_prob_a=0,
                            outcomes_incidC_prob_b=1,
                            outcomes_incidC_prob_dist_pert="truncnorm",
                            outcomes_incidC_prob_mean_pert=0,
                            outcomes_incidC_prob_sd_pert=0.05,
                            outcomes_incidC_prob_a_pert=-1,
                            outcomes_incidC_prob_b_pert=1,
                            outcomes_incidC_delay_value=7,
                            outcomes_incidC_delay_dist="fixed",
                            filtering_sims_per_slot = 300,
                            filtering_data_path = "data/us_data.csv",
                            filtering_gt_source = "csse",
                            filtering_stat_names = c("sum_deaths", "sum_confirmed"),
                            filtering_aggregator = "sum",
                            filtering_period = "1 weeks",
                            filtering_sim_var = c("incidD", "incidC"),
                            filtering_data_var = c("death_incid", "confirmed_incid"),
                            filtering_remove_na = FALSE,
                            filtering_add_one = c(FALSE, TRUE),
                            filtering_ll_dist = c("sqrtnorm", "pois"),
                            filtering_ll_param = .4,
                            filtering_final_print = FALSE,
                            hierarchical_npi_name = c("local_variance", "probability_incidI_incidC"),
                            hierarchical_module = c("seir", "hospitalization"),
                            hierarchical_geo_group_col = "USPS",
                            hierarchical_transform = c("none", "logit"),
                            hierarchical_final_print = FALSE,
                            prior_npi_name = c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar", "Seas_apr",
                                               "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
                                               "Seas_oct", "Seas_nov", "Seas_dec"),
                            prior_module = "seir",
                            prior_dist = "normal",
                            prior_param_low = NULL,
                            prior_param_high = 1){

    interventions <- readr::read_csv(intervention_path)

    if(is.null(save_path)){
        config_name <- file.path(tempdir(), config_name)
    } else{
        config_name <- file.path(save_path, config_name)
        print(paste0("Config saved in ", config_name))
    }

    sink(config_name)

    print_header(sim_name = header_sim_name,
                 sim_start_date = header_sim_start_date,
                 sim_end_date = header_sim_end_date,
                 n_simulations = header_n_simulations,
                 dt = header_dt,
                 census_year = header_census_year,
                 base_path = header_census_year,
                 sim_states = unique(interventions$USPS[!interventions$USPS %in% c("", "all") & !is.na(interventions$USPS)]),
                 setup_name = header_setup_name,
                 geodata = header_geodata,
                 mobility = header_mobility,
                 popnodes = header_popnodes,
                 nodenames = header_nodenames,
                 include_in_report = header_include_in_report,
                 state_level = header_state_level)

    print_seeding(method = seeding_method,
                  seeding_file_type = seeding_file_type,
                  folder_path = seeding_folder_path,
                  lambda_file = seeding_lambda_file ,
                  perturbation_sd = seeding_perturbation_sd)

    print_seir(alpha_val = seir_alpha_val,
               sigma_val = seir_sigma_val,
               gamma_dist = seir_gamma_dist,
               gamma_val = seir_gamma_val,
               gamma_sd = seir_gamma_sd,
               gamma_a = seir_gamma_a,
               gamma_b = seir_gamma_b,
               R0s_dist = seir_R0s_dist,
               R0s_val = seir_R0s_val,
               R0s_sd = seir_R0s_sd,
               R0s_a = seir_R0s_a,
               R0s_b = seir_R0s_b,
               incl_vacc = seir_incl_vacc,
               dose_transmission_dist = seir_dose_transmission_dist,
               dose_transmission_val = seir_dose_transmission_val,
               dose_susceptibility_dist = seir_dose_susceptibility_dist,
               dose_susceptibility_val = seir_dose_susceptibility_val,
               transitions_dist = seir_transitions_dist,
               transitions_val = seir_transitions_val)

    print_transmission_interventions(dat = interventions,
                                     scenario = transmission_scenario)

    print_outcomes(dat = interventions,
                   ifr = outcomes_ifr,
                   outcomes_parquet_file = outcomes_parquet_file,
                   incidH_prob_dist= outcomes_incidH_prob_dist,
                   incidH_prob_value= outcomes_incidH_prob_value,
                   incidH_delay_dist= outcomes_incidH_delay_dist,
                   incidH_delay_value= outcomes_incidH_delay_value,
                   incidH_duration_dist= outcomes_incidH_duration_dist,
                   incidH_duration_value= outcomes_incidH_duration_value,
                   incidD_prob_dist= outcomes_incidD_prob_dist,
                   incidD_prob_value= outcomes_incidD_prob_value,
                   incidD_delay_dist= outcomes_incidD_delay_dist,
                   incidD_delay_value= outcomes_incidD_delay_value,
                   incidICU_prob_dist= outcomes_incidICU_prob_dist,
                   incidICU_prob_value= outcomes_incidICU_prob_value,
                   incidICU_delay_dist= outcomes_incidICU_delay_dist,
                   incidICU_delay_value= outcomes_incidICU_delay_value,
                   incidICU_duration_dist= outcomes_incidICU_duration_dist,
                   incidICU_duration_value= outcomes_incidICU_duration_value,
                   incidVent_prob_dist= outcomes_incidVent_prob_dist,
                   incidVent_prob_value= outcomes_incidVent_prob_value,
                   incidVent_delay_dist= outcomes_incidVent_delay_dist,
                   incidVent_delay_value= outcomes_incidVent_delay_value,
                   incidVent_duration_dist= outcomes_incidVent_duration_dist,
                   incidVent_duration_value= outcomes_incidVent_duration_value,
                   incidC_prob_dist= outcomes_incidC_prob_dist,
                   incidC_prob_mean= outcomes_incidC_prob_mean,
                   incidC_prob_sd= outcomes_incidC_prob_sd,
                   incidC_prob_a= outcomes_incidC_prob_a,
                   incidC_prob_b= outcomes_incidC_prob_b,
                   incidC_prob_dist_pert= outcomes_incidC_prob_dist_pert,
                   incidC_prob_mean_pert= outcomes_incidC_prob_mean_pert,
                   incidC_prob_sd_pert= outcomes_incidC_prob_sd_pert,
                   incidC_prob_a_pert = outcomes_incidC_prob_a_pert,
                   incidC_prob_b_pert= outcomes_incidC_prob_b_pert,
                   incidC_delay_value= outcomes_incidC_delay_value,
                   incidC_delay_dist= outcomes_incidC_delay_dist)

    print_filtering(sims_per_slot = filtering_sims_per_slot,
                    data_path = filtering_data_path,
                    gt_source = filtering_gt_source,
                    stat_names = filtering_stat_names,
                    aggregator = filtering_aggregator,
                    period = filtering_period,
                    sim_var = filtering_sim_var,
                    data_var = filtering_data_var,
                    remove_na = filtering_remove_na,
                    add_one = filtering_add_one,
                    ll_dist = filtering_ll_dist,
                    ll_param = filtering_ll_param,
                    final_print = filtering_final_print)

    print_hierarchical(npi_name = hierarchical_npi_name,
                       module = hierarchical_module,
                       geo_group_col = hierarchical_geo_group_col,
                       transform = hierarchical_transform,
                       final_print = hierarchical_final_print)

    print_prior(dat = interventions,
                npi_name = prior_npi_name,
                module = prior_module,
                dist = prior_dist,
                param_low = prior_param_low,
                param_high = prior_param_high)

    sink()

    config <- yaml::read_yaml(config_name)

    if(is.null(save_path)){
        unlink(config_name)
    }

    return(config)
}

