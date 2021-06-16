#' 
#' @param dat  
#'
#' @return
#' @export
#'
#' @examples
collapse_intervention<- function(dat
){
    #TODO: add number to repeated names
    #TODO add a check that all end_dates are the same
    mtr <- dat %>%
        dplyr::filter(template=="MultiTimeReduce") %>%
        dplyr::mutate(end_date=paste0("end_date: ", end_date), 
                      start_date=paste0("- start_date: ", start_date)) %>%
        tidyr::unite(col="period", sep="\n              ", start_date:end_date) %>%
        # dplyr::group_by(dplyr::across(-USPS:-period)) %>%
        dplyr::group_by(dplyr::across(-period)) %>%
        dplyr::summarize(period = paste0(period, collapse="\n            ")) %>%
        dplyr::group_by(dplyr::across(-geoid)) %>%
        dplyr::summarize(geoid = paste0(geoid, collapse='", "')) %>%
        # dplyr::mutate(name = name,
        #               period = paste0("            ", period, "\n")) %>%
        dplyr::mutate(period = paste0("            ", period)) 
    
    reduce <- dat %>% 
        dplyr::select(USPS, geoid, start_date, end_date, name, template, type, category, parameter, starts_with("value_"), starts_with("pert_")) %>%
        #dplyr::filter(template=="ReduceR0" & type=="transmission") %>%
        dplyr::filter(template %in% c("ReduceR0", "Reduce", "ReduceIntervention")) %>% 
        dplyr::mutate(end_date=paste0("period_end_date: ", end_date), 
                      start_date=paste0("period_start_date: ", start_date)) %>%
        tidyr::unite(col="period", sep="\n      ", start_date:end_date) %>%
        dplyr::mutate(period = paste0("      ", period, "\n")) %>% 
        dplyr::ungroup() %>%
        dplyr::add_count(dplyr::across(-USPS)) %>%
        dplyr::group_by(across(-USPS:-geoid)) %>%
        dplyr::summarize(USPS_geo = paste0(unique(geoid), collapse = '-'),
                         geoid = paste0(unique(geoid), collapse='", "'),
                         USPS = paste0(unique(USPS), collapse='-')
        ) %>%
        dplyr::ungroup() %>% 
        dplyr::add_count(dplyr::across(-USPS)) %>%
        dplyr::mutate(name = dplyr::case_when(category =="local_variance" | USPS %in% c("all", "") ~ name, 
                                              n==1 & template=="Reduce" ~ paste0(USPS, "_", name),
                                              template=="Reduce" ~ paste0(USPS_geo, "_", name),
                                              n==1 & template!="ReduceIntervention" ~ paste0(USPS, name),
                                              template!="ReduceIntervention" ~ paste0(USPS_geo, name),
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
#' @param dat df with outputs from any of set_*_params functions
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

#' Print intervention text for MultiTimeReduce interventions
#'
#' @param dat df with outputs from any of set_*_params functions
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


#' Print stack interventions
#'
#' @param dat df with outputs from 
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

#' Convenience function to print interventions and stack them stack interventions
#'
#' @param dat df with outputs from any of set_*_params functions
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
        '    - ', scenario, '\n'
    ))
    
    dat <- collapse_intervention(dat) %>% 
        dplyr::filter(type == "transmission")
    
    reduce <- dat %>%
        dplyr::filter(template != "MultiTimeReduce")
    
    if(nrow(reduce) > 0 ){
        for(i in 1:nrow(reduce)){
            
            yaml_reduce_template(reduce[i,])

        }
    }
    
    mtr <- dat %>% 
        dplyr::filter(template=="MultiTimeReduce")
    
    npi_names <- mtr %>% dplyr::distinct(name) %>% dplyr::pull()
    for(i in 1:length(npi_names)){
        npi <- mtr %>%
            dplyr::filter(name == npi_names[i])
        
        yaml_mtr_template(npi)
    }
    
    yaml_stack(dat, 
               scenario)
}


#' Print outcomes section
#'
#' @param dat=NULL   
#' @param ifr   
#' @param outcomes_parquet_file   
#' @param incidH_prob_dist  
#' @param incidH_prob_value  
#' @param incidH_delay_dist  
#' @param incidH_delay_value   
#' @param incidH_duration_dist  
#' @param incidH_duration_value  
#' @param incidD_prob_dist 
#' @param incidD_prob_value 
#' @param incidD_delay_dist  
#' @param incidD_delay_value
#' @param incidICU_prob_dist  
#' @param incidICU_prob_value 
#' @param incidICU_delay_dist 
#' @param incidICU_delay_value  
#' @param incidICU_duration_dist 
#' @param incidICU_duration_value 
#' @param incidVent_prob_dist  
#' @param incidVent_prob_value 
#' @param incidVent_delay_dist 
#' @param incidVent_delay_value 
#' @param incidVent_duration_dist 
#' @param incidVent_duration_value  
#' @param incidC_prob_dist 
#' @param incidC_prob_mean 
#' @param incidC_prob_sd 
#' @param incidC_prob_a 
#' @param incidC_prob_b 
#' @param incidC_prob_dist_pert 
#' @param incidC_prob_mean_pert  
#' @param incidC_prob_sd_pert 
#' @param incidC_prob_a_pert  
#' @param incidC_prob_b_pert 
#' @param incidC_delay_value 
#' @param incidC_delay_dist 
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
    
    if(!is.null(dat) & is.null(ifr)){stop("You must specify a scenario/IFR name.")}
    
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
        '            distribution: fixed',incidH_duration_dist,'\n',
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
#' @param fixed_R0
#' @param sigma
#' @param gamma_dist
#' @param gamma_val
#' @param gamma_high
#' @param alpha
#' @param R0_val
#' @param R0_dist
#' @param R0_low
#' @param R0_high
#' @param incl_vacc
#' @param dose_transmission_dist
#' @param dose_transmission_val
#' @param dose_susceptibility_dist
#' @param dose_susceptibility_val
#' @param transitions_dist
#' @param transitions_val
#'
#' @return 
#' @export
#'
#' @examples
#' 

print_seir <- function(fixed_R0 = TRUE, 
                      sigma = 1/5.2,
                      gamma_dist = "fixed", 
                      gamma_val = 1/3.83, 
                      gamma_low = 1/4.5, 
                      gamma_high = 1/3,
                      alpha = 0.99,
                      R0_val = 2.3, 
                      R0_dist = "uniform", 
                      R0_low = 2, 
                      R0_high = 3, 
                      incl_vacc = TRUE, 
                      dose_transmission_dist = c("fixed","fixed", "fixed"), 
                      dose_transmission_val = c(0, 0, 0), 
                      dose_susceptibility_dist = c("fixed","fixed", "fixed"), 
                      dose_susceptibility_val = c(0, 0.75, 0.90), 
                      transitions_dist = c("fixed", "fixed"),
                      transitions_val = c(0, 0.04)
){
    
    cat(
        if(fixed_R0){
            paste0(
                "seir:\n",
                "  parameters:\n",
                "    alpha: ", alpha, "\n",
                "    sigma: ", sigma,"\n",
                "    gamma:\n",
                "      distribution: fixed\n",
                "      value: ", gamma_val,"\n",
                "    R0s:\n",
                "      distribution: fixed\n",
                "      value: ", R0_val,"\n")
        } else{
            paste0(
                "seir:\n",
                "  parameters:\n",
                "    alpha: ", alpha,"\n",
                "    sigma: ", sigma,"\n",
                "    gamma:\n",
                "      distribution: ", gamma_dist,"\n",
                "      low: ", gamma_low,"\n",
                "      high: ", gamma_high,"\n",
                "    R0s:\n",
                "      distribution: ", R0_dist,"\n",
                "      low: ", R0_low,"\n",
                "      high: ", R0_high,"\n")
        },
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
                "            value: ", dose_susceptibility_val[2],"\n",
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
                "            value: ", transitions_val[2],"\n")
        } else {""},
        sep="")
    
    cat(paste0("\n"))
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
                            ll_param = .4){
    
    if(length(stat_names)!=length(data_var)) stop("stat_names and data_var must be the same length")
    
    cat(paste0(
        "\n",
        "filtering:\n", 
        "  simulation_per_slot: ", sims_per_slot, "\n", 
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
                               transform = c("none", "logit")){
    
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

