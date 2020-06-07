###Functions that help with the running of Filter MC. Can take in full confit.



##'Function that performs aggregation and calculates likelihood data across all given locations.
##'
##' @param all_locations all of the locoations to calculate likelihood for
##' @param sim_hosp  the hospital data for the simulations
##' @param obs_nodename the name of the column containg locations.
##' @param config the full configuraiton setup
##' @param obs the full observed data
##' @param data_stat the data we are going to compare to aggregated to the right statistic
##' @param hosp_file the filename of the hosp file being used (unclear if needed in scope)
##' @param hierarchical_stats the hierarchical stats to use
##' @param defined_priors information on defined priors. 
##' @param geodata the geographics data to help with hierarchies
##' @param snpi the file with the npi information
##' @param hpar data frame of hospitalization parameters
##' 
##' @return a data frame of likelihood data.
##'
##' @export
##'
aggregate_and_calc_loc_likelihoods <- function(all_locations,
                                               sim_hosp,                                              
                                               obs_nodename,
                                               config,
                                               obs,
                                               data_stats,
                                               hosp_file,
                                               hierarchical_stats,
                                               defined_priors,
                                               geodata,
                                               snpi=NULL,
                                               hpar=NULL) {

    ##Holds the likelihoods for all locations
    likelihood_data <- list()



    ##iterate over locations
    for(location in all_locations) {

        ##Pull out the local sim from the complete sim
        local_sim_hosp <- dplyr::filter(sim_hosp, !!rlang::sym(obs_nodename) == location) %>%
            dplyr::filter(time %in% unique(obs$date[obs$geoid == location]))

        
        sim_stats <- inference::getStats(
                                    local_sim_hosp,
                                    "time",
                                    "sim_var",
                                    stat_list = config$filtering$statistics
                                )
        

  
        
        ## Get observation statistics
        log_likelihood <- list()
        for(var in names(data_stats[[location]])) {
          
            log_likelihood[[var]] <- inference::logLikStat(
                                                    obs = data_stats[[location]][[var]]$data_var,
                                                    sim = sim_stats[[var]]$sim_var,
                                                    dist = config$filtering$statistics[[var]]$likelihood$dist,
                                                    param = config$filtering$statistics[[var]]$likelihood$param,
                                                    add_one = config$filtering$statistics[[var]]$add_one
                                                )
        }
        
        ## Compute log-likelihoods
        
        likelihood_data[[location]] <- dplyr::tibble(
                                                  ll = sum(unlist(log_likelihood)),
                                                  filename = hosp_file,
                                                  geoid = location
                                              )
        names(likelihood_data)[names(likelihood_data) == 'geoid'] <- obs_nodename
    }
    
    likelihood_data <- likelihood_data %>% do.call(what=rbind)
    
    ##Update  liklihood data based on hierarchical_stats 
    for (stat in names(hierarchical_stats)) {
        
        if (hierarchical_stats[[stat]]$module=="seir") {
            ll_adjs <- inference::calc_hierarchical_likadj(stat=hierarchical_stats[[stat]]$name,
                                                           infer_frame = snpi,
                                                           geodata = geodata,
                                                           geo_group_column = hierarchical_stats[[stat]]$geo_group_col,
                                                           transform = hierarchical_stats[[stat]]$transform
                                                           )
            
        } else  if (hierarchical_stats[[stat]]$module=="hospitalization") {

            ll_adjs <- inference::calc_hierarchical_likadj(stat=hierarchical_stats[[stat]]$name,
                                                           infer_frame = hpar,
                                                           geodata = geodata,
                                                           geo_group_column = hierarchical_stats[[stat]]$geo_group_col,
                                                           transform = hierarchical_stats[[stat]]$transform,
                                                           stat_col = "value",
                                                           stat_name_col="parameter"
                                                           )
            
        } else {
            stop("unsupported hierarchical stat module")
        }
            
        ##probably a more efficient what to do this, but unclear...
        likelihood_data<- left_join(likelihood_data, ll_adjs) %>%
            mutate(ll = ll + likadj) %>%
            select(-likadj)
            
    }


    ##Update lieklihoods based on priors
    for (prior in names(defined_priors)) {
        if (defined_priors[[prior]]$module=="seir") {
            ll_adjs <- snpi %>%
                filter(npi_name==defined_priors[[prior]]$name)%>%
                mutate(likadj=calc_prior_likadj(reduction,
                                                defined_priors[[prior]]$likelihood$dist,
                                                defined_priors[[prior]]$likelihood$param
                                                ))%>%
                select(geoid, likadj)
            
        }  else if (defined_priors[[prior]]$module=="hospitalization") {

            ll_adjs <- hpar %>%
                filter(parameter==defined_priors[[prior]]$name)%>%
                mutate(likadj=calc_prior_likadj(value,
                                                defined_priors[[prior]]$likelihood$dist,
                                                defined_priors[[prior]]$likelihood$param
                                                ))%>%
                select(geoid, likadj)
            
        } else {
            stop("unsupported prior module")
        }
        
        ##probably a more efficient what to do this, but unclear...
        likelihood_data<- left_join(likelihood_data, ll_adjs) %>%
            mutate(ll = ll + likadj) %>%
            select(-likadj)
    }

    

    return(likelihood_data)
}
