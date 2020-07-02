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



        ##print(stat)
        ##print(ll_adjs)
        ##print(range(ll_adjs$likadj))
        ##print(ll_adjs%>%filter(is.na(likadj)))

        ##probably a more efficient what to do this, but unclear...
        likelihood_data<- dplyr::left_join(likelihood_data, ll_adjs) %>%
            tidyr::replace_na(list(likadj=0))%>% ##avoid unmatched location problems
            dplyr::mutate(ll = ll + likadj) %>%
            dplyr::select(-likadj)
    }


    ##Update lieklihoods based on priors
    for (prior in names(defined_priors)) {
        if (defined_priors[[prior]]$module=="seir") {
            ll_adjs <- snpi %>%
                dplyr::filter(npi_name==defined_priors[[prior]]$name)%>%
                dplyr::mutate(likadj=calc_prior_likadj(reduction,
                                                defined_priors[[prior]]$likelihood$dist,
                                                defined_priors[[prior]]$likelihood$param
                                                ))%>%
                select(geoid, likadj)

        }  else if (defined_priors[[prior]]$module=="hospitalization") {

            ll_adjs <- hpar %>%
                dplyr::filter(parameter==defined_priors[[prior]]$name)%>%
                dplyr::mutate(likadj=calc_prior_likadj(value,
                                                defined_priors[[prior]]$likelihood$dist,
                                                defined_priors[[prior]]$likelihood$param
                                                ))%>%
                dplyr::select(geoid, likadj)

        } else {
            stop("unsupported prior module")
        }

        ##print(prior)
        ##print(ll_adjs)
        ##print(range(ll_adjs$likadj))

        ##probably a more efficient what to do this, but unclear...
        likelihood_data<- dplyr::left_join(likelihood_data, ll_adjs) %>%
            dplyr::mutate(ll = ll + likadj) %>%
            dplyr::select(-likadj)
    }



    return(likelihood_data)
}



##'
##' Function that performs the necessary file copies the end of an MCMC iteration of
##' filter_MC.
##'
##'@param current_index the current indec in the run
##'@param slot what is the current slot numbe
##'@param block what is the current block
##'@param run_id what is the id of this run
##'@param global_local_prefix the prefix to be put on both global and local runs.
##'@param gf_prefix the prefix for the directory containing the current globally accepted files.
##'@param global_block_prefix prefix that describes this block.
##'
##'@return TRUE if this succeded.
##'
##'@export
##'
perform_MCMC_step_copies <- function(current_index,
                                     slot,
                                     block,
                                     run_id,
                                     global_local_prefix,
                                     gf_prefix,
                                     global_block_prefix) {


    rc <- list()

    if(current_index != 0){
        rc$hosp_gf <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hosp','parquet'),
            covidcommon::create_file_name(run_id,gf_prefix,slot,'hosp','parquet'),
	    overwrite = TRUE
        )

        rc$llik_gf <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'llik','parquet'),
            covidcommon::create_file_name(run_id,gf_prefix,slot,'llik','parquet'),
	    overwrite = TRUE
        )

        rc$snpi_gf <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'snpi','parquet'),
            covidcommon::create_file_name(run_id,gf_prefix,slot,'snpi','parquet'),
	    overwrite = TRUE
        )

        rc$spar_gf <-file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'spar','parquet'),
            covidcommon::create_file_name(run_id,gf_prefix,slot,'spar','parquet'),
	    overwrite = TRUE
        )

        rc$hpar_gf <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hpar','parquet'),
            covidcommon::create_file_name(run_id,gf_prefix,slot,'hpar','parquet'),
	    overwrite = TRUE
        )

        rc$hosp_block <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hosp','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'hosp','parquet')
        )


        rc$llik_block <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'llik','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'llik','parquet')
        )

        rc$snpi_block <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'snpi','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'snpi','parquet')
        )

        rc$spar_block <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'spar','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'spar','parquet')
        )


        rc$hpar_block <- file.copy(
            covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hpar','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'hpar','parquet')
        )
    } else {
        rc$hosp_prevblk <- file.copy(
            covidcommon::create_file_name(run_id,global_block_prefix,block - 1 ,'hosp','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'hosp','parquet')
        )

        rc$llik_prevblk <- file.copy(
            covidcommon::create_file_name(run_id,global_block_prefix,block - 1,'llik','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'llik','parquet')
        )

        rc$snpi_prvblk <-file.copy(
            covidcommon::create_file_name(run_id,global_block_prefix,block - 1,'snpi','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'snpi','parquet')
        )

        rc$spar_prvblk <- file.copy(
            covidcommon::create_file_name(run_id,global_block_prefix,block - 1,'spar','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'spar','parquet')
        )

        rc$hpar_prvblk <- file.copy(
            covidcommon::create_file_name(run_id,global_block_prefix,block - 1,'hpar','parquet'),
            covidcommon::create_file_name(run_id,global_block_prefix,block,'hpar','parquet')
        )
    }


    return(rc)

}


