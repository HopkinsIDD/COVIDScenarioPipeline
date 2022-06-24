#' @include initialize_conditions.R
###Functions that help with the running of Filter MC. Can take in full confit.


##'Function that performs aggregation and calculates likelihood data across all given locations.
##'
##' @param all_locations all of the locations to calculate likelihood for
##' @param modeled_outcome  the hospital data for the simulations
##' @param obs_nodename the name of the column containing locations.
##' @param config the full configuration setup
##' @param obs the full observed data
##' @param ground_truth_data the data we are going to compare to aggregated to the right statistic
##' @param hosp_file the filename of the hosp file being used (unclear if needed in scope)
##' @param hierarchical_stats the hierarchical stats to use
##' @param defined_priors information on defined priors.
##' @param geodata the geographics data to help with hierarchies
##' @param snpi the file with the npi information for seir, only used for heirarchical likelihoods
##' @param hnpi the file with the npi information for outcomes, only used for heirarchical likelihoods
##' @param hpar data frame of hospitalization parameters, only used for heirarchical likelihoods
##'
##' @return a data frame of likelihood data.
##'
##' @export
##'
aggregate_and_calc_loc_likelihoods <- function(
  all_locations,
  modeled_outcome,
  obs_nodename,
  targets_config,
  obs,
  ground_truth_data,
  hosp_file,
  hierarchical_stats,
  defined_priors,
  geodata,
  snpi=NULL,
  hnpi=NULL,
  hpar=NULL,
  start_date = NULL,
  end_date = NULL
) {

  ##Holds the likelihoods for all locations
  likelihood_data <- list()



  ##iterate over locations
  for (location in all_locations) {
    ##Pull out the local sim from the complete sim
    this_location_modeled_outcome <-
      ## Filter to this location
      dplyr::filter(
        modeled_outcome,
        !!rlang::sym(obs_nodename) == location,
        time %in% unique(obs$date[obs$geoid == location])
      ) %>%
      ## Reformat into form the algorithm is looking for
      inference::getStats(
        "time",
        "sim_var",
        stat_list = targets_config,
        start_date = start_date,
        end_date = end_date
      )


    ## Get observation statistics
    this_location_log_likelihood <- 0
    for (var in names(ground_truth_data[[location]])) {


      this_location_log_likelihood <- this_location_log_likelihood +
        ## Actually compute likelihood for this location and statistic here:
        sum(inference::logLikStat(
          obs = ground_truth_data[[location]][[var]]$data_var,
          sim = this_location_modeled_outcome[[var]]$sim_var,
          dist = targets_config[[var]]$likelihood$dist,
          param = targets_config[[var]]$likelihood$param,
          add_one = targets_config[[var]]$add_one
        ))
    }

    ## Compute log-likelihoods
    ## We use a data frame for debugging, only ll is used
    likelihood_data[[location]] <- dplyr::tibble(
      ll = this_location_log_likelihood,
      filename = hosp_file,
      geoid = location,
      accept = 0, # acceptance decision (0/1) . Will be updated later when accept/reject decisions made
      accept_avg = 0, # running average acceptance decision
      accept_prob = 0 # probability of acceptance of proposal
    )
    names(likelihood_data)[names(likelihood_data) == 'geoid'] <- obs_nodename
  }

  #' @importFrom magrittr %>%
  likelihood_data <- likelihood_data %>% do.call(what = rbind)

  ##Update  likelihood data based on hierarchical_stats
  for (stat in names(hierarchical_stats)) {

    if (hierarchical_stats[[stat]]$module %in% c("seir_interventions", "seir")) {
      ll_adjs <- inference::calc_hierarchical_likadj(
        stat = hierarchical_stats[[stat]]$name,
        infer_frame = snpi,
        geodata = geodata,
        geo_group_column = hierarchical_stats[[stat]]$geo_group_col,
        transform = hierarchical_stats[[stat]]$transform
      )

    } else if (hierarchical_stats[[stat]]$module == "outcomes_interventions") {
      ll_adjs <- inference::calc_hierarchical_likadj(
        stat = hierarchical_stats[[stat]]$name,
        infer_frame = hnpi,
        geodata = geodata,
        geo_group_column = hierarchical_stats[[stat]]$geo_group_col,
        transform = hierarchical_stats[[stat]]$transform
      )

    } else  if (hierarchical_stats[[stat]]$module %in% c("hospitalization", "outcomes_parameters")) {

      ll_adjs <- inference::calc_hierarchical_likadj(
        stat = hierarchical_stats[[stat]]$name,
        infer_frame = hpar,
        geodata = geodata,
        geo_group_column = hierarchical_stats[[stat]]$geo_group_col,
        transform = hierarchical_stats[[stat]]$transform,
        stat_col = "value",
        stat_name_col = "parameter"
      )

    } else  if (hierarchical_stats[[stat]]$module == "seir_parameters") {
      stop("We currently do not support hierarchies on seir parameters, since we don't do inference on them except via npis.")
    } else {
      stop("unsupported hierarchical stat module")
    }



    ##probably a more efficient what to do this, but unclear...
    likelihood_data <- dplyr::left_join(likelihood_data, ll_adjs, by = obs_nodename) %>%
      tidyr::replace_na(list(likadj = 0)) %>% ##avoid unmatched location problems
      dplyr::mutate(ll = ll + likadj) %>%
      dplyr::select(-likadj)
  }


  ##Update likelihoods based on priors
  for (prior in names(defined_priors)) {
    if (defined_priors[[prior]]$module %in% c("seir_interventions", "seir")) {
      #' @importFrom magrittr %>%
      ll_adjs <- snpi %>%
        dplyr::filter(npi_name == defined_priors[[prior]]$name) %>%
        dplyr::mutate(likadj = calc_prior_likadj(reduction,
          defined_priors[[prior]]$likelihood$dist,
          defined_priors[[prior]]$likelihood$param
        )) %>%
        dplyr::select(geoid, likadj)

    } else if (defined_priors[[prior]]$module == "outcomes_interventions") {
      #' @importFrom magrittr %>%
      ll_adjs <- hnpi %>%
        dplyr::filter(npi_name == defined_priors[[prior]]$name) %>%
        dplyr::mutate(likadj = calc_prior_likadj(reduction,
          defined_priors[[prior]]$likelihood$dist,
          defined_priors[[prior]]$likelihood$param
        )) %>%
        dplyr::select(geoid, likadj)

    }  else if (defined_priors[[prior]]$module %in% c("outcomes_parameters", "hospitalization")) {

      ll_adjs <- hpar %>%
        dplyr::filter(parameter == defined_priors[[prior]]$name) %>%
        dplyr::mutate(likadj = calc_prior_likadj(value,
          defined_priors[[prior]]$likelihood$dist,
          defined_priors[[prior]]$likelihood$param
        )) %>%
        dplyr::select(geoid, likadj)

    } else  if (hierarchical_stats[[stat]]$module == "seir_parameters") {
      stop("We currently do not support priors on seir parameters, since we don't do inference on them except via npis.")
    } else {
      stop("unsupported prior module")
    }

    ##probably a more efficient what to do this, but unclear...
    likelihood_data<- dplyr::left_join(likelihood_data, ll_adjs, by = obs_nodename) %>%
      dplyr::mutate(ll = ll + likadj) %>%
      dplyr::select(-likadj)
  }

  if(any(is.na(likelihood_data$ll))) {
    print("Full Likelihood")
    print(likelihood_data)
    print("NA only Likelihoods")
    print(likelihood_data[is.na(likelihood_data$ll), ])
    stop("The likelihood was NA")
  }

  return(likelihood_data)
}



##'
##' Function that performs the necessary file copies the end of an MCMC iteration of
##' filter_MC.
##'
##'@param current_index the current index in the run
##'@param slot what is the current slot number
##'@param block what is the current block
##'@param run_id what is the id of this run
##'@param global_local_prefix the prefix to be put on both global and local runs.
##'@param gf_prefix the prefix for the directory containing the current globally accepted files.
##'@param global_block_prefix prefix that describes this block.
##'
##'@return TRUE if this succeeded.
##'
##'@export
##'
perform_MCMC_step_copies_global <- function(current_index,
  slot,
  block,
  run_id,
  global_local_prefix,
  gf_prefix,
  global_block_prefix) {

  rc <- list()

  if(current_index != 0){ #move files from global/intermediate/slot.block.run to global/final/slot
    rc$seed_gf <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seed','csv'),
      covidcommon::create_file_name(run_id,gf_prefix,slot,'seed','csv'),
	    overwrite = TRUE
    )

    rc$cont_gf <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'cont','parquet'),
      covidcommon::create_file_name(run_id,gf_prefix,slot,'cont','parquet'),
	    overwrite = TRUE
    )

    rc$seir_gf <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seir','parquet'),
      covidcommon::create_file_name(run_id,gf_prefix,slot,'seir','parquet'),
	    overwrite = TRUE
    )

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

    rc$hnpi_gf <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hnpi','parquet'),
      covidcommon::create_file_name(run_id,gf_prefix,slot,'hnpi','parquet'),
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
    #move files from global/intermediate/slot.block.run to global/intermediate/slot
    rc$seed_block <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seed','csv'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'seed','csv')
    )

    rc$cont_block <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'cont','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'cont','parquet')
    )

    rc$seir_block <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seir','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'seir','parquet')
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

    rc$hnpi_block <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hnpi','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'hnpi','parquet')
    )

    rc$spar_block <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'spar','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'spar','parquet')
    )


    rc$hpar_block <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hpar','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'hpar','parquet')
    )
  } else { #move files from global/intermediate/slot.(block-1) to global/intermediate/slot.block
    rc$seed_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,global_block_prefix,block - 1 ,'seed','csv'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'seed','csv')
    )

    rc$cont_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,global_block_prefix,block - 1 ,'cont','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'cont','parquet')
    )

    rc$seir_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,global_block_prefix,block - 1 ,'seir','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'seir','parquet')
    )

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

    rc$hnpi_prvblk <-file.copy(
      covidcommon::create_file_name(run_id,global_block_prefix,block - 1,'hnpi','parquet'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'hnpi','parquet')
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

##'
##' Function that performs the necessary file copies the end of an MCMC iteration of
##' filter_MC.
##'
##'@param current_index the current index in the run
##'@param slot what is the current slot number
##'@param block what is the current block
##'@param run_id what is the id of this run
##'@param chimeric_local_prefix the prefix to be put on both chimeric and local runs.
##'@param cf_prefix the prefix for the directory containing the current chimericly accepted files.
##'@param chimeric_block_prefix prefix that describes this block.
##'
##'@return TRUE if this succeeded.
##'
##'@export
##'
perform_MCMC_step_copies_chimeric <- function(current_index,
                                            slot,
                                            block,
                                            run_id,
                                            chimeric_local_prefix,
                                            cf_prefix,
                                            chimeric_block_prefix) {


  rc <- list()

  if(current_index != 0){ #move files from chimeric/intermediate/slot.block.run to chimeric/final/slot
    rc$seed_gf <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'seed','csv'),
      covidcommon::create_file_name(run_id,cf_prefix,slot,'seed','csv'),
      overwrite = TRUE
    )
    
    # No chimeric SEIR or HOSP files
    
    # rc$seir_gf <- file.copy(
    #   covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'seir','parquet'),
    #   covidcommon::create_file_name(run_id,cf_prefix,slot,'seir','parquet'),
    #   overwrite = TRUE
    # )
    # 
    # rc$hosp_gf <- file.copy(
    #   covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hosp','parquet'),
    #   covidcommon::create_file_name(run_id,cf_prefix,slot,'hosp','parquet'),
    #   overwrite = TRUE
    # )

    rc$llik_gf <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'llik','parquet'),
      covidcommon::create_file_name(run_id,cf_prefix,slot,'llik','parquet'),
      overwrite = TRUE
    )


    rc$snpi_gf <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'snpi','parquet'),
      covidcommon::create_file_name(run_id,cf_prefix,slot,'snpi','parquet'),
      overwrite = TRUE
    )

    rc$hnpi_gf <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hnpi','parquet'),
      covidcommon::create_file_name(run_id,cf_prefix,slot,'hnpi','parquet'),
      overwrite = TRUE
    )

    rc$spar_gf <-file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'spar','parquet'),
      covidcommon::create_file_name(run_id,cf_prefix,slot,'spar','parquet'),
      overwrite = TRUE
    )

    rc$hpar_gf <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hpar','parquet'),
      covidcommon::create_file_name(run_id,cf_prefix,slot,'hpar','parquet'),
      overwrite = TRUE
    )
    #move files from chimeric/intermediate/slot.block.run to chimeric/intermediate/slot
    rc$seed_block <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'seed','csv'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'seed','csv')
    )

    # no chimeric SEIR or HOSP files

    # rc$seir_block <- file.copy(
    #   covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'seir','parquet'),
    #   covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'seir','parquet')
    # )
    #
    # rc$hosp_block <- file.copy(
    #   covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hosp','parquet'),
    #   covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'hosp','parquet')
    # )

    rc$llik_block <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'llik','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'llik','parquet')
    )

    rc$snpi_block <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'snpi','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'snpi','parquet')
    )

    rc$hnpi_block <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hnpi','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'hnpi','parquet')
    )

    rc$spar_block <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'spar','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'spar','parquet')
    )


    rc$hpar_block <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hpar','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'hpar','parquet')
    )
  } else { #move files from chimeric/intermediate/slot.(block-1) to chimeric/intermediate/slot.block
    rc$seed_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1 ,'seed','csv'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'seed','csv')
    )

    rc$seir_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1 ,'seir','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'seir','parquet')
    )

    rc$hosp_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1 ,'hosp','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'hosp','parquet')
    )

    rc$llik_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1,'llik','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'llik','parquet')
    )

    rc$snpi_prvblk <-file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1,'snpi','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'snpi','parquet')
    )

    rc$hnpi_prvblk <-file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1,'hnpi','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'hnpi','parquet')
    )

    rc$spar_prvblk <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1,'spar','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'spar','parquet')
    )

    rc$hpar_prvblk <- file.copy(
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block - 1,'hpar','parquet'),
      covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'hpar','parquet')
    )
  }


  return(rc)

}

## Create a list with a filename of each type/extension.  A convenience function for consistency in file names
#' @export
create_filename_list <- function(
  run_id,
  prefix,
  index,
  types = c("seed", "cont", "seir", "snpi", "hnpi", "spar", "hosp", "hpar", "llik"),
  extensions = c("csv", rep("parquet", times = length(types) - 1))
) {
  if(length(types) != length(extensions)){
    stop("Please specify the same number of types and extensions.  Given",length(types),"and",length(extensions))
  }
  rc <- mapply(
    x=types,
    y=extensions,
    function(x,y){
      covidcommon::create_file_name(run_id = run_id,prefix = prefix,index = index,type = x,extension = y, create_directory = TRUE)
    }
  )
  names(rc) <- paste(names(rc),"filename",sep='_')
  return(rc)
}

##'@name initialize_mcmc_first_block
##'@title initialize_mcmc_first_block
##'@param slot what is the current slot number
##'@param block what is the current block
##'@param run_id what is the id of this run
##'@param global_prefix the prefix to use for global files
##'@param chimeric_prefix the prefix to use for chimeric files
##'@param gempyor_inference_runner An already initialized copy of python inference runner
##' @export
initialize_mcmc_first_block <- function(
  run_id,
  block,
  global_prefix,
  chimeric_prefix,
  gempyor_inference_runner,
  likelihood_calculation_function,
  config_start_date,
  name_change_functions,
  is_resume = FALSE,
) {

  ## Only works on these files:
  types <- c("seed", "cont", "seir", "snpi", "hnpi", "spar", "hosp", "hpar", "llik")
  non_llik_types <- paste(types[types != "llik"], "filename", sep = "_")
  extensions <- c("csv", rep("parquet", times = length(types) - 1))

  global_files <- create_filename_list(run_id, global_prefix, block - 1, types, extensions) # makes file names of the form variable/name/scenario/deathrate/run_id/global/intermediate/slot.(block-1).run_ID.variable.ext
  chimeric_files <- create_filename_list(run_id, chimeric_prefix, block - 1, types, extensions) # makes file names of the form variable/name/scenario/deathrate/run_id/chimeric/intermediate/slot.(block-1).run_ID.variable.ext

  global_check <- sapply(global_files, file.exists)
  chimeric_check <- sapply(chimeric_files, file.exists)
  ## If this isn't the first block, all of the files should definitely exist
  if (block > 1) {

    if (any(!global_check)) {
      stop(paste(
        "Could not find file",
        names(global_files)[!global_check],
        ":",
        global_files[!global_check],
        "needed to resume",
        collapse = "\n"
      ))
    }
    if (any(!chimeric_check)) {
      stop(paste(
        "Could not find file",
        names(chimeric_files)[!chimeric_check],
        ":",
        chimeric_files[!chimeric_check],
        "needed to resume",
        collapse = "\n"
      ))
    }
    return(TRUE)
  }

  if (is_resume) {
    print(global_check)
    important_global_check <- global_check[
      !(names(global_check) %in% c("llik_filename", "hosp_filename", "seir_filename", "seed_filename", "cont_filename"))
    ]
    if (!all(important_global_check)) {
      all_file_types <- names(important_global_check)
      missing_file_types <- names(important_global_check)[!important_global_check]
      missing_files <- global_files[missing_file_types]
      stop(paste(
        "For a resume, all global files must be present.",
        "Could not find the following file types:",
        paste(missing_file_types, collapse = ", "),
        "\nWas expecting the following files:",
        paste(all_file_types, collapse = ", "),
        "\nLooking for them in these files",
        paste(missing_files, collapse = ", ")
      ))
    }
  }

  if (any(global_check)) {
    warning(paste(
      "Found file",
      names(global_files)[global_check],
      "when creating first block, using that",
      collapse = "\n"
    ))
  }

  if (any(chimeric_check)) {
    warning(paste(
      "Found file",
      names(global_files)[chimeric_check],
      "when creating first block, ignoring that file and replacing with global",
      collapse = "\n"
    ))
  }

  global_file_names <- names(global_files[!global_check]) # names are of the form "variable_filename", only files that DONT already exist will be in this list

  ## seed
  if ("seed_filename" %in% global_file_names) {
    if(!file.exists(config$seeding$lambda_file)) {
      err <- system(paste(
        opt$rpath,
        paste(opt$pipepath, "R", "scripts", "create_seeding.R", sep = "/"),
        "-c", opt$config
      ))
      if (err != 0) {
        stop("Could not run seeding")
      }
    }
    err <- !(file.copy(config$seeding$lambda_file, global_files[["seed_filename"]]))
    if (err != 0) {
      stop("Could not copy seeding")
    }
  }

  ## cont
  if ("cont_filename" %in% global_file_names) {
    if ("seir_filename" %in% global_file_names) {
      create_cont_file_from_seir(global_file_names[["cont_filename"]], global_file_names[["seir_filename"]], config_start_date, name_change_functions)
    } else if (is.null(config$initial_conditions$initial_condition_file)) {
      create_cont_file_from_default(global_file_names[["cont_filename"]], ...)
    } else {
      if (!file.exists(config$initial_conditions$initial_condition_file)) {
        stop(paste("Could not locate initial conditions file",config$initial_conditions$initial_condition_file))
      }
      err <- !(file.copy(config$initial_conditions$initial_condition_file, global_files[["cont_filename"]]))
      if (err != 0) {
        stop("Could not copy initial conditions")
      }
    }
  }

  ## seir, snpi, spar
  checked_par_files <- c("snpi_filename", "spar_filename", "hnpi_filename", "hpar_filename")
  checked_sim_files <- c("seir_filename", "hosp_filename")
  # These functions save variables to files of the form variable/name/scenario/deathrate/run_id/global/intermediate/slot.(block-1),runID.variable.ext
  if (any(checked_par_files %in% global_file_names)) {
    if (!all(checked_par_files %in% global_file_names)) {
      stop("Provided some InferenceSimulator input, but not all")
    }
    if (any(checked_sim_files %in% global_file_names)) {
      if (!all(checked_sim_files %in% global_file_names)) {
        stop("Provided only one of hosp or seir input file, with some output files. Not supported anymore")
      }
      gempyor_inference_runner$one_simulation(sim_id2write = block - 1)
    } else {
      stop("Provided some InferenceSimulator output(seir, hosp), but not InferenceSimulator input")
    }
  } else {
    if (any(checked_sim_files %in% global_file_names)) {
      if (!all(checked_sim_files %in% global_file_names)) {
        stop("Provided only one of hosp or seir input file, not supported anymore")
      }
        warning("SEIR and Hosp input provided, but output not found. This is unstable for stochastic runs")
        gempyor_inference_runner$one_simulation(sim_id2write=block - 1, load_ID=TRUE, sim_id2load=block - 1)
    }
  }

  ## llik
  if (!("llik_filename" %in% global_file_names)) {
    stop("Please do not provide a likelihood file")
  }

  extension <- gsub(".*[.]", "", global_files[["hosp_filename"]])
  hosp_data <- report.generation:::read_file_of_type(extension)(global_files[["hosp_filename"]])

  ## Refactor me later:
  global_likelihood_data <- likelihood_calculation_function(hosp_data)
  arrow::write_parquet(global_likelihood_data, global_files[["llik_filename"]]) # save global likelihood data to file of the form llik/name/scenario/deathrate/run_id/global/intermediate/slot.(block-1).run_ID.llik.ext

  #print("from inside initialize_mcmc_first_block: column names of likelihood dataframe")
  #print(colnames(global_likelihood_data))

  for (type in names(chimeric_files)) {
    file.copy(global_files[[type]], chimeric_files[[type]], overwrite = TRUE) # copy files that were in global directory into chimeric directory
  }
  return()
}
