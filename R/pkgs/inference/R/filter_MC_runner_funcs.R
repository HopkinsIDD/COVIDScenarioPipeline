###Functions that help with the running of Filter MC. Can take in full confit.


##'Function that performs aggregation and calculates likelihood data across all given locations.
##'
##' @param all_locations all of the locoations to calculate likelihood for
##' @param modeled_outcome  the hospital data for the simulations
##' @param obs_nodename the name of the column containg locations.
##' @param config the full configuraiton setup
##' @param obs the full observed data
##' @param ground_truth_data the data we are going to compare to aggregated to the right statistic
##' @param hosp_file the filename of the hosp file being used (unclear if needed in scope)
##' @param hierarchical_stats the hierarchical stats to use
##' @param defined_priors information on defined priors.
##' @param geodata the geographics data to help with hierarchies
##' @param snpi the file with the npi information for seir
##' @param hnpi the file with the npi information for outcomes
##' @param hpar data frame of hospitalization parameters
##'
##' @return a data frame of likelihood data.
##'
##' @export
##'
aggregate_and_calc_loc_likelihoods <- function(
  all_locations,
  modeled_outcome,
  obs_nodename,
  config,
  obs,
  ground_truth_data,
  hosp_file,
  hierarchical_stats,
  defined_priors,
  geodata,
  snpi=NULL,
  hnpi=NULL,
  hpar=NULL
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
        stat_list = config$filtering$statistics
      )


    ## Get observation statistics
    this_location_log_likelihood <- 0
    for (var in names(ground_truth_data[[location]])) {

      observed_indices <- !is.na(ground_truth_data[[location]][[var]]$data_var)

      this_location_log_likelihood <- this_location_log_likelihood +
        ## Actually compute likelihood for this location and statistic here:
        sum(inference::logLikStat(
          obs = ground_truth_data[[location]][[var]]$data_var[observed_indices],
          sim = this_location_modeled_outcome[[var]]$sim_var[observed_indices],
          dist = config$filtering$statistics[[var]]$likelihood$dist,
          param = config$filtering$statistics[[var]]$likelihood$param,
          add_one = config$filtering$statistics[[var]]$add_one
        ))
    }

    ## Compute log-likelihoods
    ## We use a data frame for debugging, only ll is used
    likelihood_data[[location]] <- dplyr::tibble(
      ll = this_location_log_likelihood,
      filename = hosp_file,
      geoid = location
    )
    names(likelihood_data)[names(likelihood_data) == 'geoid'] <- obs_nodename
  }

  #' @importFrom magrittr %>%
  likelihood_data <- likelihood_data %>% do.call(what = rbind)

  ##Update  liklihood data based on hierarchical_stats
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
    likelihood_data <- dplyr::left_join(likelihood_data, ll_adjs) %>%
      tidyr::replace_na(list(likadj = 0)) %>% ##avoid unmatched location problems
      dplyr::mutate(ll = ll + likadj) %>%
      dplyr::select(-likadj)
  }


  ##Update lieklihoods based on priors
  for (prior in names(defined_priors)) {
    if (defined_priors[[prior]]$module %in% c("seir_interventions", "seir")) {
      #' @importFrom magrittr %>%
      ll_adjs <- snpi %>%
        dplyr::filter(npi_name == defined_priors[[prior]]$name) %>%
        dplyr::mutate(likadj = calc_prior_likadj(reduction,
          defined_priors[[prior]]$likelihood$dist,
          defined_priors[[prior]]$likelihood$param
        )) %>%
        select(geoid, likadj)

    } else if (defined_priors[[prior]]$module == "outcomes_interventions") {
      #' @importFrom magrittr %>%
      ll_adjs <- hnpi %>%
        dplyr::filter(npi_name == defined_priors[[prior]]$name) %>%
        dplyr::mutate(likadj = calc_prior_likadj(reduction,
          defined_priors[[prior]]$likelihood$dist,
          defined_priors[[prior]]$likelihood$param
        )) %>%
        select(geoid, likadj)

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
    rc$seed_gf <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seed','csv'),
      covidcommon::create_file_name(run_id,gf_prefix,slot,'seed','csv'),
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

    rc$seed_block <- file.copy(
      covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seed','csv'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'seed','csv')
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
  } else {
    rc$seed_prevblk <- file.copy(
      covidcommon::create_file_name(run_id,global_block_prefix,block - 1 ,'seed','csv'),
      covidcommon::create_file_name(run_id,global_block_prefix,block,'seed','csv')
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

## Create a list with a filename of each type/extension.  A convenience function for consistency in file names
#' @export
create_filename_list <- function(
  run_id,
  prefix,
  index,
  types = c("seed", "seir", "snpi", "hnpi", "spar", "hosp", "hpar", "llik"),
  extensions = c("csv", "parquet", "parquet", "parquet", "parquet", "parquet", "parquet", "parquet")
) {
  if(length(types) != length(extensions)){
    stop("Please specify the same number of types and extensions.  Given",length(types),"and",length(extensions))
  }
  rc <- mapply(
    x=types,
    y=extensions,
    function(x,y){
      covidcommon::create_file_name(run_id,prefix,index,x,y)
    }
  )
  names(rc) <- paste(names(rc),"filename",sep='_')
  return(rc)
}

##'@name initialize_mcmc_first_block
##'@title initialize_mcmc_first_block
##'@param slot what is the current slot numbe
##'@param block what is the current block
##'@param run_id what is the id of this run
##'@param global_prefix the prefix to use for global files
##'@param chimeric_prefix the prefix to use for chimeric files
##'@param python_reticulate An already initialized copy of python set up to do hospitalization runs
##' @export
initialize_mcmc_first_block <- function(
  run_id,
  block,
  global_prefix,
  chimeric_prefix,
  python_reticulate,
  likelihood_calculation_function,
  is_resume = FALSE
) {

  ## Only works on these files:
  types <- c("seed", "seir", "snpi", "hnpi", "spar", "hosp", "hpar", "llik")
  non_llik_types <- paste(c("seed", "seir", "snpi", "hnpi", "spar", "hosp", "hpar"), "filename", sep = "_")
  extensions <- c("csv", "parquet", "parquet", "parquet", "parquet", "parquet", "parquet", "parquet")

  global_files <- create_filename_list(run_id, global_prefix, block - 1, types, extensions)
  chimeric_files <- create_filename_list(run_id, chimeric_prefix, block - 1, types, extensions)

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
      !(names(global_check) %in% c("llik_filename", "hosp_filename", "seir_filename"))
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

  global_file_names <- names(global_files[!global_check])

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

  ## seir, snpi, spar
  if (any(c("snpi_filename", "spar_filename") %in% global_file_names)) {
    if (!all(c("snpi_filename", "spar_filename") %in% global_file_names)) {
      stop("Provided some SEIR input, but not all")
    }
    if ("seir_filename" %in% global_file_names) {
      python_reticulate$onerun_SEIR(block - 1, python_reticulate$s)
    } else {
      stop("Provided SEIR output, but not SEIR input")
    }
  } else {
    if ("seir_filename" %in% global_file_names) {
      warning("SEIR input provided, but output not found. This is unstable for stochastic runs")
      python_reticulate$onerun_SEIR_loadID(block - 1, python_reticulate$s, block - 1)
    }
  }

  ## hpar
  if (any(c("hnpi_filename", "hpar_filename") %in% global_file_names)) {
    if (!all(c("hnpi_filename", "hpar_filename") %in% global_file_names)) {
      stop("Provided some Outcomes input, but not all")
    }
    if ("hosp_filename" %in% global_file_names) {
      python_reticulate$onerun_OUTCOMES(block - 1)
    } else {
      stop("Provided Outcomes output, but not Outcomes input")
    }
  } else {
    if ("hosp_filename" %in% global_file_names) {
      warning("Outcomes input provided, but output not found. This is unstable for stochastic runs")
      python_reticulate$onerun_OUTCOMES_loadID(block - 1)
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
  arrow::write_parquet(global_likelihood_data, global_files[["llik_filename"]])

  for (type in names(chimeric_files)) {
    file.copy(global_files[[type]], chimeric_files[[type]], overwrite = TRUE)
  }
  return()
}
