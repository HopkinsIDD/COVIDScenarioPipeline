options(warn=1)

#' Function to perform filtering
#' @param config_path path to the config file
#' @param scenarios name of the intervention to run, or 'all' to run all of them
#' @param deathrates name of the death scenarios to run, or 'all' to run all of them
#' @param jobs number of jobs to run in parallel
#' @param simulations_per_slot number of simulations to run for this slot
#' @param number_of_simulations number of slots to run
#' @param this_slot id of this slot
#' @param python path to python executable
#' @param rpath path to R executable
#' @param pipepath path to the COVIDScenarioPipeline directory
#' @param clean Remove old files if unused
#' @param dontclean Don't remove old files if unused
#' @return NULL
#' @export
filter_MC <- function (
  config_path,
  scenarios,
  deathrates,
  jobs,
  simulations_per_slot,
  number_of_simulations,
  this_slot,
  python,
  rpath,
  pipepath,
  clean
) {

  dontclean <- !clean

  # Checks  ------------------------------------------------------

  # Check python
  reticulate::use_python(Sys.which(python),require=TRUE)

  # Block loads the config file and geodata
  if(config_path == ""){
    stop(paste(
      "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
    ))
  }
  config <- covidcommon::load_config(config_path)

  # Check seeding pertubation parameter
  if(!('perturbation_sd' %in% names(config$seeding))) {
    stop("The key seeding::perturbation_sd is required in the config file.")
  }

  # Check folder draw parameter
  if(config$seeding$method != 'FolderDraw'){
    stop("This filtration method requires the seeding method 'FolderDraw'")
  }

  # Check lambda file
  if(!('lambda_file' %in% names(config$seeding))) {
    stop("Despite being a folder draw method, filtration method requires the seeding to provide a lambda_file argument.")
  }

  # Create directories for seeding
  suppressWarnings(dir.create(config$seeding$folder_path,recursive=TRUE))
  suppressWarnings(dir.create(sprintf("%s/%s/case_data",'importation',config$name),recursive=TRUE))


  # Arguments --------------------------------------------------------------------

  # Parse deathrate arguments
  if(all(deathrates == "all")) {
    deathrates<- config$hospitalization$parameters$p_death_names
  } else if (!(deathrates %in% config$hospitalization$parameters$p_death_names)) {
    message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
    quit("yes", status=1)
  }

  # Parse scenario arguments
  if (all(scenarios == "all")){
    scenarios <- config$interventions$scenarios
  } else if (!all(scenarios %in% config$interventions$scenarios)) {
    message(paste("Invalid scenario argument:",scenario, "did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
    quit("yes", status=1)
  }


  # Load data --------------------------------------------------------------------

  # Load geodata
  suppressMessages(geodata <- report.generation::load_geodata_file(
    paste(
      config$spatial_setup$base_path,
      config$spatial_setup$geodata, sep = "/"
    ),
    geoid_len=5
  ))

  # Column name that stores spatial unique id
  obs_nodename <- config$spatial_setup$nodenames

  # Set number of simulations
  if(is.na(simulations_per_slot)){
    simulations_per_slot <- config$filtering$simulations_per_slot
  }

  # Data path
  data_path <- config$filtering$data_path
  data_dir <- dirname(data_path)
  if(!dir.exists(data_dir)){
    suppressWarnings(dir.create(data_dir,recursive=TRUE))
  }

  # Load data
  suppressWarnings(dir.create('.lock'))
  lockfile = 'filter_MC.lock'
  # lock <- flock::lock(paste(".lock",gsub('/','-',data_path), sep = '/'))
  if (!file.exists(data_path)) {
    load_data(data_path, geodata, obs_nodename)
  }
  # flock::unlock(lock)

  # Load epi data
  if(!("obs" %in% ls())){
    suppressMessages(obs <<- readr::read_csv(data_path))
    obs <- obs %>%
      filter(date >= config$start_date,
             date <= config$end_date)

    obs <- obs %>% dplyr::right_join(
      tidyr::expand_grid(
        geoid = unique(obs$geoid),
        date = unique(obs$date)
      )
    ) %>%
      dplyr::mutate_if(is.numeric,coalesce,0)
  }

  # Get unique geonames
  geonames <- unique(obs[[obs_nodename]])

  # Compute statistics of observations
  data_stats <- lapply(
    geonames,
    function(x) {
      df <- obs[obs[[obs_nodename]] == x, ]
      getStats(
        df,
        "date",
        "data_var",
        stat_list = config$filtering$statistics)
    }) %>%
    set_names(geonames)

  # Filtering loops ------------------------------------------------------

  required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")  # packages required for dopar

  # Create likelihood output dir
  if(!dir.exists(config$filtering$likelihood_directory)){
    dir.create(config$filtering$likelihood_directory)
  }

  for(scenario in scenarios) {

    ## One time setup for python
    reticulate::py_run_string(paste0("config_path = '", config_path,"'"))
    reticulate::py_run_string(paste0("scenario = '", scenario, "'"))
    reticulate::import_from_path("SEIR", path=pipepath)
    reticulate::py_run_file(paste(pipepath,"minimal_interface.py",sep='/'))

    for(deathrate in deathrates) {
      # lock <- flock::lock(paste('.lock',gsub('/','-',config$seeding$lambda_file),sep='/'))

      # Run seeding if not found
      err <- 0
      if(!file.exists(covidcommon::seeding_file_path(config,sprintf("%09d",this_slot)))){
        if(!file.exists(config$seeding$lambda_file)){
          err <- system(paste(
            rpath,
            paste(pipepath,"R","scripts","create_seeding.R", sep='/'),
            "-c",config
          ))
          if(err != 0){
            stop("Could not run seeding")
          }
        }
        suppressMessages(initial_seeding <- readr::read_csv(config$seeding$lambda_file))
        write.csv(
          initial_seeding,
          file = covidcommon::seeding_file_path(config,sprintf("%09d",this_slot))
        )
      }

      suppressMessages(initial_seeding <- readr::read_csv(covidcommon::seeding_file_path(config,sprintf("%09d",this_slot))))
      # flock::unlock(lock)

      initial_seeding$amount <- as.integer(round(initial_seeding$amount))

      # Initialize index and likelihood list
      current_index <- 0
      current_likelihood <- data.frame()

      # First files
      first_param_file <- covidcommon::parameter_file_path(config,this_slot, scenario)
      first_npi_file   <- covidcommon::npi_file_path(config,this_slot, scenario)
      first_hosp_file  <- covidcommon::hospitalization_file_path(config,this_slot, scenario, deathrate)

      # lock <- flock::lock(paste('.lock',gsub('/','-',first_npi_file),sep='/'))
      # Run SEIR model
      if((!file.exists(first_npi_file)) | (!file.exists(first_param_file))){
        py$onerun_SEIR(this_slot,py$s)
      }

      # Initial params and NPIs
      initial_npis <- arrow::read_parquet(first_npi_file)
      initial_params <- arrow::read_parquet(first_param_file)
      # flock::unlock(lock)

      if(!file.exists(first_hosp_file)){
        ## Generate files
        this_index <- this_slot
        # lock <- flock::lock(paste(".lock",paste("SEIR",this_index,scenario,sep='.'),sep='/'))
        err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
        err <- ifelse(err == 1,0,1)
        if(err != 0){
          stop("SEIR failed to run")
        }

        ## Run hospitalization
        err <- system(paste(
          rpath,
          paste(pipepath,"R","scripts","hosp_run.R", sep='/'),
          "-j",jobs,
          "-c",config,
          "-p",pipepath,
          "-s",scenario,
          "-d",deathrate,
          "-n",1,
          "-i",this_index
        ))
        if(err != 0){
          stop("Hospitalization failed to run")
        }
      }

      initial_sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",first_hosp_file))(first_hosp_file) %>%
        filter(time >= min(obs$date), time <= max(obs$date)) %>%
        select(-date_inds)

      lhs <- unique(initial_sim_hosp[[obs_nodename]])
      rhs <- unique(names(data_stats))
      all_locations <- rhs[rhs %in% lhs]

      lhs <- lubridate::ymd(unique(initial_sim_hosp$time))
      rhs <- lubridate::ymd(unlist(Reduce(intersect,lapply(data_stats,function(x){lapply(x,function(y){y$date})}))))
      all_dates <- rhs[rhs %in% lhs]
      if(!all(all_dates == rhs)){
        stop("This should not happen")
      }

      initial_log_likelihood_file <- paste0(config$filtering$likelihood_directory,"/",sprintf("%09d",this_slot),".llik.parquet")
      if(!file.exists(initial_log_likelihood_file)){
        initial_log_likelihood_data <- list()
        for(location in all_locations) {

          local_sim_hosp <- dplyr::filter(initial_sim_hosp, !!rlang::sym(obs_nodename) == location)
          initial_sim_stats <- getStats(
            local_sim_hosp,
            "time",
            "sim_var",
            end_date = max(obs$date[obs[[obs_nodename]] == location]),
            config$filtering$statistics
          )


          # Get observation statistics
          log_likelihood <- list()
          for(var in names(data_stats[[location]])) {
            log_likelihood[[var]] <- logLikStat(
              obs = data_stats[[location]][[var]]$data_var,
              sim = initial_sim_stats[[var]]$sim_var,
              dist = config$filtering$statistics[[var]]$likelihood$dist,
              param = config$filtering$statistics[[var]]$likelihood$param,
              add_one = config$filtering$statistics[[var]]$add_one
            )
          }
          # Compute log-likelihoods

          initial_log_likelihood_data[[location]] <- dplyr::tibble(
            ll = sum(unlist(log_likelihood)),
            filename = first_hosp_file,
            geoid = location
          )
          names(initial_log_likelihood_data)[names(initial_log_likelihood_data) == 'geoid'] <- obs_nodename
        }
        rm(initial_sim_hosp)

        initial_log_likelihood_data <- initial_log_likelihood_data %>% do.call(what=rbind)
        arrow::write_parquet(initial_log_likelihood_data,initial_log_likelihood_file)
      }
      initial_log_likelihood_data <- arrow::read_parquet(initial_log_likelihood_file)

      # Compute total loglik for each sim
      likelihood <- initial_log_likelihood_data %>%
        dplyr::summarise(ll = sum(ll, na.rm = T)) %>%
        dplyr::mutate(pdeath = deathrate, scenario = scenario)

      ## For logging
      current_likelihood <- likelihood
      current_index <- 0
      previous_likelihood_data <- initial_log_likelihood_data

      for (index in seq_len(simulations_per_slot)) {
        print(index)
        current_seeding <- perturb_seeding(initial_seeding,config$seeding$perturbation_sd)
        current_npis <- perturb_npis(initial_npis, config$interventions$settings)
        current_params <- initial_params
        this_index <- simulations_per_slot * (this_slot - 1) + number_of_simulations + index
        write.csv(
          current_seeding,
          file = covidcommon::seeding_file_path(config,sprintf("%09d",this_index))
        )
        arrow::write_parquet(
          current_npis,
          covidcommon::npi_file_path(config,this_index,scenario)
        )
        arrow::write_parquet(
          current_params,
          covidcommon::parameter_file_path(config,this_index,scenario)
        )


        ## Generate files
        # lock <- flock::lock(paste(".lock",paste("SEIR",this_index,scenario,sep='.'),sep='/'))
        err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
        err <- ifelse(err == 1,0,1)
        if(err != 0){
          stop("SEIR failed to run")
        }

        ## Run hospitalization
        err <- system(paste(
          rpath,
          paste(pipepath,"R","scripts","hosp_run.R", sep='/'),
          "-j",jobs,
          "-c",config,
          "-p",pipepath,
          "-s",scenario,
          "-d",deathrate,
          "-n",1,
          "-i",this_index
        ))
        if(err != 0){
          stop("Hospitalization failed to run")
        }

        file <- covidcommon::hospitalization_file_path(config,this_index,scenario,deathrate)
        print(paste("Reading",file))

        sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",file))(file) %>%
          filter(time <= max(obs$date)) %>%
          select(-date_inds)
        # flock::unlock(lock)

        log_likelihood_data <- list()

        lhs <- unique(sim_hosp[[obs_nodename]])
        rhs <- unique(names(data_stats))
        all_locations <- rhs[rhs %in% lhs]

        for(location in all_locations) {
          # log_likelihood_data <- foreach (location = all_locations) %do% {
          # Compute log-likelihood of data for each sim
          # This part can be parallelized
          # One scenarios, one pdeath
          # if(!('sim_hosp' %in% ls())){
          #   sim_hosp <<- report.generation:::read_file_of_type(gsub(".*[.]","",file))(file) %>%
          #     filter(time <= max(obs$date)) %>%
          #     select(-date_inds)
          # }

          # load hospitalization results
          local_sim_hosp <- dplyr::filter(sim_hosp, !!rlang::sym(obs_nodename) == location) %>%
            dplyr::filter(time %in% all_dates)

          # Compute simulation statistics
          sim_stats <- getStats(
            local_sim_hosp,
            "time",
            "sim_var",
            end_date = max(obs$date[obs[[obs_nodename]] == location]),
            config$filtering$statistics
          )

          # Get observation statistics
          log_likelihood <- list()
          for(var in names(data_stats[[location]])) {
            # log_likelihood <- foreach (var = names(data_stats[[location]]), .combine = sum) %do% {

            log_likelihood[[var]] <- logLikStat(
              obs = data_stats[[location]][[var]]$data_var,
              sim = sim_stats[[var]]$sim_var,
              dist = config$filtering$statistics[[var]]$likelihood$dist,
              param = config$filtering$statistics[[var]]$likelihood$param,
              add_one = config$filtering$statistics[[var]]$add_one
            )
            # }
          }

          # Compute log-likelihoods for location
          log_likelihood_data[[location]] <- dplyr::tibble(
            ll = sum(unlist(log_likelihood)),
            filename = file,
            geoid = location
          )
          names(log_likelihood_data)[names(log_likelihood_data) == 'geoid'] <- obs_nodename
          # }
        }
        rm(sim_hosp)

        # Combine all results
        log_likelihood_data <- log_likelihood_data %>% do.call(what=rbind)

        # Compute total log-likelihood accross locations
        likelihood <- log_likelihood_data %>%
          dplyr::summarise(ll = sum(ll, na.rm = T)) %>%
          dplyr::mutate(pdeath = deathrate, scenario = scenario)

        ## For logging
        print(paste("Current likelihood",current_likelihood,"Proposed likelihood",likelihood))

        # Update states
        if(iterateAccept(current_likelihood, likelihood, 'll')){
          old_index <- simulations_per_slot * (this_slot - 1) + number_of_simulations + current_index
          current_index <- index
          current_likelihood <- likelihood
          if(clean){
            print("Removing old")
            file.remove(covidcommon::hospitalization_file_path(config,old_index,scenario,deathrate))
            file.remove(covidcommon::simulation_file_path(config,old_index,scenario))
            file.remove(covidcommon::npi_file_path(config,old_index,scenario))
            file.remove(covidcommon::parameter_file_path(config,old_index,scenario))
          }
        } else {
          old_index <- simulations_per_slot * (this_slot - 1) + number_of_simulations + index
          if(clean){
            print("Removing new")
            file.remove(covidcommon::hospitalization_file_path(config,old_index,scenario,deathrate))
            file.remove(covidcommon::simulation_file_path(config,old_index,scenario))
            file.remove(covidcommon::npi_file_path(config,old_index,scenario))
            file.remove(covidcommon::parameter_file_path(config,old_index,scenario))
          }
        }

        # Upate seeding and NPIs by location
        seeding_npis_list <- accept_reject_new_seeding_npis(
          seeding_orig = initial_seeding,
          seeding_prop = current_seeding,
          npis_orig = initial_npis,
          npis_prop = current_npis,
          orig_lls = previous_likelihood_data,
          prop_lls = log_likelihood_data
        )
        initial_seeding <- seeding_npis_list$seeding
        initial_npis <- seeding_npis_list$npis
        previous_likelihood_data <- seeding_npis_list$ll
        print(paste("Current index is ",current_index))
        print(log_likelihood_data)
        print(previous_likelihood_data)
        rm(current_npis)
        rm(current_seeding)
      }


      # Update files -----------------------------------------------------------------

      # Hospitalizations
      current_file <- covidcommon::hospitalization_file_path(
        config,
        ## GLOBAL ACCEPT/REJECT vs LOCAL ACCEPT/REJECT
        # simulations_per_slot * (this_slot - 1) + number_of_simulations + current_index,
        simulations_per_slot * (this_slot - 1) + number_of_simulations + simulations_per_slot,
        scenario,
        deathrate
      )
      target_file <- covidcommon::hospitalization_file_path(config,this_slot,scenario,deathrate)
      target_dir <- gsub('/[^/]*$','',target_file)

      print(paste("Copying",current_file,"to",target_file))
      suppressWarnings(dir.create(target_dir, recursive=TRUE))
      file.rename(from=current_file,to=target_file)

      # NPIs
      current_file <- covidcommon::npi_file_path(
        config,
        simulations_per_slot * (this_slot - 1) + number_of_simulations + simulations_per_slot,
        scenario
      )
      target_file <- covidcommon::npi_file_path(config,this_slot,scenario)
      target_dir <- gsub('/[^/]*$','',target_file)

      print(paste("Copying",current_file,"to",target_file))
      suppressWarnings(dir.create(target_dir, recursive=TRUE))
      file.rename(from=current_file,to=target_file)

      # Seeding
      current_file <- covidcommon::seeding_file_path(
        config,
        sprintf("%09d",simulations_per_slot * (this_slot - 1) + number_of_simulations + simulations_per_slot)
      )
      target_file <- covidcommon::seeding_file_path(config,sprintf("%09d",this_slot))
      target_dir <- gsub('/[^/]*$','',target_file)

      print(paste("Copying",current_file,"to",target_file))
      suppressWarnings(dir.create(target_dir, recursive=TRUE))
      file.rename(from=current_file,to=target_file)


      target_file <- initial_log_likelihood_file
      arrow::write_parquet(previous_likelihood_data,initial_log_likelihood_file)

    }
  }
  #}
}
