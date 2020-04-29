
# install.packages('xts', repos='http://cran.us.r-project.org')
# install.packages('zoo', repos='http://cran.us.r-project.org')
# devtools::install_github("HopkinsIDD/covidImportation")

# Preamble ---------------------------------------------------------------------
library(dplyr)
library(readr)
library(covidcommon)
library(report.generation)
library(covidImportation)
library(stringr)
library(foreach)
library(magrittr)
library(xts)
library(reticulate)


option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-s", "--scenarios"), action="store", default='all', type='character', help="name of the intervention to run, or 'all' to run all of them"),
  optparse::make_option(c("-d", "--deathrate"), action="store", default='all', type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default="8", type='integer', help="Number of jobs to run in parallel"),
  optparse::make_option(c("-k", "--simulations_per_slot"), action="store", default=NA, type='integer', help = "number of simulations to run for this slot"),
  optparse::make_option(c("-n", "--number_of_simulations"), action="store", default="1", type='integer', help = "number of slots to run"),
  optparse::make_option(c("-i", "--this_slot"), action="store", default="1", type='integer', help = "id of this slot"),
  optparse::make_option(c("-y", "--python"), action="store", default="python3", type='character', help="path to python executable"),
  optparse::make_option(c("-r", "--rpath"), action="store", default="Rscript", type = 'character', help = "path to R executable"),
  optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = "COVIDScenarioPipeline/")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

reticulate::use_python(Sys.which(opt$python),require=TRUE)
## Block loads the config file and geodata
if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}
config = covidcommon::load_config(opt$config)

if(!('perturbation_sd' %in% names(config$seeding))) {
  stop("The key seeding::perturbation_sd is required in the config file.")
}
if(config$seeding$method != 'FolderDraw'){
  stop("This filtration method requires the seeding method 'FolderDraw'")
}
if(!('lambda_file' %in% names(config$seeding))) {
  stop("Despite being a folder draw method, filtration method requires the seeding to provide a lambda_file argument.")
}


geodata <- report.generation::load_geodata_file(
  paste(
    config$spatial_setup$base_path,
    config$spatial_setup$geodata, sep = "/"
  ),
  geoid_len=5
)
obs_nodename <- config$spatial_setup$nodenames

if(is.na(opt$simulations_per_slot)){
  opt$simulations_per_slot <- config$filtering$simulations_per_slot
}

data_path <- config$filtering$data_path
data_dir <- gsub('[/][^/]*','',data_path)
if(!dir.exists(data_dir)){
  dir.create(data_dir,recursive=TRUE)
}
# Parse jhucsse using covidImportation
if (!file.exists(data_path)) {
  case_data_dir <- paste(config$spatial_setup$base_path,config$spatial_setup$setup_name,"case_data", sep = '/')
  if(!dir.exists(case_data_dir)){
    dir.create(case_data_dir,recursive=TRUE)
  }
  jhucsse_cases <- covidImportation::get_clean_JHUCSSE_data(aggr_level = "UID",
                                   last_date = as.POSIXct(lubridate::ymd(config$end_date)),
                                   case_data_dir = case_data_dir,
                                   save_raw_data=TRUE,
                                   us_data_only=FALSE) %>%
                     select(FIPS,Update,Confirmed)

  jhucsse_deaths <- covidImportation::get_clean_JHUCSSE_deaths(aggr_level = "UID", #"source",
                               last_date = Sys.time(),
                               case_data_dir = case_data_dir,
                               save_raw_data=TRUE,
                               us_data_only=FALSE) %>%
                     select(FIPS,Update,Deaths)

  jhucsse <- full_join(jhucsse_cases,jhucsse_deaths)
  jhucsse  <-
    jhucsse %>%
    dplyr::mutate(date = lubridate::ymd(Update)) %>%
    dplyr::filter(FIPS %in% geodata[[obs_nodename]]) %>%
    dplyr::rename(
      cumConfirmed = Confirmed,
      cumDeaths = Deaths
    ) %>%
    dplyr::arrange(date)
  if(any(is.na(jhucsse$cumConfirmed))){
    jhucsse$cumConfirmed[is.na(jhucsse$cumConfirmed)] <- 0
  }
  if(any(is.na(jhucsse$cumDeaths))){
    jhucsse$cumDeaths[is.na(jhucsse$cumDeaths)] <- 0
  }
  jhucsse <- jhucsse %>%
    dplyr::group_by(FIPS) %>%
    dplyr::group_modify(
      function(.x,.y){
        .x$cumConfirmed = cummax(.x$cumConfirmed)
        .x$conf_incid = c(.x$cumConfirmed[1],diff(.x$cumConfirmed))
        .x$cumDeaths = cummax(.x$cumDeaths)
        .x$death_incid = c(.x$cumDeaths[1],diff(.x$cumDeaths,))
        return(.x)
      }
    )
  names(jhucsse)[names(jhucsse) == 'FIPS'] <- as.character(obs_nodename)
  write_csv(jhucsse, data_path)
  rm(jhucsse)
}

# Parse scenarios arguments
deathrates <- opt$d
if(all(deathrates == "all")) {
  deathrates<- config$hospitalization$parameters$p_death_names
} else if (!(deathrates %in% config$hospitalization$parameters$p_death_names)) {
  message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
  quit("yes", status=1)
}

scenarios <- opt$s
if (all(scenarios == "all")){
  scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
  message(paste("Invalid scenario argument:",scenario, "did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
  quit("yes", status=1)
}

# Function to apply time aggreation

periodAggregate <- function(data, dates, end_date = NULL, period_unit, period_k, aggregator, na.rm = F) {
  if(na.rm) {
    dates <- dates[!is.na(data)]
    data <- data[!is.na(data)]
  }
  if (length(data) == 0) {
    return(data.frame(date = NA, stat = NA))
  }
  if (!is.null(end_date)) {
    data <- data[dates <= end_date]
    dates <- dates[dates <= end_date]
  }

  xtsobj <- as.xts(zoo(data, dates))
  stats <- period.apply(xtsobj,
                        endpoints(xtsobj, on = period_unit, k = period_k),
                        aggregator)
  return(stats)
}

getStats <- function(df, time_col, var_col, end_date = NULL, stat_list) {
  rc <- list()
  for(stat in names(stat_list)){
      s <- stat_list[[stat]]
      aggregator <- match.fun(s$aggregator)
      # Get the time period over whith to apply aggregation
      period_info <- strsplit(s$period, " ")[[1]]

      res <- periodAggregate(df[[s[[var_col]]]],
                             df[[time_col]],
                             end_date,
                             period_info[2],
                             period_info[1],
                             aggregator,
                             na.rm = s$remove_na)
      rc[[stat]] <- res %>%
        as.data.frame() %>%
        mutate(date = rownames(.)) %>%
        set_colnames(c(var_col, "date")) %>%
        select(date, one_of(var_col))
  }
  return(rc)
}


##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##'
##' @return NULL
##'
parameter_file_path <- function(config,index, scenario){
  if(length(config$interventions$scenarios) > 1){
    stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  }

  ## FIX ME 
  return(sprintf("model_parameters/%s_%s/%09d.spar.parquet", config$spatial_setup$setup_name, scenario, index))
}

npi_file_path <- function(config,index,scenario){
  if(length(config$interventions$scenarios) > 1){
    stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  }

  ## FIX ME 
  return(sprintf("model_parameters/%s_%s/%09d.snpi.parquet", config$spatial_setup$setup_name, scenario, index))
}



##' Function for determining where to write the seeding.csv file
##' @param config The config for this run
##' @param index The index of this simulation
##'
##' @return NULL
##'
seeding_file_path <- function(config,index){
  if(length(config$interventions$scenarios) > 1){
    stop("Changes need to be made to the SEIR code to support more than one scenario (in paralllel)")
  }

  return(sprintf("%s/importation_%s.csv",config$seeding$folder_path,index))
}



##' Fuction perturbs a seeding file based on a normal
##' proposal on the start date and
##' a poisson on the number of cases.
##'
##' @param seeding the original seeding
##' @param sd the standard deviation of the posson
##'
##'
##' @return a pertubed data frame
##'
perturb_seeding <- function(seeding,sd) {
    require(tidyverse)
    seeding <- seeding%>%
        group_by(place)%>%
        mutate(date = date+round(rnorm(1,0,sd)))%>%
        ungroup()%>%
        mutate(amount=rpois(length(amount),amount))

    return(seeding)

}



##' Fuction perturbs an npi parameter file based on user-specified distributions
##'
##' @param params the original paramters
##' @param perturbations a list of standard deviations
##'
##'
##' @return a pertubed data frame
##'
perturb_npis <- function(npis, perturbations) {
  require(dplyr)
  require(magrittr)
  npis_new <- npis
  geoids <- colnames(select(npis, -time, -parameter, -npi_name))
  for (par in names(perturbations)) {
    if (perturbations[[par]]$distribution == "normal") {
      pert_dist <- function(n) {rnorm(n, mean = perturbations[[par]]$mu, sd = perturbations[[par]]$sd)}
    } 
    ind <- npis[["parameter"]] == par 
    for (gid in geoids) {
      npis_new[[gid]][ind] <- npis_new[[gid]][ind] + pert_dist(1)
    }
  }
  return(npis_new)
}

##'
##' Function to go through to accept or reject seedings in a block manner based
##' on a geoid specific likelihood.
##'
##'
##' @param seeding_orig original seeding file.
##' @param seeding_prop proposal seeding file
##' @param loc_lls a dataframe with columns orig_ll and prop_ll per place
##'
##' @return a new data frame with the confirmed seedin.
##'
accept_reject_new_seeding_npis <- function(
  seeding_orig,
  seeding_prop,
  npis_orig,
  npis_prop, 
  orig_lls,
  prop_lls
) {
  rc_seeding <- seeding_orig
  rc_npis <- npis_orig

    if(!all(orig_lls$geoid == prop_lls$geoid)){stop("geoids must match")}
    ##draww accepts/rejects
    ratio <- exp(prop_lls$ll - orig_lls$ll)
    accept <- ratio>runif(length(ratio),0,1)

    orig_lls$ll[accept] <- prop_lls$ll[accept]

  for (place in orig_lls$geoid[accept]) {
    rc_seeding[rc_seeding$place ==place, ] <- seeding_prop[seeding_prop$place ==place, ]
    rc_npis[, place] <- npis_prop[, place]
  }
  
  return(list(seeding=rc_seeding, npis=rc_npis, lls = orig_lls))
}



##Calculate the model evaluation statistic.
logLikStat <- function(obs, sim, distr, param, add_one = F) {
  if (add_one) {
    sim[sim == 0] = 1
  }

  if(distr == "pois") {
      rc <- dpois(obs, sim, log = T)
  } else if (distr == "norm") {
      rc <- dnorm(obs, sim, sd = param[1], log = T)
  } else if (distr == "nbinom") {
      rc <- dnbinom(obs, sim, k = param[1], log = T)
  } else if (distr == "sqrtnorm") {
      rc <- dnorm(sqrt(obs), sqrt(sim), sd=sqrt(sim)*param[[1]], log = T)
  } else if (distr == "sqrtnorm_scale_sim") { #param 1 is cov, param 2 is multipler
      rc <- dnorm(sqrt(obs), sqrt(sim*param[[2]]), sd=sqrt(sim*param[[2]])*param[[1]],log=T)
  } else {
      stop("Invalid stat specified")
  }

  return(rc)
}

iterateAccept <- function(ll_ref,ll_new,ll_col) {
  ll_new <- ll_new[[ll_col]]
  ll_ref <- ll_ref[[ll_col]]
  ll_ratio <- exp(min(c(0, ll_new - ll_ref)))
  if (ll_ratio >= runif(1)) {
    return(TRUE)
  }
  return(FALSE)
}

if(!("obs" %in% ls())){
  obs <<- readr::read_csv(data_path)
}
geonames <- unique(obs[[obs_nodename]])
# Compute statistics
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

required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")
for(scenario in scenarios) {

  ## One time setup for python
  reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
  reticulate::py_run_string(paste0("scenario = '", scenario, "'"))
  reticulate::py_run_file("COVIDScenarioPipeline/minimal_interface.py")

  for(deathrate in deathrates) {
      # Data -------------------------------------------------------------------------
      # Load
    
    if(!file.exists(config$seeding$lambda_file)){
      err <- system(paste(
        opt$rpath,
        paste(opt$pipepath,"R","scripts","create_seeding.R", sep='/'),
        "-c",opt$config
      ))
    } else {
      err <- 0
    }
    if(err != 0){quit("no")}
    initial_seeding <- readr::read_csv(config$seeding$lambda_file)
    current_seeding <- perturb_seeding(initial_seeding,config$seeding$perturbation_sd)
    write.csv(
      current_seeding,
      file = seeding_file_path(config,sprintf("%09d",opt$this_slot))
    )

    current_index <- 0
    current_likelihood <- data.frame()

    # FIX ME : this file won't exist in general
    # TODO CHANGE TO FIRST DRAW OF SEIR CODE
    first_param_file <- parameter_file_path(config,opt$this_slot, scenario)
    first_npi_file <- npi_file_path(config,opt$this_slot, scenario)
    if((!file.exists(first_npi_file)) | (!file.exists(first_npi_file))){
      py$onerun_SEIR(opt$this_slot,py$s)
    }
    initial_npis <- arrow::read_parquet(first_npi_file)
    initial_params <- arrow::read_parquet(first_param_file)

    for( index in seq_len(opt$simulations_per_slot)) {
      print(index)
      # Load sims -----------------------------------------------------------

      current_seeding <- perturb_seeding(initial_seeding,config$seeding$perturbation_sd)
      current_npis <- perturb_npis(initial_npis, config$filtering$perturbations)
      current_params <- initial_params
      write.csv(
        current_seeding,
        file = seeding_file_path(config,sprintf("%09d",opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index))
      )
      arrow::write_parquet(
        current_npis,
        npi_file_path(config,opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index,scenario)
      )
      arrow::write_parquet(
        current_params,
        parameter_file_path(config,opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index,scenario)
      )


      ## Generate files
      this_index <- opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index
      err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
      err <- ifelse(err == 1,0,1)
      if(err != 0){quit("no")}

      ## Run hospitalization
      err <- system(paste(
        opt$rpath,
        paste(opt$pipepath,"R","scripts","hosp_run.R", sep='/'),
        "-j",opt$jobs,
        "-c",opt$config,
        "-p",opt$pipepath,
        "-n",1,
        "-i",opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index
      ))
      if(err != 0){quit("no")}

      file <- paste(
        'hospitalization',
        'model_output',
        paste0(config$name,'_',scenario),
        paste0(
          deathrate,
          '_',
          "death_death-",
          sprintf("%09d",opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index),
          '.hosp.parquet'
        ),
        sep = '/'
      )
      print(paste("Reading",file))

      sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",file))(file) %>%
        filter(time <= max(obs$date)) %>%
        select(-date_inds)

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

        local_sim_hosp <- dplyr::filter(sim_hosp, !!rlang::sym(obs_nodename) == location)
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
        # Compute log-likelihoods

        log_likelihood_data[[location]] <- dplyr::tibble(
          ll = sum(unlist(log_likelihood)),
          filename = file,
          geoid = location
        )
        names(log_likelihood_data)[names(log_likelihood_data) == 'geoid'] <- obs_nodename
      # }
      }
      rm(sim_hosp)

      log_likelihood_data <- log_likelihood_data %>% do.call(what=rbind)

      # Compute total loglik for each sim
      likelihood <- log_likelihood_data %>%
        summarise(ll = sum(ll, na.rm = T)) %>%
        mutate(pdeath = deathrate, scenario = scenario)

      ## For logging
      if(current_index == 0){
        current_likelihood <- likelihood
        current_index <- index
        initial_seeding <- current_seeding
        initial_npis <- current_npis
        initial_params <- current_params
        previous_likelihood_data <- log_likelihood_data
        next
      }
      print(paste("Current likelihood",current_likelihood,"Proposed likelihood",likelihood))
      
      if(iterateAccept(current_likelihood, likelihood, 'll')){
        current_index <- index
        current_likelihood <- likelihood
      }

      seeding_npis_list <- accept_reject_new_seeding_npis(
        current_seeding,
        initial_seeding,
        current_npis,
        initial_npis,
        log_likelihood_data,
        previous_likelihood_data
      )
      initial_seeding <- seeding_npis_list$seeding
      initial_npis <- seeding_npis_list$npis
      previous_likelihood_data <- seeding_npis_list$likelihood
      print(paste("Current index is ",current_index))
      print(log_likelihood_data)
      print(previous_likelihood_data)
    }

    current_file <- paste(
      'hospitalization',
      'model_output',
      paste0(config$name,'_',scenario),
      paste0(
        deathrate,
        '_',
        "death_death-",
        sprintf("%09d",opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + current_index),
        '.hosp.parquet'
      ),
      sep = '/'
    )

    target_file <- paste(
      'hospitalization',
      'model_output',
      paste0(config$name,'_',scenario),
      paste0(
        deathrate,
        '_',
        "death_death-",
        sprintf("%09d",opt$this_slot),
        '.hosp.parquet'
      ),
      sep = '/'
    )

    print(paste("Copying",current_file,"to",target_file))
    target_dir <- gsub('/[^/]*$','',target_file)
    dir.create(target_dir, recursive=TRUE)
    file.copy(from=current_file,to=target_file, overwrite=TRUE)
  }
}
#}
