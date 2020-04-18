
# Preamble ---------------------------------------------------------------------
library(dplyr)
library(readr)
library(covidcommon)
library(report.generation)
library(covidImportation)
library(stringr)
library(foreach)
library(magrittr)
library(itertools)
library(xts)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  #' @param -s The intervention scenario
  optparse::make_option(c("-s", "--scenario"), action="store", default='all', type='character', help="name of the intervention to run, or 'all' to run all of them"),
  #' @param -d The death rate
  optparse::make_option(c("-d", "--deathrate"), action="store", default='all', type='character', help="name of the death scenario to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default="8", type='integer', help="Number of jobs to run in parallel")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

## Block loads the config file and geodata
if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}
config = covidcommon::load_config(opt$config)

geodata <- report.generation::load_geodata_file(paste(
  config$spatial_setup$base_path,
  config$spatial_setup$geodata, sep = "/"),geoid_len=5) %>% 
  dplyr::select(geoid, USPS)

nfiles <- config$nsimulations## set to NULL or the actual number of sim files to include for final report

data_path <- config$filtering$data_path

# Parse jhucsse using covidImportation
if (!file.exists(data_path)) {
  jhucsse <- covidImportation::get_clean_JHUCSSE_data(aggr_level = "UID", 
                                   last_date = as.POSIXct(lubridate::ymd(config$end_date)),
                                   case_data_dir = file.path('importation',config$spatial_setup$setup_name,"case_data"),
                                   save_raw_data=TRUE,
                                   us_data_only=FALSE)
  jhucsse  <- 
    jhucsse %>%
    dplyr::mutate(date = as.Date(Update)) %>%
    dplyr::filter(Country_Region %in% "US") %>%
    dplyr::mutate(USPS = cdlTools::fips(str_replace_all(Province_State," ", ""),to="abbreviation")) %>% 
    group_by(date, USPS) %>%
    dplyr::summarize(cumConfirmed = sum(Confirmed), cumDeaths = sum(Deaths, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::arrange(date) %>%
    group_by(USPS) %>% 
    mutate(conf_incid = pmax(0, c(cumConfirmed[1], diff(cumConfirmed))),
           death_incid = pmax(0, c(cumConfirmed[1], diff(cumDeaths)))) %>% 
    filter(!is.na(USPS))
  write_csv(jhucsse, data_path)
  rm(jhucsse)
}

# Parse scenario arguments
deathrates <- opt$d
if(deathrates == "all") {
  deathrates<- names(as_evaled_expression(config$hospitalization$p_death)) # Run all of the configured hospitalization scenarios
} else if (!(deathrates %in% names(config$hospitalization$p_death))) {
  message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
  quit("yes", status=1)
}

scenarios <- opt$s
if (scenarios == "all"){
  scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
  message(paste("Invalid scenario argument:",scenario, "did not match any of the named args in ", paste(config$interventions$scenario, collapse = ", "), "\n"))
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
  lapply(
    stat_list,
    function(s) {
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
      res %>% 
        as.data.frame() %>% 
        mutate(date = rownames(.)) %>% 
        set_colnames(c(var_col, "date")) %>% 
        select(date, one_of(var_col))
    })
}

logLikStat <- function(obs, sim, distr, param, add_one = F) {
  if (add_one) {
    sim[sim == 0] = 1
  }
  if(distr == "pois") {
    dpois(obs, sim, log = T)
  } else if (distr == "norm") {
    dnorm(obs, sim, sd = params[1], log = T)
  } else if (distr == "nbinom") {
    dnbinom(obs, sim, k = params[1], log = T)
  }
}

iterateAccept <- function(df, ll_col) {
  ind <- 1
  lls <- df[[ll_col]]
  ll_ref <- lls[1]
  for (i in 2:length(lls)) {
    ll_new <- lls[i]
    ll_ratio <- exp(min(c(0, ll_new - ll_ref)))
    if (ll_ratio >= runif(1)) {
      ll_ref <- ll_new
      ind <- i
    }
  }
  return(data.frame(ll = ll_ref, ind_accept = ind))
}

cl <- parallel::makeCluster(opt$n)
doParallel::registerDoParallel(cl)
required_packages <- c("dplyr", "magrittr", "xts", "zoo", "report.generation", "foreach", "itertools")
foreach (scenario = scenarios) %:% {
  foreach(deathrate = deathrates) %do% {
    # Data -------------------------------------------------------------------------
    # Load
  if(!("obs" %in% ls())){
    obs <<- readr::read_csv(data_path)
  }
  obs_nodename <- "USPS"
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
  
  # Load sims -----------------------------------------------------------
  
  # The number of cores can be changed to the number available in the slot
  # Simulation outputs
    
    # One scenarios, one pdeath
    sim_hosp <- load_hosp_sims_filtered(
      sim_dir,
      num_files = nfiles,
      name_filter = config$hospitalization$parameters$p_death_names[i]) %>% 
      inner_join(geodata)
    
    # Aggregate by USPS (maybe to be replaced by function in packages that already does this?)
    sim_hosp <- sim_hosp %>% 
      select(-date_inds) %>% 
      group_by(time, USPS, sim_num, comp) %>% 
      select_if(is.numeric) %>% 
      summarise_all(sum) %>% 
      mutate(uid = str_c(USPS, sim_num, sep = "-")) 
    
    # Compute log-likelihood of data for each sim
    # This part can be parallelized
    ll_data <- foreach(sim = isplit(sim_hosp, sim_hosp$uid),
                       .packages = epacks
    ) %dopar% {
      
      USPS <- sim$value$USPS[1]
      end_date <- max(obs$date[obs[[obs_nodename]] == USPS])
      # Compute simulation statistics
      sim_stats <- getStats(sim$value, "time", "sim_var",
                            end_date = end_date,
                            config$filtering$statistics)
      
      # Get observation statistics
      USPS_obs_stats <- data_stats[[USPS]]
      
      # Compute log-likelihoods
      logliks <- lapply(
        seq_len(length(USPS_obs_stats)),
        function(i){
          # Check if there is data to compare to
          if (sum(is.na(USPS_obs_stats[[i]]$date) > 0)) {
            return(NA)
          }
          combined <- inner_join(USPS_obs_stats[[i]], sim_stats[[i]])
          lls <- logLikStat(obs = combined$data_var, 
                            sim = combined$sim_var, 
                            dist = config$filtering$statistics[[i]]$likelihood$dist,
                            param = config$filtering$statistics[[i]]$likelihood$param,
                            add_one = config$filtering$statistics[[i]]$add_one)
          return(sum(unlist(lls)))
        }) %>% 
        set_names(names(config$filtering$statistics))
      
      # add log-likelihoods together for givensimulation and USPS
      ll_tot <- sum(unlist(logliks))
      
      data.frame(ll = ll_tot, sim_num = sim$value$sim_num[1], USPS = USPS)
    }

    ll_data <- do.call(rbind,ll_data)
    
    # Compute total loglik for each sim
    ll_data %>% 
      group_by(sim_num) %>% 
      summarise(ll = sum(ll, na.rm = T)) %>% 
      mutate(pdeath = config$hospitalization$parameters$p_death[i])
  }
  
  parallel::stopCluster(cl)
  
  # Acceptance -------------------------------------------------------------------
  
  # Accept iteratively
  ind_accept <- lapply(config$hospitalization$parameters$p_death, 
                      function(x) 
                        iterateAccept(filter(ll_results, pdeath == x), "ll") %>% 
                        mutate(pdeath = x)) %>% 
    bind_rows() %>%
    print

}
