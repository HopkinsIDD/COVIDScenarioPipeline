
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
  #' @param -s The intervention scenarios
  optparse::make_option(c("-s", "--scenarios"), action="store", default='all', type='character', help="name of the intervention to run, or 'all' to run all of them"),
  #' @param -d The death rate
  optparse::make_option(c("-d", "--deathrate"), action="store", default='all', type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default="8", type='integer', help="Number of jobs to run in parallel"),
  optparse::make_option(c("-k", "--index"), action="store", default="1", type='integer', help = "id of this slot")
 
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

required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")
# foreach (scenario = scenarios) %:%
#   foreach(deathrate = deathrates) %do% {
for(scenario in scenarios) {
  for(deathrate in deathrates) {
  # profout <- profvis::profvis({
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
    
    scenario_files <- list.files(paste0('hospitalization/model_output/',config$name,"_",scenario),full.names=TRUE)
    scenario_files <- scenario_files[grepl(deathrate,gsub('.*/','',scenario_files))]
  
    cl <- parallel::makeCluster(opt$j)
    doParallel::registerDoParallel(cl)
    doParallel::registerDoParallel
    ll_data <- foreach(file = scenario_files, .packages = required_packages) %dopar% {
    # ll_data <- list()
    # for( file in scenario_files) {
      # Load sims -----------------------------------------------------------
      
      sim_hosp <<- report.generation:::read_file_of_type(gsub(".*[.]","",file))(file) %>% 
        inner_join(geodata, by = config$spatial_setup$nodenames) %>% 
        select(-date_inds) %>% 
        group_by(time, USPS, sim_num, comp) %>% 
        select_if(is.numeric) %>% 
        summarise_all(sum) %>% 
        mutate(uid = str_c(USPS, sim_num, sep = "-")) 
  
  
      
      log_likelihood_data <- list()
      for(location in sim_hosp$USPS) {
      # log_likelihood_data <- foreach (location = sim_hosp$USPS) %do% {
        # Compute log-likelihood of data for each sim
        # This part can be parallelized
        # One scenarios, one pdeath
        if(!('sim_hosp' %in% ls())){
          sim_hosp <<- report.generation:::read_file_of_type(gsub(".*[.]","",file))(file) %>% 
            inner_join(geodata, by = config$spatial_setup$nodenames) %>% 
            select(-date_inds) %>% 
            group_by(time, USPS, sim_num, comp) %>% 
            select_if(is.numeric) %>% 
            summarise_all(sum) %>% 
            mutate(uid = str_c(USPS, sim_num, sep = "-")) 
        }
        local_sim_hosp <- dplyr::filter(sim_hosp, USPS == location)
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
  
        log_likelihood_data[[file]] <- data.frame(ll = sum(unlist(log_likelihood)), filename = file, USPS = location)
      # }
      }
      rm(sim_hosp)
      print(log_likelihood_data)
  
      log_likelihood_data <- log_likelihood_data %>% do.call(what=rbind)
  
      # print(log_likelihood)
      
      # Compute total loglik for each sim
      tmp <- log_likelihood_data %>% 
        group_by(filename) %>% 
        summarise(ll = sum(ll, na.rm = T)) %>% 
        mutate(pdeath = deathrate, scenario = scenario)
      # ll_data[[file]] <- tmp
    # }
      tmp
    }
    # })
    parallel::stopCluster(cl)
    ll_data <- do.call(ll_data, what=rbind)
  
    # Acceptance -------------------------------------------------------------------
    
    accepted_data <- iterateAccept(ll_data, "ll")
    accepted_data$filename <- scenario_files[accepted_data$ind_accept]
    accepted_data$target_filename <- gsub('hosp[.]','filt.',gsub(paste0('0*',accepted_data$ind_accept),sprintf("%09d",opt$index),gsub('^','filtered_',accepted_data$filename)))
    accepted_data$target_dir <- gsub('/[^/]*$','',accepted_data$target_filename)
    dir.create(accepted_data$target_dir, recursive=TRUE)
    file.copy(from=accepted_data$filename,to=accepted_data$target_filename, overwrite=TRUE)
  }
}
#}
