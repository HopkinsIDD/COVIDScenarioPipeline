## Preamble ---------------------------------------------------------------------
suppressMessages(library(readr))
suppressWarnings(suppressMessages(library(covidcommon)))
suppressMessages(library(report.generation))
suppressMessages(library(stringr))
suppressMessages(library(foreach))
suppressMessages(library(magrittr))
suppressMessages(library(xts))
suppressMessages(library(reticulate))
suppressMessages(library(truncnorm))
suppressMessages(library(parallel))
options(warn = 1)
options(readr.num_columns = 0)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
  optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
  optparse::make_option(c("-s", "--scenarios"), action="store", default=Sys.getenv("COVID_SCENARIOS", 'all'), type='character', help="name of the intervention to run, or 'all' to run all of them"),
  optparse::make_option(c("-d", "--deathrates"), action="store", default=Sys.getenv("COVID_DEATHRATES", 'all'), type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", parallel::detectCores()), type='integer', help="Number of jobs to run in parallel"),
  optparse::make_option(c("-k", "--simulations_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT", NA), type='integer', help = "number of simulations to run for this slot"),
  optparse::make_option(c("-i", "--this_slot"), action="store", default=Sys.getenv("COVID_SLOT_INDEX", 1), type='integer', help = "id of this slot"),
  optparse::make_option(c("-b", "--this_block"), action="store", default=Sys.getenv("COVID_BLOCK_INDEX",1), type='integer', help = "id of this block"),
  optparse::make_option(c("-t", "--stoch_traj_flag"), action="store", default=Sys.getenv("COVID_STOCHASTIC",TRUE), type='logical', help = "Stochastic SEIR and outcomes trajectories if true"),
  optparse::make_option(c("--ground_truth_start"), action = "store", default = Sys.getenv("COVID_GT_START", ""), type = "character", help = "First date to include groundtruth for"),
  optparse::make_option(c("--ground_truth_end"), action = "store", default = Sys.getenv("COVID_GT_END", ""), type = "character", help = "Last date to include groundtruth for"),
  optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
  optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
  optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable"),
  optparse::make_option(c("-R", "--is-resume"), action="store", default=Sys.getenv("COVID_IS_RESUME",FALSE), type = 'logical', help = "Is this run a resume")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

covidcommon::prettyprint_optlist(opt)

# Parameters for adaptive method. Un-comment to run adaptive MCMC, otherwise no adaptation will occur. 
# Adaptive MCMC alters standard deviation of perturbation in response to the (chimeric) time-averaged acceptance rate
adapt <- list(on=F)
# adapt <- list(on=T, #T/F to do adaptive MCMC
#               accept_opt=0.3, #optimal acceptance rate
#               perturb_sd_update = 0.0 #maximum relative increase in sd of perturbation per iteration
#               )
# print(paste0('Using adaptive method with optimal acceptance rate ',100*adapt$accept_opt,'% and maximum increase in perturbation step of ',100*adapt$perturb_sd_update,"%"))


reticulate::use_python(Sys.which(opt$python),require=TRUE)
## Block loads the config file and geodata
if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}
config = covidcommon::load_config(opt$config)

if(('perturbation_sd' %in% names(config$seeding))) {
  if(('date_sd' %in% names(config$seeding))) {
    stop("Both the key seeding::perturbation_sd and the key seeding::date_sd are present in the config file, but only one allowed.")
  }
  config$seeding$date_sd <- config$seeding$perturbation_sd
}
if (!('date_sd' %in% names(config$seeding))) {
  stop("Neither the key seeding::perturbation_sd nor the key seeding::date_sd are present in the config file, but one is required.")
}
if (!('amount_sd' %in% names(config$seeding))) {
  config$seeding$amount_sd <- 1
}

if(!(config$seeding$method %in% c('FolderDraw','InitialConditionsFolderDraw'))){
  stop("This filtration method requires the seeding method 'FolderDraw'")
}

if(!(config$seeding$method %in% c('FolderDraw','InitialConditionsFolderDraw'))){
  stop("This filtration method requires the seeding method 'FolderDraw'")
}
#if(!('lambda_file' %in% names(config$seeding))) {
#  stop("Despite being a folder draw method, filtration method requires the seeding to provide a lambda_file argument.")
#}


# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)


##Load information on geographic locations from geodata file.
suppressMessages(geodata <- report.generation::load_geodata_file(
  paste(
    config$spatial_setup$base_path,
    config$spatial_setup$geodata, sep = "/"
  ),
  geoid_len=5 #Is this hardcode a good idea?
))
obs_nodename <- config$spatial_setup$nodenames

##Load simulations per slot from config if not defined on command line
##command options take precedence
if(is.na(opt$simulations_per_slot)){
  opt$simulations_per_slot <- config$filtering$simulations_per_slot
}
print(paste("Running",opt$simulations_per_slot,"simulations"))

##Define data directory and create if it does not exist
data_path <- config$filtering$data_path
data_dir <- dirname(data_path)
if(!dir.exists(data_dir)){
  suppressWarnings(dir.create(data_dir,recursive=TRUE))
}

# Parse scenarios arguments
##If death rates are specified check their existence
deathrates <- opt$deathrates
if(all(deathrates == "all")) {
  deathrates<- config$outcomes$scenarios
} else if (!(deathrates %in% config$outcomes$scenarios)) {
  message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
  quit("yes", status=1)
}

##If scenarios are specified check their existence
scenarios <- opt$scenarios
if (all(scenarios == "all")){
  scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
  message(paste("Invalid scenario argument:",scenario, "did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
  quit("yes", status=1)
}


##Creat heirarchical stats object if specified
hierarchical_stats <- list()
if("hierarchical_stats_geo"%in%names(config$filtering)) {
  hierarchical_stats <- config$filtering$hierarchical_stats_geo
}


##Create priors if specified
defined_priors <- list()
if("priors"%in%names(config$filtering)) {
  defined_priors <- config$filtering$priors
}



## Runner Script---------------------------------------------------------------------

## backwards compatibility with configs that don't have filtering$gt_source parameter will use the previous default data source (USA Facts)
if(is.null(config$filtering$gt_source)){
  gt_source <- "usafacts"
} else{
  gt_source <- config$filtering$gt_source
}

gt_scale <- ifelse(state_level, "US state", "US county")
fips_codes_ <- geodata[[obs_nodename]]

gt_start_date <- lubridate::ymd(config$start_date)
if (opt$ground_truth_start != "") {
  gt_start_date <- lubridate::ymd(opt$ground_truth_start)
} else if (!is.null(config$start_date_groundtruth)) {
  gt_start_date <- lubridate::ymd(config$start_date_groundtruth)
}
if (gt_start_date < lubridate::ymd(config$start_date)) {
  gt_start_date <- lubridate::ymd(config$start_date)
}

gt_end_date <- lubridate::ymd(config$end_date)
if (opt$ground_truth_end != "") {
  gt_end_date <- lubridate::ymd(opt$ground_truth_end)
} else if (!is.null(config$end_date_groundtruth)) {
  gt_end_date <- lubridate::ymd(config$end_date_groundtruth)
}
if (gt_end_date > lubridate::ymd(config$end_date)) {
  gt_end_date <- lubridate::ymd(config$end_date)
}


obs <- inference::get_ground_truth(
  data_path = data_path,
  fips_codes = fips_codes_,
  fips_column_name = obs_nodename,
  start_date = gt_start_date,
  end_date = gt_end_date,
  gt_source = gt_source,
  gt_scale = gt_scale
)

geonames <- unique(obs[[obs_nodename]])

## Compute statistics
data_stats <- lapply(
  geonames,
  function(x) {
    df <- obs[obs[[obs_nodename]] == x, ]
    inference::getStats(
      df,
      "date",
      "data_var",
      stat_list = config$filtering$statistics)
  }) %>%
  set_names(geonames)

required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")

## python configuration for minimal_interface.py
reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
reticulate::py_run_string(paste0("run_id = '", opt$run_id, "'"))
#reticulate::import_from_path("SEIR", path=opt$pipepath)
#reticulate::import_from_path("Outcomes", path=opt$pipepath)
reticulate::py_run_string(paste0("index = ", 1))
if(opt$stoch_traj_flag) {
  reticulate::py_run_string(paste0("stoch_traj_flag = True"))
} else {
  reticulate::py_run_string(paste0("stoch_traj_flag = False"))
}

#Temporary
#print("Setting random number seed")
#set.seed(1) # set within R
#reticulate::py_run_string(paste0("rng_seed = ", 1)) #set within Python

# Scenario loop -----

for(scenario in scenarios) {
  
  reticulate::py_run_string(paste0("scenario = '", scenario, "'"))
  
  for(deathrate in deathrates) {
    
    # file name prefixes for this scneario + deathrate combination
    
    slot_prefix <- covidcommon::create_prefix(config$name,scenario,deathrate,opt$run_id,sep='/',trailing_separator='/') # makes prefix of the form name/scenario/deathrate/run_id
    
    gf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','final',sep='/',trailing_separator='/') # makes prefix of the form name/scenario/deathrate/run_id/global/final
    cf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','final',sep='/',trailing_separator='/') # makes prefix of the form name/scenario/deathrate/run_id/chimeric/final
    ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/') # makes prefix of the form name/scenario/deathrate/run_id/chimeric/intermediate
    gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/') # makes prefix of the form name/scenario/deathrate/run_id/global/intermediate
    
    chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')  # makes prefix of the form name/scenario/deathrate/run_id/chimeric/intermediate/slot
    chimeric_local_prefix <- covidcommon::create_prefix(prefix=chimeric_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.') # makes prefix of the form name/scenario/deathrate/run_id/chimeric/intermediate/slot.block
    
    global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.') # makes prefix of the form name/scenario/deathrate/run_id/global/intermediate/slot
    global_local_prefix <- covidcommon::create_prefix(prefix=global_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.') #makes prefix of the form name/scenario/deathrate/run_id/global/intermediate/slot.block
    
    
    ### Set up initial conditions ----------
    
    
    ## pass prefix to python and use
    reticulate::py_run_string(paste0("deathrate = '", deathrate, "'"))
    reticulate::py_run_string(paste0("prefix = '", global_block_prefix, "'")) # files will be saved in global/intermediate/slot....
    reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
    
    first_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block - 1) # makes file names of the form variable/name/scenario/deathrate/run_id/global/intermediate/slot.(block-1).run_id.variable.ext
    first_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block - 1) # makes file names of the form variable/name/scenario/deathrate/run_id/chimeric/intermediate/slot.(block-1).run_id.variable.ext
    
    print("RUNNING: initialization of first block")
    
    inference::initialize_mcmc_first_block( # Functions within this function save variables to files of the form variable/name/scenario/deathrate/run_id/global/intermediate/slot.(block-1),run_id.variable.ext and also copied into the /chimeric/ version, which are referenced by first_global_files and first_chimeric_files
      opt$run_id,
      opt$this_block,
      global_block_prefix,
      chimeric_block_prefix,
      py,
      function(sim_hosp){
        sim_hosp <- dplyr::filter(sim_hosp,sim_hosp$time >= min(obs$date),sim_hosp$time <= max(obs$date))
        lhs <- unique(sim_hosp[[obs_nodename]])
        rhs <- unique(names(data_stats))
        all_locations <- rhs[rhs %in% lhs]

        inference::aggregate_and_calc_loc_likelihoods(
          all_locations = all_locations, # technically different
          modeled_outcome = sim_hosp,
          obs_nodename = obs_nodename,
          config = config,
          obs = obs,
          ground_truth_data = data_stats,
          hosp_file = first_global_files[['llik_filename']],
          hierarchical_stats = hierarchical_stats,
          defined_priors = defined_priors,
          geodata = geodata,
          snpi = arrow::read_parquet(first_global_files[['snpi_filename']]),
          hnpi = arrow::read_parquet(first_global_files[['hnpi_filename']]),
          hpar = dplyr::mutate(arrow::read_parquet(first_global_files[['hpar_filename']]),parameter=paste(quantity,source,outcome,sep='_'))
        )
      },
      is_resume = opt[['is-resume']]
    )
    
    
    ## So far no acceptances have occurred
    current_index <- 0

    ### Load initial files (were created within function initialize_mcmc_first_block)
    seeding_col_types <- NULL
    suppressMessages(initial_seeding <- readr::read_csv(first_chimeric_files[['seed_filename']], col_types=seeding_col_types))
    if (opt$stoch_traj_flag) {
      initial_seeding$amount <- as.integer(round(initial_seeding$amount))
    }
    initial_snpi <- arrow::read_parquet(first_chimeric_files[['snpi_filename']])
    initial_hnpi <- arrow::read_parquet(first_chimeric_files[['hnpi_filename']])
    initial_spar <- arrow::read_parquet(first_chimeric_files[['spar_filename']])
    initial_hpar <- arrow::read_parquet(first_chimeric_files[['hpar_filename']])
    chimeric_likelihood_data <- arrow::read_parquet(first_chimeric_files[['llik_filename']])
    global_likelihood_data <- arrow::read_parquet(first_global_files[['llik_filename']])
    
    ##Add initial perturbation sd values to parameter files----
    initial_snpi <- inference::add_perturb_column_snpi(initial_snpi,config$interventions$settings)
    initial_hnpi <- inference::add_perturb_column_hnpi(initial_hnpi,config$interventions$settings)
    
    #Need to write these parameters back to the SAME chimeric file since they have a new column now
    arrow::write_parquet(initial_snpi,first_chimeric_files[['snpi_filename']])
    arrow::write_parquet(initial_hnpi,first_chimeric_files[['hnpi_filename']])
    
    # Also need to add this column to the global file (it will always be equal in the first block) (MIGHT NOT BE WORKING)
    arrow::write_parquet(initial_snpi,first_global_files[['snpi_filename']])
    arrow::write_parquet(initial_hnpi,first_global_files[['hnpi_filename']])
    
    #write empty files for lcov for now
    arrow::write_parquet(data.frame(),first_global_files[['lcov_filename']])
    arrow::write_parquet(data.frame(),first_chimeric_files[['lcov_filename']])
    
    #####Get the full likelihood (WHY IS THIS A DATA FRAME)
    # Compute total loglik for each sim
    global_likelihood <- sum(global_likelihood_data$ll)
    
    old_avg_global_accept_rate <- global_likelihood_data$accept_avg[1] # keep track of running average global acceptance rate, since old global likelihood data not kept in memory. Each geoID has same value for acceptance rate in global case, so we just take the 1st entry
    
    
    #####LOOP NOTES
    ### initial means accepted/current
    ### current means proposed
    
    ##Loop over simulations in this block ----
    
    startTimeCount=Sys.time()
    
    for( this_index in seq_len(opt$simulations_per_slot)) {
      
      print("")
      print(paste("Running simulation", this_index))
      
      startTimeCountEach=Sys.time()
      
      ## Create filenames
      this_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, this_index) # makes file names of the form variable/name/scenario/deathrate/run_id/global/intermediate/slot.block.iter.run_id.variable.ext
      this_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_local_prefix, this_index) # makes file names of the form variable/name/scenario/deathrate/run_id/chimeric/intermediate/slot.block.iter.run_id.variable.ext
      
      ## Setup python
      reticulate::py_run_string(paste0("prefix = '", global_local_prefix, "'")) # files will be saved in global/intermediate/slot.block....
      reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
      
      ### Do perturbations from accepted parameters to get proposed parameters ----
      
      proposed_seeding <- inference::perturb_seeding(
        seeding = initial_seeding,
        date_sd = config$seeding$date_sd,
        date_bounds = c(gt_start_date, gt_end_date),
        amount_sd = config$seeding$amount_sd,
        continuous = !(opt$stoch_traj_flag)
      )

      # Old perturbation method, directly from config
      #proposed_snpi <- inference::perturb_snpi(initial_snpi, config$interventions$settings)
      #proposed_hnpi <- inference::perturb_hnpi(initial_hnpi, config$interventions$settings)
      proposed_spar <- initial_spar
      if(!deathrate %in% names(config$outcomes$settings)){
        stop(paste("Deathrate",deathrate,"does not appear in outcomes::settings in the config"))
      }
      proposed_hpar <- inference::perturb_hpar(initial_hpar, config$outcomes$settings[[deathrate]])
      
      # New perturbation method, from parameter file instead
      print("NOTE: Perturbations are being read from files instead of configs after 1st iteration in each slot for snpi and hnpi")
      proposed_snpi <- inference::perturb_snpi_from_file(initial_snpi, config$interventions$settings, chimeric_likelihood_data, adapt)
      proposed_hnpi <- inference::perturb_hnpi_from_file(initial_hnpi, config$interventions$settings, chimeric_likelihood_data, adapt)
      
      ## Write files that need to be written for other code to read
      # writes to file  of the form variable/name/scenario/deathrate/run_id/global/intermediate/slot.block.iter.run_id.variable.ext
      write.csv(proposed_seeding,this_global_files[['seed_filename']])
      arrow::write_parquet(proposed_snpi,this_global_files[['snpi_filename']])
      arrow::write_parquet(proposed_hnpi,this_global_files[['hnpi_filename']])
      arrow::write_parquet(proposed_spar,this_global_files[['spar_filename']])
      arrow::write_parquet(proposed_hpar,this_global_files[['hpar_filename']])
      
      #write empty files for lcov for now
      arrow::write_parquet(data.frame(),this_global_files[['lcov_filename']])

      #TEST
      #print("PRINTING py$s.in_run_id")
      #print(py$s)
      
      ## Run SEIR model----
      err <- py$onerun_SEIR_loadID(this_index, py$s, this_index) # This fx is in seir.py, and it calls a set-up fx in seir.py which then calls the SEIR model in steps_source.py. Reads parameters from ..., saves output to ... 
      err <- ifelse(err == 1,0,1)
      if(err != 0){
        stop("SEIR failed to run")
      }
      
      ## Run outcomes model----
      err <- py$onerun_OUTCOMES_loadID(this_index) # This fx is in seir.py, and it calls the outcomes models in outcomes.py
      print(err)
      err <- ifelse(err == 1,0,1)
      if(err != 0){
        stop("HOSP failed to run")
      }
      
      sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",this_global_files[['hosp_filename']]))(this_global_files[['hosp_filename']]) %>%
        dplyr::filter(time >= min(obs$date),time <= max(obs$date))
      
      lhs <- unique(sim_hosp[[obs_nodename]])
      rhs <- unique(names(data_stats))
      all_locations <- rhs[rhs %in% lhs]
      
      ## Compare model output to data and calculate likelihood ----
      proposed_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(
        all_locations = all_locations,
        modeled_outcome = sim_hosp,
        obs_nodename = obs_nodename,
        config = config,
        obs = obs,
        ground_truth_data = data_stats,
        hosp_file = this_global_files[["llik_filename"]],
        hierarchical_stats = hierarchical_stats,
        defined_priors = defined_priors,
        geodata = geodata,
        snpi = proposed_snpi,
        hnpi = proposed_hnpi,
        hpar = dplyr::mutate(proposed_hpar, parameter = paste(quantity, source, outcome, sep = "_"))
      )
      
      
      rm(sim_hosp)
      
      ## UNCOMMENT TO DEBUG
      ## print(global_likelihood_data)
      ## print(chimeric_likelihood_data)
      ## print(proposed_likelihood_data)
      
      ## Compute total loglik for each sim
      proposed_likelihood <- sum(proposed_likelihood_data$ll)
      
      ## For logging
      print(paste("Current likelihood",formatC(global_likelihood,digits=2,format="f"),"Proposed likelihood",formatC(proposed_likelihood,digits=2,format="f")))
      
      ## Global likelihood acceptance or rejection decision ----
      
      if(inference::iterateAccept(global_likelihood, proposed_likelihood) || ((current_index == 0) && (opt$this_block == 1))){
        
        print("**** ACCEPT (Recording) ****")
        if ((opt$this_block == 1) && (current_index == 0)) {
          print("by default because it's the first iteration of a block 1")
        }

        current_index <- this_index #IMPORTANT: This is the index of the most recent globally accepted parameters
        proposed_likelihood_data$accept <- 1 # global acceptance decision (0/1), same recorded for each geoID
        avg_global_accept_rate <- ((this_index-1)*old_avg_global_accept_rate + 1)/(this_index) # update running average acceptance probability
        proposed_likelihood_data$accept_avg <-avg_global_accept_rate
        proposed_likelihood_data$accept_prob <- exp(min(c(0, proposed_likelihood - global_likelihood))) #acceptance probability 
        
        global_likelihood <- proposed_likelihood #This carries forward to next iteration as current global likelihood
        global_likelihood_data <- proposed_likelihood_data #This carries forward to next iteration as current global likelihood data
        
      } else {
        
        print("**** REJECT (Recording) ****")
        proposed_likelihood_data$accept <- 0
        avg_global_accept_rate <- ((this_index-1)*old_avg_global_accept_rate)/(this_index) # update running average acceptance probability
        proposed_likelihood_data$accept_avg <-avg_global_accept_rate
        proposed_likelihood_data$accept_prob <- exp(min(c(0, proposed_likelihood - global_likelihood))) #acceptance probability 
        
      }
      
      old_avg_global_accept_rate <- avg_global_accept_rate # keep track, since old global likelihood data not kept in memory
      print(paste("Average global acceptance rate: ",formatC(100*avg_global_accept_rate,digits=2,format="f"),"%"))
      
      arrow::write_parquet(proposed_likelihood_data, this_global_files[['llik_filename']]) # prints to file of the form llik/name/scenario/deathrate/run_id/global/intermediate/slot.block.iter.run_id.llik.ext
      
      ## Chimeric likelihood acceptance or rejection decisions (one round) -----
      #  "Chimeric" means GeoID-specific
      
      seeding_npis_list <- inference::accept_reject_new_seeding_npis(
        seeding_orig = initial_seeding,
        seeding_prop = proposed_seeding,
        snpi_orig = initial_snpi,
        snpi_prop = proposed_snpi,
        hnpi_orig = initial_hnpi,
        hnpi_prop = proposed_hnpi,
        hpar_orig = initial_hpar,
        hpar_prop = proposed_hpar,
        orig_lls = chimeric_likelihood_data,
        prop_lls = proposed_likelihood_data
      )
      
      old_avg_chimeric_accept_rate <- chimeric_likelihood_data$accept_avg # keep track of running average chimeric acceptance rate, for each geoID, since old chimeric likelihood data not kept in memory
      
      # Update accepted parameters to start next simulation
      initial_seeding <- seeding_npis_list$seeding
      initial_snpi <- seeding_npis_list$snpi
      initial_hnpi <- seeding_npis_list$hnpi
      initial_hpar <- seeding_npis_list$hpar
      chimeric_likelihood_data <- seeding_npis_list$ll
      
      # Update running average acceptance rate
      chimeric_likelihood_data$accept_avg <- ((this_index-1)*old_avg_chimeric_accept_rate + chimeric_likelihood_data$accept)/(this_index) # update running average acceptance probability. CHECK, this depends on values being in same order in both dataframes. Better to bind??
      
      arrow::write_parquet(chimeric_likelihood_data, this_chimeric_files[['llik_filename']]) # to file of the form llik/name/scenario/deathrate/run_id/chimeric/intermediate/slot.block.iter.run_id.llik.parquet
      
      ## Write accepted parameters to file
      # writes to file of the form variable/name/scenario/deathrate/run_id/chimeric/intermediate/slot.block.iter.run_id.variable.ext
      write.csv(initial_seeding,this_chimeric_files[['seed_filename']])
      arrow::write_parquet(initial_snpi,this_chimeric_files[['snpi_filename']])
      arrow::write_parquet(initial_hnpi,this_chimeric_files[['hnpi_filename']])
      arrow::write_parquet(initial_spar,this_chimeric_files[['spar_filename']])
      arrow::write_parquet(initial_hpar,this_chimeric_files[['hpar_filename']])
      
      #write empty files for lcov for now
      arrow::write_parquet(data.frame(),this_chimeric_files[['lcov_filename']])
      
      print(paste("Current index is ",current_index))
      
      ###Memory management
      rm(proposed_snpi)
      rm(proposed_hnpi)
      rm(proposed_hpar)
      rm(proposed_seeding)
      
      endTimeCountEach=difftime(Sys.time(), startTimeCountEach, units = "secs")
      print(paste("Time to run this MCMC iteration is ",formatC(endTimeCountEach,digits=2,format="f")," seconds"))
    }
    
    endTimeCount=difftime(Sys.time(), startTimeCount, units = "secs")
    print(paste("Time to run all MCMC iterations is ",formatC(endTimeCount,digits=2,format="f")," seconds"))
    
    #####Do MCMC end copy. Fail if unsuccessful
    
    # moves the most recently globally accepted parameter values from global/intermediate file to global/final  
    cpy_res_global <- inference::perform_MCMC_step_copies_global(current_index,
                                                   opt$this_slot,
                                                   opt$this_block,
                                                   opt$run_id,
                                                   global_local_prefix, #global/intermediate/slot.block
                                                   gf_prefix, #global/final
                                                   global_block_prefix) ##global/intermediate/slot
    
    if(!prod(unlist(cpy_res_global))) {stop("File copy failed:", paste(unlist(cpy_res_global),paste(names(cpy_res_global),"|")))}
    
    # moves the most recently chimeric accepted parameter values from chimeric/intermediate file to chimeric/final  
    cpy_res_chimeric <- inference::perform_MCMC_step_copies_chimeric(this_index,
                                                                 opt$this_slot,
                                                                 opt$this_block,
                                                                 opt$run_id,
                                                                 chimeric_local_prefix, #chimeric/intermediate/slot.block
                                                                 cf_prefix, #global/final
                                                                 chimeric_block_prefix) ##chimeric/intermediate/slot
    
    if(!prod(unlist(cpy_res_chimeric))) {stop("File copy failed:", paste(unlist(cpy_res_chimeric),paste(names(cpy_res_chimeric),"|")))}
    
    
    #####Write currently accepted files to disk
    
    last_index_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, opt$simulations_per_slot) #files of the form variables/name/scenario/deathrate/run_id/global/intermediate/slot.block.iter.run_id.variable.parquet
    output_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block) #files of the form variables/name/scenario/deathrate/run_id/chimeric/intermediate/slot.block.run_id.variable.parquet
    output_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block) #files of the form variables/name/scenario/deathrate/run_id/global/intermediate/slot.block.run_id.variable.parquet
    
    # save current chimeric parameter values (just slot.block, no iter)
    readr::write_csv(initial_seeding,output_chimeric_files[['seed_filename']])
    arrow::write_parquet(initial_snpi,output_chimeric_files[['snpi_filename']])
    arrow::write_parquet(initial_hnpi,output_chimeric_files[['hnpi_filename']])
    arrow::write_parquet(initial_spar,output_chimeric_files[['spar_filename']])
    arrow::write_parquet(initial_hpar,output_chimeric_files[['hpar_filename']])
    arrow::write_parquet(chimeric_likelihood_data,output_chimeric_files[['llik_filename']])
    
    #write empty files for lcov for now
    arrow::write_parquet(data.frame(),output_chimeric_files[['lcov_filename']])
    
    warning("Chimeric hosp and seir files not yet supported, just using the most recently generated file of each type")
    file.copy(last_index_global_files[['hosp_filename']],output_chimeric_files[['hosp_filename']])
    file.copy(last_index_global_files[['seir_filename']],output_chimeric_files[['seir_filename']])
  }
}
