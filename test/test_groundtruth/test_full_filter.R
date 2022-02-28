
# Test `filter_MC.R` ------------------------------------------------------



# SETUP -------------------------------------------------------------------

library(tidyverse)

Sys.setenv(CONFIG_PATH="test/test_groundtruth/config.yml")
Sys.getenv("CONFIG_PATH")

# from full_filter.R ------------------------------------------------------


suppressMessages(library(parallel))
suppressMessages(library(foreach))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
options(readr.num_columns = 0)

option_list = list(
    optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
    optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
    optparse::make_option(c("-s", "--scenarios"), action="store", default=Sys.getenv("COVID_SCENARIOS", 'all'), type='character', help="name of the intervention to run, or 'all' to run all of them"),
    optparse::make_option(c("-d", "--deathrates"), action="store", default=Sys.getenv("COVID_DEATHRATES", 'all'), type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
    optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", parallel::detectCores()), type='integer', help="Number of jobs to run in parallel"),
    optparse::make_option(c("-k", "--sims_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT", NA), type='integer', help = "Number of simulations to run per slot"),
    optparse::make_option(c("-n", "--slots"), action="store", default=Sys.getenv("COVID_NSIMULATIONS", as.numeric(NA)), type='integer', help = "Number of slots to run."),
    optparse::make_option(c("-b", "--this_block"), action="store", default=Sys.getenv("COVID_BLOCK_INDEX",1), type='integer', help = "id of this block"),
    optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
    optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
    optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)


# not full_filter.R

# load test config
config = covidcommon::load_config(opt$config)
config$seeding$variant_filename <- "test/test_groundtruth/data/variant_props_long.csv"

##Load infromationon geographic locations from geodata file.
suppressMessages(geodata <- report.generation::load_geodata_file(
    paste(
        config$spatial_setup$base_path,
        config$spatial_setup$geodata, sep = "/"
    ),
    geoid_len=5 #Is this hardcode a good idea.
))
obs_nodename <- config$spatial_setup$nodenames

#############






print("Starting")
if(opt$config == ""){
    optparse::print_help(parser)
    stop(paste(
        "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
    ))
}

#config <- covidcommon::load_config(opt$config)

deathrates <- opt$deathrates
if(all(deathrates == "all")) {
    deathrates<- config$outcomes$scenarios
} else if (!(deathrates %in% config$outcomes$scenarios)){
    message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
    quit("yes", status=1)
}

scenarios <- opt$scenarios
if (all(scenarios == "all")){
    scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
    message(paste("Invalid scenario arguments: [",paste(setdiff(scenarios, config$interventions$scenarios)), "] did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
    quit("yes", status=1)
}

if(is.na(opt$sims_per_slot)) {
    opt$sims_per_slot <- config$filtering$simulations_per_slot
}

if(is.na(opt$slots)) {
    opt$slots <- config$nsimulations
}

if(is.null(opt$fix_negatives)) {
    opt$fix_negatives <- TRUE
}

covidcommon::prettyprint_optlist(list(scenarios=scenarios,deathrates=deathrates,slots=seq_len(opt$slots)))





# FROM filter_MC.R --------------------------------------------------------


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
    optparse::make_option(c("--cache_gt"), action = "store", default = Sys.getenv("CACHE_GT", TRUE), type = "logical", help = "Whether to use cached grount truth data"),
    optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
    optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
    optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable"),
    optparse::make_option(c("-R", "--is-resume"), action="store", default=Sys.getenv("COVID_IS_RESUME",FALSE), type = 'logical', help = "Is this run a resume"),
    optparse::make_option(c("-I", "--is-interactive"), action="store", default=Sys.getenv("COVID_INTERACTIVE",Sys.getenv("INTERACTIVE_RUN", FALSE)), type = 'logical', help = "Is this run an interactive run")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)


if(opt[["is-interactive"]]) {
    options(error=recover)
} else {
    options(
        error = function(...) {
            quit(..., status = 2)
        }
    )
}
covidcommon::prettyprint_optlist(opt)

#reticulate::use_python(Sys.which(opt$python),require=TRUE)
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

if(is.null(opt$fix_negatives)) {
  opt$fix_negatives <- TRUE
}



##Load infromationon geographic locations from geodata file.
suppressMessages(geodata <- report.generation::load_geodata_file(
    paste(
        config$spatial_setup$base_path,
        config$spatial_setup$geodata, sep = "/"
    ),
    geoid_len=5 #Is this hardcode a good idea.
))
obs_nodename <- config$spatial_setup$nodenames

##Load simulations per slot from config if not defined on command line
##command options take precendence
if (is.na(opt$simulations_per_slot)) {
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

# ground truth targets to pull
gt_targets <- c("Confirmed", "Deaths", "incidI", "incidDeath")
if(any(grepl("hosp", tolower(names(config$filtering$statistics))))){
    gt_targets <- c(gt_targets, "Hospitalizations", "incidHospitalizations")
}

opt$cache_gt <- FALSE

obs <- inference::get_ground_truth(
    data_path = data_path,
    fips_codes = fips_codes_,
    fips_column_name = obs_nodename,
    start_date = gt_start_date,
    end_date = gt_end_date,
    cache = opt$cache_gt,
    gt_source = gt_source,
    gt_scale = gt_scale,
    targets = gt_targets, 
    fix_negatives = opt$fix_negatives,
    variant_filename = config$seeding$variant_filename
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

# required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")
# 
# ## python configuration for minimal_interface.py
# reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
# reticulate::py_run_string(paste0("run_id = '", opt$run_id, "'"))
# #reticulate::import_from_path("SEIR", path=opt$pipepath)
# #reticulate::import_from_path("Outcomes", path=opt$pipepath)
# reticulate::py_run_string(paste0("index = ", 1))
# if(opt$stoch_traj_flag) {
#     reticulate::py_run_string(paste0("stoch_traj_flag = True"))
# } else {
#     reticulate::py_run_string(paste0("stoch_traj_flag = False"))
# }
# 
# for(scenario in scenarios) {
#     
#     reticulate::py_run_string(paste0("scenario = '", scenario, "'"))
#     
#     for(deathrate in deathrates) {
#         # Data -------------------------------------------------------------------------
#         # Load
#         
#         slot_prefix <- covidcommon::create_prefix(config$name,scenario,deathrate,opt$run_id,sep='/',trailing_separator='/')
#         
#         gf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','final',sep='/',trailing_separator='/')
#         ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/')
#         gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/')
#         
#         
#         chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
#         chimeric_local_prefix <- covidcommon::create_prefix(prefix=chimeric_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')
#         
#         global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
#         global_local_prefix <- covidcommon::create_prefix(prefix=global_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')
#         
#         
#         ## pass prefix to python and use
#         reticulate::py_run_string(paste0("deathrate = '", deathrate, "'"))
#         reticulate::py_run_string(paste0("prefix = '", global_block_prefix, "'"))
#         reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
#         
#         
#         
#         first_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block - 1)
#         first_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block - 1)
#         
#         inference::initialize_mcmc_first_block(
#             opt$run_id,
#             opt$this_block,
#             global_block_prefix,
#             chimeric_block_prefix,
#             py,
#             function(sim_hosp){
#                 sim_hosp <- dplyr::filter(sim_hosp,sim_hosp$time >= min(obs$date),sim_hosp$time <= max(obs$date))
#                 lhs <- unique(sim_hosp[[obs_nodename]])
#                 rhs <- unique(names(data_stats))
#                 all_locations <- rhs[rhs %in% lhs]
#                 
#                 inference::aggregate_and_calc_loc_likelihoods(
#                     all_locations = all_locations, # technically different
#                     modeled_outcome = sim_hosp,
#                     obs_nodename = obs_nodename,
#                     config = config,
#                     obs = obs,
#                     ground_truth_data = data_stats,
#                     hosp_file = first_global_files[['llik_filename']],
#                     hierarchical_stats = hierarchical_stats,
#                     defined_priors = defined_priors,
#                     geodata = geodata,
#                     snpi = arrow::read_parquet(first_global_files[['snpi_filename']]),
#                     hnpi = arrow::read_parquet(first_global_files[['hnpi_filename']]),
#                     hpar = dplyr::mutate(arrow::read_parquet(first_global_files[['hpar_filename']]),parameter=paste(quantity,!!rlang::sym(obs_nodename),outcome,sep='_'))
#                 )
#             },
#             is_resume = opt[['is-resume']]
#         )
#         
#         
#         ## So far no acceptances have occurred
#         current_index <- 0
#         ### Load initial files
#         seeding_col_types <- NULL
#         suppressMessages(initial_seeding <- readr::read_csv(first_chimeric_files[['seed_filename']], col_types=seeding_col_types))
#         if (opt$stoch_traj_flag) {
#             initial_seeding$amount <- as.integer(round(initial_seeding$amount))
#         }
#         initial_snpi <- arrow::read_parquet(first_chimeric_files[['snpi_filename']])
#         initial_hnpi <- arrow::read_parquet(first_chimeric_files[['hnpi_filename']])
#         initial_spar <- arrow::read_parquet(first_chimeric_files[['spar_filename']])
#         initial_hpar <- arrow::read_parquet(first_chimeric_files[['hpar_filename']])
#         chimeric_likelihood_data <- arrow::read_parquet(first_chimeric_files[['llik_filename']])
#         global_likelihood_data <- arrow::read_parquet(first_global_files[['llik_filename']])
#         
#         #####Get the full likelihood (WHY IS THIS A DATA FRAME)
#         # Compute total loglik for each sim
#         global_likelihood <- sum(global_likelihood_data$ll)
#         
#         #####LOOP NOTES
#         ### current means proposed
#         ### initial means accepted/current
#         
#         #####Loop over simulations in this block
#         for( this_index in seq_len(opt$simulations_per_slot)) {
#             print(paste("Running simulation", this_index))
#             
#             ## Create filenames
#             this_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, this_index)
#             this_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_local_prefix, this_index)
#             
#             ## Setup python
#             reticulate::py_run_string(paste0("prefix = '", global_local_prefix, "'"))
#             reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
#             
#             ## Do perturbations from accepted
#             proposed_seeding <- inference::perturb_seeding(
#                 seeding = initial_seeding,
#                 date_sd = config$seeding$date_sd,
#                 date_bounds = c(gt_start_date, gt_end_date),
#                 amount_sd = config$seeding$amount_sd,
#                 continuous = !(opt$stoch_traj_flag)
#             )
#             proposed_snpi <- inference::perturb_snpi(initial_snpi, config$interventions$settings)
#             proposed_hnpi <- inference::perturb_hnpi(initial_hnpi, config$interventions$settings)
#             proposed_spar <- initial_spar
#             if(!deathrate %in% names(config$outcomes$settings)){
#                 stop(paste("Deathrate",deathrate,"does not appear in outcomes::settings in the config"))
#             }
#             proposed_hpar <- inference::perturb_hpar(initial_hpar, config$outcomes$settings[[deathrate]])
#             
#             ## Write files that need to be written for other code to read
#             write.csv(proposed_seeding,this_global_files[['seed_filename']])
#             arrow::write_parquet(proposed_snpi,this_global_files[['snpi_filename']])
#             arrow::write_parquet(proposed_hnpi,this_global_files[['hnpi_filename']])
#             arrow::write_parquet(proposed_spar,this_global_files[['spar_filename']])
#             arrow::write_parquet(proposed_hpar,this_global_files[['hpar_filename']])
#             
#             ## Run SEIR
#             err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
#             err <- ifelse(err == 1,0,1)
#             if(err != 0){
#                 stop("SEIR failed to run")
#             }
#             
#             err <- py$onerun_OUTCOMES_loadID(this_index)
#             print(err)
#             err <- ifelse(err == 1,0,1)
#             if(err != 0){
#                 stop("HOSP failed to run")
#             }
#             
#             sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",this_global_files[['hosp_filename']]))(this_global_files[['hosp_filename']]) %>%
#                 dplyr::filter(time >= min(obs$date),time <= max(obs$date))
#             
#             
#             
#             lhs <- unique(sim_hosp[[obs_nodename]])
#             rhs <- unique(names(data_stats))
#             all_locations <- rhs[rhs %in% lhs]
#             
#             proposed_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(
#                 all_locations = all_locations,
#                 modeled_outcome = sim_hosp,
#                 obs_nodename = obs_nodename,
#                 config = config,
#                 obs = obs,
#                 ground_truth_data = data_stats,
#                 hosp_file = this_global_files[["llik_filename"]],
#                 hierarchical_stats = hierarchical_stats,
#                 defined_priors = defined_priors,
#                 geodata = geodata,
#                 snpi = proposed_snpi,
#                 hnpi = proposed_hnpi,
#                 hpar = dplyr::mutate(
#                     proposed_hpar,
#                     parameter = paste(quantity, !!rlang::sym(obs_nodename), outcome, sep = "_")
#                 )
#             )
#             
#             
#             rm(sim_hosp)
#             
#             ## UNCOMMENT TO DEBUG
#             ## print(global_likelihood_data)
#             ## print(chimeric_likelihood_data)
#             ## print(proposed_likelihood_data)
#             
#             ## Compute total loglik for each sim
#             proposed_likelihood <- sum(proposed_likelihood_data$ll)
#             
#             ## For logging
#             print(paste("Current likelihood",global_likelihood,"Proposed likelihood",proposed_likelihood))
#             
#             if(inference::iterateAccept(global_likelihood, proposed_likelihood) || ((current_index == 0) && (opt$this_block == 1))){
#                 print("****ACCEPT****")
#                 if ((opt$this_block == 1) && (current_index == 0)) {
#                     print("by default because it's the first iteration of a block 1")
#                 }
#                 current_index <- this_index
#                 global_likelihood <- proposed_likelihood
#                 global_likelihood_data <- proposed_likelihood_data
#             } else {
#                 print("****REJECT****")
#             }
#             arrow::write_parquet(proposed_likelihood_data, this_global_files[['llik_filename']])
#             
#             seeding_npis_list <- inference::accept_reject_new_seeding_npis(
#                 seeding_orig = initial_seeding,
#                 seeding_prop = proposed_seeding,
#                 snpi_orig = initial_snpi,
#                 snpi_prop = proposed_snpi,
#                 hnpi_orig = initial_hnpi,
#                 hnpi_prop = proposed_hnpi,
#                 hpar_orig = initial_hpar,
#                 hpar_prop = proposed_hpar,
#                 orig_lls = chimeric_likelihood_data,
#                 prop_lls = proposed_likelihood_data
#             )
#             initial_seeding <- seeding_npis_list$seeding
#             initial_snpi <- seeding_npis_list$snpi
#             initial_hnpi <- seeding_npis_list$hnpi
#             initial_hpar <- seeding_npis_list$hpar
#             chimeric_likelihood_data <- seeding_npis_list$ll
#             arrow::write_parquet(chimeric_likelihood_data, this_chimeric_files[['llik_filename']])
#             
#             print(paste("Current index is ",current_index))
#             # print(proposed_likelihood_data)
#             # print(chimeric_likelihood_data)
#             
#             ###Memory managment
#             rm(proposed_snpi)
#             rm(proposed_hnpi)
#             rm(proposed_hpar)
#             rm(proposed_seeding)
#         }
#         
#         #####Do MCMC end copy. Fail if unsucessfull
#         cpy_res <- inference::perform_MCMC_step_copies(current_index,
#                                                        opt$this_slot,
#                                                        opt$this_block,
#                                                        opt$run_id,
#                                                        global_local_prefix,
#                                                        gf_prefix,
#                                                        global_block_prefix)
#         
#         if(!prod(unlist(cpy_res))) {stop("File copy failed:", paste(unlist(cpy_res),paste(names(cpy_res),"|")))}
#         
#         
#         #####Write currently accepted files to disk
#         last_index_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, opt$simulations_per_slot)
#         output_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block)
#         output_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block)
#         readr::write_csv(initial_seeding,output_chimeric_files[['seed_filename']])
#         arrow::write_parquet(initial_snpi,output_chimeric_files[['snpi_filename']])
#         arrow::write_parquet(initial_hnpi,output_chimeric_files[['hnpi_filename']])
#         arrow::write_parquet(initial_spar,output_chimeric_files[['spar_filename']])
#         arrow::write_parquet(initial_hpar,output_chimeric_files[['hpar_filename']])
#         arrow::write_parquet(chimeric_likelihood_data,output_chimeric_files[['llik_filename']])
#         warning("Chimeric hosp and seir files not yet supported, just using the most recently generated file of each type")
#         file.copy(last_index_global_files[['hosp_filename']],output_chimeric_files[['hosp_filename']])
#         file.copy(last_index_global_files[['seir_filename']],output_chimeric_files[['seir_filename']])
#     }
# }
# 
# 
