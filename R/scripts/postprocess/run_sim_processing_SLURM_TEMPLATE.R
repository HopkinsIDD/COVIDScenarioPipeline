### This script runs processing for a SINGLE run on SLURM on rockfish. 
### It gets called automatically after a run is complete
### The default output is plots, csv and parquet files in a default FCH/Flusight format

suppressMessages(library(parallel))
suppressMessages(library(foreach))
suppressMessages(library(inference))
suppressMessages(library(tidyverse))
suppressMessages(library(doParallel))

options(readr.num_columns = 0)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
  optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
  optparse::make_option(c("-d", "--data_path"), action="store", default=Sys.getenv("DATA_PATH", Sys.getenv("DATA_PATH")), type='character', help="path to data repo"),
  optparse::make_option(c("-r","--run_processing"), action="store", default=Sys.getenv("PROCESS",FALSE), type='logical', help = "Process the run if true"),
  optparse::make_option(c("-p","--results_path"), action="store", type='character', help="Path for model output", default = Sys.getenv("FS_RESULTS_PATH", Sys.getenv("FS_RESULTS_PATH"))),
  optparse::make_option(c("-F","--full_fit"), action="store", default=Sys.getenv("FULL_FIT",FALSE), type='logical', help = "Process full fit"),
  optparse::make_option(c("-i", "--pathogen"), action="store", default=Sys.getenv("PATHOGEN", "flu"), type='character', help="Which pathogen is being run"),
  optparse::make_option(c("-g","--pull_gt"), action="store", default=Sys.getenv("PULL_GT",FALSE), type='logical', help = "Pull ground truth"),
  optparse::make_option(c("-f", "--flepimop_repo"), action="store", default=Sys.getenv("COVID_PATH", Sys.getenv("COVID_PATH")), type='character', help="path to the flepimop repo")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

print("Starting processing")
if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}

if(opt$data_path == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a data path -d option or DATA_PATH environment variable."
  ))
}

if(opt$results_path == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a results path with either -p option or FS_RESULTS_PATH environment variable."
  ))
}

if(opt$flepimop_repo == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a flepiMoP path with -f option or COVID_PATH environment variable."
  ))
}

print(paste('Processing run ',opt$run_id))


config <- covidcommon::load_config(opt$config)



# NOTES FOR USER -----------------------------------------------------------
#' 1. Set pathogen, application, and processing options in the "SETUP" block.
#' 2. Modify submission specifics in the "SUBMISSION SPECIFICS" block for any hub requirements. 
#' 3. Be sure to specify if this is a forecast ("fch") or scenario projection ("smh")
#' 4. Make sure the CSP repo (https://github.com/HopkinsIDD/COVIDScenarioPipeline) is in the same directory as this repo

gc()



# SETUP -------------------------------------------------------------------

# if using local, make sure its pulled and in same base directory as project
# Use local if no access to internet or making local changes to code
use_local_repo <- TRUE 
github_url <- "https://raw.githubusercontent.com/HopkinsIDD/COVIDScenarioPipeline/main"
csp_local_dir <- opt$flepimop_repo


# ~ Main Run Options -----------------------------------------------------------
pull_gt <- opt$pull_gt
full_fit <- opt$full_fit

# ~ Round -----------------------------------------------------------------
config_name <- opt$config
round_num <-  str_extract(config_name, "(?<=R)\\d+")
fch_date <- lubridate::as_date(config$end_date_groundtruth) + 1

# ~ Application -----------------------------------------------------------
smh_or_fch <- ifelse(grepl("FCH", opt$config), "fch", "smh") #"fch" or "smh"
pathogen <- opt$pathogen
repo <- opt$results_path
subdir <- NULL  #used for testing purposes

smh_or_fch <- tolower(smh_or_fch)

# ~ Scenarios -------------------------------------------------------------

## THIS HAS TO BE EDITED FOR EVERY ROUND - what are the scenarios
if(pathogen == 'flu'){
  scenarios <- c("highVE_optImm", "highVE_pesImm", "lowVE_optImm", "lowVE_pesImm") # include all, it will subset later based on what you put in `scenario_num`
  fch_scenario_num = 2
}else if(pathogen == 'covid19'){
  scenarios <- c("highBoo_modVar", "highBoo_highVar", "lowBoo_modVar", "lowBoo_highVar") # include all, it will subset later based on what you put in `scenario_num`
  fch_scenario_num = 3
}

scenario_num = 1:4 # which scenarios to process right now
if(tolower(smh_or_fch) == "fch"){
  scenario_num <- fch_scenario_num
}
n_weeks <- 41

# ~ Config Specifics ------------------------------------------------------
subname <- NA
subname_all <- NA
config_subname <- str_extract(config_name, paste0("(?<=", scenarios[scenario_num], "_).*?(?=\\.yml)"))


# ~ Outcomes to Include (for processing and plotting) ---------------------------------------------------
#   --- these get respecified below
outcomes_ <- c("I","C","H","D")
outcomes_time_ <- c("weekly","weekly","weekly","weekly")
outcomes_cum_ <- c(FALSE, FALSE, FALSE, FALSE)

# ~ Calibration -----------------------------------------------------------
outcomes_calibrate = c(FALSE, FALSE, TRUE, FALSE) # match outcomes_
n_calib_days = 14 # need one for each outcome to calibrate

# ~ Other Run Options -----------------------------------------------------
plot_samp <- FALSE
likelihood_prune_percentkeep <- 0.1
likelihood_prune <- FALSE
testing <- FALSE
quick_run <- FALSE
get_vaccination <- FALSE
keep_all_compartments <- FALSE
keep_variant_compartments <- FALSE 
keep_vacc_compartments <- FALSE
likelihood_sims <- FALSE
run_diagnostics <- TRUE



# OTHER SETUP -------------------------------------------------------------

proj_dir <- getwd()

# subset scenarios
if(tolower(smh_or_fch) == "fch"){
  scenario_num <- fch_scenario_num
}
scenarios <- scenarios[scenario_num]

geodata_file_path = file.path(config$spatial_setup$base_path, config$spatial_setup$geodata)



# SUBMISSION & PROCESSING SPECIFICS ----------------------------------------------------
## -- "outcomes_" are for processing. we want more than we submit for diagnostics.

# Flu Forecasts (FluSight: https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-forecasts/README.md)
if (smh_or_fch == "fch" & pathogen == "flu"){
  select_submission_targets <- function(data_comb){
    data_comb %>%
      filter(grepl("inc hosp", target)) %>%
      mutate(target = gsub("inc hosp", "inc flu hosp", target)) %>%
      dplyr::select(forecast_date=model_projection_date, target_end_date, target, location, type, quantile, value) %>%
      mutate(forecast_date = lubridate::as_date(forecast_date + 1))
  }
  forecast_date_name <- "forecast_date"
  outcomes_ <- c("I","C","H","D")
  outcomes_time_ <- c("weekly","weekly","weekly","weekly")
  outcomes_cum_ <- c(FALSE, FALSE, FALSE, FALSE)
  outcomes_cumfromgt = c(FALSE, FALSE, FALSE, FALSE)
  outcomes_calibrate = c(FALSE, FALSE, TRUE, FALSE)
}

# Flu Projections (Flu SMH: https://github.com/midas-network/flu-scenario-modeling-hub)
if (smh_or_fch == "smh" & pathogen == "flu"){
  select_submission_targets <- function(data_comb){
    data_comb %>%
      filter(grepl("inc hosp|inc death|cum hosp|cum death|peak size|peak time", target)) %>%
      filter(!(grepl("inc death|cum death", target) & location != "US")) %>%
      select(-forecast_date)
  }
  forecast_date_name <- "model_projection_date"
  outcomes_ <- c("I","C","H","D")
  outcomes_time_ <- c("weekly","weekly","weekly","weekly")
  outcomes_cum_ <- c(TRUE, TRUE, TRUE, TRUE)
  outcomes_cumfromgt = c(FALSE, FALSE, FALSE, FALSE)
  outcomes_calibrate = c(FALSE, FALSE, TRUE, FALSE)
} 

# COVID-19 Forecasts (COVID-19 FCH: https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md#Data-formatting)
if (smh_or_fch == "fch" & pathogen == "covid19"){
  
  select_submission_targets <- function(data_comb){
    targets <- c(paste0(1:20, " wk ahead cum death"),
                 paste0(1:20, " wk ahead inc death"),
                 paste0(1:8, " wk ahead inc case"),
                 paste0(0:130, " day ahead inc hosp"))
    
    data_comb <- data_comb %>%
      filter(type != "point-mean" & !(is.na(quantile) & type == "quantile")) %>%
      mutate(quantile = round(quantile, 3)) %>%
      filter(target %in% targets) %>%
      select(forecast_date=model_projection_date, scenario_id = NA, scenario_name = NA) %>% 
      select(-scenario_id, -scenario_name, -forecast_date, -age_group) %>%
      rename(forecast_date = model_projection_date) %>%
      filter(quantile %in% round(c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),3) | is.na(quantile))
    
    data_comb %>% 
      filter(str_detect(target, "inc case")) %>% 
      filter(quantile %in% c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975) | is.na(quantile)) %>%
      bind_rows(
        data_comb %>% filter(!str_detect(target, "inc case"))) %>%
      mutate(quantile = format(quantile, digits = 3))
    
  }
  
  forecast_date_name <- "forecast_date"
  outcomes_ <- c("I","C","H","D")
  outcomes_time_ <- c("weekly","weekly","daily","weekly")
  outcomes_cum_ <- c(FALSE, FALSE, FALSE, TRUE)
  outcomes_cumfromgt = c(FALSE, FALSE, FALSE, TRUE)
  outcomes_calibrate = c(FALSE, FALSE, TRUE, FALSE) # match outcomes_
}

# COVID-19 Projections (COVID-19 SMH: https://github.com/midas-network/covid-scenario-modeling-hub)
if (smh_or_fch == "smh" & pathogen == "covid19"){
  select_submission_targets <- function(data_comb){
    data_comb %>%
      filter(grepl("inc hosp|inc death|cum hosp|cum death|peak size|peak time", target))
  }
  forecast_date_name <- "model_projection_date"
  outcomes_ <- c("I","C","H","D")
  outcomes_time_ <- c("weekly","weekly","weekly","weekly")
  outcomes_cum_ <- c(TRUE, TRUE, TRUE, TRUE)
  outcomes_cumfromgt = c(FALSE, FALSE, FALSE, FALSE)
  outcomes_calibrate = c(FALSE, FALSE, TRUE, FALSE)
}





# Source Code and Functions -----------------------------------------------

# determine if local repo or pulling from github
# - if local: 
#     -- make sure you have the CSP repo in the same base directory as the project directory (i.e., same as COVID19_USA)
#     -- make sure to pull CSP so have most up-to-date
# - if github: 
#     -- code is pulled from the https://github.com/HopkinsIDD/COVIDScenarioPipeline repo
#     
if (use_local_repo){
  source_loc <- csp_local_dir
} else {
  source_loc <- github_url
}
print(source_loc)
source(paste0(source_loc, "/R/scripts/postprocess/groundtruth_source.R"))
source(paste0(source_loc, "/R/scripts/postprocess/sim_processing_source.R"))






#........................................................................................................

# change n_weeks if FCH (limit to 12 weeks)
projection_date <- lubridate::as_date(config$end_date_groundtruth)+1 # first day after groundtruth cutoff
forecast_date <- lubridate::as_date(config$start_date)+21 # date to start plotting from
end_date <- lubridate::as_date(config$end_date)
validation_date <- lubridate::as_date(config$end_date_groundtruth) # one day before defined forecast date

if (tolower(smh_or_fch)=="fch") { 
  n_weeks <- 4
  end_date <- lubridate::as_date(projection_date + n_weeks*7) - 1
}
point_est <- 0.5   # alternative: "mean"
compartment_types = c("vacc","variant","agestrat") # types of compartments, other than standard SEIR
keep_which_compartments <- c("variant") # types of compartments, other than standard SEIR
scenario_ids = paste0(c(LETTERS[1:4]), "-", lubridate::as_date(config$end_date_groundtruth)+1)[scenario_num]
scenario_names <- scenarios
scenarios_all <- scenarios
variants_ <- config$seir$compartments$variant_type
vacc_ <- config$seir$compartments$vaccination_stage
county_level <- FALSE
plot_projections <- TRUE

save_reps <- smh_or_fch=="smh" & !full_fit

scenario_dir <- opt$results_path
round_directory <- opt$results_path
data_path <- opt$data_path


# LOAD GROUND TRUTH -------------------------------------------------------

Sys.setenv(CONFIG_PATH = opt$config)
Sys.setenv(COVID_PATH = source_loc)
if (pathogen == "flu"){
  source(paste0(source_loc, "/R/scripts/build_flu_data.R"))
} else if (pathogen == "covid19"){
  source(paste0(source_loc, "/R/scripts/build_covid_data.R"))
}

gt_data <- clean_gt_forplots(readr::read_csv(config$filtering$data_path))

if (any(grepl("incidI", colnames(gt_data)))){
  colnames(gt_data) <- gsub("incidI", "incidC", colnames(gt_data))
}
if (any(grepl("cumI", colnames(gt_data)))){
  colnames(gt_data) <- gsub("cumI", "cumC", colnames(gt_data))
}

readr::write_csv(gt_data, file.path(round_directory, "gt_data_clean.csv"))

gc()





# RUN PROCESSING ----------------------------------------------------------
run_process <- ifelse(full_fit, 0, 1)
## for full_fit = TRUE, run full fit, then run full_fit = FALSE as well
while(run_process <= 1){
  
  full_fit_ <- ifelse(run_process == 0, TRUE, FALSE)
  
  i=1
  print(i)
  scenario_num <- i
  print(scenarios_all[scenario_num])
  scenario_dir <- paste0(round_directory, "/", scenarios_all[i], "/")
  
  if(smh_or_fch=='fch'){
    scenario_names[i] <- "model_output"
  }
  
  tmp_out <- process_sims(scenario_num = scenario_num,
                          scenarios_all = scenarios_all,
                          scenario_names = scenario_names,
                          scenario_ids = scenario_ids,
                          proj_id = scenario_names[i],
                          projection_date = projection_date,
                          forecast_date = forecast_date,
                          end_date = end_date,
                          smh_or_fch = smh_or_fch,
                          round_num = round_num,
                          subname_all = subname_all,
                          config_subname = config_subname,
                          round_directory = round_directory,
                          full_fit = full_fit_,
                          testing = testing,
                          quick_run = quick_run,
                          outcomes_ = outcomes_,
                          outcomes_time_ = outcomes_time_,
                          outcomes_cum_ = outcomes_cum_,
                          outcomes_cumfromgt = outcomes_cumfromgt,
                          outcomes_calibrate = outcomes_calibrate,
                          n_calib_days = n_calib_days,
                          likelihood_prune = likelihood_prune,
                          keep_variant_compartments = keep_variant_compartments,
                          keep_vacc_compartments = keep_vacc_compartments,
                          keep_all_compartments = keep_all_compartments,
                          variants_ = variants_,
                          vacc_ = vacc_,
                          plot_samp = plot_samp,
                          gt_data = gt_data,
                          geodata_file = geodata_file_path,
                          death_filter = config$outcomes$scenarios,
                          summarize_peaks = (smh_or_fch == "smh"),
                          save_reps = save_reps)
  
  gc()
  
  
  
  
  
  # COMBINE SCENARIO DATA ---------------------------------------------------
  
  print(scenarios_all) ## this is just ONE scenario for fch (code is now specific to ONE RUN - and for fch in particular)
  scenarios_all_modoutput <- "model_output"
  
  data_comb <- combine_and_format_scenarios(
    config = config,    
    round_num = round_num,
    round_directory = round_directory,
    validation_date = validation_date,
    scenarios = scenarios_all_modoutput,
    projection_date = projection_date,
    forecast_date = forecast_date,
    scenario_ids = scenario_ids,
    full_fit = full_fit_,
    forecast_date_name = forecast_date_name)
  
  
  
  # FINAL SUBMISSION CLEANUP ----------------------------------------------
  
  # Remove unwanted targets & dates
  proj_end_date <-config$end_date
  data_comb <- data_comb %>% filter(target_end_date <= proj_end_date | (is.na(target_end_date) & target == "peak size hosp")) %>%
    distinct()
  data_submission <- select_submission_targets(data_comb) 
  
  # Check
  if(smh_or_fch=='smh'){
    data_submission %>% 
      select(scenario_id, scenario_name, target) %>%
      distinct() %>%
      mutate(target = gsub('[[:digit:]]+', '', target)) %>%
      distinct()  %>% print(n=100)
  }else{
    data_submission %>% 
      select(target) %>%
      distinct() %>%
      mutate(target = gsub('[[:digit:]]+', '', target)) %>%
      distinct()  %>% print(n=100)
  }
  
  
  
  # SAVE IT
  readr::write_csv(data_submission, file.path(round_directory, paste0(lubridate::as_date(ifelse(smh_or_fch=='fch', projection_date+1, projection_date)), "-JHU_IDD-CovidSP", ifelse(full_fit_,"_FULL",""), ".csv")))
  arrow::write_parquet(data_submission, file.path(round_directory, paste0(lubridate::as_date(ifelse(smh_or_fch=='fch', projection_date+1, projection_date)), "-JHU_IDD-CovidSP", ifelse(full_fit_,"_FULL",""), ".parquet")))
  arrow::write_parquet(data_comb, file.path(round_directory, paste0(projection_date, "-JHU_IDD-CovidSP", ifelse(full_fit_,"_FULL",""), "_all.parquet")))
  
  print(paste0("Final data saved in:  [  ", file.path(round_directory, paste0(lubridate::as_date(ifelse(smh_or_fch=='fch', projection_date, projection_date)), "-JHU_IDD-CovidSP", ifelse(full_fit_,"_FULL",""), ".csv")), "  ]"))
  
  
  
  
  # ~ Save Replicates Sample ------------------------------------------------
  
  if (!full_fit & smh_or_fch == "smh" & save_reps){
    
    file_names <- file.path(round_directory, paste0(projection_date, "-JHU_IDD-CovidSP-", scenarios_all, "_100reps.parquet"))
    geodata <- read_csv(geodata_file_path)
    
    file_samp <- lapply(file_names, arrow::read_parquet)
    file_samp <- data.table::rbindlist(file_samp) %>% as_tibble() %>%
      left_join(geodata %>% select(location = USPS, geoid) %>% add_row(location="US", geoid="US")) %>%
      select(-location) %>%
      mutate(sample = as.integer(sample),
             location = stringr::str_pad(substr(geoid, 1, 2), width=2, side="right", pad = "0")) %>%
      select(-geoid) %>%
      arrange(scenario_id, target_end_date, target, location, age_group)
    
    file_samp_nums <- file_samp %>%
      select(scenario_id, scenario_name, sample) %>%
      distinct() %>% arrange(scenario_id, sample) %>%
      group_by(scenario_id, scenario_name) %>%
      mutate(sample_new = 1:length(sample)) %>%
      ungroup() %>% as_tibble()
    
    file_samp <- file_samp %>%
      full_join(file_samp_nums) %>%
      mutate(sample = sample_new) %>% select(-sample_new) %>%
      arrange(scenario_id, target_end_date, target, location, age_group, sample)
    
    # Remove unwanted targets
    file_samp <- file_samp %>%
      filter(grepl("inc hosp|inc death|cum hosp|cum death", target)) %>%
      filter(!(grepl("inc death|cum death", target) & location != "US"))
    # filter(grepl("inc case|inc hosp|inc death|cum case|cum hosp|cum death", target))
    
    arrow::write_parquet(file_samp,  file.path(round_directory, paste0(projection_date, "-JHU_IDD-CovidSP", "-sample.parquet")))
  }
  
  
  
  
  
  # PLOTTING - ALL SCENARIOS -----------------------------------------------------------
  
  if(plot_projections){
    
    sim_end_date <- lubridate::as_date(projection_date) + (n_weeks*7)-1
    keep_all_compartments <- keep_variant_compartments <- FALSE
    quant_values <- c(0.025, 0.25, 0.75, 0.975) #c(0.25, 0.75)
    quant_names <- paste0(quant_values*100, "%")
    trunc_date <- lubridate::as_date(ifelse(full_fit, lubridate::as_date(forecast_date), lubridate::as_date(projection_date) - 7*10))
    cum_wk_outcomes_ <- outcomes_[outcomes_time_=="weekly" & outcomes_cum_]
    plot_cum <- ifelse(any(outcomes_cum_),TRUE,FALSE)
    plot_incid <- TRUE
    
    #data input
    data_comb <- arrow::read_parquet(file.path(round_directory, paste0(projection_date, "-JHU_IDD-CovidSP", ifelse(full_fit_,"_FULL",""), "_all.parquet")))
    
    # PDF NAME
    stplot_fname <- file.path(round_directory, paste0(toupper(smh_or_fch), "_all_R",round_num,"_", projection_date, 
                                                      ifelse(full_fit_, "_FULL",""),
                                                      ifelse(is.na(subname), "", subname)))
    
    # Run plotting script
    source(paste0(source_loc, "/R/scripts/postprocess/plot_predictions.R"))
  }
  
  run_process = run_process + 1
}




# FINAL INFO -----------------------------------------------------------

cat(paste0(
  "Pathogen: ", toupper(pathogen), "\n",
  "Save Directory: ", round_directory, "\n",
  "SMH/FCH: ", smh_or_fch, "\n",
  "Forecast Date: ", forecast_date, "\n",
  "Projection Date: ", projection_date, "\n",
  "Scenarios: ", paste(scenarios_all, collapse = ", "), "\n",
  "Sub Name: ", subname, "\n",
  "Full Fit: ", full_fit, "\n",
  "Quick Run: ", quick_run, "\n",
  "Testing Run: ", testing, "\n",
  "Save File: ", paste0(projection_date, "-JHU_IDD-CovidSP-[SCENARIO]", ifelse(full_fit, "_FULL", ""), ".csv"), "\n"))


print('Processing Complete')



# Run diagnostics -------------------------------------


if(run_diagnostics){
  
  
  # Run diagnostics script
  source(paste0(source_loc, "/R/scripts/postprocess/processing_diagnostics_SLURM.R"))
  
}

print('Diagnostics Complete')



# Move files to pplot -------------------------------

# full fit plot 
full_fit_plot <- file.path(round_directory, paste0(toupper(smh_or_fch), "_all_R",round_num,"_", projection_date, 
                                    ifelse(full_fit_, "_FULL",""),
                                    ifelse(is.na(subname), "", subname)))
# FCH csv
submit_csv <- file.path(round_directory, paste0(lubridate::as_date(ifelse(smh_or_fch=='fch', projection_date, projection_date)),
                                  "-JHU_IDD-CovidSP", ifelse(full_fit,"_FULL",""), ".csv"))
# diagnostic
diag_plots <- paste0(round_directory, "/", fch_date, "_", pathogen, "_", smh_or_fch, "_R", round_num, "_", scenarios, "_", ymd(today()), ".pdf")

file.copy(from = c(full_fit_plot, submit_csv, diag_plots),
          to = file.path(data_path, "pplot"), 
          overwrite = TRUE)
