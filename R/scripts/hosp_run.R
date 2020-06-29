##
# @file
# @brief Runs hospitalization model
#
# @details
#
# ## Configuration Items
# 
# ```yaml
# name
#
# spatial_setup:
#   base_path: <path to directory>
#   geodata: <path to file>
#   nodenames: <string>
#
# interventions:
#   scenarios:
#     - <scenario 1 name>
#     - <scenario 2 name>
#     - ...
#
# hospitalization:
#   paths:
#     run_age_adjust: <logical, optional>
#   parameters:
#     time_hosp: <list of floats>
#     time_disch: <list of floats>
#     time_ICU: <list of floats>
#     time_ICUdur: <list of floats>
#     time_vent: <list of floats>
#     p_death: <list of probabilities>
#     p_death_names: <list of strings> # same length as p_death
# ```
#
# If `run_age_adjust` is TRUE,
# ```yaml
# hospitalization:
#   parameters:
#     time_onset_death: <list of floats>
#     p_hosp_inf: <list of probabilties> # same length as p_death
#     time_ventdur: <list of floats>
# ```
#
# If `run_age_adjust` is not TRUE or absent,
# ```yaml
# hospitalization:
#   parameters:
#     p_death_rate: <probability>
#     p_ICU: <probability>
#     p_vent: <probability>
#     time_hosp_death: <list of floats>
#     time_ventdur: <list of floats, optional, default is time_ICUdur>
# ```
#
# ## Input Data
#
# * <b>model\_output/{name}\_[scenario]</b> is a directory of csv's. 
#    + Each csv must have columns: "time", "comp", and each geoid of interest.
#    + The "comp" column must have a value of "diffI" in at least one row. 
# * <b>{spatial_setup::base_path}/{spatial_setup::geodata}</b> is a csv with columns {spatial_setup::nodenames} and {spatial_setup::popnodes}
#
# ## Output Data
#
# * <b>hospitalization/model\_output/{name}\_[scenario]/[deathrate]\_death-[simulation ID].hosp.[csv/parquet]</b> Created for each csv in input data model\_output/{name}\_[scenario]. The columns in the output csv's are:
#    + time
#    + uid
#    + geoid
#    + sim_num
#    + comp
#    + incidI
#    + hosp_curr
#    + icu_curr
#    + vent_curr
#    + incidH
#    + incidICU
#    + incidVent
#    + incidD
#    + date_inds
#    + geo_ind

## @cond

library(devtools)
library(covidcommon)
library(hospitalization)
library(report.generation)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)
library(parallel)
library(stringr)

option_list = list(

  #' @param -c The location of the config file
  optparse::make_option(c("-c", "--config"), action = "store", default = Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type = 'character', help = "path to the config file"),

  #' @param -d The death rate
  optparse::make_option(c("-d", "--deathrate"), action = "store", default = Sys.getenv("COVID_DEATHRATES", 'all'), type = 'character', help = "name of the death scenario to run, or 'all' to run all of them"),

  #' @param -j The number of tasks to run in parallel
  optparse::make_option(c("-j", "--jobs"), action = "store", default = Sys.getenv("COVID_NJOBS", detectCores()), type = 'numeric', help = "number of cores used"),

  #' @param -p The path to COVIDScenarioPipeline
  optparse::make_option(c("-p", "--path"), action = "store", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline"), type = 'character', help = "path to the COVIDScenarioPipeline directory"),

  #' @param -i The index of the first simulation to run against
  optparse::make_option(c("-i", "--index-from-sim"), action = "store", default = Sys.getenv("COVID_INDEX", 1), type = 'numeric', help = "The index of the first simulation to run against"),

  #' @param -n The number of simulations to run
  optparse::make_option(c("-n", "--num-sims"), action = "store", default = Sys.getenv("COVID_NSIMULATIONS", -1), type = 'numeric', help = "number of simulations to run"),

  optparse::make_option(c("-g", "--geoid.params.file"), action = "store", default = Sys.getenv("COVID_GEOID_PARAMS",""), type = 'character', help = "number of simulations to run"),
  optparse::make_option(c("--in-prefix"), action = "store", default = Sys.getenv("COVID_PREFIX",""), type = 'character', help = "text to use as part of filenames"),
  optparse::make_option(c("--in-id"), action = "store", default = Sys.getenv("COVID_RUN_ID",covidcommon::run_id()), type = 'character', help = "unique identifier for the run"),
  optparse::make_option(c("--out-prefix"), action = "store", default = Sys.getenv("COVID_PREFIX",""), type = 'character', help = "text to use as part of filenames"),
  optparse::make_option(c("--out-id"), action = "store", default = Sys.getenv("COVID_RUN_ID",covidcommon::run_id()), type = 'character', help = "unique identifier for the run")
)

opt = optparse::parse_args(optparse::OptionParser(option_list = option_list))

#' @description Run the hospitalization results.
#' @importFrom covidcommon load_config
config <- covidcommon::load_config(opt[['config']])
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

# toggle for running legacy script vs age adjusted script
run_age_adjust <- config$hospitalization$paths$run_age_adjust
if(is.null(run_age_adjust)){
  warning("Not specified whether to run age adjusted hospitalization script.
          Defaults to running legacy script")
  run_age_adjust <- FALSE
} else {
  geoid_params_file <- opt[['geoid.params.file']]
  if(geoid_params_file == ''){
    geoid_params_file <- config$hospitalization$paths$geoid_params_file
  }
  if(length(geoid_params_file) == 0 ){
    geoid_params_file <- paste(opt[['path']],"sample_data","geoid-params.csv",sep = '/')
  }
  print(paste("param file is ",geoid_params_file))
}

print(names(opt))
in_run_id <- opt[['in-id']]
in_prefix <- opt[['in-prefix']]
out_run_id <- opt[['out-id']]
out_prefix <- opt[['out-prefix']]

hosp_parameters = config$hospitalization$parameters

# set parameters for time to hospitalization, time to death, time to discharge
time_hosp_pars <- as_evaled_expression(hosp_parameters$time_hosp)
time_disch_pars <- as_evaled_expression(hosp_parameters$time_disch)
time_ICU_pars <- as_evaled_expression(hosp_parameters$time_ICU)
time_ICUdur_pars <- as_evaled_expression(hosp_parameters$time_ICUdur)
time_vent_pars <- as_evaled_expression(hosp_parameters$time_vent)

# set death rates
p_death <- as_evaled_expression(hosp_parameters$p_death)
names(p_death) = hosp_parameters$p_death_names

cmd <- opt[['deathrate']]
ncore <- opt[['jobs']]
start_sim <- opt[['index-from-sim']]
num_sims <- opt[['num-sims']]

# Verify that the cmd maps to a known p_death value
if (cmd == "all") {
  cmd <- names(p_death) # Run all of the configured hospitalization scenarios
} else if (is.na(p_death[cmd]) || is.null(p_death[cmd]) || p_death[cmd] == 0) {
  message(paste("Invalid cmd argument:", cmd, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
  quit("yes", status = 1)
}

## Running age-adjusted script
if(run_age_adjust){

  # read in probability file
  # NOTE(jwills): this file would ideally live inside of the hospitalization package as an .Rdata object
  prob_dat <- list()
  if(gsub('.*[.]','',geoid_params_file) == 'csv'){
    prob_dat <- readr::read_csv(geoid_params_file)
  } else if(gsub('.*[.]','',geoid_params_file) == 'parquet'){
    prob_dat <- arrow::read_parquet(geoid_params_file)
  } else {
    stop(paste("Unknown file extension",gsub('.*[.]','',geoid_params_file)))
  }

  # Check that all geoids are in geoid-params.csv
  geodata <- report.generation:::load_geodata_file(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),5,'0',TRUE)
  in_geoids <- geodata[[config$spatial_setup$nodenames]]
  missing_geoids <- setdiff(in_geoids, prob_dat$geoid)
  if(length(missing_geoids) > 0) {
    warning(paste("The hospitalization outcomes will not be calculated for geoids not present in geoid-params.csv:", missing_geoids))
  }

  time_onset_death_pars <- as_evaled_expression(hosp_parameters$time_onset_death)
  p_hosp_inf <- as_evaled_expression(hosp_parameters$p_hosp_inf)
  time_ventdur_pars <- as_evaled_expression(hosp_parameters$time_ventdur)
  names(p_hosp_inf) = hosp_parameters$p_death_names
  if (length(p_death) != length(p_hosp_inf)) {
    stop("Number of IFR and p_hosp_inf values do not match")
  }

  for (cmd0 in cmd) {
    cat(paste("Running hospitalization scenario: ", cmd0, "with IFR", p_death[cmd0], "\n"))
    res_npi3 <- build_hospdeath_geoid_fixedIFR_par(
      prob_dat = prob_dat,
      p_death = p_death[cmd0],
      p_hosp_inf = p_hosp_inf[cmd0],
      time_hosp_pars = time_hosp_pars,
      time_onset_death_pars = time_onset_death_pars,
      time_disch_pars = time_disch_pars,
      time_ICU_pars = time_ICU_pars,
      time_vent_pars = time_vent_pars,
      time_ventdur_pars = time_ventdur_pars,
      time_ICUdur_pars = time_ICUdur_pars,
      cores = ncore,
      dscenario_name = paste(cmd,"death",sep="_"),
      use_parquet = TRUE,
      start_sim = start_sim,
      num_sims = num_sims,
      in_run_id = in_run_id,
      in_prefix = in_prefix,
      out_run_id = out_run_id,
      out_prefix = out_prefix
    )
  }
} else {

  p_death_rate <- as_evaled_expression(hosp_parameters$p_death_rate)
  p_ICU <- as_evaled_expression(hosp_parameters$p_ICU)
  p_vent <- as_evaled_expression(hosp_parameters$p_vent)

  if(is.null(hosp_parameters$time_hosp_death)){
    warning("Please specify time_hosp_death instead of time_death")
    time_hosp_death_pars <- as_evaled_expression(hosp_parameters$time_death)
  }else{
  time_hosp_death_pars <- as_evaled_expression(hosp_parameters$time_hosp_death)
  }

  if(is.null(hosp_parameters$time_ventdur)){
    warning("No ventilation duration specified. Using ICU duration as a default")
    time_ventdur_pars <- as_evaled_expression(hosp_parameters$time_ICUdur)
  }else{
    time_ventdur_pars <- as_evaled_expression(hosp_parameters$time_ventdur)
  }
  
  for (cmd0 in cmd) {
    p_hosp <- p_death[cmd0]/p_death_rate
    cat(paste("Running hospitalization scenario: ", cmd0, "with p_hosp", p_hosp, "\n"))
    res_npi3 <- build_hospdeath_par(
      p_hosp = p_hosp,
      p_death = p_death_rate,
      p_vent = p_vent,
      p_ICU = p_ICU,
      time_hosp_pars = time_hosp_pars,
      time_hosp_death_pars = time_hosp_death_pars,
      time_disch_pars = time_disch_pars,
      time_ICU_pars = time_ICU_pars,
      time_vent_pars = time_vent_pars,
      time_ICUdur_pars = time_ICUdur_pars,
      time_ventdur_pars = time_ventdur_pars,
      cores = ncore,
      dscenario_name = cmd0,
      use_parquet = TRUE,
      start_sim = start_sim,
      num_sims = num_sims,
      in_run_id = in_run_id,
      in_prefix = in_prefix,
      out_run_id = out_run_id,
      out_prefix = out_prefix
    )
  }
}

## @endcond

