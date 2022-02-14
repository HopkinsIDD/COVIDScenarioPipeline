# Setup to test inference on synthetic data
# Preamble ---------------------------------------------------------------------

library(tidyverse)
library(xts)
library(data.table)
library(truncnorm)
library(foreach)
library(covidcommon)
library(hospitalization)
library(inference)
library(reticulate)

option_list = list(
  optparse::make_option(c("-t", "--test"), action="store", default=2, type='integer', help="Test id to compile"),
  optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
  optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
  optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable"),
  optparse::make_option(c("-n", "--n_slots"), action="store", default=50, type = 'integer', help = "Number of slots to run"),
  optparse::make_option(c("-k", "--n_iter"), action="store", default=250, type = 'integer', help = "Number of iterations per slot"),
  optparse::make_option(c("-j", "--n_cores"), action="store", default=parallel::detectCores() - 2, type = 'integer', help = "Number of cores to use")
)

parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(parser)

source("COVIDScenarioPipeline/R/pkgs/inference/R/InferenceTest.R")

# File names
config_file <- "configs/config_test_inference.yml"
setup <- "testInference"
data_basepath <- "data/testInference"

# Load config file for the single epi test
config <- covidcommon::load_config(config_file)

# Configuration parameters
start_date <- "2020-01-01"
end_date <- "2020-10-01"

set.seed(123)

# Functions --------------------------------------------------------------------
##'
##' Function that generates the NPI configurations for simulating and testing
##'
##' @param geoids the geoids it affects
##' @param dist_params the parameters of the distribution
##' @param pert_params the parameters of the perturbation distribution
##' @param period_bounds the dates over which the npi has effect
##' 
##' @return inference run results on this particular area
##'
#' @export
generateNPIConfig <- function(geoids, 
                              dist_params, 
                              pert_params,
                              period_bounds) {
  res <- list()
  res$template <- "ReduceR0"
  if (geoids != "all") {
    res$affected_geoids <- as.list(as.character(geoids))
  }
  res$period_start_date <- period_bounds[1]
  res$period_end_date <- period_bounds[2]
  res$value <- setDistParams(dist_params)
  res$perturbation <- setDistParams(pert_params)
  return(res)
}

setDistParams <- function(dist_params) {
  dist <- dist_params$distribution
  
  if (dist == "uniform") {
    list(distribution = dist,
         low = dist_params$low,
         high = dist_params$high)
  } else if (dist == "truncnorm") {
    list(distribution = dist, 
         mean = dist_params$mean,
         sd = dist_params$sd,
         a = dist_params$a,
         b = dist_params$b)
  } else if (dist == "fixed") {
    list(distribution = dist, 
         value = dist_params$value)
  }
}

#' @title Set mobility
#' @description Sets the mobility matrix either with high or medium connectivity
#'
#' @param test the test to set
#'
#' @details Only two scenarios considered: high (10% of the population moves) or med (1% of the population)
#' @return a matrix
setMobility <- function(test) {
  N <- test$nnodes
  pop <- test$pop
  
  if (test$mobility == "high") {
    mob <-  matrix(rep(pop/1e1, N), ncol = N)
  } else {
    mob <-  matrix(rep(pop/1e2, N), ncol = N)
  }
  
  mob_sparse <- map_df(1:N, ~tibble(ori = ., dest = 1:N, amount = mob[., ])) %>% 
    filter(ori != dest) %>% 
    mutate(ori = stringr::str_pad(ori, 5, pad = "0"),
           dest = stringr::str_pad(dest, 5, pad = "0"))
  
  return(mob_sparse)
}

#' @title Build test
#' @description Creates the test attributes based on vector of specifications
#'
#' @param param_vec
#'
#' @return a list with test specifications
buildTest <- function(param_vec, suffix = NULL) {
  test <- list(
    nnodes = as.integer(param_vec[["N"]]),
    pop = rep(1e5, param_vec[["N"]]),
    mobility = param_vec[["mob"]],
    fit_confirmation = T,
    confirmation_transform = param_vec[["conf_transform"]],
    pert_sd_npis = as.numeric(param_vec[["pert_sd_npis"]]),
    pert_bound_npis = as.numeric(param_vec[["pert_bound_npis"]]),
    pert_sd_conf = as.numeric(param_vec[["pert_sd_conf"]]),
    pert_bound_conf = as.numeric(param_vec[["pert_bound_conf"]]),
    lik_cases = param_vec[["lik_cases"]],
    lik_deaths = param_vec[["lik_deaths"]],
    scenario = param_vec[["scenario"]],
    setup = param_vec[["setup"]]
  )
  
  # test$runid <- glue::glue("N{test$nnodes}_npis-sd{test$pert_sd_npis}-b{test$pert_bound_npis}_conf-sd{test$pert_sd_conf}-b{test$pert_bound_conf}-t{test$confirmation_transform}") %>% 
  #   str_replace_all("\\.", "")
  # 
  test$runid <- glue::glue("N{test$nnodes}_npis-sd{test$pert_sd_npis}_conf-sd{test$pert_sd_conf}_lc-{test$lik_cases}_ld-{test$lik_deaths}") %>% 
    str_replace_all("\\.", "")
  
  if(!is.null(suffix)) {
    test$runid <- str_c(test$runid ,"_", suffix)
  }
  # if(!is.null(suffix)) {
  #   test$runid <- glue::glue("N{test$nnodes}_npis-sd{test$pert_sd_npis}-b{test$pert_bound_npis}_conf-sd{test$pert_sd_conf}-b{test$pert_bound_conf}-t{test$confirmation_transform}_{suffix}") %>% 
  #     str_replace_all("\\.", "")
  # }
  
  test$config_file <- glue::glue("configs/config_{test$setup}_{test$runid}_inference.yml")
  test$seeding_file <- glue::glue("{test$setup}_{test$runid}_seeding.csv")
  test$geodata_file <- glue::glue("{test$setup}_{test$runid}_geodata.csv")
  test$mobility_file <- glue::glue("{test$setup}_{test$runid}_mobility.csv")
  test$hpar_file <- glue::glue("data/generated/{test$setup}_{test$runid}_hospitalization_testInference.hpar.parquet")
  test$data_path <- glue::glue("data/generated/{test$setup}_{test$runid}_test_data.csv")
  
  return(test) 
}

# Build tests ------------------------------------------------------------------

# Test specifications
test_specs <- expand.grid(
  # Standard deviation of perturbation kernel
  pert_sd_npis = c(.1),
  # Bounds on truncated normal of perturbation kernel
  pert_bound_npis = c(1),
  # Standard deviation of perturbation kernel
  pert_sd_conf = c(.1),
  # Bounds on truncated normal of perturbation kernel
  pert_bound_conf = c(1),
  # Transformation on the confirmation rate
  conf_transform = c("none"),
  # Number of nodes
  N = c(5),
  lik_cases = c("sqrtnorm-0.01", "sqrtnorm-0.05", "pois", "sqrtnorm-0.1"),
  lik_deaths = c("sqrtnorm-0.01", "sqrtnorm-0.05", "pois", "sqrtnorm-0.1")
) %>% 
  # set whether the run is the reference for fitting
  group_by(N) %>% 
  mutate(is_ref = row_number() == 1,
         setup = setup,
         scenario = "test",
         mob = "med") %>%
  ungroup()

# Keep only reasonable combiations for confirmation rate perturbations
test_specs <- test_specs %>% 
  filter(
    (conf_transform == "none" & pert_sd_conf %in% c(.01, .1)  & pert_bound_conf %in% c(.1, 1)) |
      (conf_transform == "logit" & pert_sd_conf %in% c(.1, .5)  & pert_bound_conf %in% c(1, 5))
  )


# Build tests specifications
tests <- apply(test_specs, 1, buildTest)
names(tests) <- map_chr(tests, "runid")

# Set reference data to use
for (i in unique(test_specs[["N"]])) {
  # Get test for corresponding number of nodes
  inds <- which(map_dbl(tests, "nnodes") == i)
  ind_ref <- inds[which(test_specs$is_ref[inds])]
  for (ind in inds) {
    tests[[ind]]$ref_data_runid <- tests[[ind_ref]]$runid
  }
}

# Write
yaml::write_yaml(tests, "all_tests.yml")

# Setup ------------------------------------------------------------------------

if (!dir.exists("configs")) 
  dir.create("configs")

if (!dir.exists("data/generated")) 
  dir.create("data/generated")

# Select first test, a loop could be made here if multiple tests are to be run
for (test in tests) {
  
  # Specify configuration files 
  config_file_out_generation <- glue::glue("configs/config_{test$setup}_{test$runid}_generation.yml")
  config_file_out_inference <- glue::glue("configs/config_{test$setup}_{test$runid}_inference.yml")
  config <- yaml::read_yaml(config_file)
  
  ## Date setup - - - -
  config$start_date <- start_date
  config$end_date <- end_date
  
  ## Spatial setup - - - -
  if(!dir.exists(data_basepath)) {
    dir.create(data_basepath)
  }
  
  config$spatial_setup <- list(
    setup_name = test$setup,
    nodenames = "geoid",
    base_path = data_basepath,
    geodata = test$geodata_file,
    mobility = test$mobility_file,
    popnodes = "pop"
  )
  
  geodata <- tibble(
    geoid =  1:test$nnodes,
    pop = rep(1e5, test$nnodes)  # initial number of susceptibles in synthetic population
  ) %>% 
    mutate(geoid = stringr::str_pad(geoid, 5, pad = "0"))
  
  write_csv(geodata, paste(data_basepath, test$geodata_file, sep = "/"))
  
  ## Seeding - - - -
  config$seeding <- list(
    method = "FolderDraw",
    seeding_file_type = "seed",
    folder_path =  data_basepath,
    lambda_file = paste(data_basepath, test$seeding_file, sep = "/"),
    perturbation_sd = 1)
  
  # Initial seeding used for synthetic data
  seed_dates <- seq.Date(as.Date("2020-01-10"), as.Date("2020-01-31"), by = "1 days")
  seedings <- tibble(date = sample(seed_dates, test$nnodes, replace = F),
                     place = 1:test$nnodes,
                     amount = rpois(test$nnodes, 6)) %>% 
    mutate(place = stringr::str_pad(place, 5, pad = "0"))
  
  write_csv(seedings, path = paste(data_basepath, test$seeding_file, sep = "/"))
  
  ## Mobility matrix - - - -
  mob_sparse <- setMobility(test)
  write_csv(mob_sparse, path = paste(data_basepath, test$mobility_file, sep = "/"))
  
  ## NPIs setup - - - -
  offsets <- as.Date("2020-03-01") + round(runif(test$nnodes) * 10) # Offsets for the start of the implementation of NPIS between locations
  baseline_R0s <- runif(test$nnodes, min = -.2, max = .2) # baseline R0 multiplier values 
  npis_multis <- runif(test$nnodes, min = .2, max = .7)  # multipliers on the intervention R0s between locations
  
  ## Hpar setup
  hpars <- names(config$outcomes$settings$med) %>% 
    map_df(function(y) {
      x <- config$outcomes$settings$med[[y]]
      onames <- names(x)[names(x) != "source"]
      vals <- map_df(onames, function(o) tibble(quantity = o, value = x[[o]]$value$value))
      vals %>% mutate(outcome = y, source = x$source)
    })
  
  hpars <- map_df(unique(geodata$geoid), ~mutate(hpars, geoid = .)) %>% 
    select(geoid, quantity, outcome, source, value)
  
  hpar_generation_file <- str_replace(test$hpar_file, ".parquet", "-generation.parquet")
  hpar_inference_file <- test$hpar_file
  
  filter(hpars, quantity == "probability") %>% 
    mutate(geoid = as.character(geoid)) %>% 
    arrow::write_parquet(hpar_generation_file)
  
  # Initialize confirmation probability at 60% (20% used for generating the data)
  filter(hpars, quantity == "probability") %>% 
    mutate(value = case_when(outcome == "incidC" ~ .6, T ~ value)) %>% 
    mutate(geoid = as.character(geoid)) %>% 
    arrow::write_parquet(hpar_inference_file)
  
  # Perturbation parameters are the same for all NPIs and hpars
  pert_params_npis <- list(
    distribution = "truncnorm",
    mean = 0,
    sd = test$pert_sd_npis,
    a = -1 * test$pert_bound_npis,
    b = test$pert_bound_npis
  )
  
  # Generation setup with fixed values
  generation_config <- map(
    1:test$nnodes, 
    function(x) {
      list(generateNPIConfig(geoids = stringr::str_pad(x, 5, pad = "0"), 
                             dist_params = list(distribution = "fixed", value = baseline_R0s[x]), 
                             pert_params = pert_params_npis, 
                             period_bounds = c(start_date, end_date)),
           generateNPIConfig(geoids = stringr::str_pad(x, 5, pad = "0"), 
                             dist_params = list(distribution = "fixed", value = npis_multis[x]), 
                             pert_params = pert_params_npis, 
                             period_bounds = c(as.character(offsets[x]), end_date))
      ) %>% 
        setNames(paste0(c("local_variation", "npis"), x))
    }
  ) %>% 
    unlist(recursive = F)
  
  generation_config <- append(generation_config, 
                              list(test = list(template = "Stacked",
                                               scenarios = names(generation_config))))
  
  config$interventions$scenarios <- list("test")
  config$interventions$settings <- generation_config
  config$outcomes$param_place_file <- hpar_generation_file
  yaml::write_yaml(config, file = config_file_out_generation)
  
  # Inference setup
  dist_params_var <- list(distribution = "truncnorm", mean = 0, sd = .1, a = -1, b = 1)
  dist_params_npi <- list(distribution = "truncnorm", mean = .3, sd = .1, a = 0, b = 1)
  
  inference_config <- map(
    1:test$nnodes, 
    function(x) {
      list(generateNPIConfig(geoids = stringr::str_pad(x, 5, pad = "0"), 
                             dist_params = dist_params_var, 
                             pert_params = pert_params_npis, 
                             period_bounds = c(start_date, end_date)),
           generateNPIConfig(geoids = stringr::str_pad(x, 5, pad = "0"), 
                             dist_params = dist_params_npi, 
                             pert_params = pert_params_npis, 
                             period_bounds = c(as.character(offsets[x]), end_date))
      ) %>% 
        setNames(paste0(c("local_variation", "npis"), x))
    }
  ) %>% 
    unlist(recursive = F)
  
  inference_config <- append(inference_config, 
                             list(test = list(template = "Stacked",
                                              scenarios = names(inference_config))))
  
  config$interventions$scenarios <- list("test")
  config$interventions$settings <- inference_config
  config$filtering$data_path <- test$data_path
  config$filtering$statistics$sum_cases$likelihood <- list(dist = "sqrtnorm",
                                                           param = .1)
  
  if (test$fit_confirmation) {
    pert_params_conf <- list(
      distribution = "truncnorm",
      mean = 0,
      sd = test$pert_sd_conf,
      a = -1 * test$pert_bound_conf,
      b = test$pert_bound_conf
    )
    
    if (test$confirmation_transform != "none")
      pert_params_conf$transform <- test$confirmation_transform
    
    incidC_params <- list(
      value = list(
        distribution = "truncnorm",
        mean = 0.6,
        sd = 0.1,
        a = 0,
        b = 1),
      perturbation = pert_params_conf
    )
    
    config$outcomes$settings$med$incidC$probability <- incidC_params
    
    lik <- str_split(test$lik_cases, "-")[[1]]
    config$filtering$statistics$sum_cases$likelihood <- list(dist = lik[1],
                                                             param = as.numeric(lik[2]))
  }
  
  lik_deaths <- str_split(test$lik_deaths, "-")[[1]]
  config$filtering$statistics$sum_deaths$likelihood <- list(dist = lik_deaths[1],
                                                            param = as.numeric(lik_deaths[2]))
  config$outcomes$param_place_file <- hpar_inference_file
  yaml::write_yaml(config, file = config_file_out_inference)
  
  # Generate data ----------------------------------------------------------------
  chimeric_block_prefix <- glue::glue("generate_{test$setup}_")
  global_block_prefix <- chimeric_block_prefix
  first_spar_file <- covidcommon::create_file_name(test$runid,chimeric_block_prefix,0,'spar','parquet')
  first_snpi_file <- covidcommon::create_file_name(test$runid,chimeric_block_prefix,0,'snpi','parquet')
  first_hosp_file <- covidcommon::create_file_name(test$runid,global_block_prefix,0,'hosp','parquet')
  first_hpar_file <- covidcommon::create_file_name(test$runid,chimeric_block_prefix,0,'hpar','parquet')
  first_seed_file <- covidcommon::create_file_name(test$runid,chimeric_block_prefix,0,'seed','csv')
  write_csv(seedings, path = first_seed_file)
  
  # Write seeding lambda file for all slots to avoid calling create_seeding.R
  
  file.remove(first_hpar_file)
  file.copy(config$outcomes$param_place_file, first_hpar_file)

  reticulate::use_python(Sys.which(opt$python),require=TRUE)
  gempyor <- reticulate::import("gempyor")
  gempyor_inference_runner <- gempyor$InferenceSimulator(
                                                config_path=config_file_out_generation,
                                                run_id=test$runid,
                                                prefix=global_block_prefix,
                                                first_sim_index=1,
                                                scenario="test",
                                                deathrate="med",
                                                stoch_traj_flag=1,
                                                initialize=TRUE  # Shall we pre-compute now things that are not pertubed by inference
)

gempyor_inference_runner$one_simulation(0)
gempyor_inference_runner$one_simulation_loadID(sim_id2write=0, sim_id2load=0)

  for (i in 1:opt$n_slots) {
    slot_prefix <- covidcommon::create_prefix(config$name, test$scenario, "med",test$runid,sep='/',trailing_separator='/')
    ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/')
    gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/')
    global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(i,"%09d"), sep='.', trailing_separator='.')
    chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(i,"%09d"), sep='.', trailing_separator='.')
    first_global_files <- inference::create_filename_list(test$runid, global_block_prefix, 0)
    first_chimeric_files <- inference::create_filename_list(test$runid, chimeric_block_prefix, 0)
    
    initial_seeding <- readr::read_csv(config$seeding$lambda_file, 
                                       col_types = readr::cols(place=readr::col_character()))
    readr::write_csv(initial_seeding, path = first_global_files[['seed_filename']])
  }
}

for (test in tests) {
  chimeric_block_prefix <- glue::glue("generate_{test$setup}_")
  global_block_prefix <- chimeric_block_prefix
  first_hosp_file <- covidcommon::create_file_name(test$ref_data_runid,global_block_prefix,0,'hosp','parquet')
  first_snpi_file <- covidcommon::create_file_name(test$ref_data_runid,chimeric_block_prefix,0,'snpi','parquet')
  first_hpar_file <- covidcommon::create_file_name(test$ref_data_runid,chimeric_block_prefix,0,'hpar','parquet')
  
  arrow::read_parquet(first_snpi_file) %>% 
    write_csv(path = glue::glue("data/generated/generate_testInference_{test$runid}_snpi.csv"))
  
  arrow::read_parquet(hpar_generation_file) %>% 
    write_csv(path = glue::glue("data/generated/generate_testInference_{test$runid}_hpar.csv"))
  
  # Write data
  arrow::read_parquet(first_hosp_file) %>% 
    mutate(geoid = stringr::str_pad(geoid, 5, pad = 0),
           FIPS = geoid,
           date = as.Date(time)) %>% 
    select(-time, -geoid) %>% 
    write_csv(path = test$data_path)
}

# Plot datasets ----------------------------------------------------------------
do_plots <- F

if (do_plots) {
  library(foreach)
  
  # Data
  data <- map_df(tests, function(x) read_csv(x$data_path) %>% mutate(runid = x$runid, N = x$nnodes))
  # data <- map_df(tests, function(x) arrow::read_parquet(covidcommon::create_file_name(x$runid, glue::glue("generate_{x$setup}_"),0,'hosp','parquet')) %>% mutate(runid = x$runid, N = x$nnodes))
  
  ggplot(data, aes(x = date, y = incidC, color = runid)) +
    geom_line() +
    facet_grid(N ~ FIPS, scales = "free")
  
  # Seedings
  seedings <- map_df(tests, function(x) read_csv(str_c(data_basepath,"/", x$seeding_file)) %>% mutate(runid = x$runid, N = x$nnodes))
  
  ggplot(seedings, aes(x = date, y = amount, fill = runid)) +
    geom_bar(stat = "identity") +
    facet_grid(N ~ place)
}

# Write run_tests.sh -----------------------------------------------------------
run_file <- "run_tests.sh"

write("#!/bin/bash", file = run_file)

for (i in 1:length(tests)) {
  write(
    glue::glue("Rscript COVIDScenarioPipeline/R/scripts/full_filter.R -c '{tests[[i]]$config_file}'  -u '{tests[[i]]$runid}' -n {opt$n_slots} -k {opt$n_iter} -j {opt$n_cores}"), 
    run_file,
    append = T
  )
}


for (i in 1:length(tests)) {
  write(
    glue::glue("Rscript COVIDScenarioPipeline/R/scripts/postprocess_inference.R -t {i} -i TRUE -r FALSE -j {opt$n_cores}"), 
    run_file,
    append = T
  )
}

