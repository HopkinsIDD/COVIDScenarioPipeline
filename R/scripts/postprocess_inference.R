# Preamble ---------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(foreach)
library(doParallel)
library(itertools)

option_list = list(
  optparse::make_option(c("-c","--config"), action="store", type='character', help="config file", default = "config.yml"),
  optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
  optparse::make_option(c("-s","--scenario"), action="store", type='character', help="Which scenario to postprocess", default = "inf"),
  optparse::make_option(c("-i", "--do_int"), action = "store", default = FALSE, type='logical', help = "Postprocess intermediate trajectories"),
  optparse::make_option(c("-r", "--redo"), action = "store", default = FALSE, type='logical', help = "Postprocess intermediate trajectories"),
  optparse::make_option(c("-j", "--jobs"), action = "store", default = parallel::detectCores(), type = 'integer', help = "Number of jobs to run in parallel")
)

parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(parser)
#opt$run_id = "final_inf"
#opt$scenario = "sau_inf"
do_int <- opt$do_int
redo <- opt$redo
runid <- opt$run_id
scenario <- opt$scenario
config <- covidcommon::load_config(opt$config)
setup <- config$spatial_setup$setup_name
outdir <- glue::glue("notebooks/{setup}/{scenario}/{runid}") 

if (!dir.exists(outdir))
  dir.create(outdir, recursive = T)

if (!(scenario %in% config$interventions$scenarios))
  stop("Scenario ", scenario, " not among specified scenarios: ", str_c(config$interventions$scenarios, collapse = ","))

# Load geodata
suppressMessages(geodata <- report.generation::load_geodata_file(
  paste(
    config$spatial_setup$base_path,
    config$spatial_setup$geodata, sep = "/"
  ),
  geoid_len = ifelse(is.null(config$spatial_setup$geoid_len), 5, config$spatial_setup$geoid_len) # added a default and option to specify
))
obs_nodename <- config$spatial_setup$nodenames
us_model <- config$spatial_setup$us_model==TRUE || is.null(config$spatial_setup$us_model) # check if it's a US model or non-US model


# Functions --------------------------------------------------------------------
getPosteriors <- function(param,
                          setup,
                          scenario,
                          runid, 
                          outdir,
                          file_type = "parquet",
                          ingest_proc_fun = NULL,
                          postproc_fun = NULL,
                          return_res = F,
                          redo = F) {
  
  outfile <- glue::glue("{outdir}/{param}_{setup}_{scenario}_{runid}.csv")
  
  if (!file.exists(outfile) | redo) {
    # Get final sample files for the parameter type
    final_param_files <- dir(glue::glue("model_output/{param}/{setup}/{scenario}/med/{runid}/"), 
                             full.names = T, 
                             recursive = T,
                             pattern = file_type) %>% 
      .[str_detect(., "/final/")] 
    
    if (length(final_param_files) == 0)
      stop("Couldn't find result files for ", param)
    
    if(file_type == "parquet") {
      read_fun <- arrow::read_parquet
    } else if (file_type == "csv") {
      read_fun <- function(x) readr::read_csv(x, col_types = cols())
    } else {
      stop("Unrecognized file type ", file_type)
    }
    
    if (is.null(ingest_proc_fun)) {
      ingest_proc_fun <- function(x) x
    }
    
    # Read files
    params <-  map_df(final_param_files, 
                      function(x) {
                        read_fun(x) %>% 
                          ingest_proc_fun() %>% 
                          mutate(sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)"))) %>% 
                          { 
                            if(param == "seed") {
                              mutate(., place = as.character(place))
                            } else {
                              mutate(., geoid = as.character(geoid))
                            }
                            
                          }
                      }) 
    
    # Postprocess if required
    if (is.null(postproc_fun)) {
      postproc_fun <- function(x) x
    }
    param_proc <- postproc_fun(params)
    
    # Write result
    write_csv(param_proc, path = outfile)
  }
  
  if (return_res) {
    res <- read_csv(outfile, col_types = cols())
    return(res)
  }
}


# Simulations -----------------------------------------------------------------
sim_stats <- getPosteriors(param = "hosp", 
                           scenario = scenario,
                           setup = setup, 
                           runid = runid, 
                           outdir = outdir, 
                            ingest_proc_fun = function(x) {
                             inference::compute_totals(x) %>% 
                               inference::compute_cumulative_counts()
                           }, 
                           postproc_fun = function(x) {
                             long_x <- x %>% 
                               gather(var, value, -time, -geoid, -sim) 
                             
                             stats <- long_x %>% 
                               group_by(time, geoid, var) %>% 
                               summarise(q025 = quantile(value, .025),
                                         q25 = quantile(value, .25),
                                         q75 = quantile(value, .75),
                                         q975 = quantile(value, .975))
                             
                             peaks <- long_x %>% 
                               group_by(geoid, var, sim) %>% 
                               mutate(value = cummax(value)) %>% ungroup() %>%
			       filter(!str_detect(var, "cum")) %>% 
                               group_by(time, geoid, var) %>% 
                               summarise(q025 = quantile(value, .025),
                                         q25 = quantile(value, .25),
                                         q75 = quantile(value, .75),
                                         q975 = quantile(value, .975)) %>% 
                                 mutate(var = str_c(var, "_peak"))
                               
                               bind_rows(stats, peaks)
                           }, 
                           return_res = T,
                           redo = opt$redo)

# Get simulated compartments that were used for fitting
fitted_compartments <- map_chr(config$filtering$statistics, "sim_var")
var_dict <- map_chr(config$filtering$statistics, "data_var") %>% set_names(fitted_compartments)

cat("---- DONE SIMULATIONS \n")

# Plot NPIs --------------------------------------------------------------------

npis <- getPosteriors(param = "snpi", 
                      scenario = scenario,
                      setup = setup, 
                      runid = runid, 
                      outdir = outdir, 
                      return_res = T,
                      redo = opt$redo)

# Generate priors
npi_priors <- pmap_df(list(x = config$interventions$settings, 
                           y = names(config$interventions$settings)),
                      function(x, y) {
                        if (x$template == "ReduceR0") {
                          pert_dist <- covidcommon::as_random_distribution(x[['value']])
                          tibble(reduction = pert_dist(100), npi_name = y)
                        }
                      })

cat("---- DONE NPIS \n")

# Plot seeding -----------------------------------------------------------------

seedings <- getPosteriors(param = "seed", 
                          scenario = scenario,
                          setup = setup, 
                          runid = runid, 
                          outdir = outdir, 
                          postproc_fun = function(x) {
                            select(x, -contains("X")) %>% 
                              group_by(sim, place) %>% 
                              mutate(seed_id = row_number())
                          },
                          file_type = "csv",
                          return_res = T,
                          redo = opt$redo)

cat("---- DONE SEEDING \n")

# Plot hpars -------------------------------------------------------------------

# Generate priors
hpar_priors <- pmap_df(list(x = config$outcomes$settings$med, 
                            y = names(config$outcomes$settings$med)),
                       function(x, y) {
                         if ("perturbation" %in% names(x$probability)) {
                           value_dist <- covidcommon::as_random_distribution(x$probability$value)
                           tibble(value = value_dist(1e3), outcome = y, source = x$source)
                         }
                       })

hpar <- getPosteriors(param = "hpar", 
                      scenario = scenario,
                      setup = setup, 
                      runid = runid, 
                      outdir = outdir, 
                      postproc_fun = function(x) {
                        filter(x, (outcome %in% hpar_priors$outcome) & (source %in% hpar_priors$source))
                      },
                      return_res = T,
                      redo = opt$redo)

cat("---- DONE OUTCOMES \n")

# Logliks ----------------------------------------------------------------------

liks <- getPosteriors(param = "llik", 
                      scenario = scenario,
                      setup = setup, 
                      runid = runid, 
                      outdir = outdir, 
                      return_res = T,
                      redo = opt$redo)

cat("---- DONE LIKS \n")
