library(covidImportation)
library(doParallel)
library(parallel)
#options(error=function(){quit(2)})
# options(tigris_use_cache = TRUE)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=detectCores(), type='numeric', help="number of cores used")
)

opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opts$c)
if (is.na(config)) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

dest <- sort(config$spatial_setup$modeled_states)
print(dest)

outdir <- file.path('importation',config$spatial_setup$setup_name)
print(outdir)

setup_name <- config$spatial_setup$setup_name

if(!dir.exists(outdir)){
  dir.create(outdir,recursive=TRUE)
}
## SHOULD REMOVE THIS
# if(!dir.exists(file.path(outdir,paste(dest,collapse='-')))){
#   dir.create(file.path(outdir,paste(dest,collapse='-')),recursive=TRUE)
# }
# ## SHOULD REMOVE THIS
# case_data_dir <- file.path('importation',config$spatial_setup$setup_name,"case_data")
# if(!dir.exists(case_data_dir)){
#   dir.create(case_data_dir,recursive=TRUE)
# }
# if(!dir.exists(file.path(config$spatial_setup$base_path,paste(dest,collapse='-')))){
#   dir.create(file.path(config$spatial_setup$base_path,paste(dest,collapse='-')),recursive=TRUE)
# }
param_list <- config$param_list

tidycensus::census_api_key(key = config$importation$census_api_key)

case_data_dir <- "data/case_data"

# dont need this anymore #
#shapefile_path = file.path(config$spatial_setup$base_path,config$spatial_setup$shapefile)


print("IMPORT 1: SETUP")
setup_importations(
  dest=dest,
  dest_type = config$importation$dest_type,
  dest_country = config$importation$dest_country,
  dest_aggr_level=config$importation$aggregate_to,
  first_date=as.POSIXct(lubridate::ymd(config$start_date)),
  last_date=as.POSIXct(lubridate::ymd(config$end_date)),
  update_case_data = config$importation$update_case_data,
  case_data_dir = case_data_dir,
  output_dir = outdir,
  check_saved_data=config$importation$cache_work,
  save_case_data=config$importation$cache_work,
  get_travel=TRUE, # FUTURE FUNCTIONALITY
  n_top_dests=config$importation$maximum_destinations,
  travel_dispersion=config$importation$travel_dispersion,
  param_list=config$importation$param_list
)


print("IMPORT 2: RUN MODEL")
run_importations(
  n_sim=config$nsimulations,
  cores=opts$j,
  get_detection_time=FALSE, # NOT CURRENTLY USED FOR THIS PURPOSE
  travel_dispersion=config$importation$travel_dispersion,
  allow_travel_variance=config$importation$draw_travel_from_distribution,
  print_progress = config$importation$print_progress,
  output_dir = outdir,
  param_list = config$importation$param_list
)


print("IMPORT 3: DISTRIBUTE")
run_full_distrib_imports(
  states_of_interest=dest,
  regioncode=setup_name,
  yr=config$spatial_setup$census_year,
  mean_travel_file = file.path(
    outdir,
    "travel_mean.csv"
  ),
  travelers_threshold=config$importation$travelers_threshold,
  airport_cluster_threshold=config$importation$airport_cluster_distance,
  shapefile_path = NULL,
  model_output_dir = outdir,
  local_dir=paste0(config$spatial_setup$base_path,'/'),
  plot=FALSE,
  cores=opts$j,
  n_sim=config$nsimulations
)


