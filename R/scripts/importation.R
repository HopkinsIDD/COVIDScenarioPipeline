##
# @file
# @brief Generates importation data
#
# @details
# 
# ## Configuration Options
# 
# ```yaml
# start_date: <date>
# end_date: <date>
# nsimulations: <integer, optional> overridden by command-line option -n
#
# spatial_setup:
#   base_path: <path to directory>
#   modeled_states: <list of state postal codes> e.g. MD, CA, NY
#   setup_name: <string>
#   census_year: <4-digit year>
#   census_api_key: <string, optional> default is environment variable CENSUS_API_KEY. Environment variable is preferred so you don't accidentally commit your key.
#
# importation:
#   dest_type: <choose one: "airport", "city", "state", "country">
#   dest_country: <string>, e.g. USA
#   aggregate_to: <choose one: "airport", "city", "state", "country", "metro"> "metro" is only available for CA
#   update_case_data: <logical>
#   cache_work: <logical>
#   maximum_destinations: <integer or Inf>
#   travel_dispersion: 
#   travelers_threshold:
#   airports_cluster_distance:
#   draw_travel_from_distribution: <logical>
#   print_progress: <logical>
#   param_list:
#     shift_incid_days: <integer, optional>
#     p_report_source:
#     delta:
#     incub_mean_log:
#     incub_sd_log:
#     inf_period_hosp_mean_log:
#     inf_period_hosp_sd_log:
#     inf_period_nohosp_mean:
#     inf_period_nohosp_sd:
# ```
#
# ## Input Data
# None
#
# ## Output Data
#
# * data/case\_data/jhucsse\_case\_data.csv: case data freshly pulled from JHU CSSE
# * importation/{spatial\_setup::setup\_name}/input\_data.csv
# * importation/{spatial\_setup::setup\_name}/travel\_data\_monthly.csv
# * importation/{spatial\_setup::setup\_name}/travel\_mean.csv
# * importation/{spatial\_setup::setup\_name}/travel\_data_daily.csv
# * importation/{spatial\_setup::setup\_name}/imports\_sim[simulation-id].csv
# * importation/{spatial\_setup::setup\_name}/importation\_[simulation-id].csv
# * {spatial\_setup::base\_path}/county\_pops\_{spatial\_setup::census\_year}
# * {spatial\_setup::base\_path}/shp/counties\_{spatial\_setup::census\_year}\_{spatial\_setup::setup\_name}.shp
# * {spatial\_setup::base\_path}/{spatial\_setup::setup\_name}/airport\_attribution\_{spatial\_setup::census\_year}.csv
#
# @cond

library(covidImportation)
library(parallel)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=detectCores(), type='numeric', help="number of cores used"),
  optparse::make_option(c("-n", "--num_simulations"), action="store", default=-1, type='numeric', help="number of simulations to run, overrides config file value")
)

opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opts$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

num_simulations <- ifelse(opts$n > 0, opts$n, config$nsimulations)

dest <- sort(config$spatial_setup$modeled_states)
print(dest)

outdir <- file.path('importation',config$spatial_setup$setup_name)
print(outdir)

if(!dir.exists(outdir)){
  dir.create(outdir,recursive=TRUE)
}

census_key = Sys.getenv("CENSUS_API_KEY")
if(length(config$importation$census_api_key) != 0)
{
  census_key = config$importation$census_api_key
}
if(census_key == "")
{
  stop("no census key found -- please set CENSUS_API_KEY environment variable or specify importation::census_api_key in config file")
}
tidycensus::census_api_key(key = census_key)

case_data_dir <- "data/case_data"

if (!file.exists(file.path(outdir, "input_data.csv"))) {
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
    param_list=lapply(config$importation$param_list,covidcommon::as_evaled_expression),
    check_error = FALSE
  )
}


if (!file.exists(file.path(outdir, paste0("imports_sim", num_simulations, ".csv")))) {
  print("IMPORT 2: RUN MODEL")
  run_importations(
    n_sim=num_simulations,
    cores=opts$j,
    get_detection_time=FALSE, # NOT CURRENTLY USED FOR THIS PURPOSE
    travel_dispersion=config$importation$travel_dispersion,
    allow_travel_variance=config$importation$draw_travel_from_distribution,
    print_progress = config$importation$print_progress,
    output_dir = outdir,
    param_list=lapply(config$importation$param_list,covidcommon::as_evaled_expression)
  )
}


if (!file.exists(file.path(outdir, paste0("importation_", num_simulations, ".csv")))) {
  print("IMPORT 3: DISTRIBUTE")
  run_full_distrib_imports(
    states_of_interest=dest,
    regioncode=config$spatial_setup$setup_name,
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
    n_sim=num_simulations
  )
 } 

## @endcond