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
# * importation/{spatial\_setup::setup\_name}/[simulation-id].imps.csv
# * importation/{spatial\_setup::setup\_name}/[simulation-id].impa.csv
# * {spatial\_setup::base\_path}/county\_pops\_{spatial\_setup::census\_year}
# * {spatial\_setup::base\_path}/shp/counties\_{spatial\_setup::census\_year}\_{spatial\_setup::setup\_name}.shp
# * {spatial\_setup::base\_path}/{spatial\_setup::setup\_name}/airport\_attribution\_{spatial\_setup::census\_year}.csv
#
# @cond

library(covidImportation)
library(parallel)
library(stringr)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", detectCores()), type='numeric', help="number of cores used"),
  optparse::make_option(c("-n", "--num_simulations"), action="store", default=Sys.getenv("COVID_NSIMULATIONS",-1), type='numeric', help="number of simulations to run, overrides config file value"),
  optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id()))
)

opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opts$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

prefix <- covidcommon::create_prefix(config$name,trailing_separator='/')

num_simulations <- ifelse(opts$n > 0, opts$n, config$nsimulations)
last_sim_id <- stringr::str_pad(num_simulations, width=9, pad="0")

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

if (!file.exists(file.path(outdir, "input_data.csv"))) {
  print("IMPORT 1: SETUP")
  setup_importations(
    dest=dest,
    dest_type = config$importation$dest_type,
    dest_country = config$importation$dest_country,
    dest_aggr_level=config$importation$aggregate_to,
    first_date=as.POSIXct(lubridate::ymd(config$start_date)),
    last_date=as.POSIXct(lubridate::ymd(config$end_date)),
    output_dir = outdir,
    save_case_data=config$importation$cache_work,
    get_travel=TRUE, # FUTURE FUNCTIONALITY
    n_top_dests=config$importation$maximum_destinations,
    travel_dispersion=config$importation$travel_dispersion,
    param_list=lapply(config$importation$param_list,covidcommon::as_evaled_expression),
    check_error = FALSE
  )
}

if (!file.exists(file.path(outdir, paste0(last_sim_id, "imps.csv")))) {
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


if (!file.exists(file.path(outdir, paste0(sprintf("%09d",num_simulations), ".impa.csv")))) {
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

## Move files
for(simulation in seq_len(num_simulations)){
  source_file <- sprintf("%s/%09d.impa.csv",outdir,simulation)
  dest_file <- covidcommon::create_file_name(
    run_id = opts$run_id,
    prefix = prefix,
    type = config$seeding$seeding_file_type,
    index = simulation,
    extension = "csv"
  )
  config$spatial_setup$base_path
  if(!file.exists(source_file)){
    stop(sprintf("Importation did not create file %s, but expected to",source_file))
  }
  if(!dir.exists(dirname(dest_file))){
    dir.create(dest_file)
  }
  cases_deaths <- read.csv(source_file)

  required_column_names <- NULL
  check_required_names <- function(df, cols, msg) {
      if (!all(cols %in% names(df))) {
          stop(msg)
      }
  }

  if ("compartments" %in% names(config[["seir"]])) {

    if (all(names(config$seeding$seeding_compartments) %in% names(cases_deaths))) {
      required_column_names <- c("place", "date", names(config$seeding$seeding_compartments))
      check_required_names(
        cases_deaths,
        required_column_names,
        paste(
          "To create the seeding, we require the following columns to exist in the case data",
          paste(required_column_names, collapse = ", ")
        )
      )
      incident_cases <- cases_deaths[, required_column_names] %>%
        tidyr::pivot_longer(!!names(config$seeding$seeding_compartments), names_to = "seeding_group") %>%
        dplyr::mutate(
          source_column = sapply(
            config$seeding$seeding_compartments[seeding_group],
            function(x){
              paste(x$source_compartment, collapse = "_")
            }
          ),
          destination_column = sapply(
            config$seeding$seeding_compartments[seeding_group],
            function(x){
              paste(x$destination_compartment, collapse = "_")
            }
          )
        ) %>%
        tidyr::separate(source_column, paste("source", names(config$seir$compartments), sep = "_")) %>%
        tidyr::separate(destination_column, paste("destination", names(config$seir$compartments), sep = "_"))
      required_column_names <- c("place", "date", "value", paste("source", names(config$seir$compartments), sep = "_"), paste("destination", names(config$seir$compartments), sep = "_"))
      incident_cases <- incident_cases[, required_column_names]

      if (!is.null(config$smh_roun)) {
        if (config$smh_round=="R11"){
          incident_cases_om <- incident_cases %>%
            dplyr::filter(Update==lubridate::as_date("2021-12-01")) %>%
            dplyr::group_by(FIPS, Update, source_infection_stage, source_vaccination_stage, source_age_strata,
              destination_vaccination_stage, destination_age_strata, destination_infection_stage) %>%
            dplyr::summarise(value = sum(value, na.rm=TRUE)) %>%
            dplyr::mutate(source_variant_type = "WILD", destination_variant_type = "OMICRON") %>%
            dplyr::mutate(value = round(ifelse(FIPS %in% c("06000","36000"), 10,
                                        ifelse(FIPS %in% c("53000","12000"), 5, 1)))) %>%
            tibble::as_tibble()
        }
      }


    } else if ("seeding_compartments" %in% names(config$seeding) ) {
      stop(paste(
        "Could not find all compartments.  Looking for",
        paste(names(config$seeding$seeding_compartments), collapse = ", "),
        "from selection",
        paste(names(cases_deaths), collapse = ", ")
      ))
    } else {
      stop("Please add a seeding_compartments section to the config")
    }
  } else {
    required_column_names <- c("place", "date", "amount")
    check_required_names(
      cases_deaths,
      required_column_names,
      paste(
        "To create the seeding, we require the following columns to exist in the case data",
        paste(required_column_names, collapse = ", ")
      )
    )
    incident_cases <- cases_deaths[, required_column_names] %>%
      tidyr::pivot_longer(cols = "amount", names_to = "source_infection_stage", values_to = "value")
    incident_cases$destination_infection_stage <- "E"
    incident_cases$source_infection_stage <- "S"
    required_column_names <- c("place", "date", "value", "source_infection_stage", "destination_infection_stage")

    if ("parallel_structure" %in% names(config[["seir"]][["parameters"]])) {
      parallel_compartments <- config[["seir"]][["parameters"]][["parallel_structure"]][["compartments"]]
    } else {
      parallel_compartments <- setNames(NA, "unvaccinated")
    }
    incident_cases[["source_vaccination_stage"]] <- names(parallel_compartments)[[1]]
    incident_cases[["destination_vaccination_stage"]] <- names(parallel_compartments)[[1]]
    incident_cases$amount <- incident_cases$value
    required_column_names <- c(required_column_names, "amount", "source_vaccination_stage", "destination_vaccination_stage")
  }

  incident_cases <- incident_cases[, required_column_names]
  write.csv(incident_cases, file = dest_file, row.names = FALSE)
}
## @endcond
