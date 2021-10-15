##
# @file
# @brief Creates a seeding file
#
# @details
#
#
# ## Configuration Items
#
# ```yaml
# start_date: <date>
# end_date: <date>
#
# spatial_setup:
#   setup_name: <string>
#   base_path: <path to directory>
#   geodata: <path to file>
#   nodenames: <string>
#
# seeding:
#   lambda_file: <path to file>

# ```
#
# ## Input Data
#
# * <b>{spatial_setup::base_path}/{spatial_setup::geodata}</b> is a csv with column {spatial_setup::nodenames} that denotes the geoids
#
# ## Output Data
#
# * <b>data/case_data/USAFacts_case_data.csv</b> is the case csv downloaded from USAFacts
# * <b>data/case_data/USAFacts_death_data.csv</b> is the death csv downloaded from USAFacts
# * <b>{seeding::lambda_file}</b>: filter file
#

## @cond

library(covidcommon)
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file")
)

opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

print(paste0("Using config file: ", opt$config))
config <- covidcommon::load_config(opt$config)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

if(is.null(config$spatial_setup$us_model)) {
  config$spatial_setup$us_model <- FALSE
  if("modeled_states" %in% names(config$spatial_setup)){
    config$spatial_setup$us_model <- TRUE
  }
}

is_US_run <- config$spatial_setup$us_model

## backwards compatibility with configs that don't have filtering$gt_source parameter will use the previous default data source (USA Facts)
if(is.null(config$filtering$gt_source)){
  if(is_US_run){
    gt_source <- "usafacts"
  } else {
    gt_source <- NULL
  }
} else{
  gt_source <- config$filtering$gt_source
}

if(gt_source == "LA health dpt"){
  gt_source <- "csse"
}
if(is.null(config$seeding$delay_incidC)){
  config$seeding$delay_incidC <- 5
}
if(is.null(config$seeding$ratio_incidC)){
  config$seeding$ratio_incidC <- 10
}

if(is_US_run){
  state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)
  if(state_level){
    gt_scale <- "US state"
    include_unassigned <- TRUE
  } else{
    gt_scale <- "US county"
    include_unassigned <- FALSE
  }
} else{
  gt_scale <- "US county"
  include_unassigned <- FALSE
}

if(!is.null(gt_source)){
  cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale, incl_unass = include_unassigned)
  print(paste("Successfully pulled", gt_source, "data for seeding."))
  if(is_US_run){
    cases_deaths <- cases_deaths %>%
      mutate(FIPS = stringr::str_pad(FIPS, width = 5, side="right", pad="0"))
  }
} else {
  data_path <- config$filtering$data_path
  if(is.null(data_path)){
    data_path <- config$seeding$casedata_file
    if(is.null(data_path)){
      stop("Please provide a ground truth file for non-us runs with no data source as filtering::data_path or seeding::casedata_file")
    }
  }
  cases_deaths <- read.csv(data_path)
  print(paste("Successfully loaded data from ", data_path, "for seeding."))
}

# # Aggregation to state level if in config
# if(is_US_run){
#   state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)
#   if(state_level){
#     gt_scale <- "US state"
#     cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale, incl_unass = TRUE) 
#   } else{
#     gt_scale <- "US county"
#     cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale) 
#   }
#   cases_deaths <- cases_deaths %>%
#     mutate(FIPS = stringr::str_pad(FIPS, width = 5, side="right", pad="0"))
# }

## Check some data attributes:
## This is a hack:
if("geoid" %in% names(cases_deaths)){
  cases_deaths$FIPS <- cases_deaths$geoid
  warning("Changing FIPS name in seeding. This is a hack")
}
if("date" %in% names(cases_deaths)){
  cases_deaths$Update <- cases_deaths$date
  warning("Changing Update name in seeding. This is a hack")
}
obs_nodename <- config$spatial_setup$nodenames
required_column_names <- c("FIPS", "Update", "incidI")
if(!(all(required_column_names %in% names(cases_deaths)))){
  stop(paste("To create seeding, we require the following columns to exist in the case data", paste(required_column_names, collapse = ", ")))
}


all_times <- lubridate::ymd(config$start_date) +
  seq_len(lubridate::ymd(config$end_date) - lubridate::ymd(config$start_date))

geodata <- report.generation:::load_geodata_file(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),5,'0',TRUE)

all_geoids <- geodata[[config$spatial_setup$nodenames]]

incident_cases <- cases_deaths %>%
  dplyr::filter(FIPS %in% all_geoids) %>%
  dplyr::select(Update, FIPS, incidI)

incident_cases$Update <- as.Date(incident_cases$Update)

incident_cases <- incident_cases %>%
  dplyr::group_by(FIPS) %>%
  dplyr::group_modify(function(.x,.y){
    .x %>%
      dplyr::arrange(Update) %>%
      dplyr::filter(incidI > 0) %>%
      .[seq_len(min(nrow(.x),5)),] %>%
      dplyr::mutate(
        Update = Update - lubridate::days(5),
        incidI = 10 * incidI + .05
      )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(FIPS, Update, incidI)

names(incident_cases) <- c('place','date','amount')

incident_cases <- incident_cases %>%
  dplyr::filter(!is.na(amount) | !is.na(date))

lambda_dir <- dirname(config$seeding$lambda_file)
if(!dir.exists(lambda_dir)){
  suppressWarnings(dir.create(lambda_dir,recursive=TRUE))
}

write.csv(
  incident_cases,
  file=file.path(config$seeding$lambda_file),
  row.names=FALSE
)

print(paste("Saved seeding to",config$seeding$lambda_file))

## @endcond
