##
# @file
# @brief Creates a filter file
#
# @details
#
# 
# ## Configuration Items
# 
# ```yaml
# dynfilter_path: <path to file>
# start_date: <date>
# end_date: <date>
#
# spatial_setup:
#   setup_name: <string>
#   base_path: <path to directory>
#   geodata: <path to file>
#   nodenames: <string>
# ```
#
# ## Input Data
#
# * <b>{spatial_setup::base_path}/{spatial_setup::geodata}</b> is a csv with column {spatial_setup::nodenames} that denotes the geoids
#
# ## Output Data
#
# * <b>importation/{spatial_setup::setup_name}/case_data/jhucsse_case_data.csv</b> is a csv with case data from JHU CSSE
# * <b>{dynfilter_path}</b>: filter file
#

## @cond

library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-d", "--data"), action="store", default=file.path("data","case_data","case_data.csv"), type='character', help="path to the case data file")
)

opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opts$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

all_times <- lubridate::ymd(config$start_date) +
  seq_len(lubridate::ymd(config$end_date) - lubridate::ymd(config$start_date))




# Get the data



# get data if a US model  ---------------------------------

if (is.NULL(config$spatial_setup$us_model) || config$spatial_setup$us_model==TRUE){
  # jhucsse <- covidImportation::get_clean_JHUCSSE_data(aggr_level = "UID", 
  #                                  last_date = as.POSIXct(lubridate::ymd(config$end_date)),
  #                                  case_data_dir = file.path('importation',config$spatial_setup$setup_name,"case_data"),
  #                                  save_raw_data=TRUE,
  #                                  us_data_only=FALSE)
  # 
  # print("Successfully pulled JHU CSSE data for seeding.")
  # 
  cases_deaths <- covidcommon::get_USAFacts_data()
  print("Successfully pulled USAFacts data for seeding.")
  
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
        
    })


  
  
# from file ---------------------------------
  
} else {
  
  case_data <- readr::read_csv(opts$data)
  print(paste0("Case data from: ", opts$data))
  
  
  all_times <- lubridate::ymd(config$start_date) +
    seq_len(lubridate::ymd(config$end_date) - lubridate::ymd(config$start_date))
  
  geodata <- report.generation:::load_geodata_file(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),
                                                   config$spatial_setup$geoid_len,'0',TRUE)
  
  all_geoids <- geodata[[config$spatial_setup$nodenames]]
  
  incident_cases <- case_data %>%
    dplyr::filter(geoid %in% all_geoids) %>%
    dplyr::select(date, geoid, incidI)
  
  incident_cases$date <- as.Date(incident_cases$date)
  
  incident_cases <- incident_cases %>%
    dplyr::group_by(geoid) %>%
    dplyr::group_modify(function(.x,.y){
      .x %>%
        dplyr::arrange(date) %>%
        dplyr::filter(incidI > 0) %>%
        .[seq_len(min(nrow(.x),5)),] %>%
        dplyr::mutate(
          date = date - lubridate::days(5),
          incidI = 10 * incidI + .05
        )
    })
}


names(incident_cases) <- c('place','date','amount')

incident_cases <- incident_cases %>%
  dplyr::filter(!is.na(amount) | !is.na(date))

lambda_dir <- dirname(config$seeding$lambda_file)
if(!dir.exists(lambda_dir)){
  suppressWarnings(dir.create(lambda_dir,recursive=TRUE))
}

readr::write_csv(
  incident_cases,
  file.path(config$seeding$lambda_file)
)

print(paste("Saved seeding to",config$seeding$lambda_file))

## @endcond
