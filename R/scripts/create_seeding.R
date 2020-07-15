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
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
  optparse::make_option(c("-d", "--data"), action="store", default=file.path("data","case_data","case_data.csv"), type='character', help="path to the case data file"),
  optparse::make_option(c("-i", "--incid_x"), action="store", default=10, type='integer', help="incidence multiplier for reported cases")
)

opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))

print(paste0("Using config file: ", opts$config))
config <- covidcommon::load_config(opts$config)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}


# Check if US model
incid_x <- opts$incid_x
us_model <- config$spatial_setup$us_model==TRUE || is.null(config$spatial_setup$us_model) 


# Get the data


# get data if a US model  ---------------------------------

if (us_model){
  
  cases_deaths <- covidcommon::get_USAFacts_data()
  
  print("Successfully pulled USAFacts data for seeding.")																				   
  all_times <- lubridate::ymd(config$start_date) +
    seq_len(lubridate::ymd(config$end_date) - lubridate::ymd(config$start_date))
  
  
  geodata <- report.generation:::load_geodata_file(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),5,'0',TRUE)
  
  all_geoids <- geodata[[config$spatial_setup$nodenames]]
  
  incident_cases <- cases_deaths %>%
    dplyr::filter(FIPS %in% all_geoids) %>%
    dplyr::select(Update, FIPS, incidI)
  
  incident_cases$Update <- as.Date(incident_cases$Update)
  
  incident_cases <- incident_cases %>%
    group_by(FIPS) %>%
    group_modify(function(.x,.y){
      .x %>%
        arrange(Update) %>%
        filter(incidI > 0) %>%
        .[seq_len(min(nrow(.x),5)),] %>%
        mutate(
          Update = Update - lubridate::days(5),
          incidI = incid_x * incidI + .05
        )
      
    })
  
  
  # from file ---------------------------------
  
} else {
  
  print(paste0("Using case data from ", opts$data, " for seeding ", config$spatial_setup$setup_name))
  
  case_data <- readr::read_csv(opts$data)
  if (!exists("case_data") || is.null(case_data)){
    stop(paste0("ERROR: ", opts$data, "does not exist!"))
  }
  
  geodata <- report.generation::load_geodata_file(filename = file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),
                                                  geoid_len = config$spatial_setup$geoid_len, geoid_pad = '0', to_lower = TRUE)
  
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
          incidI = incid_x * incidI + .05
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