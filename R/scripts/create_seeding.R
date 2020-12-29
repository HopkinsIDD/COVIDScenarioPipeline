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

opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opts$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

## backwards compatibility with configs that don't have filtering$gt_source parameter will use the previous default data source (USA Facts)
if(is.null(config$filtering$gt_source)){
  gt_source <- "usafacts"
} else{
  gt_source <- config$filtering$gt_source
}

# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)
if(state_level){
  gt_scale <- "US state"
  cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale, incl_unass = TRUE) 
} else{
  gt_scale <- "US county"
  cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale) 
}

cases_deaths <- cases_deaths %>%
  mutate(FIPS = stringr::str_pad(FIPS, width = 5, side="right", pad="0"))

print(paste("Successfully pulled", gt_source, "data for seeding."))

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
        incidI = 10 * incidI + .05
      )
      
  })

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
