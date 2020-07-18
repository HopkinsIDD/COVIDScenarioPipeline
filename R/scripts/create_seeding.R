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
#   lambda_file: <string>

# ```
#
# ## Input Data
#
# * <b>{spatial_setup::base_path}/{spatial_setup::geodata}</b> is a csv with column {spatial_setup::nodenames} that denotes the geoids
#
# ## Output Data
#
# * <b>importation/{spatial_setup::setup_name}/case_data/jhucsse_case_data.csv</b> is a csv with case data from JHU CSSE
# * <b>{spatial_setup::lambda_file}</b>: seeding file
#

## @cond

library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-s", "--source"), action="store", default="CSSE", type='character', help="source of case data: USAFacts or CSSE"),
  optparse::make_option(c("-d", "--data"), action="store", default=file.path("data","case_data","case_data.csv"), type='character', help="path to the case data file"),
)

opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

print(paste0("Using config file: ", opt$config))
config <- covidcommon::load_config(opt$config)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

all_times <- lubridate::ymd(config$start_date) +
  seq_len(lubridate::ymd(config$end_date) - lubridate::ymd(config$start_date))




# Get the data


# get data if a US model  ---------------------------------

if (is.null(config$spatial_setup$us_model) || config$spatial_setup$us_model==TRUE){
  
  # Load either JHUCSSE or USAFacts data
  if (tolower(opt$source) %in% c("csse", "jhucsse", "jhu csse")){
    
    dir.create(file.path('importation',config$spatial_setup$setup_name,"case_data"), showWarnings = FALSE, recursive = TRUE)
    
    print("Pulling case data from JHU CSSE for US seeding.")
    cases_deaths <- covidImportation::get_clean_JHUCSSE_data(aggr_level = "UID",
                                     last_date = as.POSIXct(lubridate::ymd(config$end_date)),
                                     case_data_dir = file.path('importation',config$spatial_setup$setup_name,"case_data"),
                                     save_raw_data=TRUE,
                                     us_data_only=FALSE)
    print("Successfully pulled JHU CSSE data for seeding.")
    
  } else if (tolower(opt$source) %in% c("usafacts", "usa facts", "usa")){
    print("Pulling case data from USA Facts for US seeding.")
    cases_deaths <- covidcommon::get_USAFacts_data()
    print("Successfully pulled USAFacts data for seeding.")
  } else { 
    stop(paste(opt$source,"is not a recognized source of case data"))
  }
  
  geodata <- report.generation::load_geodata_file(filename = file.path(config$spatial_setup$base_path, config$spatial_setup$geodata), 
                                                  geoid_len = 5, geoid_pad = '0', to_lower = TRUE)


  all_geoids <- geodata[[config$spatial_setup$nodenames]]
  
  incident_cases <- cases_deaths %>%
    dplyr::filter(FIPS %in% all_geoids) %>%
    dplyr::select(Update, FIPS, incidI)
  
  incident_cases$Update <- as.Date(incident_cases$Update)
  
  if(is.null(config$seeding$delay_incidC)){
    config$seeding$delay_incidC <- 5
  }
  if(is.null(config$seeding$ratio_incidC)){
    config$seeding$ratio_incidC <- 10
  }

  incident_cases <- incident_cases %>%
    dplyr::group_by(FIPS) %>%
    dplyr::group_modify(function(.x,.y){
      .x %>%
        dplyr::arrange(Update) %>%
        dplyr::filter(incidI > 0) %>%
        .[seq_len(min(nrow(.x),5)),] %>%
        dplyr::mutate(
          Update = Update - lubridate::days(config$seeding$delay_incidC),
          incidI = config$seeding$ratio_incidC * incidI + .05
        )
    })


  
# from file ---------------------------------
  
} else {
  
  print(paste0("Using case data from ", opt$data, "for seeding ", config$spatial_setup$setup_name))
  
  case_data <- readr::read_csv(opt$data)
  if (!exists("case_data") || is.null(case_data)){
      stop(paste0("ERROR: ", opt$data, "does not exist!"))
  }
  
  geodata <- report.generation::load_geodata_file(filename = file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),
                                                   geoid_len = config$spatial_setup$geoid_len, geoid_pad = '0', to_lower = TRUE)
  
  all_geoids <- geodata[[config$spatial_setup$nodenames]]
  
  incident_cases <- case_data %>%
    dplyr::filter(geoid %in% all_geoids) %>%
    dplyr::select(date, geoid, incidI)
  
  incident_cases$date <- as.Date(incident_cases$date)

  if(is.null(config$seeding$delay_incidC)){
    config$seeding$delay_incidC <- 5
  }
  if(is.null(config$seeding$ratio_incidC)){
    config$seeding$ratio_incidC <- 10
  }
  
  incident_cases <- incident_cases %>%
    dplyr::group_by(geoid) %>%
    dplyr::group_modify(function(.x,.y){
      .x %>%
        dplyr::arrange(date) %>%
        dplyr::filter(incidI > 0) %>%
        .[seq_len(min(nrow(.x),5)),] %>%
        dplyr::mutate(
          date = date - lubridate::days(config$seeding$delay_incidC),
          incidI = config$seeding$ratio_incidC * incidI + .05
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
