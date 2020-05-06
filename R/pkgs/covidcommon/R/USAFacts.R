##'
##' Download USAFacts data
##'
##' Downloads the USAFacts case and death count data
##'
##' @param filename where case data will be stored
##' @param url URL to CSV on USAFacts website
##' @return data frame
##'
##' @importFrom dplyr select rename filter mutate
##' @importFrom lubridate mdy
##' @importFrom readr read_csv col_character
##' @importFrom tidyr pivot_longer
##'
download_USAFacts_data <- function(filename, url, value_col_name){

  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  message(paste("Downloading", url, "to", filename))
  download.file(url, filename, "auto")

  usafacts_data <- readr::read_csv(filename, col_types=list(stateFIPS=readr::col_character()))
  usafacts_data <- usafacts_data %>% 
    dplyr::select(-stateFIPS,-`County Name`) %>% # Drop stateFIPS columns
    dplyr::rename(FIPS=countyFIPS, source=State) %>%
    dplyr::filter(FIPS != 0) # Remove "Statewide Unallocated" cases
  col_names <- names(usafacts_data)
  date_cols <- col_names[grepl("^\\d+/\\d+/\\d{2}$", col_names)]
  usafacts_data <- usafacts_data %>% 
    tidyr::pivot_longer(date_cols, names_to="Update", values_to=value_col_name) %>%
    dplyr::mutate(Update=lubridate::mdy(Update), FIPS=sprintf("%05d", FIPS))

  return(usafacts_data)
}

##'
##' Pull USAFacts data
##'
##' Pulls the USAFacts total case count data and total death count data
##'
##' Returned data preview:
##' 'data.frame': 330225 obs. of  5 variables:
##' $ FIPS     : chr  "01001" "01001" "01001" "01001" ...
##' $ source   : chr  "AL" "AL" "AL" "AL" ...
##' $ Update   : Date, format: "2020-01-22" "2020-01-23" ...
##' $ Confirmed: num  0 0 0 0 0 0 0 0 0 0 ...
##' $ Deaths   : num  0 0 0 0 0 0 0 0 0 0 ...
##'
##' @param case_data_filename where case data will be stored
##' @param death_data_filename where death data will be stored
##'
##' @return the case data frame
##'
##' @export
##' 
get_USAFacts_data <- function(case_data_filename = "data/case_data/USAFacts_case_data.csv",
                              death_data_filename = "data/case_data/USAFacts_death_data.csv"){
  
  USAFACTS_CASE_DATA_URL = "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  USAFACTS_DEATH_DATA_URL = "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
  usafacts_case <- download_USAFacts_data(case_data_filename, USAFACTS_CASE_DATA_URL, "Confirmed")
  usafacts_death <- download_USAFacts_data(death_data_filename, USAFACTS_DEATH_DATA_URL, "Deaths")

  usafacts_data <- cbind(usafacts_case, usafacts_death["Deaths"])
  
  return(usafacts_data)
}





