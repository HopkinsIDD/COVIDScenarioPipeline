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
##' @importFrom magrittr %>%
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
##' Pulls island area data from NY Times
##'
##' Downloads data from NY Times and filters on island area
##'
##' Returned data preview:
##' tibble [198 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##'  $ Update   : Date[1:198], format: "2020-03-13" "2020-03-14" "2020-03-14" ...
##'  $ source   : chr [1:198] "PR" "PR" "VI" "GU" ...
##'  $ FIPS     : chr [1:198] "72000" "72000" "78000" "66000" ...
##'  $ Confirmed: num [1:198] 3 4 1 3 5 1 3 5 2 3 ...
##'  $ Deaths   : num [1:198] 0 0 0 0 0 0 0 0 0 0 ...
##'
##' @return the case data frame
##' 
##' @importFrom dplyr rename filter mutate
##' @importFrom plyr revalue
##' @importFrom readr read_csv
##' @importFrom magrittr %>%
##'
get_islandareas_data <- function() {
  NYTIMES_DATA_URL <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  nyt_data <- readr::read_csv(url(NYTIMES_DATA_URL))

  # Note: As of 2020-05-06, American Samoa is not in this dataset since there are no cases.
  ISLAND_AREAS = c(
    "American Samoa" = "AS",
    "Northern Mariana Islands" = "MP", 
    "Guam" = "GU", 
    "Puerto Rico" = "PR", 
    "Virgin Islands" = "VI")
  
  nyt_data <- nyt_data %>% 
    dplyr::filter(state %in% names(ISLAND_AREAS)) %>%
    dplyr::rename(Update=date, source=state, FIPS=fips, Confirmed=cases, Deaths=deaths) %>% # Rename columns
    dplyr::mutate(FIPS=paste0(FIPS,"000"), source=plyr::revalue(source, ISLAND_AREAS, warn_missing=FALSE))

  return(nyt_data)
}

# Fix counts that go negative
fix_negative_counts <- function(df, cum_col_name, incid_col_name) {

  df <- df %>% dplyr::mutate(incid_new=!!rlang::sym(incid_col_name),
                              cum_new=!!rlang::sym(cum_col_name))

  while(sum(df$incid_new<0)>0){
            
    # first try to just remove the row
    df <- df %>% 
        dplyr::arrange(FIPS, source, Update) 
    df <- df %>% dplyr::filter(incid_new >= 0)
    df <- df %>%
        dplyr::group_by(FIPS, source) %>%
        dplyr::mutate(incid_new = diff(c(0,cum_new))) %>% 
        dplyr::ungroup()
    
    negs_ind <- which(df$incid_new < 0)
    if (length(negs_ind)>0){
        df <- df %>% 
            dplyr::arrange(FIPS, source, Update) 
        df$cum_new[negs_ind - 1] <- df$cum_new[negs_ind - 1] + df$incid_new[negs_ind]
        df <- df %>%
            dplyr::group_by(FIPS, source) %>%
            dplyr::mutate(incid_new = diff(c(0,cum_new))) %>% 
            dplyr::ungroup()
    }
  }

  df <- df %>%
    dplyr::select(-!!cum_col_name, -!!incid_col_name) %>%
    rename(!!cum_col_name:=cum_new, !!incid_col_name:=incid_new)

  return(df)
}

##'
##' Pull case and death count data from USAFacts
##'
##' Pulls the USAFacts cumulative case count and death data. Calculates incident counts.
##'
##' Returned data preview:
##' tibble [352,466 × 7] (S3: grouped_df/tbl_df/tbl/data.frame)
##'  $ FIPS       : chr [1:352466] "00001" "00001" "00001" "00001" ...
##'  $ source     : chr [1:352466] "NY" "NY" "NY" "NY" ...
##'  $ Update     : Date[1:352466], format: "2020-01-22" "2020-01-23" ...
##'  $ Confirmed  : num [1:352466] 0 0 0 0 0 0 0 0 0 0 ...
##'  $ Deaths     : num [1:352466] 0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidI     : num [1:352466] 0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidDeath : num [1:352466] 0 0 0 0 0 0 0 0 0 0 ...
##'
##' @return the case data frame
##'
##' @importFrom covidImportation est_daily_incidence
##' @importFrom dplyr rename group_modify group_by
##'
##' @export
##' 
get_USAFacts_data <- function(case_data_filename = "data/case_data/USAFacts_case_data.csv",
                              death_data_filename = "data/case_data/USAFacts_death_data.csv"){
  
  USAFACTS_CASE_DATA_URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  USAFACTS_DEATH_DATA_URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
  usafacts_case <- download_USAFacts_data(case_data_filename, USAFACTS_CASE_DATA_URL, "Confirmed")
  usafacts_death <- download_USAFacts_data(death_data_filename, USAFACTS_DEATH_DATA_URL, "Deaths")

  usafacts_data <- cbind(usafacts_case, usafacts_death["Deaths"])
  usafacts_data <- rbind(usafacts_data, get_islandareas_data()) # Append island areas

  # Create columns incidI and incidDeath
  usafacts_data <- dplyr::group_modify(
    dplyr::group_by(
      usafacts_data,
      FIPS
    ),
    function(.x,.y){
      # .x$Confirmed = cummax(.x$Confirmed) # cumulative column only increases; alternative way to avoid negative incidents
      .x$incidI = c(.x$Confirmed[1],diff(.x$Confirmed))
      # .x$Deaths = cummax(.x$Deaths) # cumulative column only increases; alternative way to avoid negative incidents
      .x$incidDeath = c(.x$Deaths[1],diff(.x$Deaths,))
      return(.x)
    }
  )

  # Fix counts that go negative
  usafacts_data <- fix_negative_counts(usafacts_data, "Confirmed", "incidI")
  usafacts_data <- fix_negative_counts(usafacts_data, "Deaths", "incidDeath")

  return(usafacts_data)
}



