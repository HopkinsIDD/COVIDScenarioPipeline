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
  date_cols <- col_names[grepl("^\\d+/\\d+/\\d+$", col_names)]
  usafacts_data <- usafacts_data %>% 
    tidyr::pivot_longer(date_cols, names_to="Update", values_to=value_col_name) %>%
    dplyr::mutate(Update=lubridate::mdy(Update), FIPS=sprintf("%05d", FIPS))
  
  validation_date <- Sys.getenv("VALIDATION_DATE")
  if ( validation_date != '' ) {
    print(paste("(DataUtils.R) Limiting USAFacts data to:", validation_date, sep=" "))
    usafacts_data <- usafacts_data %>% dplyr::filter( Update < validation_date )
  }
  
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

# There are three different ways of dealing with negative incidence counts, specified by the type argument
# "high": highest estimate; this modifies the cumulative column to be the cummax of the previous cumulative counts
# "low": method: Assume all negative incidents are corrections to previous reports.
#               So, reduce previous reports to make incidents not decrease.
# "mid" method: Combination of both low and high
#               For consecutive negatives, the first negative will be modified so that the
#               cumulative is the cummax so far.
#               For the following negatives, use the low method: reduce previous reports.
# For get_USAFacts_data(), this is always the default ("mid"), but the other code is here for posterity.
fix_negative_counts_single_geoid <- function(.x,.y, incid_col_name, date_col_name, cum_col_name, type){
  original_names <- names(.x)

  .x <- dplyr::arrange(.x,!!rlang::sym(date_col_name))

  # Calculate new cumulative column
  calc_col_name <- paste(cum_col_name, type, sep="_")

  if (type == "high") {

    .x[[calc_col_name]] <- .x[[cum_col_name]]
    .x[[calc_col_name]][is.na(.x[[calc_col_name]]) ] <- 0 # Fixes NA values
    .x[[calc_col_name]] <- cummax(.x[[calc_col_name]])

  } else if (type == "low") {

    .x[[calc_col_name]] <- .x[[cum_col_name]]
    .x[[calc_col_name]][is.na(.x[[calc_col_name]]) ] <- Inf # Fixes NA values
    .x[[calc_col_name]] <- rev(cummin(rev(.x[[calc_col_name]])))
    .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- 0 # In case last thing in row is NA/infinity
    .x[[calc_col_name]] <- cummax(.x[[calc_col_name]]) # Replace 0's with cummax

  } else if (type == "mid") {

    .x[[calc_col_name]] <- .x[[cum_col_name]]

    ## In mid, do a local max followed by a global min (like "low")

    # Get indices where the cumulative count is bigger than the one after it
    # i.e. where incidence is negative next time
    unlagged <- .x[[calc_col_name]]
    unlagged[is.na(.x[[calc_col_name]]) ] <- -Inf # Fixes NA values
    lagged <- dplyr::lag(.x[[calc_col_name]])
    lagged[is.na(.x[[calc_col_name]]) ] <- Inf # Fixes NA values

    max_indices <- which(unlagged < lagged)
    .x[[calc_col_name]][max_indices] <- lagged[max_indices]

    # Global min
    .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- Inf
    .x[[calc_col_name]] <- rev(cummin(rev(.x[[calc_col_name]])))
    .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- 0
    .x[[calc_col_name]] <- cummax(.x[[calc_col_name]])

  } else {

    stop(paste("Invalid fix_negative_counts type. Must be ['low', 'med', 'high']. Actual: ", type))

  }

  .x[[cum_col_name]] <- .x[[calc_col_name]]

  # Recover incidence from the new cumulative
  .x[[incid_col_name]] <- c(.x[[calc_col_name]][[1]], diff(.x[[calc_col_name]]))

  .x <- .x[,original_names]
  for(col in names(.y)){
    .x[[col]] <- .y[[col]]
  }

  return(.x)
}

# Add missing dates, fix counts that go negative, and fix NA values
#
# See fix_negative_counts_single_geoid() for more details on the algorithm,
# specified by argument "type"
fix_negative_counts <- function(
  df,
  cum_col_name,
  incid_col_name,
  date_col_name = "Update",
  min_date = min(df[[date_col_name]]),
  max_date = max(df[[date_col_name]]),
  type="mid" # "low" or "high"
  ) {

  if(nrow(dplyr::filter(df, !!incid_col_name < 0)) > 0) {
    return(df)
  }

  df <- df %>%
    dplyr::group_by(FIPS,source) %>%
    # Add missing dates
    tidyr::complete(!!rlang::sym(date_col_name) := min_date + seq_len(max_date - min_date)-1) %>%
    dplyr::group_map(fix_negative_counts_single_geoid,
                          incid_col_name=incid_col_name, 
                          date_col_name=date_col_name, 
                          cum_col_name=cum_col_name,
                          type=type) %>%
    do.call(what=rbind)

  return(df)
}


# Add missing dates, fix counts that go negative, and fix NA values for global dataset (group by Country_Region and Province_State instead of by FIPS)
#
# See fix_negative_counts_single_geoid() for more details on the algorithm,
# specified by argument "type"
fix_negative_counts_global <- function(
  df,
  cum_col_name,
  incid_col_name,
  date_col_name = "Update",
  min_date = min(df[[date_col_name]]),
  max_date = max(df[[date_col_name]]),
  type="mid" # "low" or "high"
  ) {

  if(nrow(dplyr::filter(df, !!incid_col_name < 0)) > 0) {
    return(df)
  }

  df <- df %>%
    dplyr::group_by(Country_Region, Province_State, source) %>%
    # Add missing dates
    tidyr::complete(!!rlang::sym(date_col_name) := min_date + seq_len(max_date - min_date)-1) %>%
    dplyr::group_map(fix_negative_counts_single_geoid,
                          incid_col_name=incid_col_name, 
                          date_col_name=date_col_name, 
                          cum_col_name=cum_col_name,
                          type=type) %>%
    do.call(what=rbind)

  return(df)
}


# Aggregate county data to state level
#
aggregate_counties_to_state <- function(df, state_fips){
  aggregated <- df %>%
    dplyr::filter(grepl(paste0("^", state_fips), FIPS)) %>%
    group_by(source, Update) %>%
    dplyr::summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), incidI = sum(incidI), incidDeath = sum(incidDeath)) %>%
    dplyr::mutate(FIPS = paste0(state_fips, "000")) %>%
    dplyr::ungroup()
  nonaggregated <- df %>%
    dplyr::filter(!grepl(paste0("^", state_fips), FIPS))

  rc <- dplyr::bind_rows(nonaggregated, aggregated) %>%
    dplyr::arrange(source, FIPS, Update)

  return(rc)
}


##'
##' Pull case and death count data from USAFacts
##'
##' Pulls the USAFacts cumulative case count and death data. Calculates incident counts.
##' USAFacts does not include data for all the territories (aka island areas). These data are pulled from NYTimes.
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
##' @return the case and deaths data frame
##'
##' @importFrom dplyr rename group_modify group_by full_join select
##'
##' @export
##' 
get_USAFacts_data <- function(case_data_filename = "data/case_data/USAFacts_case_data.csv",
                              death_data_filename = "data/case_data/USAFacts_death_data.csv"){
  
  USAFACTS_CASE_DATA_URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  USAFACTS_DEATH_DATA_URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
  usafacts_case <- download_USAFacts_data(case_data_filename, USAFACTS_CASE_DATA_URL, "Confirmed")
  usafacts_death <- download_USAFacts_data(death_data_filename, USAFACTS_DEATH_DATA_URL, "Deaths")

  usafacts_data <- dplyr::full_join(usafacts_case, usafacts_death)
  usafacts_data <- dplyr::select(usafacts_data, Update, source, FIPS, Confirmed, Deaths)
  usafacts_data <- rbind(usafacts_data, get_islandareas_data()) # Append island areas
  usafacts_data <- dplyr::arrange(usafacts_data, source, FIPS, Update)

  # Create columns incidI and incidDeath
  usafacts_data <- dplyr::group_modify(
    dplyr::group_by(
      usafacts_data,
      FIPS
    ),
    function(.x,.y){
      .x$incidI = c(.x$Confirmed[1],diff(.x$Confirmed))
      .x$incidDeath = c(.x$Deaths[1],diff(.x$Deaths,))
      return(.x)
    }
  )

  # Fix incidence counts that go negative and NA values or missing dates
  usafacts_data <- fix_negative_counts(usafacts_data, "Confirmed", "incidI")
  usafacts_data <- fix_negative_counts(usafacts_data, "Deaths", "incidDeath")

  return(usafacts_data)
}



##'
##' Download CSSE US data
##'
##' Downloads the CSSE US case and death count data
##'
##' @param filename where case data will be stored
##' @param url URL to CSV on CSSE website
##' @return data frame
##'
##' @importFrom dplyr select rename filter mutate distinct
##' @importFrom lubridate mdy
##' @importFrom readr read_csv col_character
##' @importFrom tidyr pivot_longer
##' @importFrom magrittr %>%
##' @importFrom stringr str_replace str_length str_pad fixed
##' @importFrom tibble as_tibble
##'
download_CSSE_US_data <- function(filename, url, value_col_name){

  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  message(paste("Downloading", url, "to", filename))
  download.file(url, filename, "auto")

  csse_data <- readr::read_csv(filename, col_types = list("FIPS" = col_character())) %>% tibble::as_tibble() 
  csse_data <- csse_data %>%
    dplyr::filter(!grepl("out of", Admin2, ignore.case = TRUE) & ## out of state records
                  !grepl("unassigned", Admin2, ignore.case = TRUE) & ## probable cases
                  !grepl("princess", Province_State, ignore.case = TRUE) & ## cruise ship cases
                  !is.na(FIPS)) 
  csse_data <- csse_data %>%
    tidyr::pivot_longer(cols=contains("/"), names_to="Update", values_to=value_col_name) %>%
    dplyr::mutate(Update=as.Date(lubridate::mdy(Update)),
                  FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""), # clean FIPS if numeric
                  FIPS = ifelse(stringr::str_length(FIPS)<=2, paste0(FIPS, "000"), stringr::str_pad(FIPS, 5, pad = "0"))) %>% 
    dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>% 
    dplyr::distinct()
  csse_data <- suppressWarnings(
    csse_data %>%
        dplyr::mutate(state_abb = state.abb[match(Province_State, state.name)]) %>%
        dplyr::mutate(source = ifelse(Province_State=="District of Columbia", "DC",
                                         ifelse(is.na(state_abb) & Country_Region=="US", iso2, state_abb)))
    )
  csse_data <- csse_data %>%
    dplyr::select(FIPS, source, Update, !!value_col_name)
  
  validation_date <- Sys.getenv("VALIDATION_DATE")
  if ( validation_date != '' ) {
    print(paste("(DataUtils.R) Limiting CSSE US data to:", validation_date, sep=" "))
    csse_data <- csse_data %>% dplyr::filter( Update < validation_date )
  }
  
  return(csse_data)
}



##'
##' Pull US case and death count data from JHU CSSE
##'
##' Pulls the CSSE cumulative case count and death data. Calculates incident counts.
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
##' @return the case and deaths data frame
##'
##' @importFrom dplyr rename group_modify group_by full_join select
##'
##' @export
##' 
get_CSSE_US_data <- function(case_data_filename = "data/case_data/jhucsse_us_case_data_crude.csv",
                              death_data_filename = "data/case_data/jhucsse_us_death_data_crude.csv"){
  
  CSSE_US_CASE_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  CSSE_US_DEATH_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  csse_us_case <- download_CSSE_US_data(case_data_filename, CSSE_US_CASE_DATA_URL, "Confirmed")
  csse_us_death <- download_CSSE_US_data(death_data_filename, CSSE_US_DEATH_DATA_URL, "Deaths")

  csse_us_data <- dplyr::full_join(csse_us_case, csse_us_death)
  csse_us_data <- dplyr::select(csse_us_data, Update, source, FIPS, Confirmed, Deaths)
  csse_us_data <- dplyr::arrange(csse_us_data, source, FIPS, Update)

  # Create columns incidI and incidDeath
  csse_us_data <- dplyr::group_modify(
    dplyr::group_by(
      csse_us_data,
      FIPS
    ),
    function(.x,.y){
      .x$incidI = c(.x$Confirmed[1],diff(.x$Confirmed))
      .x$incidDeath = c(.x$Deaths[1],diff(.x$Deaths,))
      return(.x)
    }
  )

  # Fix incidence counts that go negative and NA values or missing dates
  csse_us_data <- fix_negative_counts(csse_us_data, "Confirmed", "incidI")
  csse_us_data <- fix_negative_counts(csse_us_data, "Deaths", "incidDeath")

  # Aggregate county-level data for Puerto Rico
  csse_us_data <- aggregate_counties_to_state(csse_us_data, "72")

  return(csse_us_data)
}



##'
##' Download CSSE global data
##'
##' Downloads the CSSE global case and death count data
##'
##' @param filename where case data will be stored
##' @param url URL to CSV on CSSE website
##' @return data frame
##'
##' @importFrom dplyr select rename filter mutate distinct
##' @importFrom lubridate mdy
##' @importFrom readr read_csv col_character
##' @importFrom tidyr pivot_longer
##' @importFrom magrittr %>%
##' @importFrom stringr str_replace str_length str_pad fixed
##' @importFrom tibble as_tibble
##' @importFrom globaltoolboxlite get_iso get_iso2_from_ISO3
##' @importFrom tidyselect everything
##'
download_CSSE_global_data <- function(filename, url, value_col_name){

  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  message(paste("Downloading", url, "to", filename))
  download.file(url, filename, "auto")

  csse_data <- readr::read_csv(filename) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(Province_State = `Province/State`,
                  Country_Region = `Country/Region`,
                  Latitude = Lat,
                  Longitude = Long)
  
  csse_data <- csse_data %>%
    dplyr::mutate(iso3 = globaltoolboxlite::get_iso(Country_Region)) %>%
    dplyr::mutate(iso2 = globaltoolboxlite::get_iso2_from_ISO3(iso3),
                  UID = ifelse(!is.na(Province_State), paste0(iso3, "-", Province_State), iso3)) %>%
    dplyr::select(UID, Province_State:Longitude, iso2, iso3, tidyselect::everything())
  
  csse_data <- csse_data %>%
    tidyr::pivot_longer(cols=contains("/"), names_to="Update", values_to=value_col_name) %>%
    dplyr::mutate(Update=as.Date(lubridate::mdy(Update))) %>% 
    dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>% 
    dplyr::distinct()

  # Define a single source location variable
  # - USA: States used for source
  # - China: Provinces used for source
  # - Others: Country used for source
  csse_data <- csse_data %>% 
      dplyr::mutate(source = ifelse(iso3=="CHN" & !is.na(Province_State), Province_State, iso3)) 

  csse_data <- csse_data %>%
    dplyr::select(UID, iso2, iso3, Province_State, Country_Region, source, Latitude, Longitude, Update, !!value_col_name)
  
  validation_date <- Sys.getenv("VALIDATION_DATE")
  if ( validation_date != '' ) {
    print(paste("(DataUtils.R) Limiting CSSE global data to:", validation_date, sep=" "))
    csse_data <- csse_data %>% dplyr::filter( Update < validation_date )
  }
  
  return(csse_data)
}



##'
##' Pull global case and death count data from JHU CSSE
##'
##' Pulls the CSSE cumulative case count and death data. Calculates incident counts.
##'
##' Data preview:
##'  $ Update     : Date "2019-12-01" "2019-12-02" ...
##'  $ UID        : chr "AFG" "AFG" "AFG" "AFG" ...
##'  $ iso2       : chr  "AF" "AF" "AF" "AF" ...
##'  $ iso3       : chr  "AFG" "AFG" "AFG" "AFG" ...
##'  $ Latitude   : num  "AF" "AF" "AF" "AF" ...
##'  $ Longitude  : num  "AFG" "AFG" "AFG" "AFG" ...
##'  $ Confirmed  : num  0 0 0 0 0 0 0 0 0 0 ...
##'  $ Deaths     : num  0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidI     : num  0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidDeath : num  0 0 0 0 0 0 0 0 0 0 ...
##'  $ Country_Region  : chr  "Afghanistan" "Afghanistan" "Afghanistan" "Afghanistan" ...
##'  $ Province_State  : chr NA NA NA NA ...
##'  $ source     : chr "AFG" "AFG" "AFG" "AFG" ...
##'
##' @return the case and deaths data frame
##'
##' @importFrom dplyr rename group_modify group_by full_join select arrange bind_rows mutate
##'
##' @export
##' 
get_CSSE_global_data <- function(case_data_filename = "data/case_data/jhucsse_case_data_crude.csv",
                              death_data_filename = "data/case_data/jhucsse_death_data_crude.csv",
                              append_wiki = TRUE){
  
  CSSE_GLOBAL_CASE_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  CSSE_GLOBAL_DEATH_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  csse_global_case <- download_CSSE_global_data(case_data_filename, CSSE_GLOBAL_CASE_DATA_URL, "Confirmed")
  csse_global_death <- download_CSSE_global_data(death_data_filename, CSSE_GLOBAL_DEATH_DATA_URL, "Deaths")

  csse_global_data <- dplyr::full_join(csse_global_case, csse_global_death)
  csse_global_data <- dplyr::select(csse_global_data, UID, iso2, iso3, Province_State, Country_Region, Latitude, Longitude, Update, source, Confirmed, Deaths)
  csse_global_data <- dplyr::arrange(csse_global_data, Country_Region, Province_State, Update)

  if (append_wiki){
    data("wikipedia_cases", package = "covidcommon")
    wikipedia_cases <- wikipedia_cases %>%
      dplyr::mutate(Country_Region = ifelse(Country_Region=="Mainland China", "China", Country_Region), Update = as.Date(Update), iso2 = "CN", iso3 = "CHN", Latitude = 30.9756, Longitude = 112.2707) %>%
      dplyr::mutate(UID = ifelse(!is.na(Province_State), paste0(iso3, "-", Province_State), iso3)) %>%
      dplyr::select(-Suspected)

    csse_global_data <- dplyr::bind_rows(csse_global_data, wikipedia_cases)
    csse_global_data <- dplyr::arrange(csse_global_data, Country_Region, Province_State, Update)
  }

  # Create columns incidI and incidDeath
  csse_global_data <- dplyr::group_modify(
    dplyr::group_by(
      csse_global_data,
      Country_Region,
      Province_State
    ),
    function(.x,.y){
      .x$incidI = c(.x$Confirmed[1],diff(.x$Confirmed))
      .x$incidDeath = c(.x$Deaths[1],diff(.x$Deaths,))
      return(.x)
    }
  )

  # Fix incidence counts that go negative and NA values or missing dates
  csse_global_data <- fix_negative_counts_global(csse_global_data, "Confirmed", "incidI")
  csse_global_data <- fix_negative_counts_global(csse_global_data, "Deaths", "incidDeath")

  return(csse_global_data)
}



##'
##' Download Reich Lab data
##'
##' Downloads the Reich Lab's US case and death count data
##'
##' @param filename where case data will be stored
##' @param url URL to CSV on Reich Lab website
##' @return data frame
##'
##' @importFrom dplyr select rename mutate filter
##' @importFrom lubridate mdy
##' @importFrom readr read_csv col_character
##' @importFrom magrittr %>%
##' @importFrom stringr str_pad str_sub str_length
##' @importFrom tibble as_tibble
##' @importFrom cdlTools fips
##'
download_reichlab_data <- function(filename, url, value_col_name){
  
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  message(paste("Downloading", url, "to", filename))
  download.file(url, filename, "auto")

  reichlab_data <- readr::read_csv(filename, col_types = list("location" = col_character())) %>% 
    tibble::as_tibble() 
  reichlab_data <- reichlab_data %>%
    dplyr::mutate(Update = as.Date(date),
                  source = cdlTools::fips(stringr::str_sub(location, 1, 2), to = "Abbreviation"),
                  scale = ifelse(nchar(location)==2, "state", "county")) %>%
    dplyr::filter(location != "US") %>%
    dplyr::rename(!!value_col_name := value,
                  FIPS = location) %>%
    dplyr::mutate(FIPS = ifelse(stringr::str_length(FIPS)<=2, paste0(FIPS, "000"), stringr::str_pad(FIPS, 5, pad = "0"))) %>%
    dplyr::select(FIPS, source, scale, Update, !!value_col_name)

  validation_date <- Sys.getenv("VALIDATION_DATE")
  if ( validation_date != '' ) {
    print(paste("(DataUtils.R) Limiting Reich Lab data to:", validation_date, sep=" "))
    reichlab_data <- reichlab_data %>% dplyr::filter( Update < validation_date )
  }

  return(reichlab_data)
}




##'
##' Pull state-level case and death count data from Reich Lab
##'
##' Pulls the Reich Lab incident and cumulative case count and death data. 
##'
##' Returned data preview:
##' tibble 
##'  $ Update     : Date "2020-01-22" "2020-01-23" ...
##'  $ Confirmed  : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ Deaths     : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidI     : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidDeath : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ FIPS       : chr "01000" "01000" "01000" ...
##'  $ source     : chr "NY" "NY" "NY" "NY" ...
##'
##' @return the case and deaths data frame
##'
##' @importFrom dplyr full_join select filter arrange
##'
##' @export
##' 
get_reichlab_st_data <- function(cum_case_filename = "data/case_data/rlab_cum_case_data.csv",
                  cum_death_filename = "data/case_data/rlab_cum_death_data.csv",
                  inc_case_filename = "data/case_data/rlab_inc_case_data.csv",
                  inc_death_filename = "data/case_data/rlab_inc_death_data.csv"){
  
  REICHLAB_CUM_CASE_DATA_URL <-  "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Cumulative%20Cases.csv"
  REICHLAB_CUM_DEATH_DATA_URL <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Cumulative%20Deaths.csv"
  REICHLAB_INC_CASE_DATA_URL <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Cases.csv"
  REICHLAB_INC_DEATH_DATA_URL <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Deaths.csv"

  rlab_cum_case <- download_reichlab_data(cum_case_filename, REICHLAB_CUM_CASE_DATA_URL, "Confirmed")
  rlab_cum_death <- download_reichlab_data(cum_death_filename, REICHLAB_CUM_DEATH_DATA_URL, "Deaths")
  rlab_inc_case <- download_reichlab_data(inc_case_filename, REICHLAB_INC_CASE_DATA_URL, "incidI")
  rlab_inc_death <- download_reichlab_data(inc_death_filename, REICHLAB_INC_DEATH_DATA_URL, "incidDeath")

  rlab_st_data <- dplyr::full_join(rlab_cum_case, rlab_cum_death) %>%
    dplyr::full_join(rlab_inc_case) %>%
    dplyr::full_join(rlab_inc_death) %>%
    dplyr::filter(scale == "state") %>%
    dplyr::select(Update, Confirmed, Deaths, incidI, incidDeath, FIPS, source) %>%
    dplyr::mutate(incidDeath = ifelse(is.na(incidDeath), 0, incidDeath),
                  incidI = ifelse(is.na(incidI), 0, incidI),
                  Confirmed = ifelse(is.na(Confirmed) & Update < "2020-02-01", 0, Confirmed),
                  Deaths = ifelse(is.na(Deaths) & Update < "2020-02-01", 0, Deaths)) %>%
    dplyr::arrange(source, FIPS, Update)

  return(rlab_st_data)

}



##'
##' Pull county-level case and death count data from Reich Lab
##'
##' Pulls the Reich Lab incident and cumulative case count and death data. 
##'
##' Returned data preview:
##' tibble 
##'  $ Update     : Date "2020-01-22" "2020-01-23" ...
##'  $ Confirmed  : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ Deaths     : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidI     : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ incidDeath : num 0 0 0 0 0 0 0 0 0 0 ...
##'  $ FIPS       : chr "01001" "01001" "01001" ...
##'  $ source     : chr "NY" "NY" "NY" "NY" ...
##'
##' @return the case and deaths data frame
##'
##' @importFrom dplyr full_join select filter arrange
##'
##' @export
##' 
get_reichlab_cty_data <- function(cum_case_filename = "data/case_data/rlab_cum_case_data.csv",
                  cum_death_filename = "data/case_data/rlab_cum_death_data.csv",
                  inc_case_filename = "data/case_data/rlab_inc_case_data.csv",
                  inc_death_filename = "data/case_data/rlab_inc_death_data.csv"){
  
  REICHLAB_CUM_CASE_DATA_URL <-  "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Cumulative%20Cases.csv"
  REICHLAB_CUM_DEATH_DATA_URL <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Cumulative%20Deaths.csv"
  REICHLAB_INC_CASE_DATA_URL <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Cases.csv"
  REICHLAB_INC_DEATH_DATA_URL <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Deaths.csv"

  rlab_cum_case <- download_reichlab_data(cum_case_filename, REICHLAB_CUM_CASE_DATA_URL, "Confirmed")
  rlab_cum_death <- download_reichlab_data(cum_death_filename, REICHLAB_CUM_DEATH_DATA_URL, "Deaths")
  rlab_inc_case <- download_reichlab_data(inc_case_filename, REICHLAB_INC_CASE_DATA_URL, "incidI")
  rlab_inc_death <- download_reichlab_data(inc_death_filename, REICHLAB_INC_DEATH_DATA_URL, "incidDeath")

  rlab_cty_data <- dplyr::full_join(rlab_cum_case, rlab_cum_death) %>%
    dplyr::full_join(rlab_inc_case) %>%
    dplyr::full_join(rlab_inc_death) %>%
    dplyr::filter(scale == "county") %>%
    dplyr::select(Update, Confirmed, Deaths, incidI, incidDeath, FIPS, source) %>%
    dplyr::mutate(incidDeath = ifelse(is.na(incidDeath), 0, incidDeath),
                  incidI = ifelse(is.na(incidI), 0, incidI),
                  Confirmed = ifelse(is.na(Confirmed) & Update < "2020-02-01", 0, Confirmed),
                  Deaths = ifelse(is.na(Deaths) & Update < "2020-02-01", 0, Deaths)) %>%
    dplyr::arrange(source, FIPS, Update)

  return(rlab_cty_data)

}

##'
##' Wrapper function to pull data from different sources
##'
##' Pulls a groundtruth dataset with the variables specified
##'
##' @param source name of data source: reichlab, usafacts, csse
##' @param scale geographic scale: US county, US state, country (csse only), complete (csse only)
##' @param variables vector that may include one or more of the following variable names: Confirmed, Deaths, incidI, incidDeath
##' @return data frame
##'
##' @importFrom dplyr select mutate filter group_by summarise_if bind_rows
##' @importFrom magrittr %>%
##' @importFrom stringr str_sub
##'
##'
##' @export
##' 
get_groundtruth_from_source <- function(source = "reichlab", scale = "US county", variables = c("Confirmed", "Deaths", "incidI", "incidDeath")){

  if(source == "reichlab" & scale == "US county"){

    rc <- get_reichlab_cty_data() %>%
      dplyr::select(Update, FIPS, source, !!variables) %>%
      tidyr::drop_na(tidyselect::everything())

  } else if(source == "reichlab" & scale == "US state"){

    rc <- get_reichlab_st_data() %>%
      dplyr::select(Update, FIPS, source, !!variables) %>%
      tidyr::drop_na(tidyselect::everything())

  } else if(source == "usafacts" & scale == "US county"){

    rc <- get_USAFacts_data() %>%
      dplyr::select(Update, FIPS, source, !!variables) %>%
      tidyr::drop_na(tidyselect::everything())

  } else if(source == "usafacts" & scale == "US state"){

    rc <- get_USAFacts_data() %>%
      dplyr::select(Update, FIPS, source, !!variables) %>%
      dplyr::mutate(FIPS = stringr::str_sub(FIPS, 1, 2)) %>%
      dplyr::group_by(Update, FIPS, source) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      tidyr::drop_na(tidyselect::everything())

  } else if(source == "csse" & scale == "US county"){

    rc <- get_CSSE_US_data() %>%
      dplyr::select(Update, FIPS, source, !!variables) %>%
      tidyr::drop_na(tidyselect::everything())

  } else if(source == "csse" & scale == "US state"){

    rc <- get_CSSE_global_data() %>%
      dplyr::select(Update, FIPS, source, !!variables) %>%
      dplyr::mutate(FIPS = stringr::str_sub(FIPS, 1, 2)) %>%
      dplyr::group_by(Update, FIPS, source) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      tidyr::drop_na(tidyselect::everything())

  } else if(source == "csse" & scale == "country"){

    rc <- get_CSSE_global_data() %>%
      dplyr::select(UID, iso2, iso3, Province_State, Country_Region, Latitude, Longitude, Update, source, !!variables) %>%
      tidyr::drop_na(tidyselect::everything())

  } else if(source == "csse" & scale == "complete"){

    us <- get_CSSE_US_matchGlobal_data() %>%
      dplyr::select(Update, UID, iso2, iso3, Latitude, Longitude, source, !!variables, Country_Region, Province_State, source) 
    rc <- get_CSSE_global_data() %>%
      dplyr::select(Update, UID, iso2, iso3, Latitude, Longitude, source, !!variables, Country_Region, Province_State, source) %>%
      dplyr::bind_rows(us) %>%
      tidyr::drop_na(tidyselect::everything())
  
  } else{
    warning(print(paste("The combination of ", source, "and", scale, "is not valid. Returning NULL object.")))
    rc <- NULL
  }

  return(rc)

}

##'
##' Pull CSSE US data in format similar to that of global data
##'
##' Pulls the CSSE US confirmed cases and deaths and calculates incident cases and deaths, putting them into a verbose geographic format like that with the global dataframe
##' @return data frame
##'
##'
##'
##' @export
##' 
get_CSSE_US_matchGlobal_data <- function(){

  warning("This function is still in progress. Returning NA-filled dataframe.")
  rc <- data.frame(Update = NA, UID = NA, iso2 = NA, iso3 = NA, Latitude = NA, Longitude = NA, source = NA, Confirmed = NA, Deaths = NA, incidI = NA, incidDeath = NA, Country_Region = NA, Province_State = NA, source = NA)
  return(rc)

}