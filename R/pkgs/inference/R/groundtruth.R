#' Function to load US COVID data from JHUCSSE
#' @param data_path Path where to write the data
#'
#' @return NULL
#'
#' @export
get_ground_truth_file <- function(data_path, cache = TRUE) {
  data_dir <- dirname(data_path)
  if(!dir.exists(data_dir)){
    suppressWarnings(dir.create(data_dir,recursive=TRUE))
  }
  if(!(file.exists(data_path) & cache)){
    message("*** Loading Data from USAFacts\n")
    cases_deaths <- suppressMessages(covidcommon::get_USAFacts_data(tempfile(),tempfile()))
    cases_deaths  <- dplyr::arrange(
      dplyr::rename(
        dplyr::mutate(
          cases_deaths,
          Update = lubridate::ymd(Update)
        ),
        date = Update,
        cumConfirmed = Confirmed,
        cumDeaths = Deaths,
        confirmed_incid = incidI,
        death_incid = incidDeath
      ),
      date
    )
    if(any(is.na(cases_deaths$cumConfirmed))){
      cases_deaths$cumConfirmed[is.na(cases_deaths$cumConfirmed)] <- 0
    }
    if(any(is.na(cases_deaths$cumDeaths))){
      cases_deaths$cumDeaths[is.na(cases_deaths$cumDeaths)] <- 0
    }
    readr::write_csv(cases_deaths, data_path)
    rm(cases_deaths)
    message("*** DONE Loading Data \n")
  } else {
    message("*** USING CACHED Data\n")
  }

  return()
}

#' Function to load US COVID data from USAfacts
#' @param data_path Path where to write the data
#'
#' @export
get_ground_truth <- function(data_path, fips_codes, fips_column_name, start_date, end_date, cache = TRUE, use_USAfacts=TRUE){
  
  if (use_USAfacts){
    get_ground_truth_file(data_path,cache)
  }
  
  rc <- suppressMessages(readr::read_csv(data_path, col_types = list(FIPS = readr::col_character())))
  rc <- dplyr::filter(
    rc,
    FIPS %in% fips_codes,
    date >= start_date,
    date <= end_date
  )
  rc <- dplyr::right_join(
    rc,
    tidyr::expand_grid(
      FIPS = unique(rc$FIPS),
      date = unique(rc$date)
    )
  )
  rc <- dplyr::mutate_if(rc, is.numeric, dplyr::coalesce, 0)
  names(rc)[names(rc) == "FIPS"] <- fips_column_name
  return(rc)
}
