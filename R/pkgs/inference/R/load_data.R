#' Function to load US COVID data from JHUCSSE
#' @param data_path Path where to write the data
#'
#' @return NULL
#' @export
load_data <- function(data_path, geodata, obs_nodename) {
  cat("*** Loading Data \n")
  cases_deaths <- covidcommon::get_USAFacts_data()
  cases_deaths  <-
    cases_deaths %>%
    dplyr::mutate(date = lubridate::ymd(Update)) %>%
    dplyr::filter(FIPS %in% geodata[[obs_nodename]]) %>%
    dplyr::rename(
      cumConfirmed = Confirmed,
      cumDeaths = Deaths
    ) %>%
    dplyr::arrange(date)
  if(any(is.na(cases_deaths$cumConfirmed))){
    cases_deaths$cumConfirmed[is.na(cases_deaths$cumConfirmed)] <- 0
  }
  if(any(is.na(cases_deaths$cumDeaths))){
    cases_deaths$cumDeaths[is.na(cases_deaths$cumDeaths)] <- 0
  }
  cases_deaths <- cases_deaths %>%
    dplyr::group_by(FIPS) %>%
    dplyr::group_modify(
      function(.x,.y){
        .x$cumConfirmed = cummax(.x$cumConfirmed)
        .x$conf_incid = c(.x$cumConfirmed[1],diff(.x$cumConfirmed))
        .x$cumDeaths = cummax(.x$cumDeaths)
        .x$death_incid = c(.x$cumDeaths[1],diff(.x$cumDeaths,))
        return(.x)
      }
    )
  names(cases_deaths)[names(cases_deaths) == 'FIPS'] <- as.character(obs_nodename)
  write_csv(cases_deaths, data_path)
  rm(cases_deaths)
  cat("*** DONE Loading Data \n")
}
