library(dplyr)
Sys.setenv(VALIDATION_DATE="2020-08-01")

test_that("get_CSSE_US_data works", {
  csse <- get_CSSE_US_data()

  # No NA
  expect_false(any(is.na(csse$Confirmed)))
  expect_false(any(is.na(csse$incidI)))
  expect_false(any(is.na(csse$Deaths)))
  expect_false(any(is.na(csse$incidDeath)))

  # No negative incidence
  expect_gte(min(csse$incidI), 0)
  expect_gte(min(csse$incidDeath), 0)

  # Max of cumulative is not infinity
  expect_false(is.infinite(max(csse$Confirmed)))
  expect_false(is.infinite(max(csse$Deaths)))

  # Cumsum of incidence is equal to cumulative count
  csse_cumsums <- csse %>%
                    dplyr:: group_by(FIPS) %>%
                    dplyr::mutate(incidICumSum = cumsum(incidI),
                                  incidDeathCumSum = cumsum(incidDeath)) %>%
                    dplyr::ungroup()
  expect_equal(csse_cumsums$incidICumSum, csse$Confirmed)
  expect_equal(csse_cumsums$incidDeathCumSum, csse$Deaths)
  
})
test_that("download_CSSE_US_data cases and deaths will join",{

  case_data_filename = "data/case_data/jhucsse_us_case_data_crude.csv"
  death_data_filename = "data/case_data/jhucsse_us_death_data_crude.csv"
  CSSE_US_CASE_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  CSSE_US_DEATH_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  
  csse_us_case <- download_CSSE_US_data(case_data_filename, CSSE_US_CASE_DATA_URL, "Confirmed")
  csse_us_death <- download_CSSE_US_data(death_data_filename, CSSE_US_DEATH_DATA_URL, "Deaths")

  expect_true(any(names(csse_us_case) %in% names(csse_us_death)))
})
test_that("filtering by VALIDATION_DATE results in less data",{

  case_data_filename = "data/case_data/jhucsse_us_case_data_crude.csv"
  CSSE_US_CASE_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  
  csse_us_case_all <- download_CSSE_US_data(case_data_filename, CSSE_US_CASE_DATA_URL, "Confirmed")

  Sys.setenv(VALIDATION_DATE="2020-03-01")
  
  csse_us_case_filtered <- download_CSSE_US_data(case_data_filename, CSSE_US_CASE_DATA_URL, "Confirmed")
  
  expect_true(nrow(csse_us_case_filtered) < nrow(csse_us_case_all))
})
