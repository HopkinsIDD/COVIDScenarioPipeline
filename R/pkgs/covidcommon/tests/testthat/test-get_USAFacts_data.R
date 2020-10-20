library(dplyr)
Sys.setenv(VALIDATION_DATE="2020-08-01")

test_that("get_USAFacts_data works", {
  usaf <- get_USAFacts_data()

  # No NA
  expect_false(any(is.na(usaf$Confirmed)))
  expect_false(any(is.na(usaf$incidI)))
  expect_false(any(is.na(usaf$Deaths)))
  expect_false(any(is.na(usaf$incidDeath)))

  # No negative incidence
  expect_gte(min(usaf$incidI), 0)
  expect_gte(min(usaf$incidDeath), 0)

  # Max of cumulative is not infinity
  expect_false(is.infinite(max(usaf$Confirmed)))
  expect_false(is.infinite(max(usaf$Deaths)))

  # Cumsum of incidence is equal to cumulative count
  usaf_cumsums <- usaf %>%
                    dplyr:: group_by(FIPS) %>%
                    dplyr::mutate(incidICumSum = cumsum(incidI),
                                  incidDeathCumSum = cumsum(incidDeath)) %>%
                    dplyr::ungroup()
  expect_equal(usaf_cumsums$incidICumSum, usaf$Confirmed)
  expect_equal(usaf_cumsums$incidDeathCumSum, usaf$Deaths)
  
})
test_that("download_USAFacts_data cases and deaths will join",{

  case_data_filename = "data/case_data/USAFacts_case_data.csv"
  death_data_filename = "data/case_data/USAFacts_death_data.csv"
  USAFACTS_CASE_DATA_URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  USAFACTS_DEATH_DATA_URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
  
  usafacts_case <- download_USAFacts_data(case_data_filename, USAFACTS_CASE_DATA_URL, "Confirmed")
  usafacts_death <- download_USAFacts_data(death_data_filename, USAFACTS_DEATH_DATA_URL, "Deaths")

  expect_true(any(names(usafacts_case) %in% names(usafacts_death)))
})
test_that("filtering by VALIDATION_DATE results in less data",{

  case_data_filename = "data/case_data/USAFacts_case_data.csv"
  USAFACTS_CASE_DATA_URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  
  usafacts_case_all <- download_USAFacts_data(case_data_filename, USAFACTS_CASE_DATA_URL, "Confirmed")

  Sys.setenv(VALIDATION_DATE="2020-03-01")
  
  usafacts_case_filtered <- download_USAFacts_data(case_data_filename, USAFACTS_CASE_DATA_URL, "Confirmed")
  
  expect_true(nrow(usafacts_case_filtered) < nrow(usafacts_case_all))
})
