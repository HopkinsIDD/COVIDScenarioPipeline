library(dplyr)
Sys.setenv(VALIDATION_DATE="2020-10-01")

test_that("get_hhsCMU_allHosp_st_data works", {
  cmu <- get_hhsCMU_allHosp_st_data()

  expect_true(all(c("Update", "FIPS", "source", "previous_day_admission_adult_covid_confirmed", "previous_day_admission_pediatric_covid_confirmed", "previous_day_admission_adult_covid_suspected", "previous_day_admission_pediatric_covid_suspected", "total_adult_patients_hosp_confirmed_covid", "total_pediatric_patients_hosp_confirmed_covid", "total_adult_patients_hosp_confirmed_suspected_covid", "total_pediatric_patients_hosp_confirmed_suspected_covid") %in% names(cmu)))
  expect_true(max(cmu$Update) <= as.Date("2020-10-01"))
  expect_equal(min(cmu$Update), as.Date("2020-01-01"))
  
})
test_that("get_hhsCMU_incidH_st_data works",{

  incidH <- get_hhsCMU_incidH_st_data()

  expect_true(all(c("Update", "FIPS", "source", "incidH_confirmed", "incidH_all") %in% names(incidH)))
  expect_equal(any(is.na(incidH$incidH_confirmed)), FALSE)
  expect_equal(any(is.na(incidH$incidH_all)), FALSE)

})
test_that("get_hhsCMU_hospCurr_st_data works",{

  hospCurr <- get_hhsCMU_hospCurr_st_data()
  expect_true(all(c("Update", "FIPS", "source", "hospCurr_confirmed", "hospCurr_all") %in% names(hospCurr)))
  expect_equal(any(is.na(hospCurr$hospCurr_confirmed)), FALSE)
  expect_equal(any(is.na(hospCurr$hospCurr_all)), FALSE)
  
})
test_that("filtering by VALIDATION_DATE results in less data",{

  big_data <- get_hhsCMU_allHosp_st_data()

  Sys.setenv(VALIDATION_DATE="2020-03-01")
  
  small_data <- get_hhsCMU_allHosp_st_data()
  
  expect_true(nrow(small_data) < nrow(big_data))
})
