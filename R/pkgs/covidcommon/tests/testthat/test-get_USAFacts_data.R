library(dplyr)
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
