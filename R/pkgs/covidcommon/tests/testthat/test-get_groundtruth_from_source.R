library(dplyr)
library(magrittr)
test_that("get_groundtruth_from_source works", {
  usaf <- get_groundtruth_from_source(source = "usafacts", scale = "US state", variables = c("Confirmed", "incidI"))
  usaf_cty <- get_groundtruth_from_source(source = "usafacts", scale = "US county", variables = c("Confirmed", "incidI"))
  csse_cty <- get_groundtruth_from_source(source = "csse", scale = "US county")
  csse_comp <- get_groundtruth_from_source(source = "csse", scale = "complete")
  fake <- get_groundtruth_from_source(source = "fakesource")

  expect_null(fake)
  usaf_cty_processed <- usaf_cty %>%
    dplyr::mutate(FIPS = stringr::str_sub(FIPS, 1, 2)) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum)
  expect_true(sum(usaf$Confirmed) == sum(usaf_cty_processed$Confirmed))

  # No NA
  expect_false(any(is.na(usaf$Confirmed)))
  expect_false(any(is.na(usaf$incidI)))
  expect_false(any(is.na(csse_cty$Confirmed)))
  expect_false(any(is.na(csse_cty$incidI)))
  expect_false(any(is.na(csse_cty$Deaths)))
  expect_false(any(is.na(csse_cty$incidDeath)))
  expect_false(any(is.na(csse_comp$Confirmed)))
  expect_false(any(is.na(csse_comp$incidI)))
  expect_false(any(is.na(csse_comp$Deaths)))
  expect_false(any(is.na(csse_comp$incidDeath)))

  expect_type(usaf$FIPS, "character")
  expect_type(usaf$Confirmed, "numeric")
  expect_type(csse_cty$Update, "date")
  expect_type(csse_cty$source, "character")
  expect_type(csse_comp$source, "character")

  # csse county should be included in csse comp
  expect_true(nrow(csse_comp) > nrow(csse_cty))

  # No negative incidence
  expect_gte(min(csse_cty$incidI), 0)
  expect_gte(min(csse_comp$incidDeath), 0)

  # Max of cumulative is not infinity
  expect_false(is.infinite(max(csse_cty$Confirmed)))
  expect_false(is.infinite(max(csse_cty$Deaths)))
  expect_false(is.infinite(max(csse_comp$Confirmed)))
  expect_false(is.infinite(max(csse_comp$Deaths)))

 
})