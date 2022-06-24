context("groundtruth")
test_that("get_ground_truth_file creates a file",{
  data_path <- tempfile()
  if(file.exists(data_path)){
    warning("Testing should not be using a file that already exists")
    file.remove(data_path)
  }
  expect_error(get_ground_truth_file(data_path,FALSE, variant_filename = NULL),NA)
  expect_equal(file.exists(data_path),TRUE)

  expect_error(get_ground_truth_file(data_path,TRUE, variant_filename = NULL),NA)
  expect_equal(file.exists(data_path),TRUE)
})

test_that("get_ground_truth returns an appropriate data frame",{
  data_path <- tempfile()
  fips_codes <- c('36061')
  fips_column_name <- "test_fips_column"
  start_date <- lubridate::ymd("2020-04-15")
  end_date <- lubridate::ymd("2020-04-30")
  expect_error({get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,FALSE, variant_filename = NULL)},NA)
  expect_error({get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,TRUE, variant_filename = NULL)},NA)
  expect_equal({
    all(c(fips_column_name,"date","incidI","incidDeath", "Confirmed", "Deaths") %in% names(get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,TRUE, variant_filename = NULL)))
  },TRUE)
  expect_equal({
    all(c(fips_column_name,"date","incidI","incidDeath", "Confirmed", "Deaths") %in% names(get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,TRUE, variant_filename = NULL)))
  },TRUE)
  expect_gt(nrow(get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,TRUE, variant_filename = NULL)),0)
  expect_equal(all(get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,TRUE, variant_filename = NULL)[[fips_column_name]] %in% fips_codes),TRUE)
  expect_equal(all(get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,TRUE, variant_filename = NULL)$date >= start_date),TRUE)
  expect_equal(all(get_ground_truth(data_path,fips_codes,fips_column_name,start_date,end_date,TRUE, variant_filename = NULL)$date <= end_date),TRUE)
})
