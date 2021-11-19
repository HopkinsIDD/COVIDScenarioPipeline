context("groundtruth")
test_that("get_ground_truth_file creates a file",{
  data_path <- tempfile()
  if(file.exists(data_path)){
    warning("Testing should not be using a file that already exists")
    file.remove(data_path)
  }
  expect_error(get_ground_truth_file(data_path,cache=FALSE),NA)
  expect_equal(file.exists(data_path),TRUE)

  expect_error(get_ground_truth_file(data_path,cache=TRUE),NA)
  expect_equal(file.exists(data_path),TRUE)
})

test_that("get_ground_truth returns an appropriate data frame",{
  data_path <- tempfile()
  fips_codes <- c('36061')
  fips_column_name <- "test_fips_column"
  start_date <- lubridate::ymd("2020-04-15")
  end_date <- lubridate::ymd("2020-04-30")
  new_vars <- c("cumConfirmed", "cumDeaths", "confirmed_incid","death_incid")
  expect_error({get_ground_truth(data_path = data_path,fips_codes = fips_codes,fips_column_name = fips_column_name,start_date = start_date,end_date = end_date, new_vars = new_vars, cache=FALSE)},NA)
  expect_error({get_ground_truth(data_path = data_path,fips_codes = fips_codes, fips_column_name = fips_column_name,start_date = start_date,end_date = end_date, new_vars = new_vars, cache=TRUE)},NA)
  expect_equal({
    all(c(fips_column_name,"date","cumConfirmed", "cumDeaths", "confirmed_incid","death_incid") %in% names(get_ground_truth(data_path = data_path,fips_codes = fips_codes, fips_column_name = fips_column_name,start_date = start_date,end_date = end_date, new_vars = new_vars, cache=TRUE)))
  },TRUE)
  expect_gt(nrow(get_ground_truth(data_path = data_path,fips_codes = fips_codes, fips_column_name = fips_column_name,start_date = start_date,end_date = end_date, new_vars = new_vars,cache=TRUE)),0)
  expect_equal(all(get_ground_truth(data_path = data_path,fips_codes = fips_codes, fips_column_name = fips_column_name,start_date = start_date,end_date = end_date, new_vars = new_vars,cache=TRUE)[[fips_column_name]] %in% fips_codes),TRUE)
  expect_equal(all(get_ground_truth(data_path = data_path,fips_codes = fips_codes, fips_column_name = fips_column_name,start_date = start_date,end_date = end_date, new_vars = new_vars,cache=TRUE)$date >= start_date),TRUE)
  expect_equal(all(get_ground_truth(data_path = data_path,fips_codes = fips_codes, fips_column_name = fips_column_name,start_date = start_date,end_date = end_date, new_vars = new_vars,cache=TRUE)$date <= end_date),TRUE)
})
