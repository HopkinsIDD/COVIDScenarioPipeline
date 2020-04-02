test_that("create_delay_frame works",{
  require(data.table)
  X <- "input"
  varname <- "output"
  p_X <- 0.1
  X_pars <- c(10,1)
  data_ <- data.table(
    input = 1:1e6,
    time = lubridate::ymd('2020-01-01') + 1:1e6,
    uid = 1:1e6
  )

  expect_error({
    hosp_create_delay_frame(X=X,varname=varname,p_X=p_X,data_=data_,X_pars=X_pars)
  }, NA)

  expect_lt({
    rc <- hosp_create_delay_frame(X=X,varname=varname,p_X=p_X,data_=data_,X_pars=X_pars)
    abs(mean(rc[[paste0('incid',varname)]] - data_[[X]] * p_X))
  }, .75)
})
