context("cu_death_forecast")


test_that("cum_death_forecast gives correct results, for one loc 100 and on 0", {
  sim_data <- dplyr::tibble(incidD=rpois(200, 10),
                           time=rep(as.Date("2020-01-01")+1:100,2),
                           sim_num=1,
                           USPS=rep(c("NY","WA"), each=100))
  
  cum_data <- dplyr::tibble( USPS=c("NY","WA"),
                          cumDeaths=c(0,100))
  
  

  rc <- cum_death_forecast(sim_data ,
                         "2019-12-31",
                         cum_data,
                         "USPS")
  
  expect_that(prod(rc$cum_deaths_corr[rc$USPS=="NY"]==cumsum(sim_data$incidD[sim_data$USPS=="NY"])),
              equals(1))
  
  
  expect_that(prod(rc$cum_deaths_corr[rc$USPS=="WA"]-cumsum(sim_data$incidD[sim_data$USPS=="WA"]) == 100),
              equals(1))

})

test_that("Giving a date results in an appropriate forecast only for the future", {
  sim_data <- dplyr::tibble(incidD=rpois(200, 10),
                            time=rep(as.Date("2020-01-01")+0:99,2),
                            sim_num=1,
                            USPS=rep(c("NY","WA"), each=100))
  
  cum_data <- dplyr::tibble( USPS=c("NY","WA"),
                             cumDeaths=c(0,100))
  
  
  
  rc <- cum_death_forecast(sim_data ,
                           "2020-01-15",
                           cum_data,
                           "USPS")
  

  expect_that(min(rc$time), equals(as.Date("2020-01-16")))
  expect_that(nrow(rc),equals(2*85))
  expect_that(max(rc$cum_deaths_corr[rc$USPS=="NY"]),
             equals(sum(sim_data$incidD[sim_data$time>"2020-01-15" & sim_data$USPS=="NY"])))
  expect_that(max(rc$cum_deaths_corr[rc$USPS=="WA"]),
              equals(sum(sim_data$incidD[sim_data$time>"2020-01-15" & sim_data$USPS=="WA"])+100))
  
})
