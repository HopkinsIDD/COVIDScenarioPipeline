context("periodAggregate")

day_period_unit_function <- c(lubridate::day,lubridate::month,lubridate::year)
day_period_unit_validator <- function(dates,units){return(TRUE)}

## This is wrong:
k_day_period_unit_function <- c(lubridate::day,lubridate::month,lubridate::year)
k_day_period_unit_validator <- function(dates, units) {
  return(length(unique(dates)) == 7)
}

month_period_unit_function <- c(lubridate::month,lubridate::year)
month_period_unit_validator <- function(dates, units, local_period_unit_function = month_period_unit_function) {
  first_date <- min(dates)
  last_date <- min(dates) + (length(unique(dates))-1)
  return(all(c(
    local_period_unit_function[[1]](first_date) != local_period_unit_function[[1]](first_date - 1)
  , local_period_unit_function[[1]](last_date) != local_period_unit_function[[1]](last_date + 1)
  )))
}

test_that("perriodAggregate returns the same data when aggregating to days", {
    test_data <- rbinom(100,100,.1)
    test_dates <- as.Date("2020-02-01")+1:100
    

    expect_that(
      as.numeric(periodAggregate(test_data, test_dates, period_unit_function  = day_period_unit_function, period_unit_validator = day_period_unit_validator, aggregator=sum)),
      equals(test_data)
    )
})


test_that("periodAggregate returns the right number when aggregating 1s to a partticular level", {
    test_data <- rep(1,100)
    test_dates <- as.Date("2020-02-01")+1:100

    for (k in c(1,2,4)) { #must be divisors of 100
        expect_that(prod(as.numeric(periodAggregate(test_data, test_dates, period_unit_function = k_day_period_unit_function, period_unit_validator = k_day_period_unit_validator, aggregator=sum))==k),
                    equals(1))
    }

    ##Make the data exactly weeks starting on a monday
    test_data <- rep(1,21)
    test_dates <- as.Date("2020-05-11")+0:20

    expect_that(prod(as.numeric(periodAggregate(test_data, test_dates, period_unit_function = k_day_period_unit_function, period_unit_validator = k_day_period_unit_validator, aggregator=sum))==7),
                equals(1))

    ##Make data span 3 months
    test_dates <- seq(as.Date("2020-02-01"),as.Date("2020-04-30"), by="day")
    test_data <- rep(1, length(test_dates))

     expect_that(as.numeric(periodAggregate(test_data, test_dates, period_unit_function = month_period_unit_function, period_unit_validator = month_period_unit_validator, aggregator=sum)),
                equals(c(29,31,30)))

})

