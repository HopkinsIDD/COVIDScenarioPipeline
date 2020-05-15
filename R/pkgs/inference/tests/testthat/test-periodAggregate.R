context("periodAggregate")

test_that("perriodAggregate returns the same data when aggregating to days", {
    test_data <- rbinom(100,100,.1)
    test_dates <- as.Date("2020-02-01")+1:100

    expect_that(as.numeric(periodAggregate(test_data, test_dates, period_unit  = "days", period_k=1, aggregator="sum")),
                equals(test_data))
})


test_that("periodAggregate returns the right number when aggregating 1s to a partticular level", {
    test_data <- rep(1,100)
    test_dates <- as.Date("2020-02-01")+1:100

    for (k in c(1,2,4)) { #must be divisors of 100
        expect_that(prod(as.numeric(periodAggregate(test_data, test_dates, period_unit  = "days", period_k=k, aggregator="sum"))==k),
                    equals(1))
    }

    ##Make the data exactly weeks starting on a monday
    test_data <- rep(1,21)
    test_dates <- as.Date("2020-05-11")+0:20

    expect_that(prod(as.numeric(periodAggregate(test_data, test_dates, period_unit  = "weeks", period_k=1, aggregator="sum"))==7),
                equals(1))

    ##Make data span 3 months
    test_dates <- seq(as.Date("2020-02-01"),as.Date("2020-04-30"), by="day")
    test_data <- rep(1, length(test_dates))

     expect_that(as.numeric(periodAggregate(test_data, test_dates, period_unit  = "months", period_k=1, aggregator="sum")),
                equals(c(29,31,30)))

})

