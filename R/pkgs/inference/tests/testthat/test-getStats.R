context("getStats")

test_that("getStats returns identity for mean and sum with 1 day aggregation", {

    data<- data.frame(data=1:100,
                      date=as.Date("2020-01-01")+1:100)

    configs <- list(sumstat=list(
                        aggregator="sum",
                        period="1 days",
                        value_var="data",
                        remove_na=FALSE
                    ),
                    meanstat=list(
                        aggregator="mean",
                        period="1 days",
                        value_var="data",
                        remove_na=FALSE
                    ))



    tmp <- getStats(data, "date","value_var",stat_list=configs)

    expect_that(as.numeric(tmp$sumstat$value_var),equals(data$data))
    expect_that(as.numeric(tmp$meanstat$value_var),equals(data$data))
})


test_that("getStats returns correct means and totalsw hwen aggregating", {

    data<- data.frame(data=c(rep(10,29), rep(20,31), rep(30,30)),
                      date= seq(as.Date("2020-02-01"),as.Date("2020-04-30"), by="day"))

    configs <- list(sumstat=list(
                        aggregator="sum",
                        period="1 months",
                        value_var="data",
                        remove_na=FALSE
                    ),
                    meanstat=list(
                        aggregator="mean",
                        period="1 months",
                        value_var="data",
                        remove_na=FALSE
                    ))



    tmp <- getStats(data, "date","value_var",stat_list=configs)

    expect_that(as.numeric(tmp$sumstat$value_var),equals(c(290, 620, 900)))
    expect_that(as.numeric(tmp$meanstat$value_var),equals(c(10,20,30)))
})
