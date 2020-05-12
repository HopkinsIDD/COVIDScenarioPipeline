context("perturb_seeding")

test_that("seeding date always stays within date bounds", {
    N <- 10000
    seeding <- data.frame(date=rep(as.Date("2020-02-01"), N),
                          place=1:N,
                          amount=rep(10,N))

    date_bounds <- as.Date(c("2020-01-31", "2020-02-02"))

    tmp <- perturb_seeding(seeding, 5, date_bounds=date_bounds)

    expect_that(prod(tmp$date>=date_bounds[1]),equals(1))
    expect_that(prod(tmp$date<=date_bounds[2]),equals(1))
})


test_that("the median of the seeding pertubations is 0 after 10000 sims", {
    N <- 10000
    seeding <- data.frame(date=rep(as.Date("2020-02-01"), N),
                          place=1:N,
                          amount=rep(10,N))

    date_bounds <- as.Date(c("2020-01-20", "2020-02-20"))

    tmp <- perturb_seeding(seeding, 5, date_bounds=date_bounds)

    expect_that(median(as.numeric(tmp$date-seeding$date)), equals(0))
    expect_that(median(as.numeric(tmp$amount-seeding$amount)), equals(0))


})



