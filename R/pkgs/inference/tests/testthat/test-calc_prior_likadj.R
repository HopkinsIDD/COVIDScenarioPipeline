context("test-calc_prior_likadj")


test_that("Usupported distributions throw errors",  {

    expect_error(calc_prior_likadj((1:5)/10, "lognormal", c(1,1)))

})


test_that("The log likelihood is correct for the given distirbution", {

    params <- runif(100, -2,2)

    ##normal distribution
    expect_that(sum(calc_prior_likadj(params, "normal", c(0,1))),
               equals(sum(dnorm(params, 0, 1, log=TRUE))))

})


