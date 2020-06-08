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

test_that("logit_normal behaves sensibly with  0s and 1s (i.e., does not return NAs", {
    
    tmp <- calc_prior_likadj(c(0,1),
                             "logit_normal",
                             c(0,1))

    print(tmp)

    expect_false(is.nan(sum(tmp)))
})
