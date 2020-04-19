test_that("as_random_distribution works", {
  expect_true({
    rn <- as_random_distribution(list(distribution = "uniform", high = "0.9", low = .3))
    all(is.numeric(rn(100)) & (rn(100) >= .3) & (rn(100) <= .9))
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "poisson", high = "0.9", low = .3))
    rn(100)
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "binomial", high = "0.9", low = .3))
    rn(100)
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "binomial", size = 5, prob = "1/10"))
    all(is.numeric(rn(100)) & (rn(100) >= 0) & (rn(100) <= 5))
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "uniform", size = 5, prob = "1/10"))
    rn(100)
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "poisson", size = 5, prob = "1/10"))
    rn(100)
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "poisson",lam="5"))
    all(is.numeric(rn(100)))
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "binomial",lam="5"))
    rn(100)
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "uniform",lam="5"))
    rn(100)
  })

  expect_error({
    rn <- as_random_distribution(list(something = 1))
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "fish"))
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "uniform",low = 0, high = 1))
    mean(rn(100000)) > 0.5*.97 & mean(rn(100000)) < 0.5*1.03
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "uniform",low = 0, high = 1))
    var(rn(100000)) > (1/12*(1-0)^2)*.97 & var(rn(100000)) < (1/12*(1-0)^2)*1.03
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "poisson",lam=4))
    mean(rn(100000)) > 4*.97 & mean(rn(100000)) < 4*1.03
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "poisson",lam=4))
    var(rn(100000)) > 4*.97 & var(rn(100000)) < 4*1.03
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "binomial", size = 5, prob = "1/10"))
    mean(rn(100000)) > 5*1/10*.97 & mean(rn(100000)) < 5*1/10*1.03
  })

  expect_true({
    rn <- as_random_distribution(list(distribution = "binomial", size = 5, prob = "1/10"))
    var(rn(100000)) > 5*1/10*9/10*.97 & var(rn(100000)) < 5*1/10*9/10*1.03
  })

})
