test_that("load_config works", {
  fname <- tempfile()
  cat("yaml: TRUE\n",file=fname)

  expect_equal(
    load_config(fname)$yaml,
    TRUE
  )
})

test_that("as_evaled_expression works", {
  expect_equal(
    as_evaled_expression(1),
    1
  )
  expect_equal(
    as_evaled_expression("01"),
    1
  )
  expect_equal(
    as_evaled_expression("log(10)"),
    log(10)
  )
})

test_that("unsafe evaluation prevented", {
  expect_error({
    .GlobalEnv$x <- 3
    as_evaled_expression(".GlobalEnv$x <- 5")
    .GlobalEnv$x
  },".GlobalEnv")

  expect_error({
    as_evaled_expression("install.packages('dplyr')")
  })

})

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
    rn <- as_random_distribution(list(distribution = "binomial",n=5,p="1/10"))
    all(is.numeric(rn(100)) & (rn(100) >= 0) & (rn(100) <= 5))
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "uniform",n=5,p="1/10"))
    rn(100)
  })

  expect_error({
    rn <- as_random_distribution(list(distribution = "poisson",n=5,p="1/10"))
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
})
