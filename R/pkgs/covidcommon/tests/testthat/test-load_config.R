test_that("load_config works", {
  fname <- tempfile()
  cat("yaml: TRUE\n",file=fname)
  fname_bad <- tempfile()
  cat("yaml: TRUE\n yaml2: FALSE\n",file=fname_bad)

  expect_equal(
    load_config(fname)$yaml,
    TRUE
  )

  expect_error(
    load_config(";lkdjaoijdsfjoasidjfaoiwerfj q2fu8ja8erfasdiofj aewr;fj aff409a urfa8rf a';j 38i a0fuadf "),
    "Could not find"
  )

  expect_error(
    load_config(fname)$badkey,
    "badkey"
  )

  expect_error(
    load_config(fname)$missing$badkey,
    "missing"
  )

  expect_error(
    load_config(fname_bad),
    "yaml::read_yaml"
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

