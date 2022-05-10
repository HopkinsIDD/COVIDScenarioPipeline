context("initial MCMC setup")

test_that("initialize_mcmc_first_block works for block > 1",{
  filenames <- c(
    create_filename_list(
      "test_run",
      "global",
      1,
      c("seed", "seir", "snpi", "spar", "hosp", "hnpi", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet","parquet")
    ),
    create_filename_list(
      "test_run",
      "chimeric",
      1,
      c("seed", "seir", "snpi", "spar", "hosp", "hnpi", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet","parquet")
    )
  )

  expect_false({
    suppressWarnings(unlink("model_output",recursive=TRUE))
    # suppressWarnings(lapply(filenames,file.remove))
    any(file.exists(filenames))
  })

  expect_error({
    initialize_mcmc_first_block(
      run_id = "test_run",
      block = 2,
      global_prefix = "global",
      chimeric_prefix = "chimeric",
      python_reticulate = NULL,
      likelihood_calculation_function = NULL
    )
  })

  filenames <- c(
    create_filename_list(
      "test_run",
      "global",
      1,
      c("seed", "seir", "snpi", "spar", "hosp", "hnpi", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet","parquet")
    ),
    create_filename_list(
      "test_run",
      "chimeric",
      1,
      c("seed", "seir", "snpi", "spar", "hosp", "hnpi", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet","parquet")
    )
  )

  expect_true({
    lapply(filenames,function(x){write.csv(file=x,data.frame(missing=TRUE))})
    all(file.exists(filenames))
  })

  expect_error({
    initialize_mcmc_first_block(
      run_id = "test_run",
      block = 2,
      global_prefix = "global",
      chimeric_prefix = "chimeric",
      gempyor_inference_runner = NULL,
      likelihood_calculation_function = NULL
    )
  }, NA)

  expect_false({
    suppressWarnings(unlink("model_output",recursive=TRUE))
    any(file.exists(filenames))
  })

})

test_that("initialize_mcmc_first_block works for block < 1",{
  filenames <- c(
    create_filename_list(
      "test_run",
      "global",
      -1,
      c("seed", "seir", "snpi", "spar", "hosp", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet")
    ),
    create_filename_list(
      "test_run",
      "chimeric",
      -1,
      c("seed", "seir", "snpi", "spar", "hosp", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet")
    )
  )

  expect_false({
    suppressWarnings(unlink("model_output",recursive=TRUE))
    # suppressWarnings(lapply(filenames,file.remove))
    all(file.exists(filenames))
  })

  expect_error({
    initialize_mcmc_first_block(
      run_id = "test_run",
      block = 0,
      global_prefix = "global",
      chimeric_prefix = "chimeric",
      python_reticulate = NULL,
      likelihood_calculation_function = NULL
    )
  })

  filenames <- c(
    create_filename_list(
      "test_run",
      "global",
      -1,
      c("seed", "seir", "snpi", "spar", "hosp", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet")
    ),
    create_filename_list(
      "test_run",
      "chimeric",
      -1,
      c("seed", "seir", "snpi", "spar", "hosp", "hpar","llik"),
      c("csv","parquet","parquet","parquet","parquet","parquet","parquet")
    )
  )

  expect_true({
    lapply(filenames,function(x){write.csv(file=x,data.frame(missing=TRUE))})
    all(file.exists(filenames))
  })

  expect_error({
    initialize_mcmc_first_block(
      run_id = "test_run",
      block = 0,
      global_prefix = "global",
      chimeric_prefix = "chimeric",
      gempyor_inference_runner = NULL,
      likelihood_calculation_function = NULL
    )
  })

  expect_false({
    suppressWarnings(unlink("model_output",recursive=TRUE))
    any(file.exists(filenames))
  })

})
