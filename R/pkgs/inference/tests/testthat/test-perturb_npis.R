context("perturb_npis")

test_that("perturb_snpi always stays within support", {
    N <- 10000
    npis <- data.frame(
        geoid = rep('00000',times=N),
        npi_name = rep("test_npi",times=N),
        start_date = rep("2020-02-01",times=N),
        end_date = rep("2020-02-02",times=N),
        parameter = rep("r0",times=N),
        reduction = rep(-.099,times=N)
    )
    npi_settings <- list(test_npi = list(
        template = "Reduce",
        parameter = "r0",
        value = list(
          distribution = "truncnorm",
          mean = "0",
          sd = "0.1",
          a = "-.11",
          b = "-.097"
        ),
        perturbation = list(
          distribution = "truncnorm",
          mean = "0",
          sd = "0.001",
          a = "-.1",
          b = ".1"
        )
    ))
    expect_equal(all(perturb_snpi(npis,npi_settings)$reduction <= as.numeric(npi_settings$test_npi$value$b)),TRUE)
    expect_equal(all(perturb_snpi(npis,npi_settings)$reduction >= as.numeric(npi_settings$test_npi$value$a)),TRUE)
})

test_that("perturb_snpi has a median of 0 after 10000 sims",{
    N <- 10000
    npis <- data.frame(
        geoid = rep('00000',times=N),
        npi_name = rep("test_npi",times=N),
        start_date = rep("2020-02-01",times=N),
        end_date = rep("2020-02-02",times=N),
        parameter = rep("r0",times=N),
        reduction = rep(0,times=N)
    )
    npi_settings <- list(
        template = "Reduce",
        parameter = "r0",
        value = list(
          distribution = "truncnorm",
          mean = "0",
          sd = "0.1",
          a = "-1",
          b = "1"
        ),
        perturbation = list(
          distribution = "truncnorm",
          mean = "0",
          sd = "0.05",
          a = "-.1",
          b = ".1"
        )
    )
    expect_lt({
      local_npis <- npis
      for(i in seq_len(N)){
        local_npis <- perturb_snpi(local_npis,npi_settings)
      }
      abs(mean(local_npis$reduction))
    },0.1)
    expect_lt({
      local_npis <- npis
      for(i in seq_len(N)){
        local_npis <- perturb_snpi(local_npis,npi_settings)
      }
      abs(median(local_npis$reduction))
    },0.1)
})

test_that("perturb_snpi does not perturb npis without a perturbation section", {
    N <- 10000
    npis <- data.frame(
        geoid = rep('00000',times=N),
        npi_name = rep("test_npi",times=N),
        start_date = rep("2020-02-01",times=N),
        end_date = rep("2020-02-02",times=N),
        parameter = rep("r0",times=N),
        reduction = rep(-.099,times=N)
    )
    npi_settings <- list(test_npi = list(
        template = "Reduce",
        parameter = "r0",
        value = list(
          distribution = "truncnorm",
          mean = "0",
          sd = "0.1",
          a = "-.11",
          b = "-.097"
        )
    )) 
    expect_equal({
      local_npis <- npis
      for(i in seq_len(N)){
        local_npis <- perturb_snpi(local_npis,npi_settings)
      }
      local_npis
    },npis)
})
