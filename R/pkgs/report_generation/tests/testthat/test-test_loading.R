create_testing_simulations <- function(){
  dir <- tempdir()
  dir.create(paste(dir,"model_output","a_b", sep = '/'), recursive=TRUE)
  dir.create(paste(dir,"model_output","a_c", sep = '/'), recursive=TRUE)
  for(i in seq_len(10)){
    write.csv(data.frame(
      time = lubridate::ymd('2020-01-01') + lubridate::days(1:10),
      comp = 'diffI',
      g10001 = seq_len(10),
      g00301 = seq_len(10)
    ), file = paste0(dir,"/model_output/","a_b/",i,".csv"), row.names=FALSE)
  }
  for(i in seq_len(10)){
    write.csv(data.frame(
      time = lubridate::ymd('2020-01-01') + lubridate::days(1:10),
      comp = 'diffI',
      g10001 = seq_len(10) + 10,
      g00301 = seq_len(10) + 10
    ), file = paste0(dir,"/model_output/","a_c/",i,".csv"),row.names=FALSE)
  }
  return(dir)
}

test_that("Simulation loading works", {
  dir <- create_testing_simulations()
  setwd(dir)
  expect_error({
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
    )
  }, NA)

  expect_error({
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      num_files <- 1
    )
  }, NA)

  expect_error({
    load_scenario_sims_filtered(
      scenario_dir = c('a_b', 'a_c')
    )
  }, NA)

  expect_error({
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      num_files <- 15
    )
  }, NULL)

  expect_error({
    load_scenario_sims_filtered(
      scenario_dir = 'a_d',
      num_files <- 15
    )
  }, NULL)

  expect_equal({
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      pre_process = function(x){x}
    )
  },
    load_scenario_sims_filtered(
      scenario_dir = 'a_b'
    )
  )

  expect_equal({
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      post_process = function(x){x}
    )
  },
    load_scenario_sims_filtered(
      scenario_dir = 'a_b'
    )
  )

  expect_error({
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      post_process = function(x){x[x$geoid == "g10001",]}
    )
  }, NA)

  expect_equal({
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      pre_process= function(x){x[,c("time","comp","g10001")]}
    )
  }, 
    load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      post_process = function(x){x[x$geoid == "g10001",]}
    )
  )
  
})

test_that("Simulation loading loads the correct number of simulations", {
  dir <- create_testing_simulations()
  setwd(dir)

  expect_equal({
    a <- load_scenario_sims_filtered(
      scenario_dir = 'a_b'
    )
    nrow(a)
  }, 10 * 10 * 2 * 1)

  expect_equal({
    a <- load_scenario_sims_filtered(
      scenario_dir = c('a_b','a_c')
    )
    nrow(a)
  }, 10 * 10 * 2 * 2)

  expect_equal({
    a <- load_scenario_sims_filtered(
      scenario_dir = c('a_b','a_c'),
      num_files = 1
    )
    nrow(a)
  }, 1 * 10 * 2 * 2)

  expect_equal({
    rc <- load_scenario_sims_filtered(
      scenario_dir = 'a_b',
      post_process = function(x){x[x$geoid == "g10001",]}
    )
    nrow(rc)
  }, 10 * 10 * 1 * 1)
})
