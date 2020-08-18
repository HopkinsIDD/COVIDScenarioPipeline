create_testing_simulations <- function(){
    dir <- tempdir()
    dir.create(paste(dir,"a_b", "snpi/loc/scn/high/2020.06.19/global/final/", sep = '/'), recursive=TRUE)
    dir.create(paste(dir,"a_c", "snpi/loc/scn/high/2020.08.17/global/final/", sep = '/'), recursive=TRUE)
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            start_date = rep(c(lubridate::ymd('2020-01-01'),
                               lubridate::ymd('2020-03-14'),
                               lubridate::ymd('2020-05-15')),
                             10),
            end_date = rep(c(lubridate::ymd('2020-03-13'),
                             lubridate::ymd('2020-05-14'),
                             lubridate::ymd('2020-08-31')),
                           10),
            geoid = rep(1:10, each = 3),
            npi_name = rep(c("local_variance", "phase_1", "phase_2"), 10),
            parameter = rep("r0", 30),
            reduction = runif(30)
        ), sink = paste0(dir,"/a_b/snpi/loc/scn/high/2020.06.19/global/final/0000",i, ".parquet")
        )
    }
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            start_date = rep(c(lubridate::ymd('2020-01-01'),
                               lubridate::ymd('2020-03-14'),
                               lubridate::ymd('2020-05-15')),
                             10),
            end_date = rep(c(lubridate::ymd('2020-03-13'),
                             lubridate::ymd('2020-05-14'),
                             lubridate::ymd('2020-08-31')),
                           10),
            geoid = rep(1:10, each = 3),
            npi_name = rep(c("local_variance", "phase_1", "phase_2"), 10),
            parameter = rep("r0", 30),
            reduction = runif(30)
        ), sink = paste0(dir,"/a_c/snpi/loc/scn/high/2020.08.17/global/final/0000",i,".parquet")
        )
    }
    return(dir)
}

test_that("Simulation loading works", {
    dir <- create_testing_simulations()
    setwd(dir)
    expect_error({
        load_snpi_sims_filtered(
            outcome_dir = c('a_b', 'a_c')
        )
    }, "'x' must be a string")
    expect_error({
        load_snpi_sims_filtered(
            outcome_dir = 'a_b',
            partitions = c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id")
        )
    }, "'pdeath' not found")
    expect_error({
        load_snpi_sims_filtered(
            outcome_dir = "a_b", 
            partitions = c("location", "scenario", "pdeath", "lik_type", "is_final")
        )
    }, "at least one array to create a converter")
    
    expect_equal({
        load_snpi_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = function(x){x}
        )
    },
    load_snpi_sims_filtered(
        outcome_dir = 'a_b'
    )
    )
    
    expect_equal({
        ncol(load_snpi_sims_filtered(
            outcome_dir = 'a_b'
        ))
    }, 19
    )
    
    # expect_error({
    #     load_snpi_sims_filtered(
    #         outcome_dir = 'a_b',
    #         post_process = "not a function")
    # }, "could not find function")
    
    expect_error({
        load_snpi_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = "not a function"
        )
    }, "could not find function")
    
    expect_warning({
        load_snpi_sims_filtered(
            outcome_dir = 'a_b'
        )
    }, "Finished loading")
    
    unlink(dir, recursive=TRUE)  
})

