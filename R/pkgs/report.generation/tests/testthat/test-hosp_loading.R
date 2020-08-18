create_testing_simulations <- function(){
    dir <- tempdir()
    dir.create(paste(dir,"a_b", "hosp/loc/scn/high/2020.06.19/global/final/", sep = '/'), recursive=TRUE)
    dir.create(paste(dir,"a_c", "hosp/loc/scn/high/2020.08.17/global/final/", sep = '/'), recursive=TRUE)
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            time = rep(lubridate::ymd('2020-01-01') + lubridate::days(1:10), each = 10),
            geoid = rep(1:10, each=10),
            incidI = sample(1:100, 100),
            incidC = sample(1:100, 100),
            incidH = sample(1:100, 100),
            incidD = sample(1:100, 100),
            incidICU = sample(1:100, 100),
            incidVent = sample(1:100, 100),
            icu_curr = sample(1:100, 100),
            hosp_curr = sample(1:100, 100),
            vent_curr = sample(1:100, 100)
        ), sink = paste0(dir,"/a_b/hosp/loc/scn/high/2020.06.19/global/final/0000",i, ".parquet")
        )
    }
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            time = rep(lubridate::ymd('2020-01-01') + lubridate::days(1:10), each = 10),
            geoid = rep(1:10, each=10),
            incidI = sample(1:100, 100),
            incidC = sample(1:100, 100),
            incidH = sample(1:100, 100),
            incidD = sample(1:100, 100),
            incidICU = sample(1:100, 100),
            incidVent = sample(1:100, 100),
            icu_curr = sample(1:100, 100),
            hosp_curr = sample(1:100, 100),
            vent_curr = sample(1:100, 100)
        ), sink = paste0(dir,"/a_c/hosp/loc/scn/high/2020.08.17/global/final/0000",i,".parquet")
        )
    }
    return(dir)
}

test_that("Simulation loading works", {
    dir <- create_testing_simulations()
    setwd(dir)
    expect_error({
        load_hosp_sims_filtered(
            outcome_dir = c('a_b', 'a_c')
        )
    }, "'x' must be a string")
    expect_error({
        load_hosp_sims_filtered(
            outcome_dir = 'a_b',
            partitions = c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id")
        )
    }, "'pdeath' not found")
    expect_error({
        load_hosp_sims_filtered(
            outcome_dir = "a_b", 
            partitions = c("location", "scenario", "pdeath", "lik_type", "is_final")
        )
    }, "at least one array to create a converter")
    
    expect_equal({
        load_hosp_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = function(x){x}
        )
    },
    load_hosp_sims_filtered(
        outcome_dir = 'a_b'
    )
    )
    
    expect_equal({
        ncol(load_hosp_sims_filtered(
            outcome_dir = 'a_b'
        ))
    }, 19
    )
    
    # expect_error({
    #     load_hosp_sims_filtered(
    #         outcome_dir = 'a_b',
    #         post_process = "not a function")
    # }, "could not find function")
    
    expect_error({
        load_hosp_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = "not a function"
        )
    }, "could not find function")
    
    expect_warning({
        load_hosp_sims_filtered(
            outcome_dir = 'a_b'
        )
    }, "Finished loading")
    
    unlink(dir, recursive=TRUE)  
})

