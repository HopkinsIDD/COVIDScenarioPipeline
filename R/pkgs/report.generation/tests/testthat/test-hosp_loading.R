create_testing_simulations <- function(){
    dir <- tempdir()
    dir.create(paste(dir,"a_b", "hosp/loc/scn/high/2020.06.19/global/final/", sep = '/'), recursive=TRUE)
    dir.create(paste(dir,"a_c", "hosp/loc/scn/high/2020.08.17/", sep = '/'), recursive=TRUE)
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
            incidH = sample(1:100, 100),
            incidD = sample(1:100, 100),
            incidICU = sample(1:100, 100),
            incidVent = sample(1:100, 100),
            icu_curr = sample(1:100, 100),
            hosp_curr = sample(1:100, 100),
            vent_curr = sample(1:100, 100)
        ), sink = paste0(dir,"/a_c/hosp/loc/scn/high/2020.08.17/0000",i,".parquet")
        )
    }
    return(dir)
}

test_that("Simulation loading works", {
    dir <- create_testing_simulations()
    setwd(dir)
    included_geoids <- 1:3
    
    expect_error({
        load_hosp_sims_filtered(
            outcome_dir = c('a_b', 'a_c'),
            incl_geoids = included_geoids
        )
    }, "'x' must be a string")
    expect_error({ # test error when specifying too many partitions
        load_hosp_sims_filtered(
            outcome_dir = 'a_b',
            partitions = c("location", "scenario", "pdeath", "date", "lik_type", "is_final", "sim_id", "other_partition"),
            incl_geoids = included_geoids
        )
    }, "couldn't infer type")
    
    expect_equal(
        (load_hosp_sims_filtered(
            outcome_dir = "a_b", 
            partitions = c("location", "scenario", "pdeath", "date", "lik_type", "is_final", "sim_id"),
            incl_geoids = included_geoids,
            inference=FALSE
        ))$sim_num, (
            load_hosp_sims_filtered(
            outcome_dir = "a_b", 
            partitions = c("location", "scens", "death_rate", "date", "likelihood", "final", "sim_id"),
            incl_geoids = included_geoids,
            inference=FALSE
        ))$sim_num
        )
    
    
    # expect_equal({
    #     load_hosp_sims_filtered(
    #         outcome_dir = 'a_b',
    #         pre_process = function(x){x},
    #         incl_geoids = included_geoids
    #     )
    # },
    # load_hosp_sims_filtered(
    #     outcome_dir = 'a_b',
    #     incl_geoids = included_geoids
    # )
    # )
    
    # expect_error({
    #     load_hosp_sims_filtered(
    #         outcome_dir = 'a_b',
    #         post_process = "not a function")
    # }, "could not find function")
    
    expect_error({
        load_hosp_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = "not a function",
            incl_geoids = included_geoids
        )
    }, "could not find function")
    
    expect_message({
        load_hosp_sims_filtered(
            outcome_dir = 'a_b',
            incl_geoids = included_geoids
        )
    }, "Finished loading")
    
    unlink(dir, recursive=TRUE)  
})

