create_testing_simulations <- function(){
    dir <- tempdir()
    dir.create(paste(dir,"a_b", "hpar/loc/scn/high/2020.06.19/global/final/", sep = '/'), recursive=TRUE)
    dir.create(paste(dir,"a_c", "hpar/loc/scn/high/2020.08.17/global/final/", sep = '/'), recursive=TRUE)
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            geoid = rep(seq_len(10), each=5),
            quantity = rep("probability", 25), 
            outcome = rep(c("incidH", "incidICU", "incidVent", "incidD", "incidC"), 5),
            source = rep(c("incidI", "incidH", "incidI", "incidI", "incidI"), 5),
            value = seq_len(25)/100
        ), sink = paste0(dir,"/a_b/hpar/loc/scn/high/2020.06.19/global/final/0000",i, ".parquet")
        )
    }
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            geoid = rep(seq_len(10), each=5),
            quantity = rep("probability", 25), 
            outcome = rep(c("incidH", "incidICU", "incidVent", "incidD", "incidC"), 5),
            source = rep(c("incidI", "incidH", "incidI", "incidI", "incidI"), 5),
            value = seq_len(25)/100
        ), sink = paste0(dir,"/a_c/hpar/loc/scn/high/2020.08.17/global/final/0000",i,".parquet")
        )
    }
    return(dir)
}

test_that("Simulation loading works", {
    dir <- create_testing_simulations()
    setwd(dir)
    expect_error({
        load_hpar_sims_filtered(
            outcome_dir = c('a_b', 'a_c')
        )
    }, "'x' must be a string")
    expect_error({
        load_hpar_sims_filtered(
            outcome_dir = 'a_b',
            partitions = c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id")
        )
    }, "'pdeath' not found")
    expect_error({
        load_hpar_sims_filtered(
            outcome_dir = "a_b", 
            partitions = c("location", "scenario", "pdeath", "lik_type", "is_final")
        )
    }, "at least one array to create a converter")
    
    expect_equal({
        load_hpar_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = function(x){x}
        )
    },
    load_hpar_sims_filtered(
        outcome_dir = 'a_b'
    )
    )
    
    expect_equal({
        ncol(load_hpar_sims_filtered(
            outcome_dir = 'a_b'
        ))
    }, 13
    )
    
    expect_error({
        load_hpar_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = "not a function"
        )
    }, "could not find function")
    
    expect_warning({
        load_hpar_sims_filtered(
            outcome_dir = 'a_b'
        )
    }, "Finished loading")
    
    unlink(dir, recursive=TRUE)  
})

