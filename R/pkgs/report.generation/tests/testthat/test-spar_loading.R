create_testing_simulations <- function(){
    dir <- tempdir()
    dir.create(paste(dir,"a_b", "spar/loc/scn/high/2020.06.19/global/final/", sep = '/'), recursive=TRUE)
    dir.create(paste(dir,"a_c", "spar/loc/scn/high/2020.08.17/global/final/", sep = '/'), recursive=TRUE)
    for(i in seq_len(50)){
        arrow::write_parquet(data.frame(
            parameter = rep(c("alpha", "R0", "sigma", "gamma"), 5),
            value = seq_len(20)/100
        ), sink = paste0(dir,"/a_b/spar/loc/scn/high/2020.06.19/global/final/0000",i, ".parquet")
        )
    }
    for(i in seq_len(50)){
        arrow::write_parquet(data.frame(
            parameter = rep(c("alpha", "R0", "sigma", "gamma"), 5),
            value = seq_len(20)/100
        ), sink = paste0(dir,"/a_c/spar/loc/scn/high/2020.08.17/global/final/0000",i,".parquet")
        )
    }
    return(dir)
}

test_that("Simulation loading works", {
    dir <- create_testing_simulations()
    setwd(dir)
    
    expect_error({
        load_spar_sims_filtered(
            outcome_dir = c('a_b', 'a_c')
        )
    }, "'x' must be a string")
    expect_error({
        load_spar_sims_filtered(
            outcome_dir = 'a_b',
            partitions = c("location", "scenario", "death_rate", "date", "lik_type", "is_final", "sim_id", "other_partition")
        )
    }, "couldn't infer type")
    expect_error({
        load_spar_sims_filtered(
            outcome_dir = "a_b", 
            partitions = c("location", "scenario", "pdeath", "lik_type", "is_final")
        )
    }, "object 'NA' not found")
    
    # expect_equal({
    #     load_spar_sims_filtered(
    #         outcome_dir = 'a_b',
    #         pre_process = function(x){x}
    #     )
    # },
    # load_spar_sims_filtered(
    #     outcome_dir = 'a_b'
    # )
    # )
    
    
    expect_error({
        load_spar_sims_filtered(
            outcome_dir = 'a_b',
            pre_process = "not a function"
        )
    }, "could not find function")
    
    expect_message({
        load_spar_sims_filtered(
            outcome_dir = 'a_b'
        )
    }, "Finished loading")
    
    unlink(dir, recursive=TRUE)  
})

