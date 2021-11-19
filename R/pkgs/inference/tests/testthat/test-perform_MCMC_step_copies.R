context("perform_MCMC_step_copies")


##THESE TESTS CAN BE MADE MORE DETAILED...JUST MAKING PLACE HOLDERS
test_that("MCMC step copies (global) are correctly performed when we are not at the start of a block", {
    ##some information on our phantom runs
    current_index <- 2
    slot <- 2
    block <- 5
    run_id <- "TEST_RUN"
    slot_prefix <- covidcommon::create_prefix("config","scenario","deathrate",run_id,sep='/',trailing_separator='/')
    gf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','final',sep='/',trailing_separator='/')
    gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/')
    global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')
    global_local_prefix <- covidcommon::create_prefix(prefix=global_block_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')

    ##To be save make a directory
    dir.create("MCMC_step_copy_test")
    setwd("MCMC_step_copy_test")
    ##get file names
    seed_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seed','csv')
    seir_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'seir','parquet')
    hosp_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hosp','parquet')
    llik_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'llik','parquet')
    snpi_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'snpi','parquet')
    spar_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'spar','parquet')
    hnpi_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hnpi','parquet')
    hpar_src <- covidcommon::create_file_name(run_id,global_local_prefix,current_index,'hpar','parquet')



    ##create the copy from  files
    arrow::write_parquet(data.frame(file="seed"), seed_src)
    arrow::write_parquet(data.frame(file="seir"), seir_src)
    arrow::write_parquet(data.frame(file="hosp"), hosp_src)
    arrow::write_parquet(data.frame(file="llik"), llik_src)
    arrow::write_parquet(data.frame(file="snpi"), snpi_src)
    arrow::write_parquet(data.frame(file="spar"), spar_src)
    arrow::write_parquet(data.frame(file="hnpi"), hnpi_src)
    arrow::write_parquet(data.frame(file="hpar"), hpar_src)

    ##print(hosp_src)
    ##print(covidcommon::create_file_name(run_id,gf_prefix,slot,'hosp','parquet'))

    res <- perform_MCMC_step_copies_global(current_index,
                                    slot,
                                    block,
                                    run_id,
                                    global_local_prefix,
                                    gf_prefix,
                                    global_block_prefix)


    expect_equal(prod(unlist(res)),1)

    ##clean up
    setwd("..")
    unlink("MCMC_step_copy_test", recursive=TRUE)


})


test_that("MCMC step copies (global) are correctly performed when we are at the start of a block", {
    ##some information on our phantom runs
    current_index <- 0
    slot <- 2
    block <- 5
    run_id <- "TEST_RUN"
    slot_prefix <- covidcommon::create_prefix("config","scenario","deathrate",run_id,sep='/',trailing_separator='/')
    gf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','final',sep='/',trailing_separator='/')
    gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/')
    global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')
    global_local_prefix <- covidcommon::create_prefix(prefix=global_block_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')

    ##To be save make a direectory
    dir.create("MCMC_step_copy_test")
    setwd("MCMC_step_copy_test")
    ##get file names
    seed_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'seed','csv')
    seir_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'seir','parquet')
    hosp_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'hosp','parquet')
    llik_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'llik','parquet')
    snpi_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'snpi','parquet')
    spar_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'spar','parquet')
    hnpi_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'hnpi','parquet')
    hpar_src <- covidcommon::create_file_name(run_id,global_block_prefix,block-1,'hpar','parquet')



    ##create the copy from  files
    arrow::write_parquet(data.frame(file="seed"), seed_src)
    arrow::write_parquet(data.frame(file="seir"), seir_src)
    arrow::write_parquet(data.frame(file="hosp"), hosp_src)
    arrow::write_parquet(data.frame(file="llik"), llik_src)
    arrow::write_parquet(data.frame(file="snpi"), snpi_src)
    arrow::write_parquet(data.frame(file="spar"), spar_src)
    arrow::write_parquet(data.frame(file="hnpi"), hnpi_src)
    arrow::write_parquet(data.frame(file="hpar"), hpar_src)

    print(hosp_src)
    print(covidcommon::create_file_name(run_id,global_block_prefix,block,'hosp','parquet'))

    res <- perform_MCMC_step_copies_global(current_index,
                                    slot,
                                    block,
                                    run_id,
                                    global_local_prefix,
                                    gf_prefix,
                                    global_block_prefix)


    expect_equal(prod(unlist(res)),1)

    ##clean up
    setwd("..")
    unlink("MCMC_step_copy_test", recursive=TRUE)


})

test_that("MCMC step copies (chimeric) are correctly performed when we are not at the start of a block", {
    ##some information on our phantom runs
    current_index <- 2
    slot <- 2
    block <- 5
    run_id <- "TEST_RUN"
    slot_prefix <- covidcommon::create_prefix("config","scenario","deathrate",run_id,sep='/',trailing_separator='/')
    cf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','final',sep='/',trailing_separator='/')
    ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/')
    chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')
    chimeric_local_prefix <- covidcommon::create_prefix(prefix=chimeric_block_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')
    
    ##To be save make a directory
    dir.create("MCMC_step_copy_test")
    setwd("MCMC_step_copy_test")
    ##get file names
    seed_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'seed','csv')
    seir_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'seir','parquet')
    hosp_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hosp','parquet')
    llik_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'llik','parquet')
    snpi_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'snpi','parquet')
    spar_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'spar','parquet')
    hnpi_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hnpi','parquet')
    hpar_src <- covidcommon::create_file_name(run_id,chimeric_local_prefix,current_index,'hpar','parquet')
    
    
    
    ##create the copy from  files
    arrow::write_parquet(data.frame(file="seed"), seed_src)
    arrow::write_parquet(data.frame(file="seir"), seir_src)
    arrow::write_parquet(data.frame(file="hosp"), hosp_src)
    arrow::write_parquet(data.frame(file="llik"), llik_src)
    arrow::write_parquet(data.frame(file="snpi"), snpi_src)
    arrow::write_parquet(data.frame(file="spar"), spar_src)
    arrow::write_parquet(data.frame(file="hnpi"), hnpi_src)
    arrow::write_parquet(data.frame(file="hpar"), hpar_src)
    
    ##print(hosp_src)
    ##print(covidcommon::create_file_name(run_id,cf_prefix,slot,'hosp','parquet'))
    
    res <- perform_MCMC_step_copies_chimeric(current_index,
                                           slot,
                                           block,
                                           run_id,
                                           chimeric_local_prefix,
                                           cf_prefix,
                                           chimeric_block_prefix)
    
    
    expect_equal(prod(unlist(res)),1)
    
    ##clean up
    setwd("..")
    unlink("MCMC_step_copy_test", recursive=TRUE)
    
    
})


test_that("MCMC step copies (chimeric) are correctly performed when we are at the start of a block", {
    ##some information on our phantom runs
    current_index <- 0
    slot <- 2
    block <- 5
    run_id <- "TEST_RUN"
    slot_prefix <- covidcommon::create_prefix("config","scenario","deathrate",run_id,sep='/',trailing_separator='/')
    cf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','final',sep='/',trailing_separator='/')
    ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/')
    chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')
    chimeric_local_prefix <- covidcommon::create_prefix(prefix=chimeric_block_prefix, slot=list(slot,"%09d"), sep='.',
                                                      trailing_separator='.')
    
    ##To be save make a direectory
    dir.create("MCMC_step_copy_test")
    setwd("MCMC_step_copy_test")
    ##get file names
    seed_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'seed','csv')
    seir_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'seir','parquet')
    hosp_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'hosp','parquet')
    llik_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'llik','parquet')
    snpi_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'snpi','parquet')
    spar_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'spar','parquet')
    hnpi_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'hnpi','parquet')
    hpar_src <- covidcommon::create_file_name(run_id,chimeric_block_prefix,block-1,'hpar','parquet')
    
    
    
    ##create the copy from  files
    arrow::write_parquet(data.frame(file="seed"), seed_src)
    arrow::write_parquet(data.frame(file="seir"), seir_src)
    arrow::write_parquet(data.frame(file="hosp"), hosp_src)
    arrow::write_parquet(data.frame(file="llik"), llik_src)
    arrow::write_parquet(data.frame(file="snpi"), snpi_src)
    arrow::write_parquet(data.frame(file="spar"), spar_src)
    arrow::write_parquet(data.frame(file="hnpi"), hnpi_src)
    arrow::write_parquet(data.frame(file="hpar"), hpar_src)
    
    print(hosp_src)
    print(covidcommon::create_file_name(run_id,chimeric_block_prefix,block,'hosp','parquet'))
    
    res <- perform_MCMC_step_copies_chimeric(current_index,
                                           slot,
                                           block,
                                           run_id,
                                           chimeric_local_prefix,
                                           cf_prefix,
                                           chimeric_block_prefix)
    
    
    expect_equal(prod(unlist(res)),1)
    
    ##clean up
    setwd("..")
    unlink("MCMC_step_copy_test", recursive=TRUE)
    
    
})

