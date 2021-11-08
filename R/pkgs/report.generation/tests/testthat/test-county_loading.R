create_testing_simulations <- function(){
    dir <- tempdir()
    dir.create(paste(dir,"a_b", "hosp/loc/scn/high/2020.06.19/global/final/", sep = '/'), recursive=TRUE)
    dir.create(paste(dir,"a_c", "hosp/loc/scn/med/2020.06.19/", sep = '/'), recursive=TRUE)
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            time = rep(lubridate::ymd('2020-01-01') + lubridate::days(1:5), each = 5),
            geoid = rep(1:5, 5),
            incidI = sample(1:100, 25),
            incidC = sample(1:100, 25),
            incidH = sample(1:100, 25),
            incidD = sample(1:100, 25),
            incidICU = sample(1:100, 25),
            incidVent = sample(1:100, 25),
            icu_curr = sample(1:100, 25),
            hosp_curr = sample(1:100, 25),
            vent_curr = sample(1:100, 25)
        ), sink = paste0(dir,"/a_b/hosp/loc/scn/high/2020.06.19/global/final/0000",i, ".parquet")
        )
    }
    for(i in seq_len(10)){
        arrow::write_parquet(data.frame(
            time = rep(lubridate::ymd('2020-01-01') + lubridate::days(1:5), each = 5),
            geoid = rep(1:5, 5),
            incidI = sample(1:100, 25),
            incidC = sample(1:100, 25),
            incidH = sample(1:100, 25),
            incidD = sample(1:100, 25),
            incidICU = sample(1:100, 25),
            incidVent = sample(1:100, 25),
            icu_curr = sample(1:100, 25),
            hosp_curr = sample(1:100, 25),
            vent_curr = sample(1:100, 25)
        ), sink = paste0(dir,"/a_c/hosp/loc/scn/med/2020.06.19/0000",i,".parquet")
        )
    }
    return(dir)
}

monotonic_increasing <- function(x, pdeath_filter, cum){
    for(i in 1:length(x$sim_num)){
        for(j in unique(x$geoid)){
            t<-x %>%
                dplyr::filter(sim_num==i, 
                              geoid==j,
                              pdeath==pdeath_filter) %>%
                dplyr::select(!!as.symbol(cum)) %>%
                dplyr::pull()
            if(all(t==cummax(t))==FALSE) stop(FALSE)
        }
    }
    TRUE
}


test_that("Simulation loading works", {
    dir <- create_testing_simulations()
    setwd(dir)
    included_geoids <- 1:3
    
    expect_error({
        load_hosp_county(
            outcome_dir = 'a_b',
            scenario_levels = 'scn',
            scenario_labels = 'baseline',
            pdeath_filter = "low",
            incl_geoids = included_geoids
        )
    }, NULL
    )
    
    expect_warning({
        load_hosp_county(
            outcome_dir = 'a_b',
            scenario_levels = 'SCN',
            scenario_labels = 'baseline',
            incl_geoids = included_geoids
        )
    }, "Scenario levels were not correctly specified"
    )
    
    expect_equal({
        ncol(load_hosp_county(
            outcome_dir = 'a_b',
            scenario_levels = 'scn',
            scenario_labels = 'baseline',
            incl_geoids = included_geoids
        ))
    }, {ncol(load_hosp_county(
            outcome_dir = 'a_c',
            scenario_levels = 'scn',
            scenario_labels = 'baseline',
            incl_geoids = included_geoids,
            inference=FALSE
    ))
        }
    )
    
    unlink(dir, recursive=TRUE)  
})

test_that("Correct output", {
    dir <- create_testing_simulations()
    setwd(dir)
    dat <- load_hosp_county(outcome_dir = 'a_b',
                            scenario_levels = 'scn',
                            scenario_labels = 'baseline',
                            incl_geoids = 1:3)
    ifr <- "high"
    
    expect_true(
        monotonic_increasing(dat, ifr, "cum_inf"))
    
    
    expect_true(
        monotonic_increasing(dat, ifr, "cum_case"))
    
    
    expect_true(
        monotonic_increasing(dat, ifr, "cum_hosp"))
    
    
    expect_true(
        monotonic_increasing(dat, ifr, "cum_death"))
    
    unlink(dir, recursive=TRUE)  
    
})

