context("aggregate_and_calc_loc_likelihoods")

##' convenience function to get a basic woring setup
##'
##' returns a list with all of the things we need to run
##' tests on aggregate_and_calc_loc_likelihoods
##'
get_minimal_setup <- function () {

    #3geoids
    geoids <- c("06001", "06002", "06003", "32001","32002","32003")
    USPS <- c(rep("CA",3), rep("NY",3))

    ##list of lcations to consider...all of them
    all_locations <- geoids

    obs_nodename <- "geoid"


    ##Generate observed data per geoid  the simulated data will be compared too
    ##TODO
    times <- seq(as.Date("2020-02-15"),as.Date("2020-06-30"), by="days")
    day <- 1:length(times)

    obs_sims <- list()
    for (i in 1:length(geoids)) {
        obs_sims[[i]] <- dplyr::tibble(date = times,
                              geoid = geoids[i],
                              death_incid = rpois(length(day), 1000*dnorm(day, 32, 10)),
                              confirmed_incid = rpois(length(day), 10000*dnorm(day, 32, 10)))
    }
    obs <- dplyr::bind_rows(obs_sims)


    ##Aggregate the observed data to the appropriate level
    geonames <- unique(obs[[obs_nodename]])

    ##minimal confif information used by function
    config <- list()
    config$filtering <- list()
    config$filtering$statistics <- list()
    config$filtering$statistics$sum_deaths <-
        list(name="sum_deaths",
             aggregator= "sum",
             period = "1 weeks",
             sim_var = "incidD",
             data_var = "death_incid",
             remove_na = TRUE,
             add_one = FALSE,
             likelihood = list(
                 dist="sqrtnorm",
                 param=0.1))
    config$filtering$statistics$sum_conf <-
        list(name="sum_conf",
             aggregator= "sum",
             period = "1 days",
             sim_var = "incidC",
             data_var = "confirmed_incid",
             remove_na = TRUE,
             add_one = FALSE,
             likelihood = list(
                 dist="sqrtnorm",
                 param=0.5))


    ## Compute statistics
    data_stats <- lapply(
        geonames,
        function(x) {
            df <- obs[obs[[obs_nodename]] == x, ]
            inference::getStats(
                           df,
                           "date",
                           "data_var",
                           stat_list = config$filtering$statistics)
        }) %>%
        setNames(geonames)

    ##Simulated data per geoid, multiple vars. Just perturb obs  by default
    sim_hosp <- obs %>%
        dplyr::rename(incidD = death_incid, incidC = confirmed_incid) %>%
        dplyr::mutate(incidD = incidD + rpois(length(incidD), incidD))%>%
        dplyr::mutate(incidC = incidC + rpois(length(incidC), incidC))%>%
        dplyr::rename(time=date)

    ##the observed node name.
    obs_nodename <- "geoid"



    ##dummy file name
    hosp_file <- "SillyFile.parquet"

    ##List of the hierarchical stats to consider. Blank place holder by default.
    hierarchical_stats <- list()


    ##List of defined priors. Black place holder by default
    defined_priors <- list()


    ##geodata data frame
    geodata <- dplyr::tibble(geoid = geoids,
                      USPS = USPS)


    ##The file containing information on the given npis. Creating 2 by default.
    npi1 <- dplyr::tibble(geoid=geoids,
                   npi_name = "local_variance",
                   start_date = "2020-01-01",
                   end_date = "2020-06-30",
                   parameter = "r0",
                   reduction = runif(6,-.5, .5))

    npi2A <- dplyr::tibble(geoid = geoids[1:3],
                    npi_name = "full_lockdown_CA",
                    start_date = "2020-03-25",
                    end_date = "2020-06-01",
                    parameter = "r0",
                    reduction = runif(3,-.8, -.5))

    npi2B <- dplyr::tibble(geoid = geoids[4:6],
                    npi_name = "full_lockdown_NY",
                    start_date = "2020-03-15",
                    end_date = "2020-05-22",
                    parameter = "r0",
                    reduction = runif(3,-.8, -.5))

    snpi <- dplyr::bind_rows(npi1, npi2A, npi2B)

    ##The file containing information on the given hospitalization npis. Creating 2 by default.
    npi1 <- dplyr::tibble(geoid=geoids,
                   npi_name = "local_variance",
                   start_date = "2020-01-01",
                   end_date = "2020-06-30",
                   parameter = "hosp::inf",
                   reduction = runif(6,-.5, .5))

    npi2 <- dplyr::tibble(geoid = geoids[1:3],
                    npi_name = "full_lockdown_CA",
                    start_date = "2020-03-25",
                    end_date = "2020-06-01",
                    parameter = "confirmed::inf",
                    reduction = runif(3,-.8, -.5))


    hnpi <- dplyr::bind_rows(npi1, npi2)

    ##Set up hospitalizatoin params.
    hpar1 <- dplyr::tibble(geoid=geoids,
                    parameter="p_confirmed_inf",
                    value=0.1)

    hpar2 <- dplyr::tibble(geoid=geoids,
                    parameter="p_hosp_inf",
                    value=.07)

    hpar <- dplyr::bind_rows(hpar1, hpar2)


    return(list(all_locations=all_locations,
                sim_hosp=sim_hosp,
                obs_nodename=obs_nodename,
                config=config,
                obs=obs,
                data_stats=data_stats,
                hosp_file=hosp_file,
                hierarchical_stats=hierarchical_stats,
                defined_priors=defined_priors,
                geodata = geodata,
                snpi=snpi,
                hnpi=hnpi,
                hpar=hpar))
}



test_that("aggregate_and_calc_loc_likelihoods returns a likelihood per location and right columns", {

    stuff <- get_minimal_setup()

    tmp <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )


    expect_that(nrow(tmp), equals(length(stuff$all_locations)))
    expect_that(sort(colnames(tmp)), equals(sort(c("ll","accept","accept_prob","accept_avg","filename",stuff$obs_nodename))))

})

test_that("likelihood of perfect data is less that likelihood of imperfect data", {

    stuff <- get_minimal_setup()

    alt_sim_hosp <- stuff$sim_hosp

    alt_sim_hosp$incidD <- stuff$obs$death_incid
    alt_sim_hosp$incidC <- stuff$obs$confirmed_incid


    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )

    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = alt_sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    expect_gt(sum(tmp2$ll), sum(tmp1$ll))

})


test_that("removing deaths as a stat makes the likelihood invariant to changes in deaths",{

    stuff <- get_minimal_setup()

    stuff$config$filtering$statistics$sum_deaths <- NULL

    ##remove deaths as a variable
    for(location in stuff$all_locations) {
        stuff$data_stats[[location]]$sum_deaths <- NULL
    }


    alt_sim_hosp <-stuff$sim_hosp
    alt_sim_hosp$incidD <-  alt_sim_hosp$incidD + 200 #or any arbitrary pertibation

    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = alt_sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )

     expect_equal(sum(tmp1$ll), sum(tmp2$ll))

     ##make sure perturbing confirmed still has an effect
     alt_sim_hosp$incidC <-  alt_sim_hosp$incidC + 2 #or any arbitrary pertibationb

    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = alt_sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )

     expect_false(sum(tmp1$ll)==sum(tmp2$ll))

})



test_that("removing confirmed as a stat makes the likelihood invariant to changes in deaths",{

    stuff <- get_minimal_setup()

    stuff$configfiltering$statistics$sum_conf <- NULL

    ##remove deaths as a variable
    for(location in stuff$all_locations) {
        stuff$data_stats[[location]]$sum_conf <- NULL
    }


    alt_sim_hosp <-stuff$sim_hosp
    alt_sim_hosp$incidC <-  alt_sim_hosp$incidC + 200 #or any arbitrary pertibation

    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = alt_sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )

     expect_equal(sum(tmp1$ll), sum(tmp2$ll))

     ##make sure perturbing deaths  still has an effect
     alt_sim_hosp$incidD <-  alt_sim_hosp$incidD + 2 #or any arbitrary pertibationb

    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = alt_sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )


     expect_false(sum(tmp1$ll)==sum(tmp2$ll))

})


test_that("likelihoood insenstive to parameters with no multi-level compoenent or prior", {

    stuff <- get_minimal_setup()

    snpi2 <- stuff$snpi
    snpi2$reduction <- snpi2$reduction*runif(6)


    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi2
    )



     expect_equal(sum(tmp1$ll), sum(tmp2$ll))
})


test_that("likelihood is senstive to changes to correct npi paramerers when multi-level component specified", {
    stuff <- get_minimal_setup()

    stuff$hierarchical_stats$local_var_hierarchy <- list(
        name="local_variance",
        module="seir",
        geo_group_col = "USPS",
        transform= "none"
    )

    snpi2 <- stuff$snpi
    snpi2$reduction[snpi2$npi_name=="local_variance"] <- snpi2$reduction[snpi2$npi_name=="local_variance"]*runif(6)


    snpi3 <- stuff$snpi
    snpi3$reduction[snpi3$npi_name=="full_lockdown_NY"] <- snpi3$reduction[snpi3$npi_name=="full_lockdown_NY"]*runif(3)


    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi2
    )


    tmp3 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi3
    )



    expect_true(sum(tmp1$ll) != sum(tmp2$ll))
    expect_equal(sum(tmp1$ll), sum(tmp3$ll))

})



test_that("likelihood is sensitive to changes to correct hpar parameters when multi-level component is specified", {
    stuff <- get_minimal_setup()

    stuff$hierarchical_stats$local_conf <- list(name="p_confirmed_inf",
                                                module="hospitalization",
                                                geo_group_col = "USPS",
                                                transform="logit")


    hpar2 <- stuff$hpar
    hpar2$value[hpar2$parameter=="p_confirmed_inf"] <- hpar2$value[hpar2$parameter=="p_confirmed_inf"]*runif(6)


    hpar3 <- stuff$hpar
    hpar3$value[hpar3$parameter=="p_hosp_inf"] <- hpar3$value[hpar3$parameter=="p_hosp_inf"]*runif(6)


    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi,
      hpar = stuff$hpar
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi,
      hpar = hpar2
    )


    tmp3 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi,
      hpar = hpar3
    )


    expect_true(sum(tmp1$ll) != sum(tmp2$ll))
    expect_equal(sum(tmp1$ll), sum(tmp3$ll))


})


test_that("when prior is specified, likilhood is higher when nearer prior mean for npis and insensitive to non specified pars", {
    stuff <- get_minimal_setup()

    stuff$defined_priors$local_var_prior <-list(name="local_variance",
                                                module="seir",
                                                likelihood=list(
                                                    dist="normal",
                                                    param=c(0,1)))

    #makes it closer to 0
    snpi2 <- stuff$snpi
    snpi2$reduction[snpi2$npi_name=="local_variance"] <- snpi2$reduction[snpi2$npi_name=="local_variance"]/4


    snpi3 <- stuff$snpi
    snpi3$reduction[snpi3$npi_name=="full_lockdown_NY"] <- snpi3$reduction[snpi3$npi_name=="full_lockdown_NY"]/4


    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi2
    )


    tmp3 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi3
    )



    expect_lte(sum(tmp1$ll), sum(tmp2$ll))
    expect_equal(sum(tmp1$ll), sum(tmp3$ll))
})


test_that("when prior is specified, likilhood is higher when nearer prior mean for hpar and insensitive to non specified", {

    stuff <- get_minimal_setup()

    stuff$defined_priors$conf_rate_prior <- list(name="p_confirmed_inf",
                                                 module="hospitalization",
                                                 likelihood=list(
                                                     dist="logit_normal",
                                                     param=c(.1,4)))


    hpar2 <- stuff$hpar
    hpar2$value[hpar2$parameter=="p_confirmed_inf"] <- .1 ##sure there is something more cleverl that could be done


    hpar3 <- stuff$hpar
    hpar3$value[hpar3$parameter=="p_hosp_inf"] <- .1


    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi,
      hpar = stuff$hpar
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi,
      hpar = hpar2
    )


    tmp3 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi,
      hpar = hpar3
    )


    expect_lte(sum(tmp1$ll), sum(tmp2$ll))
    expect_equal(sum(tmp1$ll), sum(tmp3$ll))


})


test_that("Hierarchical structure works on interventions not defined for all locations (npis)", {
    stuff <- get_minimal_setup()

     stuff$hierarchical_stats$local_var_hierarchy <- list(
        name="full_lockdown_NY",
        module="seir",
        geo_group_col = "USPS",
        transform= "none"
     )


    snpi2 <- stuff$snpi
    snpi2$reduction[snpi2$npi_name=="local_variance"] <- snpi2$reduction[snpi2$npi_name=="local_variance"]*runif(6)


    snpi3 <- stuff$snpi
    snpi3$reduction[snpi3$npi_name=="full_lockdown_NY"] <- snpi3$reduction[snpi3$npi_name=="full_lockdown_NY"]*runif(3)

    snpi4 <- stuff$snpi
    snpi4$reduction[snpi3$npi_name=="full_lockdown_CA"] <- snpi3$reduction[snpi3$npi_name=="full_lockdown_CA"]*runif(3)


    tmp1 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = stuff$snpi
    )



    tmp2 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi2
    )


    tmp3 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi3
    )


    tmp4 <- aggregate_and_calc_loc_likelihoods(
      all_locations = stuff$all_locations,
      modeled_outcome = stuff$sim_hosp,
      obs_nodename = stuff$obs_nodename,
      config = stuff$config,
      obs = stuff$obs,
      ground_truth_data = stuff$data_stats,
      hosp_file = stuff$hosp_file,
      hierarchical_stats = stuff$hierarchical_stats,
      defined_priors = stuff$defined_priors,
      geodata = stuff$geodata,
      snpi = snpi4
    )



    ##print(tmp1)
    ##print(tmp3)

    expect_equal(sum(tmp1$ll), sum(tmp2$ll))
    expect_true(sum(tmp1$ll)!=sum(tmp3$ll))
    expect_equal(sum(tmp1$ll), sum(tmp4$ll))


})
