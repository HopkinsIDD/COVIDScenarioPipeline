context("logLikStat")


test_that("Identical timeseries are the MLE for all log like stats",{
    obs <- rpois(100,1:100)

    sims <- list()
    for (i in 1:49) {
        sims[[i]] <- tidyr::replace_na(dplyr::lead(obs,50-i),max(obs))
    }
    sims[[50]] <- obs

    for (i in 51:100) {
        sims[[i]] <- tidyr::replace_na(dplyr::lag(obs,i),0)
    }


    ##first the pois stat
    lik <- rep(NA,100)

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "pois",list(),add_one=FALSE))
    }

    expect_that(which.max(lik), equals(50))

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "pois",list(),add_one=TRUE))
    }

    expect_that(which.max(lik), equals(50))


    ##next the normal stat
    lik <- rep(NA,100)

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "norm",list(sd=.1),add_one=FALSE))
    }

    expect_that(which.max(lik), equals(50))

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "norm",list(sd=.1),add_one=TRUE))
    }

    expect_that(which.max(lik), equals(50))


    ##next the normal stat with coefficient of variation
    lik <- rep(NA,100)

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "norm_cov",list(cov=.2),add_one=FALSE))
    }

    expect_that(which.max(lik), equals(50))

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "norm_cov" ,list(cov=.2),add_one=TRUE))
    }

    expect_that(which.max(lik), equals(50))




    ##next the negative binimial stat
    lik <- rep(NA,100)

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "nbinom",list(k=1),add_one=FALSE))
    }

    expect_that(which.max(lik), equals(50))

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "nbinom",list(k=1),add_one=TRUE))
    }

    expect_that(which.max(lik), equals(50))



    ##next sqrtnormall stat
    lik <- rep(NA,100)

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "sqrtnorm",list(sd=.2),add_one=FALSE))
    }

    expect_that(which.max(lik), equals(50), info="sqrtnorm not plus 1 failed")

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(obs, sims[[i]], "sqrtnorm",list(sd=.2),add_one=TRUE))
    }

    expect_that(which.max(lik), equals(50), info="sqrtnorm plus 1 failed")


    ##next sqrtnormal multiplier stat
    lik <- rep(NA,100)

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(2*obs, sims[[i]], "sqrtnorm_scale_sim",list(sd=.2,scale=2),add_one=FALSE))
    }

    expect_that(which.max(lik), equals(50), info="scaled sqrtnorm not plus 1 failed")

    for(i in 1:100) {
        lik[i] <- sum(logLikStat(2*obs, sims[[i]], "sqrtnorm_scale_sim",list(sd=.2,scale=2),add_one=TRUE))
    }

    expect_that(which.max(lik), equals(50), info="not scaled sqrtnorm plus 1 failed")

})



test_that("logLikStat returns errors on null parameters when appropriate", {

    obs <- c(0,1,2,3)
    sim <- c(0,1,2,3)

    logLikStat(obs,sim,"pois") #should run error free

    expect_error( logLikStat(obs,sim,"norm"))
    expect_error( logLikStat(obs,sim,"norm_cov"))
    expect_error( logLikStat(obs,sim,"nnbinom"))
    expect_error( logLikStat(obs,sim,"sqrtnorm"))
    expect_error( logLikStat(obs,sim,"sqrtnorm_scale_sim"))
    
                 
})
