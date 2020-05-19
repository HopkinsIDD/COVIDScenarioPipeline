##'
##' Function that does a rapid testing of the inference procedures on a single
##' time series.
##'
##' @param to_fit the epid data to fit...should look like hospitalization code output.
##' @param init_seed the initial seeding
##' @param config the config file wilth inference info.
##' @param chain_len the length of he MCMC chain
##'
##' @return inference run results on this particular area
##'
##'
##'
single_loc_inference_test <- function(to_fit, init_seed, config, chain_len) {


    ##generate initial NPIs
    ##npis <-



    ##generate inital epi curve


    ##generate hospitalizatoins from epi curve

}




##'
##'
##' Fill in the epi curve from a baseline state given current parameters
##'
##' TODO: Modify to just call python code
##'
##' @param times the times to run the epidemic on
##' @param seeding the seeding to use
##' @param R0 the reproductive number
##' @param S0 the initial number susceptible
##' @param gamma the time between compartments
##' @param sigma the incubation period/latent period
##' @param beta_mults multipliers to capture the impact of interventions
##' @param alpha defaults to 1
##'
##' @return the epimic states
##'
##'
simulate_single_epi <- function(times,
                                seeding,
                                S0,
                                R0,
                                gamma,
                                sigma,
                                beta_mults = rep(1, length(times)),
                                alpha = 1) {

    require(tidyverse)

    ##Scale R0 to beta
    beta <- R0 * gamma/3

    ##set up return matrix
    epi <- matrix(0, nrow=length(times), ncol=7)
    colnames(epi) <- c("S","E","I1","I2","I3","R","incidI")


    ##column indices for convenience
    S <- 1
    E <- 2
    I1 <- 3
    I2 <- 4
    I3 <- 5
    R  <- 6
    incidI <- 7

    ##seed the first case
    epi[1,S] <- S0



    ##get the indices where seeding occurs
    seed_times <- which(times%in%seeding$date)

    for (i in 1:(length(times)-1)) {
        ##Seed if possible
        if(i%in% seed_times) {
            tmp <- seeding$amount[which(i==seed_times)]
            epi[i,S] <- epi[i,S] - tmp
            epi[i,E] <- epi[i,E] + tmp
        }

        ##Draw transitions
        #print(beta)
        dSE <- rbinom(1,epi[i,S],beta*beta_mults[i]*sum(epi[i,I1:I3])^alpha/S0)
        dEI <- rbinom(1,epi[i,E],sigma)
        dI12 <- rbinom(1, epi[i,I1], gamma)
        dI23 <- rbinom(1, epi[i,I2], gamma)
        dIR <- rbinom(1, epi[i,I3], gamma)

        ##Make transitions
        epi[i+1,S] <- epi[i,S] - dSE
        epi[i+1,E] <- epi[i,E] + dSE - dEI
        epi[i+1,I1] <- epi[i,I1] + dEI - dI12
        epi[i+1,I2] <- epi[i,I2] + dI12 - dI23
        epi[i+1,I3] <- epi[i,I3] + dI23 - dIR
        epi[i+1,R] <= epi[i,R] + dIR
        epi[i+1,incidI] <- dEI

    }

    epi <- as.data.frame(epi) %>%
        mutate(time=times)%>%
        pivot_longer(-time,values_to="N", names_to="comp")

    return(epi)
}
