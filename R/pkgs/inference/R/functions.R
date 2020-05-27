# Likelihood stuff -------------------------------------------------------------

##' Function for applying time aggregation of variables on which to comput likelihoods
##' Note that bahavior is not consistent when multiples of time nits are passed in.
##'
##' @param data Vector of data to aggregate
##' @param dates Vector of dates
##' @param end_date Last date to consider
##' @param period_unit Unit of period over which to aggregate
##' @param period_k Number of time units defining period over which to aggregate
##' @param aggregator Function for aggregations
##' @param na.rm Remove Nas?
##' @return NULL
#' @export
periodAggregate <- function(data, dates, end_date = NULL, period_unit, period_k, aggregator, na.rm = F) {
  if(na.rm) {
    dates <- dates[!is.na(data)]
    data <- data[!is.na(data)]
  }
  if (length(data) == 0) {
    return(data.frame(date = NA, stat = NA))
  }
  if (!is.null(end_date)) {
    data <- data[dates <= end_date]
    dates <- dates[dates <= end_date]
  }


  xtsobj <- xts::as.xts(zoo::zoo(data, dates))
  stats <- xts::period.apply(xtsobj,
                        xts::endpoints(xtsobj, on = period_unit, k = period_k),
                        aggregator)
  return(stats)
}


##' Function for computing statistics over which to compute likelihoods
##' @param df Data frame with data
##' @param time_col Name of the column with time
##' @param var_col Name of the variable with name of the  column with data to process
##' @param end_date Last date to consider
##' @param stat_list List with specifications of statistics to compute
##' @return NULL
#' @export
getStats <- function(df, time_col, var_col, end_date = NULL, stat_list) {
  rc <- list()
  for(stat in names(stat_list)){
    s <- stat_list[[stat]]
    aggregator <- match.fun(s$aggregator)
    ## Get the time period over whith to apply aggregation
    period_info <- strsplit(s$period, " ")[[1]]


    res <- inference::periodAggregate(df[[s[[var_col]]]],
                           df[[time_col]],
                           end_date,
                           period_info[2],
                           period_info[1],
                           aggregator,
                           na.rm = s$remove_na)
    rc[[stat]] <- res %>%
      as.data.frame() %>%
      dplyr::mutate(date = rownames(.)) %>%
      magrittr::set_colnames(c(var_col, "date")) %>%
      dplyr::select(date, one_of(var_col))
  }
  return(rc)
}


##' Function for computing statistics over which to compute likelihoods
##' @param obs Vector of observed statistics
##' @param sim Vector of simulated statistics
##' @param distr Distribution to use for likelihood calculation
##' @param param a list opf parameters to the distibution
##' @param add_one Whether to add one to simulations to avoid Infs
##' @return NULL
#' @export
logLikStat <- function(obs, sim, distr, param, add_one = F) {
  if(length(obs) != length(sim)){
    stop(sprintf("Expecting sim (%d) and obs (%d) to be the same length",length(sim),length(obs)))
  }
  if (add_one) {
    sim[sim == 0] = 1
  }

  if(distr == "pois") {
    rc <- dpois(obs, sim, log = T)
  } else if (distr == "norm") {
    rc <- dnorm(obs, sim, sd = param[[1]], log = T)
  } else  if (distr == "norm_cov") {
      rc <- dnorm(obs, sim, sd = pmax(obs,5)*param[[1]], log = T)
  }  else if (distr == "nbinom") {
    rc <- dnbinom(obs, mu=sim, size = param[[1]], log = T)
  } else if (distr == "sqrtnorm") {
      ##rc <- dnorm(sqrt(obs), sqrt(sim), sd=sqrt(sim)*param[[1]], log = T)
      rc <- dnorm(sqrt(obs), sqrt(sim), sd=sqrt(pmax(obs,5))*param[[1]], log = T)
  } else if (distr == "sqrtnorm_scale_sim") { #param 1 is cov, param 2 is multipler
    rc <- dnorm(sqrt(obs), sqrt(sim*param[[2]]), sd=sqrt(pmax(obs,5)*param[[2]])*param[[1]],log=T)
  } else {
    stop("Invalid stat specified")
  }

  return(rc)
}



# MCMC stuff -------------------------------------------------------------------

##' Fuction perturbs a seeding file based on a normal
##' proposal on the start date and
##' a poisson on the number of cases.
##'
##' @param seeding the original seeding
##' @param sd the standard deviation of the posson
##'
##'
##' @return a pertubed data frame
##'
##' @export
  perturb_seeding <- function(seeding,sd,date_bounds) {
    seeding <- seeding %>%
        dplyr::group_by(place) %>%
        dplyr::mutate(date = date+round(rnorm(1,0,sd))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          amount=round(pmax(rnorm(length(amount),amount,1),0)),
          date = pmin(pmax(date,date_bounds[1]),date_bounds[2])
        )

    return(seeding)

}


##' Fuction perturbs an npi parameter file based on
##' user-specified distributions
##'
##' @param snpi the original npis.
##' @param intervention_settings a list of perturbation specificationss
##'
##'
##' @return a pertubed data frame
##' @export
perturb_snpi <- function(snpi, intervention_settings) {
    ##Loop over all interventions
    for (intervention in names(intervention_settings)) { # consider doing unique(npis$npi_name) instead

        ##Only perform pertubations on interventions where it is specified ot do so.

        if ('perturbation' %in% names(intervention_settings[[intervention]])){

            ##get the random distribution from covidcommon package
            pert_dist <- covidcommon::as_random_distribution(intervention_settings[[intervention]][['perturbation']])

            ##get the npi values for this distribution
            ind <- (snpi[["npi_name"]] == intervention)

            ##add the pertubation...for now always parameterized in terms of a "reduction"
            snpi_new <- snpi[["reduction"]][ind] + pert_dist(sum(ind))

            ##check that this is in bounds (equivalent to having a positive probability)
            in_bounds_index <- covidcommon::as_density_distribution(
                                                intervention_settings[[intervention]][['value']]
                                            )(snpi_new) > 0

            ##return all in bounds proposals
            snpi$reduction[ind][in_bounds_index] <- snpi_new[in_bounds_index]
        }
    }
    return(snpi)
}

##' Fuction perturbs an npi parameter file based on
##' user-specified distributions
##'
##' @param hpar the original hospitalization parameters.
##' @param intervention_settings a list of perturbation specifications
##'
##'
##' @return a pertubed data frame
##' @export
perturb_hpar <- function(hpar, intervention_settings) {
    ##Loop over all interventions

    interventions <- c('p_confirmed_inf')

    for(intervention in interventions){
      hpar[hpar$parameter == intervention,]$value <-
        hpar[hpar$parameter == intervention,]$value +
        rnorm(sum(hpar$parameter == intervention),0,.01)
      hpar[hpar$parameter == intervention,]$value <- pmax(pmin(hpar$value[hpar$parameter == intervention],1),0)
    }

    return(hpar)
}
##' Function to go through to accept or reject seedings in a block manner based
##' on a geoid specific likelihood.
##'
##'
##' @param seeding_orig original seeding data frame (must have column place)
##' @param seeding_prop proposal seeding (must have column place)
##' @param snpi_orig original npi data frame  (must have column geoid)
##' @param snpi_prop proposal npi data frame  (must have column geoid)
##' @param orig_lls original ll data frame  (must have column ll and geoid)
##' @param prop_lls proposal ll fata frame (must have column ll and geoid)
##' @return a new data frame with the confirmed seedin.
##' @export
accept_reject_new_seeding_npis <- function(
  seeding_orig,
  seeding_prop,
  snpi_orig,
  snpi_prop,
  hpar_orig,
  hpar_prop,
  orig_lls,
  prop_lls
) {
  rc_seeding <- seeding_orig
  rc_snpi <- snpi_orig
  rc_hpar <- hpar_orig

  if(!all(orig_lls$geoid == prop_lls$geoid)){stop("geoids must match")}
  ##draw accepts/rejects
  ratio <- exp(prop_lls$ll - orig_lls$ll)
  accept <- ratio>runif(length(ratio),0,1)

  orig_lls$ll[accept] <- prop_lls$ll[accept]


  for (place in orig_lls$geoid[accept]) {
    rc_seeding[rc_seeding$place ==place, ] <- seeding_prop[seeding_prop$place ==place, ]
    rc_snpi[rc_snpi$geoid == place,] <- snpi_prop[snpi_prop$geoid == place, ]
    rc_hpar[rc_hpar$geoid == place,] <- hpar_prop[hpar_prop$geoid == place, ]
  }

  return(list(seeding=rc_seeding, snpi=rc_snpi, hpar = rc_hpar, lls = orig_lls))
}


##' Function accept proposals
##'
##'
##' @param ll_ref original seeding file.
##' @param ll_new proposal seeding file
##' @param ll_col a dataframe with columns orig_ll and prop_ll per place
##' @return a new data frame with the confirmed seedin.
##' @export
iterateAccept <- function(ll_ref,ll_new,ll_col) {
  ll_new <- ll_new[[ll_col]]
  ll_ref <- ll_ref[[ll_col]]
  ll_ratio <- exp(min(c(0, ll_new - ll_ref)))
  if (ll_ratio >= runif(1)) {
    return(TRUE)
  }
  return(FALSE)
}
