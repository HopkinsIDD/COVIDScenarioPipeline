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
    
    if(!all(c(time_col, s[[var_col]]) %in% names(df)))
    {
      stop(paste0("At least one of columns: [",time_col,",", s[[var_col]],"] not in df columns: ", paste(names(df), collapse=",")))
    }
    
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


##'
##' Function to calculate a hierarchical adjustment to the LL
##' contribution under the assumption that everything comes from
##' a normal distribution with some variance.
##'
##' @param stat the statistic to calculate the penalty on
##' @param infer_frame data frame with the statistics in it
##' @param geodata geodata containing geoid from npi fram and the grouping column
##' @param geo_group_col the column to group on
##' @param stat_name_col column holding stats name...default is npi_name
##' @param stat_col column hold the stat
##' @param transform how should the data be transformed before calc
##' @param min_sd what is the minimum SD to consider. Default is .1
##'
##' @return a data frame with geoids and a per geoid LL adjustment
##'
##' @export
##'
calc_hierarchical_likadj <- function (stat,
                                      infer_frame,
                                      geodata,
                                      geo_group_column,
                                      stat_name_col = "npi_name",
                                      stat_col="reduction",
                                      transform = "none",
                                      min_sd=.1) {
  
  require(dplyr)
  
  if (transform == "logit") {
    infer_frame <- infer_frame  %>%
      #mutate(value = value)
      mutate(!!sym(stat_col) := qlogis(!!sym(stat_col)),
             !!sym(stat_col):=ifelse(!!sym(stat_col)< -2*10^12, -2*10^12, !!sym(stat_col)),
             !!sym(stat_col):=ifelse(!!sym(stat_col)> 2*10^12, 2*10^12, !!sym(stat_col)))
  } else if (transform!="none") {
    stop("specified transform not yet supported")
  }
  
  ##print(stat)
  ##cat("sd=",max(sd(infer_frame[[stat_col]]), min_sd,na.rm=T),"\n")
  ##cat("mean=",mean(infer_frame[[stat_col]]),"\n")
  ##print(range(infer_frame[[stat_col]]))
  
  rc <- infer_frame%>%
    filter(!!sym(stat_name_col)==stat)%>%
    inner_join(geodata)%>%
    group_by(!!sym(geo_group_column))%>%
    mutate(likadj = dnorm(!!sym(stat_col),
                          mean(!!sym(stat_col)),
                          max(sd(!!sym(stat_col)), min_sd, na.rm=T), log=TRUE))%>%
    ungroup()%>%
    select(geoid, likadj)
  
  return(rc)
}


##'
##'
##' Function to calcualte the likelihood adjustment based on a prior
##'
##' @param params the parameter values to calculate the likelihood adjust for
##' @param dist the distribution to use
##' @param dist_pars the parameters of the distribution
##'
##' @return a likelihood sdjustment per param
##'
##' @export
##'
calc_prior_likadj  <- function(params,
                               dist,
                               dist_pars) {
  
  if (dist=="normal") {
    rc <- dnorm(params, dist_pars[[1]], dist_pars[[2]], log=TRUE)
  } else  if (dist=="logit_normal") {
    params <- pmax(params, 10^-12)
    params <- pmin(params, 1-10^-12)
    rc <- dnorm(qlogis(params), qlogis(dist_pars[[1]]), dist_pars[[2]], log=TRUE)
  } else {
    stop("This distribution is unsupported")
  }
  
  return(rc)
}

##'
##'
##' Function to compute cumulative counts across geoids
##'
##' @param sim_hosp output of ouctomes branching process
##'
##' @return dataframe with the added columns for cumulative counts
##'
##' @export
##'
compute_cumulative_counts <- function(sim_hosp) {
  res <- sim_hosp %>% 
    gather(var, value, -time, -geoid) %>% 
    group_by(geoid, var) %>% 
    arrange(time) %>% 
    mutate(cumul = cumsum(value)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = "var", values_from = c("value", "cumul")) %>% 
    select(-(contains("cumul") & contains("curr")))
  
  colnames(res) <- str_replace_all(colnames(res), c("value_" = "", "cumul_incid" = "cumul"))
  return(res)
}

##'
##'
##' Function to compute total counts across geoids
##'
##' @param sim_hosp output of ouctomes branching process
##'
##' @return dataframe with the added rows for all counts
##'
##' @export
##'
compute_totals <- function(sim_hosp) {
  sim_hosp %>% 
    group_by(time) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    mutate(geoid = "all") %>% 
    select(all_of(colnames(sim_hosp))) %>% 
    rbind(sim_hosp)
}

#' @title Add national likelihood
#' @description adds the national-level lieklihood to each geoid weighing by each
#' geoid's contribution to the global likelihood
#'
#' @param data_likelihood computed data likelihoods by geoid
#' @param obs_nodename node name
#' @param all_locations all unique geoids
#'
#' @return return
#' @export
#' 
add_national_likelihood <- function(data_likelihood,
                                    obs_nodename,
                                    all_locations) {
  
  # Of no national level likelihood return without modifications
  if (!("all" %in% unique(data_likelihood[[obs_nodename]]))){
    return(data_likelihood)
  }
  
  if (obs_nodename != "geoid") 
    stop("add_national_likelihood.R does not accept other obs_nodenames than geoid for now.")
  
  # Get likelihoods at geoid level
  geoid_liks <- data_likelihood$ll[data_likelihood[[obs_nodename]] != "all"]
  national_lik <- data_likelihood$ll[data_likelihood[[obs_nodename]] == "all"]
  
  # Compute weights
  if (length(geoid_liks) > 0) {
    lik_w <- geoid_liks/sum(geoid_liks)
  } else {
    lik_w <- 1/length(all_locations)
    data_likelihood <- tidyr::complete(data_likelihood, 
                                       geoid = all_locations)
  }
  
  data_likelihood <- filter(data_likelihood,  !!rlang::sym(obs_nodename) != "all") %>%
    mutate(ll = ifelse(is.na(ll), 0, ll) + national_lik * lik_w)
  
  return(data_likelihood)
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
      if(!any(ind)){
        next
      }
      
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
  
  for(intervention in names(intervention_settings)){
    for(quantity in names(intervention_settings[[intervention]])){
      if('perturbation' %in% names(intervention_settings[[intervention]][[quantity]])){
        intervention_quantity <- intervention_settings[[intervention]][[quantity]]
        ## get the random distribution from covidcommon package
        pert_dist <- covidcommon::as_random_distribution(intervention_quantity[['perturbation']])
        
        ##get the hpar values for this distribution
        ind <- (hpar[["outcome"]] == intervention) & (hpar[["quantity"]] == quantity) # & (hpar[['source']] == intervention_settings[[intervention]][['source']])
        if(!any(ind)){
          next
        }
        
        ## add the perturbation...
        if (!is.null(intervention_quantity[['perturbation']][["transform"]])) {
          if (intervention_quantity[['perturbation']][["transform"]] == "logit") {
            # For [0,1] bounded parameters add on logit scale
            x <- hpar[["value"]][ind]
            hpar_new <- 1/(1+exp(-(log(x/(1-x)) + pert_dist(sum(ind)))))
          } else if (intervention_quantity[['perturbation']][["transform"]] == "log") {
            # For [0, Inf) bounded parameters add on log scale
            hpar_new <- exp(log(hpar[["value"]][ind]) + pert_dist(sum(ind)))
          } else {
            stop("unkown transform")
          }
        } else {
          hpar_new <- hpar[["value"]][ind] + pert_dist(sum(ind))
        }
        
        ## Check that this is in the support of the original distribution
        in_bounds_index <- covidcommon::as_density_distribution(intervention_quantity[['value']])(hpar_new) > 0
        hpar$value[ind][in_bounds_index] <- hpar_new[in_bounds_index]
      }
    }
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
##' @param ll_ref current accepted likelihood
##' @param ll_new likelihood of proposal
##' @return boolean whether to accept the likelihood
##' @export
iterateAccept <- function(ll_ref,ll_new) {
    if (length(ll_ref) != 1 | length(ll_new) !=1) {
        stop("Iterate accept currently on works with single row data frames")
    }


  ll_ratio <- exp(min(c(0, ll_new - ll_ref)))
  if (ll_ratio >= runif(1)) {
    return(TRUE)
  }
  return(FALSE)
}
