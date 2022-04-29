# Likelihood stuff -------------------------------------------------------------

##' Function for applying time aggregation of variables on which to comput likelihoods
##' Note that bahavior is not consistent when multiples of time nits are passed in.
##'
##' @param data Vector of data to aggregate
##' @param dates Vector of dates
##' @param start_date First date to consider
##' @param end_date Last date to consider
##' @param period_unit Unit of period over which to aggregate
##' @param period_k Number of time units defining period over which to aggregate
##' @param aggregator Function for aggregations
##' @param na.rm Remove Nas?
##' @return NULL
#' @export
periodAggregate <- function(data, dates, start_date = NULL, end_date = NULL, period_unit_function, period_unit_validator, aggregator, na.rm = F) {
  if (na.rm) {
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

  if (!is.null(start_date)) {
    data <- data[dates >= start_date]
    dates <- dates[dates >= start_date]
  }

  tmp <- data.frame(date = dates, value = data)

  for (this_unit in seq_len(length(period_unit_function))) {
    tmp[[paste("time_unit", this_unit, sep = "_")]] <- period_unit_function[[this_unit]](dates)
  }
  tmp <- tmp %>%
    tidyr::unite("time_unit", names(tmp)[grepl("time_unit_", names(tmp))]) %>%
    dplyr::group_by(time_unit) %>%
    dplyr::summarize(first_date = min(date), value = aggregator(value), valid = period_unit_validator(date,time_unit)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(first_date) %>%
    dplyr::filter(valid)
  return(matrix(tmp$value, ncol = 1, dimnames = list(as.character(tmp$first_date))))
}


##' Function for computing statistics over which to compute likelihoods
##' @param df Data frame with data
##' @param time_col Name of the column with time
##' @param var_col Name of the variable with name of the  column with data to process
##' @param start_date First date to consider
##' @param end_date Last date to consider
##' @param stat_list List with specifications of statistics to compute
##' @return NULL
#' @export
getStats <- function(df, time_col, var_col, start_date = NULL, end_date = NULL, stat_list, debug_mode = FALSE) {
  rc <- list()
  for (stat in names(stat_list)) {
    s <- stat_list[[stat]]
    if (!is.null(start_date)) {
      stat_list[[stat]][["gt_start_date"]] <- max(c(start_date, stat_list[[stat]][["gt_start_date"]]))
    }
    if (!is.null(end_date)) {
      stat_list[[stat]][["gt_end_date"]] <- min(c(end_date, stat_list[[stat]][["gt_end_date"]]))
    }
    aggregator <- match.fun(s$aggregator)
    ## Get the time period over whith to apply aggregation
    period_info <- strsplit(s$period, " ")[[1]]
    if (period_info[2] == "weeks") {
      period_unit_function <- c(lubridate::epiweek, lubridate::epiyear)
    } else if (period_info[2] == "days") {
      period_unit_function <- c(lubridate::day, lubridate::month, lubridate::year)
    } else if (period_info[2] == "months") {
      period_unit_function <- c(lubridate::month, lubridate::year)
    } else {
      stop(paste(period_info[2], "as an aggregation unit is not supported right now"))
    }

    if (period_info[1] != 1) {
      stop(paste(period_info[1], period_info[2], "as an aggregation unit is not supported right now"))
    }


    period_unit_validator <- function(dates, units, local_period_unit_function = period_unit_function) {
      first_date <- min(dates)
      last_date <- min(dates) + (length(unique(dates))-1)
      return(all(c(
        local_period_unit_function[[1]](first_date) != local_period_unit_function[[1]](first_date - 1)
      , local_period_unit_function[[1]](last_date) != local_period_unit_function[[1]](last_date + 1)
      )))
    }

    if (s$period == "1 weeks") {
      period_unit_validator <- function(dates, units) {
        return(length(unique(dates)) == 7)
      }
    } else if (s$period == "1 days") {
      period_unit_validator <- function(dates, units) {
        return(TRUE)
      }
    }
    if (debug_mode) {
      period_unit_validator <- function(dates, units, local_period_unit_function = period_unit_function) {
        first_date <- min(dates)
        last_date <- min(dates) + (length(unique(dates)) - 1)
        return(
          any(sapply(local_period_unit_function, function(x) {
            x(last_date) != x(last_date + 1)
          }))
          && any(sapply(local_period_unit_function, function(x) {
            x(first_date) != x(first_date - 1)
          }))
          && all(sapply(local_period_unit_function, function(x) {
            x(first_date) == x(last_date)
          }))
          && all(first_date:last_date == dates)
        )
      }
    }

    if (!all(c(time_col, s[[var_col]]) %in% names(df))) {
      stop(paste0(
        "At least one of columns: [",
        time_col,
        ",",
        s[[var_col]],
        "] not in df columns: ",
        paste(names(df), collapse = ",")
      ))
    }

    res <- inference::periodAggregate(df[[s[[var_col]]]],
                                      df[[time_col]],
                                      stat_list[[stat]][["gt_start_date"]],
                                      stat_list[[stat]][["gt_end_date"]],
                                      period_unit_function,
                                      period_unit_validator,
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
    rc <- dpois(round(obs), sim, log = T)
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
##' Function to compute cumulative counts across geoids
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

# MCMC stuff -------------------------------------------------------------------

##' Function perturbs a seeding file based on a normal
##' proposal on the start date and
##' a poisson on the number of cases.
##'
##' @param seeding the original seeding
##' @param date_sd the standard deviation parameter of the normal distribution used to perturb date
##' @param amount_sd the standard deviation parameter of the normal distribution used to perturb amount
##' @param continuous Whether the seeding is passed to a continuous model or not
##'
##' @return a perturbed data frame
##'
##' @export
perturb_seeding <- function(seeding, date_sd, date_bounds, amount_sd = 1, continuous = FALSE) {

  if (!("no_perturb" %in% colnames(seeding))){
      perturb <- !logical(nrow(seeding))
  } else {
      perturb <- !seeding$no_perturb
  }

  if (date_sd > 0) {
    seeding$date[perturb] <- pmin(pmax(seeding$date + round(rnorm(nrow(seeding),0,date_sd)), date_bounds[1]), date_bounds[2])[perturb]
  }
  if (amount_sd > 0) {
    round_func <- ifelse(continuous, function(x){return(x)}, round)
    seeding$amount[perturb] <- round_func(pmax(rnorm(nrow(seeding),seeding$amount, amount_sd),0))[perturb]
  }

  return(seeding)

}




##' Function perturbs an npi parameter file based on
##' user-specified distributions
##'
##' @param snpi the original npis.
##' @param intervention_settings a list of perturbation specifications
##'
##'
##' @return a perturbed data frame
##' @export
perturb_snpi <- function(snpi, intervention_settings) {
  ##Loop over all interventions
  for (intervention in names(intervention_settings)) { # consider doing unique(npis$npi_name) instead

    ##Only perform perturbations on interventions where it is specified to do so.

    if ('perturbation' %in% names(intervention_settings[[intervention]])){

      ##get the random distribution from covidcommon package
      pert_dist <- covidcommon::as_random_distribution(intervention_settings[[intervention]][['perturbation']])

      ##get the npi values for this distribution
      ind <- (snpi[["npi_name"]] == intervention)
      if(!any(ind)){
        next
      }

      ##add the perturbation...for now always parameterized in terms of a "reduction"
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


##' Function perturbs an npi parameter file based on
##' user-specified distributions
##'
##' @param hnpi the original npis.
##' @param intervention_settings a list of perturbation specificationss
##'
##'
##' @return a perturbed data frame
##' @export
perturb_hnpi <- function(hnpi, intervention_settings) {
  ##Loop over all interventions
  for (intervention in names(intervention_settings)) { # consider doing unique(npis$npi_name) instead

    ##Only perform perturbations on interventions where it is specified to do so.

    if ('perturbation' %in% names(intervention_settings[[intervention]])){

      ##get the random distribution from covidcommon package
      pert_dist <- covidcommon::as_random_distribution(intervention_settings[[intervention]][['perturbation']])

      ##get the npi values for this distribution
      ind <- (hnpi[["npi_name"]] == intervention)
      if(!any(ind)){
        next
      }

      ##add the perturbation...for now always parameterized in terms of a "reduction"
      hnpi_new <- hnpi[["reduction"]][ind] + pert_dist(sum(ind))

      ##check that this is in bounds (equivalent to having a positive probability)
      in_bounds_index <- covidcommon::as_density_distribution(
        intervention_settings[[intervention]][['value']]
      )(hnpi_new) > 0

      ##return all in bounds proposals
      hnpi$reduction[ind][in_bounds_index] <- hnpi_new[in_bounds_index]
    }
  }
  return(hnpi)
}

##' Fucction perturbs an outcomes parameter file based on
##' user-specified distributions
##'
##' @param hpar the original hospitalization (outcomes) parameters.
##' @param intervention_settings a list of perturbation specifications
##'
##'
##' @return a perturbed data frame
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
##' Function to go through to accept or reject proposed parameters for each geoid based
##' on a geoid specific likelihood.
##'
##'
##' @param seeding_orig original seeding data frame (must have column place)
##' @param seeding_prop proposal seeding (must have column place)
##' @param snpi_orig original npi data frame  (must have column geoid)
##' @param snpi_prop proposal npi data frame  (must have column geoid)
##' @param hnpi_orig original npi data frame  (must have column geoid)
##' @param hnpi_prop proposal npi data frame  (must have column geoid)
##' @param orig_lls original ll data frame  (must have column ll and geoid)
##' @param prop_lls proposal ll fata frame (must have column ll and geoid)
##' @return a new data frame with the confirmed seedin.
##' @export
accept_reject_new_seeding_npis <- function(
  seeding_orig,
  seeding_prop,
  snpi_orig,
  snpi_prop,
  hnpi_orig,
  hnpi_prop,
  hpar_orig,
  hpar_prop,
  orig_lls,
  prop_lls
) {
  rc_seeding <- seeding_orig
  rc_snpi <- snpi_orig
  rc_hnpi <- hnpi_orig
  rc_hpar <- hpar_orig

  if (!all(orig_lls$geoid == prop_lls$geoid)) {
    stop("geoids must match")
  }
  ##draw accepts/rejects
  ratio <- exp(prop_lls$ll - orig_lls$ll)
  accept <- ratio > runif(length(ratio), 0, 1)

  orig_lls$ll[accept] <- prop_lls$ll[accept]
  
  orig_lls$accept <- as.numeric(accept) # added column for acceptance decision
  orig_lls$accept_prob <- min(1,ratio) # added column for acceptance decision

  for (place in orig_lls$geoid[accept]) {
    rc_seeding[rc_seeding$place == place, ] <- seeding_prop[seeding_prop$place ==place, ]
    rc_snpi[rc_snpi$geoid == place, ] <- snpi_prop[snpi_prop$geoid == place, ]
    rc_hnpi[rc_hnpi$geoid == place, ] <- hnpi_prop[hnpi_prop$geoid == place, ]
    rc_hpar[rc_hpar$geoid == place, ] <- hpar_prop[hpar_prop$geoid == place, ]
  }

  return(list(
    seeding = rc_seeding,
    snpi = rc_snpi,
    hnpi = rc_hnpi,
    hpar = rc_hpar,
    lls = orig_lls
  ))
}


##' Function accept proposals
##'
##'
##' @param ll_ref current accepted likelihood
##' @param ll_new likelihood of proposal
##' @return boolean whether to accept the likelihood
##' @export
iterateAccept <- function(ll_ref, ll_new) {
  if (length(ll_ref) != 1 | length(ll_new) !=1) {
    stop("Iterate accept currently on works with single row data frames")
  }


  ll_ratio <- exp(min(c(0, ll_new - ll_ref)))
  if (ll_ratio >= runif(1)) {
    return(TRUE)
  }
  return(FALSE)
}

# Extra functions for MCMC diagnostics and adaptation ------------------

##' Function adds a column to the npi parameter file to record the perturbation standard deviation, initially taken from the config file
##'
##' @param snpi the original npis.
##' @param intervention_settings a list of perturbation specifications
##'
##'
##' @return data frame with perturb_sd column added
##' @export
add_perturb_column_snpi <- function(snpi, intervention_settings) {

  snpi$perturb_sd <- 0 # create a column in the parameter data frame to hold the perturbation sd
  
  ##Loop over all interventions
  for (intervention in names(intervention_settings)) {
    ##Only perform perturbations on interventions where it is specified to do so.
    
    if ('perturbation' %in% names(intervention_settings[[intervention]])){
      
      ##find the npi with this name
      ind <- (snpi[["npi_name"]] == intervention)
      if(!any(ind)){
        next
      }
      
      if(!'sd' %in% names(intervention_settings[[intervention]][['perturbation']])){
        stop("Cannot add perturbation sd to column unless 'sd' values exists in config$interventions$settings$this_intervention$perturbation")
      }
      
      pert_sd <-intervention_settings[[intervention]][['perturbation']][['sd']]
      #print(paste0(intervention," initial perturbation sd is ",pert_sd))
      
      snpi$perturb_sd[ind] <- pert_sd # update perturbation
      
    }
  }
  
  return(snpi)
}




##' Function perturbs an npi parameter file based on the current perturbation standard deviation in the file, and also can update the perturbation value if the adaptive mode is turned on
##'
##' @param snpi the original npis.
##' @param intervention_settings a list of perturbation specifications
##' @param llik log likelihood values
##' 
##' @return a perturbed data frame
##' @export
perturb_snpi_from_file  <- function(snpi, intervention_settings, llik){
  

  ##Loop over all interventions
  for (intervention in names(intervention_settings)) {
    
    ##Only perform perturbations on interventions where it is specified to do so.
    
    if ('perturbation' %in% names(intervention_settings[[intervention]])){
      
      ##find all the npi with this name (might be one for each geoID)
      ind <- (snpi[["npi_name"]] == intervention)
      if(!any(ind)){
        next
      }
      
      ## for each of them generate the perturbation and update their value
      for (this_npi_ind in which(ind)){ # for each geoid that has this interventions
        
        this_geoid <- snpi[["geoid"]][this_npi_ind]
        this_accept_avg <- llik$accept_avg[llik$geoid==this_geoid] 
        his_accept_prob <- llik$accept_prob[llik$geoid==this_geoid] 
        this_intervention_setting<- intervention_settings[[intervention]]
        
        ##get the random distribution from covidcommon package
        pert_dist <- covidcommon::as_random_distribution(this_intervention_setting$perturbation)
        
        ##add the perturbation...for now always parameterized in terms of a "reduction"
        snpi_new <- snpi[["reduction"]][this_npi_ind] + pert_dist(1)
        
        ##check that this is in bounds (equivalent to having a positive probability)
        in_bounds_index <- covidcommon::as_density_distribution(
          intervention_settings[[intervention]][['value']]
        )(snpi_new) > 0
        
        ## include this perturbed parameter if it is in bounds
        snpi$reduction[this_npi_ind][in_bounds_index] <- snpi_new[in_bounds_index]
        
      }
    }
  }
  
  return(snpi)
}

##' Function adds a column to the npi parameter file to record the perturbation standard deviation, initially taken from the config file
##'
##' @param hnpi the original npis.
##' @param intervention_settings a list of perturbation specifications
##'
##'
##' @return data frame with perturb_sd column added
##' @export
add_perturb_column_hnpi <- function(hnpi, intervention_settings) {
  
  hnpi$perturb_sd <- 0 # create a column in the parameter data frame to hold the perturbation sd
  
  ##Loop over all interventions
  for (intervention in names(intervention_settings)) {
    ##Only perform perturbations on interventions where it is specified to do so.
    
    if ('perturbation' %in% names(intervention_settings[[intervention]])){
      
      ##find the npi with this name
      ind <- (hnpi[["npi_name"]] == intervention)
      if(!any(ind)){
        next
      }
      
      if(!'sd' %in% names(intervention_settings[[intervention]][['perturbation']])){
        stop("Cannot add perturbation sd to column unless 'sd' values exists in config$interventions$settings$this_intervention$perturbation")
      }
      
      pert_sd <-intervention_settings[[intervention]][['perturbation']][['sd']]
      #print(paste0(intervention," initial perturbation sd is ",pert_sd))
      
      hnpi$perturb_sd[ind] <- pert_sd # update perturbation
      
    }
  }
  
  return(hnpi)
}




##' Function perturbs an npi parameter file based on the current perturbation standard deviation in the file, and also can update the perturbation value if the adaptive mode is turned on
##'
##' @param hnpi the original npis.
##' @param intervention_settings a list of perturbation specifications
##' @param llik log likelihood values
##' 
##' @return a perturbed data frame
##' @export
perturb_hnpi_from_file  <- function(hnpi, intervention_settings, llik){
  
  
  ##Loop over all interventions
  for (intervention in names(intervention_settings)) {
    
    ##Only perform perturbations on interventions where it is specified to do so.
    
    if ('perturbation' %in% names(intervention_settings[[intervention]])){
      
      ##find all the npi with this name (might be one for each geoID)
      ind <- (hnpi[["npi_name"]] == intervention)
      if(!any(ind)){
        next
      }
      
      ## for each of them generate the perturbation and update their value
      for (this_npi_ind in which(ind)){ # for each geoid that has this interventions
        
        this_geoid <- hnpi[["geoid"]][this_npi_ind]
        this_accept_avg <- llik$accept_avg[llik$geoid==this_geoid] 
        this_intervention_setting<- intervention_settings[[intervention]]
        
        ##get the random distribution from covidcommon package
        pert_dist <- covidcommon::as_random_distribution(this_intervention_setting$perturbation)
        
        ##add the perturbation...for now always parameterized in terms of a "reduction"
        hnpi_new <- hnpi[["reduction"]][this_npi_ind] + pert_dist(1)
        
        ##check that this is in bounds (equivalent to having a positive probability)
        in_bounds_index <- covidcommon::as_density_distribution(
          intervention_settings[[intervention]][['value']]
        )(hnpi_new) > 0
        
        ## include this perturbed parameter if it is in bounds
        hnpi$reduction[this_npi_ind][in_bounds_index] <- hnpi_new[in_bounds_index]
        
      }
    }
  }
  
  return(hnpi)
}

