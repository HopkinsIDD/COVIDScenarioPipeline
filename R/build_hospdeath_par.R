

# This function is set to work in parallel. Need to make sure thats possible on the given machine

##' 
##' Build a set of sampled hospitalizations, deaths, and recoveries 
##'  from the incident infection data from the simulation model.
##'  
##' @param data data.frame of t (time in days) and incidI (incident infections)
##' @param p_hosp probability of hospitalization, among infections
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' 
build_hospdeath_par <- function(data, p_hosp, p_death, p_hosp_type="gamma",
                                time_hosp_pars = c(1.23, 0.79), 
                                time_death_pars = c(log(11.25), log(1.15)), 
                                time_disch_pars = c(log(11.5), log(1.22)),
                                cores=4) {
    
    require(parallel)
    
    t_ <- 1:nrow(data)
    dates_ <- as.Date(data$date)
    uid <- data$sim_beta
    date_tmp <- seq(min(dates_), (max(dates_)+125), by="days")
    
    
    # Get counts of hospitalization, death, and recovery, by day
    I_ <- data$incid
    if (p_hosp_type=="single"){
        H_ <- rbinom(I_, I_, rep(p_hosp,  length(I_)))  # Add hosp
    } else if (p_hosp_type=="range"){
        H_ <- rbinom(I_, I_, runif(length(I_), p_hosp[1],  p_hosp[2]))  # Add hosp
    } else if (p_hosp_type=="gamma"){
        H_ <- rbinom(I_, I_, rgamma(length(I_), shape=p_hosp$shape, rate=p_hosp$rate))  # Add hosp
    }
    
    D_ <- rbinom(H_, H_, rep(p_death, length(H_))) # Add Death
    R_ <- H_ - D_                                  # Add hospitalized recoveries
    names(H_) <- uid
    names(D_) <- uid
    names(R_) <- uid
    
    
    # Time to hospitalization
    H_time_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_date_ <- rep(dates_,H_) + H_time_
    names(H_time_) <- rep(names(H_), H_)
    names(H_date_) <- rep(names(H_), H_)
    
    # Time to death
    D_time_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_date_ <- rep(dates_,D_) + D_time_
    D_start_ <- rep(dates_, D_)
    names(D_time_) <- rep(names(D_), D_)
    names(D_date_) <- rep(names(D_), D_)
    
    # Time to recovery
    R_time_ <- floor(rlnorm(sum(R_), meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))
    R_date_ <- rep(dates_,R_) + R_time_
    R_start_ <- rep(dates_,R_)
    names(R_time_) <- rep(names(R_), R_)
    names(R_date_) <- rep(names(R_), R_)
    
    
    cl <- makeCluster(cores)
    
    # seq_R_fn <- function(x=X, R_start_, R_date_){
    #     seq(as.Date(R_start_[x]), as.Date(R_date_[x]), "days")
    # }
    
    # Get current hospitalization days and accumulate them -- Recoveries
    clusterExport(cl=cl, varlist=c('R_start_', 'R_date_'), envir=environment())
    curr_hosp_date <- rev(as.Date(unlist(
        parSapply(cl, 1:sum(R_), function(x) seq(as.Date(R_start_[x]), as.Date(R_date_[x]), "days"))),
        origin = "1970-01-01"))
    names(curr_hosp_date) <- rep(names(R_time_), R_time_)
    
    # Get current hospitalization days and accumulate them -- Deaths
    clusterExport(cl=cl, varlist=c('D_start_', 'D_date_'), envir=environment())
    curr_hospD_date <- rev(as.Date(unlist(
        parSapply(cl, 1:sum(D_), function(x) seq(as.Date(D_start_[x]), as.Date(D_date_[x]), "days"))),
        origin = "1970-01-01"))
    names(curr_hospD_date) <- rep(names(D_time_), D_time_)
    
    stopCluster(cl)
    
    
    # combine them
    curr_hosp_date <- c(curr_hosp_date, curr_hospD_date)
    data_currhosp <- as.data.frame(table(curr_hosp_date, names(curr_hosp_date)))
    colnames(data_currhosp) <- c("t","sim_beta","hosp_curr")
    
    
    
    data_H <- as.data.frame(table(H_date_, names(H_date_)))
    data_D <- as.data.frame(table(D_date_, names(D_date_)))
    colnames(data_H) <- c("t", "sim_beta", "incidH")
    colnames(data_D) <- c("t", "sim_beta", "incidD")
    
    data_D$t <- as.Date(data_D$t)
    data_H$t <- as.Date(data_H$t)
    data_currhosp$t <- as.Date(data_currhosp$t)
    
    
    res <- full_join(data.frame(t=as.Date(date_tmp)), data_H, by=c("t"="t"))
    res <- full_join(res, data_D, by=c("t"="t", "sim_beta"="sim_beta"))
    res <- full_join(res, data_currhosp, by=c("t"="t", "sim_beta"="sim_beta"))
    
    res <- res %>% mutate(incidH = ifelse(is.na(incidH), 0, incidH),
                          incidD = ifelse(is.na(incidD), 0, incidD),
                          hosp_curr = ifelse(is.na(hosp_curr), 0, hosp_curr))
    
    res <- res %>% filter(!is.na(sim_beta)) %>%
        arrange(sim_beta, t)
    
    return(res)
}

