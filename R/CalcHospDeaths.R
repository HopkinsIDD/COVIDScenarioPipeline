

# SETUP -------------------------------------------------------------------

library(tidyverse)






# RUN THE CALCULATION -----------------------------------------------------

# will have to adjust to model output and time
# add detection and confirmation later
# 
# test_data <- data.frame(t = 0:8, S = c(1000, 999, 999, 990, 980, 960, 800, 700, 500), incidI = c(1, 1, 0, 10, 10, 20, 160, 100, 200))
# 
# 
# data <- test_data %>% select(t, incidI)
# res_data <- build_hospdeath(data, p_hosp=p_hosp[3], p_death=p_death[3], time_hosp_pars, time_death_pars, time_disch_pars)


##' Function 
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
build_hospdeath <- function(data, p_hosp, p_death,
                            time_hosp_pars = c(1.23, 0.79), 
                            time_death_pars = c(log(11.25), log(1.15)), 
                            time_disch_pars = c(log(11.5), log(1.22))) {
    
    # Set up results data
    #res_data <- data.frame(t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0, incidR=0) 
    res_data <- data.frame(date=NA, t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0) 
    res_data$date <- seq(as.Date(data$time[1]),(as.Date(data$time[1])+(nrow(res_data)-1)), by="days")
    res_data$incidI[1:nrow(data)] <- data$incidI
    
    t_ <- 1:nrow(data)
    
    # Add hosp    
    I_ <- data$incidI
    H_ <- rbinom(I_, I_, rep(p_hosp, length(I_)))
    
    # Add Death
    D_ <- rbinom(H_, H_, rep(p_death, length(H_)))
    # R_ <- H_ - D_  # hospitalized recoveries
    
    # Time to hospitalization
    H_time_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_time_ <- rep(t_,H_) + H_time_
    
    H_time_count <- as.integer(table(H_time_))
    H_time_times <- as.integer(names(table(H_time_)))
    res_data$incidH[H_time_times] <- H_time_count
    
    # Time to death
    D_time_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_time_ <- rep(t_,D_) + D_time_
    
    D_time_count <- as.integer(table(D_time_))
    D_time_times <- as.integer(names(table(D_time_)))
    res_data$incidD[D_time_times] <- D_time_count
    
    # # Time to recovery
    # R_time_ <- floor(rlnorm(R_, meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))
    # R_time_ <- R_time_ + H_time_[!die_] # get final time to recover from hospitalization
    # R_time_count <- as.integer(table(R_time_))
    # R_time_times <- as.integer(names(table(R_time_)))
    # incidR_[R_time_times + d] <- R_time_count
    # 
    # Current Hospitalized
    # NEED TO ADD THIS 
    
    return(res_data)
}





##' Function 
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
build_hospdeath_fullsim <- function(data, p_hosp, p_death,
                            time_hosp_pars = c(1.23, 0.79), 
                            time_death_pars = c(log(11.25), log(1.15)), 
                            time_disch_pars = c(log(11.5), log(1.22))) {
    
    # Set up results data
    #res_data <- data.frame(t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0, incidR=0) 
    res_data <- data.frame(date=NA, t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0) 
    res_data$incidI[1:nrow(data)] <- data$incidI
    
    t_ <- 1:nrow(data)
    dates_ <- as.Date(data$time)
    sim_num <- data$sim_num
    county <- data$county
    uid <- paste0(county, "-",sim_num)
    date_tmp <- seq(min(dates_), (max(dates_)+125), by="days")
    
    # Add hosp    
    I_ <- data$incidI
    H_ <- rbinom(I_, I_, rep(p_hosp, length(I_)))
    names(H_) <- uid
    # Add Death
    D_ <- rbinom(H_, H_, rep(p_death, length(H_)))
    # R_ <- H_ - D_  # hospitalized recoveries
    names(D_) <- uid
    
    
    # Time to hospitalization
    H_time_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_time_ <- rep(dates_,H_) + H_time_
    
    # Time to death
    D_time_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_time_ <- rep(dates_,D_) + D_time_
    
    names(H_time_) <- rep(uid, H_)
    names(D_time_) <- rep(uid, D_)
    
    data_H <- data.frame(county_sim = names(H_time_), date=H_time_, incidH=1)
    data_D <- data.frame(county_sim = names(D_time_), date=D_time_, incidD=1)
    
    res <- full_join(data.frame(time=date_tmp), data_H, by=c("time"="date"))
    res <- full_join(res, data_D, by=c("time"="date", "county_sim"="county_sim"))
    res <- res %>% mutate(incidH = ifelse(is.na(incidH), 0, incidH),
                          incidD = ifelse(is.na(incidD), 0, incidD),
                          county = substr(county_sim,1,4),
                          sim_num= substr(county_sim, 6,10))
    
    res <- res %>% filter(!is.na(county_sim)) %>% select(-county_sim) %>%
        arrange(county, sim_num, time)
    
    return(res)
}



##' Function 
##'  Build a set of sampled hospitalizations, deaths, and recoveries 
##'  from the incident infection data from the simulation model.
##'  But instantly summarize to save memory
##'  
##' @param data data.frame of t (time in days) and incidI (incident infections)
##' @param p_hosp probability of hospitalization, among infections
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' 

build_hospdeath_summary <- function(data, p_hosp, p_death,
                                    time_hosp_pars = c(1.23, 0.79), 
                                    time_death_pars = c(log(11.25), log(1.15)), 
                                    time_disch_pars = c(log(11.5), log(1.22)),
                                    end_date = "2020-04-01",
                                    length_geoid = 5) {
    
    # Set up results data
    #res_data <- data.frame(t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0, incidR=0) 
    res_data <- data.frame(date=NA, t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0) 
    res_data$incidI[1:nrow(data)] <- data$incidI
    
    t_ <- 1:nrow(data)
    dates_ <- as.Date(data$time)
    sim_num <- data$sim_num
    geoid <- data$geoid
    uid <- paste0(geoid, "-",sim_num)
    date_tmp <- seq(min(dates_), (max(dates_)+125), by="days")
    
    # Add hosp    
    I_ <- data$incidI
    H_ <- rbinom(I_, I_, rep(p_hosp, length(I_)))
    names(H_) <- uid
    # Add Death
    D_ <- rbinom(H_, H_, rep(p_death, length(H_)))
    # R_ <- H_ - D_  # hospitalized recoveries
    names(D_) <- uid
    
    
    # Time to hospitalization
    H_time_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_time_ <- rep(dates_,H_) + H_time_
    
    # Time to death
    D_time_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_time_ <- rep(dates_,D_) + D_time_
    
    names(H_time_) <- rep(uid, H_)
    names(D_time_) <- rep(uid, D_)
    
    data_H <- data.frame(county_sim = names(H_time_), date=H_time_, incidH=1)
    data_D <- data.frame(county_sim = names(D_time_), date=D_time_, incidD=1)
    
    res <- full_join(data.frame(time=date_tmp), data_H, by=c("time"="date"))
    res <- full_join(res, data_D, by=c("time"="date", "county_sim"="county_sim"))
    res <- res %>% mutate(incidH = ifelse(is.na(incidH), 0, incidH),
                          incidD = ifelse(is.na(incidD), 0, incidD),
                          geoid = substr(county_sim,1,length_geoid),
                          sim_num= substr(county_sim,length_geoid+1,length_geoid+4))
    
    res_county <- res %>% 
                  filter(!is.na(county_sim)) %>% 
                  select(-county_sim) %>%
                  filter(time <= as.Date(end_date)) %>%
                  group_by(geoid, sim_num) %>% 
                  summarize(nhosp = sum(incidH), ndeath = sum(incidD)) %>%
                  ungroup() %>% 
                  group_by(geoid) %>% 
                  summarize(nhosp_final = mean(nhosp),
                            nhosp_lo = quantile(nhosp, 0.2),
                            nhosp_hi = quantile(nhosp, 0.8),
                            ndeath_final = mean(ndeath),
                            ndeath_lo = quantile(ndeath, 0.2),
                            ndeath_hi = quantile(ndeath, 0.8))
    res_total <- res %>% 
                 filter(!is.na(county_sim)) %>% 
                 select(-county_sim) %>%
                 filter(time <= as.Date(end_date)) %>%
                 group_by(sim_num) %>% 
                 summarize(nhosp = sum(incidH), ndeath = sum(incidD)) %>%
                 ungroup() %>% 
                 summarize(nhosp_final = mean(nhosp),
                          nhosp_lo = quantile(nhosp, 0.2),
                          nhosp_hi = quantile(nhosp, 0.8),
                          ndeath_final = mean(ndeath),
                          ndeath_lo = quantile(ndeath, 0.2),
                          ndeath_hi = quantile(ndeath, 0.8))
    return(list(res_county, res_total))
}





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
build_hospdeath_SLOW <- function(data, p_hosp, p_death,
                            time_hosp_pars = c(1.23, 0.79), 
                            time_death_pars = c(log(11.25), log(1.15)), 
                            time_disch_pars = c(log(11.5), log(1.22))) {

    # Set up results data
    #res_data <- data.frame(t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0, incidR=0) 
    res_data <- data.frame(t=1:(nrow(data)+125), incidI=0, incidH=0, incidD=0) 
    
    # Loop through the rows
    for (d in 1:nrow(data)){
        
        #incidH_ <- incidD_ <- incidR_ <- rep(0, nrow(res_data))
        incidH_ <- incidD_ <- rep(0, nrow(res_data))
        
        # Add hosp    
        I_ <- data$incidI[d] 
        H_ <- rbinom(1, I_, p_hosp)
    
        # Add Death
        die_ <- rbinom(H_, 1, p_death)
        D_ <- sum(die_)
        # R_ <- H_ - D_  # hospitalized recoveries
        
        # Time to hospitalization
        H_time_ <- floor(rlnorm(H_, meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
        H_time_count <- as.integer(table(H_time_))
        H_time_times <- as.integer(names(table(H_time_)))
        incidH_[H_time_times + d] <- H_time_count
        
        # Time to death
        D_time_ <- floor(rlnorm(D_, meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
        D_time_ <- D_time_ + H_time_[die_] # get final time to recover from hospitalization
        D_time_count <- as.integer(table(D_time_))
        D_time_times <- as.integer(names(table(D_time_)))
        incidD_[D_time_times + d] <- D_time_count
        
        # # Time to recovery
        # R_time_ <- floor(rlnorm(R_, meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))
        # R_time_ <- R_time_ + H_time_[!die_] # get final time to recover from hospitalization
        # R_time_count <- as.integer(table(R_time_))
        # R_time_times <- as.integer(names(table(R_time_)))
        # incidR_[R_time_times + d] <- R_time_count
        # 
        # Current Hospitalized
        # --> NEED TO ADD THIS
        
        # Add all back to the full data
        res_data$incidH <- res_data$incidH + incidH_
        res_data$incidD <- res_data$incidD + incidD_ 
        #res_data$incidR <- res_data$incidR + incidR_ 
        res_data$incidI[d] <- I_ 
    }
    
    return(res_data)
}


# # Test
# data <- test_data %>% select(t, incidI)
# res_data <- build_hospdeath(data, p_hosp=p_hosp[3], p_death=p_death[3], time_hosp_pars, time_death_pars, time_disch_pars)
# 



# GET SAMPLED SUMMARY STATS -----------------------------------------------



##' Build a set of sampled hospitalizations, deaths, and recoveries 
##'  from the incident infection data from the simulation model.
##' 
##' @param iter number of sanpling iterations to run for the summary
##' @param data data.frame of t (time in days) and incidI (incident infections)
##' @param p_hosp probability of hospitalization, among infections
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' 
get_hospdeath_ests <- function(iters=100, data, p_hosp, p_death, 
                                time_hosp_pars = c(1.23, 0.79), 
                                time_death_pars = c(log(11.25), log(1.15)), 
                                time_disch_pars = c(log(11.5), log(1.22))) {
    
    # Run some iterations
    res_data <- NULL
    for (i in 1:iters){
        res_data_ <- build_hospdeath(data, p_hosp, p_death, time_hosp_pars, time_death_pars, time_disch_pars)
        res_data <- bind_rows(res_data, res_data_ %>% mutate(sim=i))
    }
    
    # Summarize it
    res <- res_data %>% group_by(t) %>%
        summarize(incidI = mean(incidI), 
                  
                  incidH_mean = mean(incidH),
                  incidH_ll = quantile(incidH, probs=0.025),
                  incidH_ul = quantile(incidH, probs=0.975),
                  
                  incidD_mean = mean(incidD),
                  incidD_ll = quantile(incidD, probs=0.025),
                  incidD_ul = quantile(incidD, probs=0.975),
                  
                  incidR_mean = mean(incidR),
                  incidR_ll = quantile(incidR, probs=0.025),
                  incidR_ul = quantile(incidR, probs=0.975))
    
    return(res)
}



# # Test
# data <- test_data %>% select(t, incidI)
# res_data <- get_hospdeath_ests(iter=100, data, p_hosp=p_hosp[3], p_death=p_death[3], 
#                                time_hosp_pars, time_death_pars, time_disch_pars)







##' Build a set of sampled hospitalizations, deaths, and recoveries 
##'  from the incident infection data from the simulation model.
##' 
##' @param sims_data data.frame of simulation results
##' @param p_hosp probability of hospitalization, among infections
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' 
get_hospdeath_sims <- function(incid_data, p_hosp, p_death, 
                               time_hosp_pars, time_death_pars, time_disch_pars){
    
    # incid_data <- sims_data %>% filter(comp=="diffI")
    county_ <- unique(incid_data$county)
    nsims <- length(unique(incid_data$sim_num))
    
    rc <- list()
    
    for (s in 1:nsims){
        
        rc_sim <- list()
        
        for (c in 1:length(county_)){
            
            rc_sim[[c]] <- build_hospdeath(data = incid_data %>% filter(county==county_[c] & sim_num==s),
                                        p_hosp, p_death, time_hosp_pars, time_death_pars, time_disch_pars) %>% 
                mutate(county=county_[c])
        }
        
        rc[[s]] <- rbindlist(rc_sim) %>% mutate(sim_num=s)

    }
    
    rc <- rbindlist(rc)
    
    return(rc)
}






# # TEST
# source("R/DataUtils.R")
# sims_data <- load_scenario_sims(scenario_dir="SanFrancisco/low")
# sim_hospdeath_lowhigh <- get_hospdeath_sims(sims_data=sims_data, p_hosp=p_hosp[3], p_death=p_hosp[3])
#     





# # If we want to sample N iters for each simulation, can run this:
# c=1
# county_ <- unique(incidentI$county)
# res_data <- get_hospdeath_ests(iter=100, data=incidentI %>% filter(county==county_[c]) %>% mutate(incidI=N), 
#                                p_hosp=p_hosp[3], p_death=p_death[3],
#                                time_hosp_pars, time_death_pars, time_disch_pars)
# res_data










