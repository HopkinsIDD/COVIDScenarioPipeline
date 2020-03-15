

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
##' @param p_ICU probability of ICU among hospitalization
##' @param p_vent probability of ventilation among ICU
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_ICU_pars parameters for time from onset to hospitalization distribution
##' @param time_Vent_pars parameters for time from onset to hospitalization distribution
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' 
build_hospdeath_fullsim <- function(data, p_hosp, p_death, p_ICU, p_vent,
                            time_hosp_pars = c(1.23, 0.79),
                            time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                            time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
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
    # Add ICU
    ICU_ <- rbinom(H_, H_, rep(p_ICU, length(H_)))
    names(ICU_) <- uid
    # Add Ventilator 
    Vent_ <- rbinom(ICU_, ICU_, rep(p_vent, length(ICU_)))
    names(Vent_) <- uid
    # Add Death
    D_ <- rbinom(H_, H_, rep(p_death, length(H_)))
    # R_ <- H_ - D_  # hospitalized recoveries
    names(D_) <- uid
    
    
    # Time to hospitalization
    H_time_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_time_ <- rep(dates_,H_) + H_time_
    
    # Time from hospitalization to ICU
    ICU_time_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICU_pars[1], sdlog=time_ICU_pars[2]))
    ICU_time_ <- rep(dates_,ICU_) + ICU_time_    
    
    # Time from onset of symptoms to mechanical ventilation
    Vent_time_ <- floor(rlnorm(sum(Vent_), meanlog=time_vent_pars[1], sdlog=time_vent_pars[2]))
    Vent_time_ <- rep(dates_,Vent_) + Vent_time_
    
    # Time to death
    D_time_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_time_ <- rep(dates_,D_) + D_time_
    
    names(H_time_) <- rep(uid, H_)
    names(ICU_time_) <- rep(uid, ICU_)
    names(Vent_time_) <- rep(uid, Vent_)
    names(D_time_) <- rep(uid, D_)
    
    data_H <- data.frame(county_sim = names(H_time_), date=H_time_, incidH=1)
    data_ICU <- data.frame(county_sim = names(ICU_time_), date=ICU_time_, incidICU=1)
    data_Vent <- data.frame(county_sim = names(Vent_time_), date=Vent_time_, incidVent=1)
    data_D <- data.frame(county_sim = names(D_time_), date=D_time_, incidD=1)
    
    res <- full_join(data.frame(time=date_tmp), data_H, by=c("time"="date"))
    res <- full_join(res, data_ICU, by=c("time"="date", "county_sim"="county_sim"))
    res <- full_join(res, data_Vent, by=c("time"="date", "county_sim"="county_sim"))
    res <- full_join(res, data_D, by=c("time"="date", "county_sim"="county_sim"))
    res <- res %>% mutate(incidH = ifelse(is.na(incidH), 0, incidH),
                          incidICU = ifelse(is.na(incidICU), 0, incidICU),
                          incidVent = ifelse(is.na(incidVent), 0, incidVent),
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
##' @param p_ICU probability of ICU among hospitalization
##' @param p_vent probability of ventilation among ICU
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_ICU_pars parameters for time from onset to hospitalization distribution
##' @param time_Vent_pars parameters for time from onset to hospitalization distribution
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' @param length_geoid length of the geoid identifier to split
##' @param incl_county logical, whether to produce a table grouped by geoid in addition to metrop_labels + state
##' 



build_hospdeath_summary <- function(data, p_hosp, p_death, p_vent, p_ICU,
                                    time_hosp_pars = c(1.23, 0.79), 
                                    time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                    time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                    time_death_pars = c(log(11.25), log(1.15)), 
                                    time_disch_pars = c(log(11.5), log(1.22)),
                                    end_date = "2020-04-01",
                                    length_geoid = 5,
                                    incl.county=FALSE,
                                    run_parallel=FALSE, cores=1,
                                    durations=FALSE) {
    
    
    if (run_parallel){
        require(doParallel)
    }
    
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
    # Add ICU
    ICU_ <- rbinom(H_, H_, rep(p_ICU, length(H_)))
    names(ICU_) <- uid
    # Add Ventilator 
    Vent_ <- rbinom(ICU_, ICU_, rep(p_vent, length(ICU_)))
    names(Vent_) <- uid
    # Add Death
    D_ <- rbinom(H_, H_, rep(p_death, length(H_)))
    R_ <- H_ - D_  # hospitalized recoveries
    names(D_) <- uid
    names(R_) <- uid
    
    
    # Time to hospitalization
    H_delay_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_time_ <- rep(dates_,H_) + H_delay_
    names(H_time_) <- rep(uid, H_)
    data_H <- as.data.frame(table(H_time_, names(H_time_)), stringsAsFactors = FALSE)
    colnames(data_H) <- c("time","county_sim","incidH")
    
    # Add ICU
    ICU_ <- rbinom(data_H$incidH, data_H$incidH, rep(p_ICU, length(nrow(data_H))))
    names(ICU_) <- data_H$county_sim
    # Time from hospitalization to ICU
    ICU_delay_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICU_pars[1], sdlog=time_ICU_pars[2]))
    ICU_time_ <- rep(as.Date(data_H$time), ICU_) + ICU_delay_    
    names(ICU_time_) <- rep(data_H$county_sim, ICU_)
    data_ICU <- as.data.frame(table(ICU_time_, names(ICU_time_)), stringsAsFactors = FALSE)
    colnames(data_ICU) <- c("time","county_sim","incidICU")
    
    # Time from ICU admit to ICU discharge
    ICU_dur_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICUdur_pars[1], sdlog=time_ICUdur_pars[2]))
    ICU_end_ <- ICU_time_ + ICU_dur_   
    names(ICU_end_) <- rep(data_H$county_sim, ICU_)
    data_ICUend <- as.data.frame(table(ICU_end_, names(ICU_end_)), stringsAsFactors = FALSE)
    colnames(data_ICUend) <- c("time","county_sim","endICU")
    
    
    # Add Vent
    Vent_ <- rbinom(data_ICU$incidICU, data_ICU$incidICU, rep(p_vent, length(nrow(data_ICU))))
    names(Vent_) <- data_ICU$county_sim
    # Time from ICU to mechanical ventilation
    Vent_delay_ <- floor(rlnorm(sum(Vent_), meanlog=time_vent_pars[1], sdlog=time_vent_pars[2]))
    Vent_time_ <- rep(as.Date(data_ICU$time), Vent_) + Vent_delay_    
    
    
    
    # Add D
    D_ <- rbinom(data_H$incidH, data_H$incidH, rep(p_death, length(nrow(data_H))))
    names(D_) <- data_H$county_sim
    # Date of Death
    D_delay_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_time_ <- rep(as.Date(data_H$time), D_) + D_delay_  
    D_date_hosp <- rep(as.Date(data_H$time), D_)
    names(D_time_) <- rep(data_H$county_sim, D_)
    names(D_date_hosp) <- rep(data_H$county_sim, D_)
    data_D <- as.data.frame(table(D_time_, names(D_time_)), stringsAsFactors = FALSE)
    colnames(data_D) <- c("time","county_sim","incidD")
    
    # Add R
    # Add D
    R_ <- data_H$incidH - D_
    # Rate of Recovery
    R_delay_ <- floor(rlnorm(sum(R_), meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))
    R_time_ <- rep(as.Date(data_H$time), R_) + R_delay_  
    R_date_hosp <- rep(as.Date(data_H$time), R_)
    names(R_time_) <- rep(data_H$county_sim, R_)
    names(R_date_hosp) <- rep(data_H$county_sim, R_)
    data_R <- as.data.frame(table(R_time_, names(R_time_)), stringsAsFactors = FALSE)
    colnames(data_R) <- c("time","county_sim","incidR")
    
    
    
    # Get durations ------------------------------
    
    cl <- makeCluster(cores)
    
    # Get current hospitalization days and accumulate them -- Recoveries
    clusterExport(cl=cl, varlist=c('R_', 'R_date_hosp', 'R_time_'), envir=environment())
    curr_hosp_date <- rev(as.Date(unlist(
        parSapply(cl, 1:sum(R_), function(x) seq(as.Date(R_date_hosp[x]), as.Date(R_time_[x]), "days"))),
        origin = "1970-01-01"))
    names(curr_hosp_date) <- rep(names(R_time_), (R_delay_+1)) # add country_sim
    
    
    # Get current hospitalization days and accumulate them -- Deaths
    clusterExport(cl=cl, varlist=c('D_', 'D_date_hosp', 'D_time_'), envir=environment())
    curr_hospD_date <- rev(as.Date(unlist(
        parSapply(cl, 1:sum(D_), function(x) seq(as.Date(D_date_hosp[x]), as.Date(D_time_[x]), "days"))),
        origin = "1970-01-01"))
    names(curr_hospD_date) <- rep(names(D_time_), (D_delay_+1))
    
    
    # Get current ICU days and accumulate them -- ALL (add ICU eventually)
    clusterExport(cl=cl, varlist=c('ICU_', 'ICU_time_', 'ICU_end_'), envir=environment())
    curr_icu_date <- rev(as.Date(unlist(
        parSapply(cl, 1:sum(ICU_), function(x) seq(as.Date(ICU_time_[x]), as.Date(ICU_end_[x]), "days"))),
        origin = "1970-01-01"))
    names(curr_icu_date) <- rep(names(ICU_time_), (ICU_dur_+1))
    
    
    stopCluster(cl)
    
    # ----------------------------------------------
    
    
    # combine them
    curr_hosp_date <- c(curr_hosp_date, curr_hospD_date)
    data_currhosp <- as.data.frame(table(curr_hosp_date, names(curr_hosp_date)))
    colnames(data_currhosp) <- c("time","county_sim","hosp_curr")
    
    data_curricu <- as.data.frame(table(curr_icu_date, names(curr_icu_date)))
    colnames(data_curricu) <- c("time","county_sim","icu_curr")
    
    
    # Merge them all
    res <- full_join(data.frame(time=as.character(date_tmp)), data_H, by=c("time"))
    res <- full_join(res, data_ICU, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_Vent, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_D, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_currhosp, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_curricu, by=c("time", "county_sim"="county_sim"))
    
    
    res <- res %>% 
        replace_na(
            list(incidH = 0,
                 incidICU = 0,
                 incidVent = 0,
                 incidD = 0)
        ) %>%
        mutate(geoid = substr(county_sim,1,length_geoid),
               sim_num= substr(county_sim,length_geoid+1,length_geoid+4)) %>%
        left_join(data %>% select(geoid, metrop_labels) %>% distinct(), by='geoid')
    
    
    # Summarization starts here
    
    res_metro <- res %>%
        filter(!is.na(county_sim) & !is.na(metrop_labels)) %>% 
        select(-county_sim) %>%
        filter(time <= as.Date(end_date)) %>%
        group_by(metrop_labels, sim_num) %>% 
        summarize(nhosp = sum(incidH), nICU = sum(incidICU), nVent = sum(incidVent), ndeath = sum(incidD)) %>%
        ungroup() %>% 
        group_by(metrop_labels) %>% 
        summarize(nhosp_final = mean(nhosp),
                  nhosp_lo = quantile(nhosp, 0.2),
                  nhosp_hi = quantile(nhosp, 0.8),
                  nICU_final = mean(nICU),
                  nICU_lo = quantile(nICU, 0.2),
                  nICU_hi = quantile(nICU, 0.8),
                  nVent_final = mean(nVent),
                  nVent_lo = quantile(nVent, 0.2),
                  nVent_hi = quantile(nVent, 0.8),
                  ndeath_final = mean(ndeath),
                  ndeath_lo = quantile(ndeath, 0.2),
                  ndeath_hi = quantile(ndeath, 0.8))
    
    res_total <- res %>% 
        filter(!is.na(county_sim)) %>% 
        select(-county_sim) %>%
        filter(time <= as.Date(end_date)) %>%
        group_by(sim_num) %>% 
        summarize(nhosp = sum(incidH),nICU = sum(incidICU),nVent = sum(incidVent), ndeath = sum(incidD)) %>%
        ungroup() %>% 
        summarize(nhosp_final = mean(nhosp),
                  nhosp_lo = quantile(nhosp, 0.2),
                  nhosp_hi = quantile(nhosp, 0.8),
                  ndeath_ICU = mean(nICU),
                  nICU_lo = quantile(nICU, 0.2),
                  nICU_hi = quantile(nICU, 0.8),
                  nVent_final = mean(nVent),
                  nVent_lo = quantile(nVent, 0.2),
                  nVent_hi = quantile(nVent, 0.8),
                  ndeath_final = mean(ndeath),
                  ndeath_lo = quantile(ndeath, 0.2),
                  ndeath_hi = quantile(ndeath, 0.8))
    
    out <- list(res_total = res_total, res_metro = res_metro)
    
    if(incl.county){
        res_geoid <- res %>% 
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
                      nICU_final = mean(nICU),
                      nICU_lo = quantile(nICU, 0.2),
                      nICU_hi = quantile(nICU, 0.8),
                      nVent_final = mean(nVent),
                      nVent_lo = quantile(nVent, 0.2),
                      nVent_hi = quantile(nVent, 0.8),
                      ndeath_final = mean(ndeath),
                      ndeath_lo = quantile(ndeath, 0.2),
                      ndeath_hi = quantile(ndeath, 0.8))
        
        out <- list(res_total = res_total, res_metro = res_metro, res_geoid = res_geoid)
    }
    
    return(out)
}



build_hospdeath_summaryOLD <- function(data, p_hosp, p_death, p_vent, p_ICU,
                                    time_hosp_pars = c(1.23, 0.79), 
                                    time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                    time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                    time_death_pars = c(log(11.25), log(1.15)), 
                                    time_disch_pars = c(log(11.5), log(1.22)),
                                    end_date = "2020-04-01",
                                    length_geoid = 5,
                                    incl.county=FALSE) {
    
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
    # Add ICU
    ICU_ <- rbinom(H_, H_, rep(p_ICU, length(H_)))
    names(ICU_) <- uid
    # Add Ventilator 
    Vent_ <- rbinom(ICU_, ICU_, rep(p_vent, length(ICU_)))
    names(Vent_) <- uid
    # Add Death
    D_ <- rbinom(H_, H_, rep(p_death, length(H_)))
    # R_ <- H_ - D_  # hospitalized recoveries
    names(D_) <- uid
    
    
    # Time to hospitalization
    H_time_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_time_ <- rep(dates_,H_) + H_time_
    
    # Time from hospitalization to ICU
    ICU_time_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICU_pars[1], sdlog=time_ICU_pars[2]))
    ICU_time_ <- rep(dates_,ICU_) + ICU_time_    
    
    # Time from onset of symptoms to mechanical ventilation
    Vent_time_ <- floor(rlnorm(sum(Vent_), meanlog=time_vent_pars[1], sdlog=time_vent_pars[2]))
    Vent_time_ <- rep(dates_,Vent_) + Vent_time_
    
    # Time to death
    D_time_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_time_ <- rep(dates_,D_) + D_time_
    
    names(H_time_) <- rep(uid, H_)
    names(ICU_time_) <- rep(uid, ICU_)
    names(Vent_time_) <- rep(uid, Vent_)
    names(D_time_) <- rep(uid, D_)
    
    
    data_H <- data.frame(county_sim = names(H_time_), date=H_time_, incidH=1)
    data_ICU <- data.frame(county_sim = names(ICU_time_), date=ICU_time_, incidICU=1)
    data_Vent <- data.frame(county_sim = names(Vent_time_), date=Vent_time_, incidVent=1)
    data_D <- data.frame(county_sim = names(D_time_), date=D_time_, incidD=1)
    
    res <- full_join(data.frame(time=date_tmp), data_H, by=c("time"="date"))
    res <- full_join(res, data_ICU, by=c("time"="date", "county_sim"="county_sim"))
    res <- full_join(res, data_Vent, by=c("time"="date", "county_sim"="county_sim"))
    res <- full_join(res, data_D, by=c("time"="date", "county_sim"="county_sim"))

    res <- res %>% mutate(incidH = ifelse(is.na(incidH), 0, incidH),
                          incidICU = ifelse(is.na(incidICU), 0, incidICU),
                          incidVent = ifelse(is.na(incidVent), 0, incidVent),
                          incidD = ifelse(is.na(incidD), 0, incidD),
                          geoid = substr(county_sim,1,length_geoid),
                          sim_num= substr(county_sim,length_geoid+1,length_geoid+4)) %>%
                    left_join(data %>% select(geoid, metrop_labels) %>% distinct(), by='geoid')
    
    res_metro <- res %>%
                filter(!is.na(county_sim) & !is.na(metrop_labels)) %>% 
                select(-county_sim) %>%
                filter(time <= as.Date(end_date)) %>%
                group_by(metrop_labels, sim_num) %>% 
                summarize(nhosp = sum(incidH), nICU = sum(incidICU), nVent = sum(incidVent), ndeath = sum(incidD)) %>%
                ungroup() %>% 
                group_by(metrop_labels) %>% 
                summarize(nhosp_final = mean(nhosp),
                          nhosp_lo = quantile(nhosp, 0.2),
                          nhosp_hi = quantile(nhosp, 0.8),
                          nICU_final = mean(nICU),
                          nICU_lo = quantile(nICU, 0.2),
                          nICU_hi = quantile(nICU, 0.8),
                          nVent_final = mean(nVent),
                          nVent_lo = quantile(nVent, 0.2),
                          nVent_hi = quantile(nVent, 0.8),
                          ndeath_final = mean(ndeath),
                          ndeath_lo = quantile(ndeath, 0.2),
                          ndeath_hi = quantile(ndeath, 0.8))
    
    res_total <- res %>% 
                 filter(!is.na(county_sim)) %>% 
                 select(-county_sim) %>%
                 filter(time <= as.Date(end_date)) %>%
                 group_by(sim_num) %>% 
                 summarize(nhosp = sum(incidH),nICU = sum(incidICU),nVent = sum(incidVent), ndeath = sum(incidD)) %>%
                 ungroup() %>% 
                 summarize(nhosp_final = mean(nhosp),
                          nhosp_lo = quantile(nhosp, 0.2),
                          nhosp_hi = quantile(nhosp, 0.8),
                          ndeath_ICU = mean(nICU),
                          nICU_lo = quantile(nICU, 0.2),
                          nICU_hi = quantile(nICU, 0.8),
                          nVent_final = mean(nVent),
                          nVent_lo = quantile(nVent, 0.2),
                          nVent_hi = quantile(nVent, 0.8),
                          ndeath_final = mean(ndeath),
                          ndeath_lo = quantile(ndeath, 0.2),
                          ndeath_hi = quantile(ndeath, 0.8))
    
    out <- list(res_total = res_total, res_metro = res_metro)
                
    if(incl.county){
        res_geoid <- res %>% 
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
                      nICU_final = mean(nICU),
                      nICU_lo = quantile(nICU, 0.2),
                      nICU_hi = quantile(nICU, 0.8),
                      nVent_final = mean(nVent),
                      nVent_lo = quantile(nVent, 0.2),
                      nVent_hi = quantile(nVent, 0.8),
                      ndeath_final = mean(ndeath),
                      ndeath_lo = quantile(ndeath, 0.2),
                      ndeath_hi = quantile(ndeath, 0.8))
        
        out <- list(res_total = res_total, res_metro = res_metro, res_geoid = res_geoid)
    }
    
    return(out)
}


##' Function 
##' Build a set of sampled hospitalizations, deaths, and recoveries 
##' from the incident infection data from the simulation model.
##' 
##' Instantly summarize to save memory + run for multiple p_death
##'  
##' @param data data.frame of t (time in days) and incidI (incident infections)
##' @param p_hosp_vec vector of probability of hospitalization, among infections
##' @param p_death_vec vector probability of death, among infections (hospitalization is required for death)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' @param length_geoid length of the geoid identifier to split
##' @param incl_county logical, whether to produce a table grouped by geoid in addition to state + metrop
##' 

build_hospdeath_summary_multiplePDeath <- function(data, 
                                                   p_hosp_vec, 
                                                   p_death_vec,
                                                   p_ICU,
                                                   p_vent,
                                                   time_hosp_pars = c(1.23, 0.79), 
                                                   time_death_pars = c(log(11.25), log(1.15)), 
                                                   time_disch_pars = c(log(11.5), log(1.22)),
                                                   time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                                   time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                                   end_date = "2020-04-01",
                                                   length_geoid = 5,
                                                   incl.county=FALSE){
    
    tmp_out <- build_hospdeath_summary(data, 
                                       p_hosp=p_hosp_vec[1], 
                                       p_death=p_death_vec[1],
                                       p_ICU = p_ICU,
                                       p_vent = p_vent,
                                       time_hosp_pars = time_hosp_pars, 
                                       time_death_pars = time_death_pars, 
                                       time_disch_pars = time_disch_pars,
                                       time_vent_pars = time_vent_pars,
                                       time_ICU_pars = time_ICU_pars,
                                       end_date = end_date,
                                       length_geoid = length_geoid,
                                       incl.county = incl.county) 
    
    tmp_metro <- tmp_out[['res_metro']] %>% mutate(p_death = p_death[1])
    tmp_total <- tmp_out[['res_total']] %>% mutate(p_death = p_death[1])
    if(incl.county){ tmp_geoid <- tmp_out[['res_geoid']] %>% mutate(p_death = p_death[1]) }
    
    for(i in 2:length(p_death)){
        tmp_out <- build_hospdeath_summary(data, 
                                           p_hosp=p_hosp_vec[1], 
                                           p_death=p_death_vec[1],
                                           p_ICU = p_ICU,
                                           p_vent = p_vent,
                                           time_hosp_pars = time_hosp_pars, 
                                           time_death_pars = time_death_pars, 
                                           time_disch_pars = time_disch_pars,
                                           time_vent_pars = time_vent_pars,
                                           time_ICU_pars = time_ICU_pars,
                                           end_date = end_date,
                                           length_geoid = length_geoid,
                                           incl.county = incl.county) 
        tmp_metro <- bind_rows(tmp_metro, tmp_out[['res_metro']] %>% mutate(p_death = p_death[i]))
        tmp_total <- bind_rows(tmp_total, tmp_out[['res_total']] %>% mutate(p_death = p_death[i]))
        if(incl.county){tmp_geoid <- bind_rows(tmp_geoid, tmp_out[['res_geoid']] %>% mutate(p_death = p_death[i]))}
    }
    
    out <- list(res_total = tmp_total, res_metro = tmp_metro)
    
    if(incl.county){
    out <- list(res_total = tmp_total, res_metro = tmp_metro, res_geoid = tmp_geoid)
    }
    
   return(out)
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










