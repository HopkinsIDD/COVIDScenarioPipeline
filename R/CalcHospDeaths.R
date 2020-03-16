


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

## NOTE FOR FUTURE IMPROVEMENT: This function could be made faster by indexing off a start date and just 
##  using integer days for everything, then applying dates at the end.

build_hospdeath_summary <- function(data, p_hosp, p_death, p_vent, p_ICU,
                                    time_hosp_pars = c(1.23, 0.79), 
                                    time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                    time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                    time_death_pars = c(log(11.25), log(1.15)), 
                                    time_disch_pars = c(log(11.5), log(1.22)),
                                    time_ICUdur_pars = c(log(17.46), log(4.044)),
                                    end_date = "2020-04-01",
                                    length_geoid = 5,
                                    incl.county=FALSE,
                                    cores=1, 
                                    run_parallel=FALSE,
                                    get_curr_hosp=FALSE) {
    
    require(doParallel)
    require(data.table)
    
    # filter to earlier than the end_date
    data <- data %>% filter(time<=end_date)
    
    # Set up results data

    # t_ <- 1:nrow(data)
    dates_ <- as.Date(data$time)
    sim_num <- data$sim_num
    geoid <- data$geoid
    uid <- paste0(geoid, "-",sim_num)
    data$county_sim <- uid
    # date_tmp <- seq(min(dates_), (max(dates_)+125), by="days")
    
    # Add hosp    
    I_ <- data$incidI
    H_ <- rbinom(I_, I_, rep(p_hosp, length(I_)))
    names(H_) <- uid

    # Time to hospitalization
    #H_delay_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_delay_ <- round(exp(time_hosp_pars[1]))
    H_time_ <- rep(dates_,H_) + H_delay_
    names(H_time_) <- rep(uid, H_)
    data_H <- data.frame(time=H_time_, county_sim=names(H_time_))
    data_H <- setDT(data_H)[, .N, by = .(time, county_sim)]
    colnames(data_H) <- c("time","county_sim","incidH")
    
    
    # Add ICU
    ICU_ <- rbinom(data_H$incidH, data_H$incidH, rep(p_ICU, length(nrow(data_H))))
    names(ICU_) <- data_H$county_sim
    # Time from hospitalization to ICU
    #ICU_delay_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICU_pars[1], sdlog=time_ICU_pars[2]))
    ICU_delay_ <- round(exp(time_ICU_pars[2]))
    ICU_time_ <- rep(as.Date(data_H$time), ICU_) + ICU_delay_    
    names(ICU_time_) <- rep(data_H$county_sim, ICU_)
    data_ICU <- data.frame(time=ICU_time_, county_sim=names(ICU_time_))
    data_ICU <- setDT(data_ICU)[, .N, by = .(time, county_sim)]
    colnames(data_ICU) <- c("time","county_sim","incidICU")
    
    
    # Time from ICU admit to ICU discharge
    #ICU_dur_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICUdur_pars[1], sdlog=time_ICUdur_pars[2]))
    ICU_dur_ <- round(exp(time_ICUdur_pars[1]))
    ICU_end_ <- ICU_time_ + ICU_dur_   
    names(ICU_end_) <- rep(data_H$county_sim, ICU_)
    data_ICUend <- data.frame(time=ICU_end_, county_sim=names(ICU_end_))
    data_ICUend <- setDT(data_ICUend)[, .N, by = .(time, county_sim)]
    colnames(data_ICUend) <- c("time","county_sim","endICU")
    
    
    # Add Vent
    Vent_ <- rbinom(data_ICU$incidICU, data_ICU$incidICU, rep(p_vent, length(nrow(data_ICU))))
    names(Vent_) <- data_ICU$county_sim
    # Time from ICU to mechanical ventilation
    #Vent_delay_ <- floor(rlnorm(sum(Vent_), meanlog=time_vent_pars[1], sdlog=time_vent_pars[2]))
    Vent_delay_ <- exp(time_vent_pars[1])
    Vent_time_ <- rep(as.Date(data_ICU$time), Vent_) + Vent_delay_    
    names(Vent_time_) <- rep(data_ICU$county_sim, Vent_)
    data_Vent <- data.frame(time=Vent_time_, county_sim=names(Vent_time_))
    data_Vent <- setDT(data_Vent)[, .N, by = .(time, county_sim)]
    colnames(data_Vent) <- c("time","county_sim","incidVent")
    
    
    # Add D
    D_ <- rbinom(data_H$incidH, data_H$incidH, rep(p_death, length(nrow(data_H))))
    names(D_) <- data_H$county_sim
    # Date of Death
    #D_delay_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_delay_ <- exp(time_death_pars[1])
    D_time_ <- rep(as.Date(data_H$time), D_) + D_delay_  
    D_date_hosp <- rep(as.Date(data_H$time), D_)
    names(D_time_) <- rep(data_H$county_sim, D_)
    names(D_date_hosp) <- rep(data_H$county_sim, D_)
    data_D <- data.frame(time=D_time_, county_sim=names(D_time_))
    data_D <- setDT(data_D)[, .N, by = .(time, county_sim)]
    colnames(data_D) <- c("time","county_sim","incidD")
    
    
    # Add R
    R_ <- data_H$incidH - D_
    # Rate of Recovery
    #R_delay_ <- floor(rlnorm(sum(R_), meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))
    R_delay_ <- round(exp(time_disch_pars[1]))
    R_time_ <- rep(as.Date(data_H$time), R_) + R_delay_  
    R_date_hosp <- rep(as.Date(data_H$time), R_)
    names(R_time_) <- rep(data_H$county_sim, R_)
    names(R_date_hosp) <- rep(data_H$county_sim, R_)
    data_R <- data.frame(time=R_time_, county_sim=names(R_time_))
    data_R <- setDT(data_R)[, .N, by = .(time, county_sim)]
    colnames(data_R) <- c("time","county_sim","incidR")

    
    # Merge them 
    res <- full_join(data_H, data_ICU, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_Vent, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_D, by=c("time", "county_sim"="county_sim"))
    
    
    
    
    if (get_curr_hosp){

        # Get durations ....................
        
        curr_hosp_date <- NULL
        curr_hospD_date <- NULL
        curr_icu_date <- NULL
        
        if (run_parallel){
            
            cl <- makeCluster(cores)
            
            # Get current hospitalization days and accumulate them -- Recoveries
            if (sum(R_)>0){
                curr_hosp_date <- rev(foreach(n=1:sum(R_), .combine = c) %dopar% {
                        seq(R_date_hosp[n], R_time_[n], "days") })
                names(curr_hosp_date) <- rep(names(R_time_), (R_delay_+1)) # add country_sim
            }
            
            # Get current hospitalization days and accumulate them -- Deaths
            if (sum(D_)>0){
                curr_hospD_date <- rev(foreach(n=1:sum(D_), .combine = c) %dopar% {
                    seq(D_date_hosp[n], D_time_[n], "days") })
                names(curr_hospD_date) <- rep(names(D_time_), (D_delay_+1))
            }
            
            # Get current ICU days and accumulate them -- ALL (add ICU eventually)
            if (sum(ICU_)>0){
                curr_icu_date <- rev(foreach(n=1:sum(ICU_), .combine = c) %dopar% {
                    seq(ICU_time_[n], ICU_end_[n], "days") })
                names(curr_icu_date) <- rep(names(ICU_time_), (ICU_dur_+1))
            }
            
            stopCluster(cl)
            
        } else {
            
            # Get current hospitalization days and accumulate them -- Recoveries
            if (sum(R_)>0){
                curr_hosp_date <- rev(unlist(
                    sapply(1:sum(R_), function(x) seq(R_date_hosp[x], R_time_[x], "days"))))
                names(curr_hosp_date) <- rep(names(R_time_), (R_delay_+1)) # add country_sim
            }
            
            if (sum(D_)>0){
                # Get current hospitalization days and accumulate them -- Deaths
                curr_hospD_date <- rev(unlist(
                    sapply(1:sum(D_), function(x) seq(D_date_hosp[x], D_time_[x], "days"))))
                names(curr_hospD_date) <- rep(names(D_time_), (D_delay_+1))
            }
            
            if (sum(ICU_)>0){
                # Get current ICU days and accumulate them -- ALL (add ICU eventually)
                curr_icu_date <- rev(unlist(
                    sapply(1:sum(ICU_), function(x) seq(ICU_time_[x], ICU_end_[x], "days"))))
                names(curr_icu_date) <- rep(names(ICU_time_), (ICU_dur_+1))
            }
        }
        
        
        # ----------------------------------------------
        
        data_currhosp <- data_curricu <- NULL
        if (sum(H_)>0){
            # combine them & tabulate
            curr_hosp_date <- c(curr_hosp_date, curr_hospD_date)
            curr_hosp_date <- as.Date(curr_hosp_date, origin = "1970-01-01")
            data_currhosp <- data.frame(time=curr_hosp_date, county_sim=names(curr_hosp_date))
            data_currhosp <- setDT(data_currhosp)[, .N, by = .(time, county_sim)]
            colnames(data_currhosp) <- c("time","county_sim","hosp_curr")
            
            curr_icu_date <- as.Date(curr_icu_date, origin = "1970-01-01")
            data_curricu <- data.frame(time=curr_icu_date, county_sim=names(curr_icu_date))
            data_curricu <- setDT(data_curricu)[, .N, by = .(time, county_sim)]
            colnames(data_curricu) <- c("time","county_sim","icu_curr")
        
            curr_hosp_date <- curr_icu_date <- curr_hospD_date <- NULL
        }

        if (sum(H_)>0){
            res <- full_join(res, data_currhosp, by=c("time", "county_sim"="county_sim"))
            res <- full_join(res, data_curricu, by=c("time", "county_sim"="county_sim"))
        } else {
            res$hosp_curr <- 0
            res$icu_curr <- 0
        }
        
        data_currhosp <- data_curricu <- NULL
        
    }
   
    # # Add incidence for checking -- leaving out to save memory
    # res <- full_join(res, 
    #                  data %>% select(time, county_sim, incidI), 
    #                  by=c("time", "county_sim"="county_sim"))
    data_ICU <- data_ICUend <- data_Vent <- data_D <- date_tmp <- data_H <- NULL
    
    
    res <- res %>% 
        replace_na(
            list(incidI = 0,
                 incidH = 0,
                 incidICU = 0,
                 incidVent = 0,
                 incidD = 0)
                 # hosp_curr = 0,
                 # icu_curr = 0)
        ) %>%
        mutate(geoid = substr(county_sim,1,length_geoid),
               sim_num= substr(county_sim,length_geoid+2,length_geoid+7)) %>%
        left_join(data %>% select(geoid, metrop_labels) %>% distinct(), by='geoid')
    
    
    # Summarization starts here
    
    res_metro <- res %>%
        filter(!is.na(county_sim) & !is.na(metrop_labels)) %>% 
        select(-county_sim) %>%
        mutate(time = as.Date(time)) %>%
        #filter(time <= as.Date(end_date)) %>%
        group_by(metrop_labels, sim_num) %>% 
        summarize(
                  # nInf = sum(incidI, na.rm = TRUE), 
                  nhosp = sum(incidH, na.rm = TRUE), 
                  nICU = sum(incidICU, na.rm = TRUE), 
                  nVent = sum(incidVent, na.rm = TRUE), 
                  ndeath = sum(incidD, na.rm = TRUE),
                  maxHospAdm = max(incidH, na.rm=TRUE),
                  maxICUAdm = max(incidICU, na.rm=TRUE)#,
                  # maxHospCap = max(hosp_curr, na.rm = TRUE),
                  # maxICUCap = max(icu_curr, na.rm=TRUE)
                  ) %>%
        ungroup() %>% 
        group_by(metrop_labels) %>% 
        summarize(#nInf_final = mean(nInf),
                  #nInf_lo = quantile(nInf, 0.25),
                  #nInf_hi = quantile(nInf, 0.75),
                  nhosp_final = mean(nhosp),
                  nhosp_lo = quantile(nhosp, 0.25),
                  nhosp_hi = quantile(nhosp, 0.75),
                  phosp_final = mean(maxHospAdm),
                  phosp_lo = quantile(maxHospAdm, 0.25),
                  phosp_hi = quantile(maxHospAdm, 0.75),
                  nICU_final = mean(nICU),
                  nICU_lo = quantile(nICU, 0.25),
                  nICU_hi = quantile(nICU, 0.75),
                  pICU_final = mean(maxICUAdm),
                  pICU_lo = quantile(maxICUAdm, 0.25),
                  pICU_hi = quantile(maxICUAdm, 0.75),
                  nVent_final = mean(nVent),
                  nVent_lo = quantile(nVent, 0.25),
                  nVent_hi = quantile(nVent, 0.75),
                  ndeath_final = mean(ndeath),
                  ndeath_lo = quantile(ndeath, 0.25),
                  ndeath_hi = quantile(ndeath, 0.75)#,
                  # nhosp_curr_final = mean(maxHospCap),
                  # nhosp_curr_lo = quantile(maxHospCap, 0.25),
                  # nhosp_curr_hi = quantile(maxHospCap, 0.75),
                  # nicu_curr_final = mean(maxICUCap),
                  # nicu_curr_lo = quantile(maxICUCap, 0.25),
                  # nicu_curr_hi = quantile(maxICUCap, 0.75)
                  )
    
    res_total <- res %>% 
        filter(!is.na(county_sim)) %>% 
        select(-county_sim) %>%
        #filter(time <= as.Date(end_date)) %>%
        group_by(sim_num) %>% 
        summarize(#nInf = sum(incidI, na.rm = TRUE), 
                  nhosp = sum(incidH, na.rm = TRUE), 
                  nICU = sum(incidICU, na.rm = TRUE), 
                  nVent = sum(incidVent, na.rm = TRUE), 
                  ndeath = sum(incidD, na.rm = TRUE),
                  maxHospAdm = max(incidH, na.rm=TRUE),
                  maxICUAdm = max(incidICU, na.rm=TRUE)#,
                  # maxHospCap = max(hosp_curr, na.rm = TRUE),
                  # maxICUCap = max(icu_curr, na.rm=TRUE)
                  ) %>%
        ungroup() %>% 
        summarize(#nInf_final = mean(nInf),
                  #nInf_lo = quantile(nInf, 0.25),
                  #nInf_hi = quantile(nInf, 0.75),
                  nhosp_final = mean(nhosp),
                  nhosp_lo = quantile(nhosp, 0.25),
                  nhosp_hi = quantile(nhosp, 0.75),
                  phosp_final = mean(maxHospAdm),
                  phosp_lo = quantile(maxHospAdm, 0.25),
                  phosp_hi = quantile(maxHospAdm, 0.75),
                  nICU_final = mean(nICU),
                  nICU_lo = quantile(nICU, 0.25),
                  nICU_hi = quantile(nICU, 0.75),
                  pICU_final = mean(maxICUAdm),
                  pICU_lo = quantile(maxICUAdm, 0.25),
                  pICU_hi = quantile(maxICUAdm, 0.75),
                  nVent_final = mean(nVent),
                  nVent_lo = quantile(nVent, 0.25),
                  nVent_hi = quantile(nVent, 0.75),
                  ndeath_final = mean(ndeath),
                  ndeath_lo = quantile(ndeath, 0.25),
                  ndeath_hi = quantile(ndeath, 0.75))#,
                  # nhosp_curr_final = mean(maxHospCap),
                  # nhosp_curr_lo = quantile(maxHospCap, 0.25),
                  # nhosp_curr_hi = quantile(maxHospCap, 0.75),
                  # nicu_curr_final = mean(maxICUCap),
                  # nicu_curr_lo = quantile(maxICUCap, 0.25),
                  # nicu_curr_hi = quantile(maxICUCap, 0.75))
    
    out <- list(res_total = as.data.frame(res_total), res_metro = as.data.frame(res_metro))
    
    if(incl.county){
        res_geoid <- res %>% 
            filter(!is.na(county_sim)) %>% 
            select(-county_sim) %>%
            #filter(time <= as.Date(end_date)) %>%
            group_by(geoid, sim_num) %>% 
            summarize(#nInf = sum(incidI, na.rm = TRUE), 
                      nhosp = sum(incidH, na.rm = TRUE), 
                      nICU = sum(incidICU, na.rm = TRUE), 
                      nVent = sum(incidVent, na.rm = TRUE), 
                      ndeath = sum(incidD, na.rm = TRUE),
                      maxHospAdm = max(incidH, na.rm=TRUE),
                      maxICUAdm = max(incidICU, na.rm=TRUE)#,
                      # maxHospCap = max(hosp_curr, na.rm = TRUE),
                      # maxICUCap = max(icu_curr, na.rm=TRUE)
                      ) %>%
            ungroup() %>% 
            group_by(geoid) %>% 
            summarize(
                #nInf_final = mean(nInf),
                 #     nInf_lo = quantile(nInf, 0.25),
                  #    nInf_hi = quantile(nInf, 0.75),
                      nhosp_final = mean(nhosp),
                      nhosp_lo = quantile(nhosp, 0.25),
                      nhosp_hi = quantile(nhosp, 0.75),
                      phosp_final = mean(maxHospAdm),
                      phosp_lo = quantile(maxHospAdm, 0.25),
                      phosp_hi = quantile(maxHospAdm, 0.75),
                      nICU_final = mean(nICU),
                      nICU_lo = quantile(nICU, 0.25),
                      nICU_hi = quantile(nICU, 0.75),
                      pICU_final = mean(maxICUAdm),
                      pICU_lo = quantile(maxICUAdm, 0.25),
                      pICU_hi = quantile(maxICUAdm, 0.75),
                      nVent_final = mean(nVent),
                      nVent_lo = quantile(nVent, 0.25),
                      nVent_hi = quantile(nVent, 0.75),
                      ndeath_final = mean(ndeath),
                      ndeath_lo = quantile(ndeath, 0.25),
                      ndeath_hi = quantile(ndeath, 0.75)#,
                      # nhosp_curr_final = mean(maxHospCap),
                      # nhosp_curr_lo = quantile(maxHospCap, 0.25),
                      # nhosp_curr_hi = quantile(maxHospCap, 0.75),
                      # nicu_curr_final = mean(maxICUCap),
                      # nicu_curr_lo = quantile(maxICUCap, 0.25),
                      # nicu_curr_hi = quantile(maxICUCap, 0.75)
                      )
        
        out <- list(res_total = as.data.frame(res_total), res_metro = as.data.frame(res_metro), res_geoid = as.data.frame(res_geoid))
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
                                                   time_ICUdur_pars = c(log(17.46), log(4.044)),
                                                   end_date = "2020-04-01",
                                                   length_geoid = 5,
                                                   incl.county=FALSE,
                                                   cores=1,
                                                   run_parallel=FALSE){
    
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
                                       time_ICUdur_pars = time_ICUdur_pars,
                                       end_date = end_date,
                                       length_geoid = length_geoid,
                                       incl.county = incl.county,
                                       cores=cores,
                                       run_parallel=run_parallel,
                                       get_curr_hosp = FALSE) 
    
    tmp_metro <- tmp_out[['res_metro']] %>% mutate(p_death = p_death[1])
    tmp_total <- tmp_out[['res_total']] %>% mutate(p_death = p_death[1])
    if(incl.county){ tmp_geoid <- tmp_out[['res_geoid']] %>% mutate(p_death = p_death[1]) }
    
    for(i in 2:length(p_death)){
        tmp_out <- build_hospdeath_summary(data, 
                                           p_hosp=p_hosp_vec[i], 
                                           p_death=p_death_vec[i],
                                           p_ICU = p_ICU,
                                           p_vent = p_vent,
                                           time_hosp_pars = time_hosp_pars, 
                                           time_death_pars = time_death_pars, 
                                           time_disch_pars = time_disch_pars,
                                           time_vent_pars = time_vent_pars,
                                           time_ICU_pars = time_ICU_pars,
                                           time_ICUdur_pars = time_ICUdur_pars,
                                           end_date = end_date,
                                           length_geoid = length_geoid,
                                           incl.county = incl.county,
                                           cores=cores,
                                           run_parallel=run_parallel,
                                           get_curr_hosp = FALSE) 
        
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













build_hospdeath_summary_faster <- function(data, p_hosp, p_death, p_vent, p_ICU,
                                    time_hosp_pars = c(1.23, 0.79), 
                                    time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                    time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                    time_death_pars = c(log(11.25), log(1.15)), 
                                    time_disch_pars = c(log(11.5), log(1.22)),
                                    time_ICUdur_pars = c(log(17.46), log(4.044)),
                                    end_date = "2020-04-01",
                                    length_geoid = 5,
                                    incl.county=FALSE,
                                    cores=1, 
                                    run_parallel=FALSE) {
    
    require(doParallel)
    require(data.table)
    
    # filter to earlier than the end_date
    #data <- data %>% filter(time<=end_date)
    
    # Set up results data
    t_ <- 1:nrow(data)
    dates_ <- as.Date(data$time)
    sim_num <- data$sim_num
    geoid <- data$geoid
    uid <- paste0(geoid, "-",sim_num)
    data$county_sim <- uid
    date_tmp <- seq(min(dates_), (max(dates_)+125), by="days")
    
    # Add hosp    
    I_ <- data$incidI
    H_ <- rbinom(I_, I_, rep(p_hosp, length(I_)))
    names(H_) <- uid
    
    # Time to hospitalization
    #H_delay_ <- floor(rlnorm(sum(H_), meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))
    H_delay_ <- round(exp(time_hosp_pars[1]))
    H_time_ <- rep(dates_,H_) + H_delay_
    names(H_time_) <- rep(uid, H_)
    data_H <- data.frame(time=H_time_, county_sim=names(H_time_))
    data_H <- setDT(data_H)[, .N, by = .(time, county_sim)]
    colnames(data_H) <- c("time","county_sim","incidH")
    
    
    # Add ICU
    ICU_ <- rbinom(data_H$incidH, data_H$incidH, rep(p_ICU, length(nrow(data_H))))
    names(ICU_) <- data_H$county_sim
    # Time from hospitalization to ICU
    #ICU_delay_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICU_pars[1], sdlog=time_ICU_pars[2]))
    ICU_delay_ <- round(exp(time_ICU_pars[2]))
    ICU_time_ <- rep(as.Date(data_H$time), ICU_) + ICU_delay_    
    names(ICU_time_) <- rep(data_H$county_sim, ICU_)
    data_ICU <- data.frame(time=ICU_time_, county_sim=names(ICU_time_))
    data_ICU <- setDT(data_ICU)[, .N, by = .(time, county_sim)]
    colnames(data_ICU) <- c("time","county_sim","incidICU")
    
    
    # Time from ICU admit to ICU discharge
    #ICU_dur_ <- floor(rlnorm(sum(ICU_), meanlog=time_ICUdur_pars[1], sdlog=time_ICUdur_pars[2]))
    ICU_dur_ <- round(exp(time_ICUdur_pars[1]))
    ICU_end_ <- ICU_time_ + ICU_dur_   
    names(ICU_end_) <- rep(data_H$county_sim, ICU_)
    data_ICUend <- data.frame(time=ICU_end_, county_sim=names(ICU_end_))
    data_ICUend <- setDT(data_ICUend)[, .N, by = .(time, county_sim)]
    colnames(data_ICUend) <- c("time","county_sim","endICU")
    
    
    # Add Vent
    Vent_ <- rbinom(data_ICU$incidICU, data_ICU$incidICU, rep(p_vent, length(nrow(data_ICU))))
    names(Vent_) <- data_ICU$county_sim
    # Time from ICU to mechanical ventilation
    #Vent_delay_ <- floor(rlnorm(sum(Vent_), meanlog=time_vent_pars[1], sdlog=time_vent_pars[2]))
    Vent_delay_ <- exp(time_vent_pars[1])
    Vent_time_ <- rep(as.Date(data_ICU$time), Vent_) + Vent_delay_    
    names(Vent_time_) <- rep(data_ICU$county_sim, Vent_)
    data_Vent <- data.frame(time=Vent_time_, county_sim=names(Vent_time_))
    data_Vent <- setDT(data_Vent)[, .N, by = .(time, county_sim)]
    colnames(data_Vent) <- c("time","county_sim","incidVent")
    
    
    # Add D
    D_ <- rbinom(data_H$incidH, data_H$incidH, rep(p_death, length(nrow(data_H))))
    names(D_) <- data_H$county_sim
    # Date of Death
    #D_delay_ <- floor(rlnorm(sum(D_), meanlog=time_death_pars[1], sdlog=time_death_pars[2]))
    D_delay_ <- exp(time_death_pars[1])
    D_time_ <- rep(as.Date(data_H$time), D_) + D_delay_  
    D_date_hosp <- rep(as.Date(data_H$time), D_)
    names(D_time_) <- rep(data_H$county_sim, D_)
    names(D_date_hosp) <- rep(data_H$county_sim, D_)
    data_D <- data.frame(time=D_time_, county_sim=names(D_time_))
    data_D <- setDT(data_D)[, .N, by = .(time, county_sim)]
    colnames(data_D) <- c("time","county_sim","incidD")
    
    
    # Add R
    R_ <- data_H$incidH - D_
    # Rate of Recovery
    #R_delay_ <- floor(rlnorm(sum(R_), meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))
    R_delay_ <- round(exp(time_disch_pars[1]))
    R_time_ <- rep(as.Date(data_H$time), R_) + R_delay_  
    R_date_hosp <- rep(as.Date(data_H$time), R_)
    names(R_time_) <- rep(data_H$county_sim, R_)
    names(R_date_hosp) <- rep(data_H$county_sim, R_)
    data_R <- data.frame(time=R_time_, county_sim=names(R_time_))
    data_R <- setDT(data_R)[, .N, by = .(time, county_sim)]
    colnames(data_R) <- c("time","county_sim","incidR")
    
    
    
    # Get durations ....................
    
    curr_hosp_date <- NULL
    curr_hospD_date <- NULL
    curr_icu_date <- NULL
    
    
    
    
    if (run_parallel){
        
        cl <- makeCluster(cores)
        
        # Get current hospitalization days and accumulate them -- Recoveries
        if (sum(R_)>0){
            curr_hosp_date <- rev(foreach(n=1:sum(R_), .combine = c) %dopar% {
                R_date_hosp[n] + 0:R_delay_ })
            names(curr_hosp_date) <- rep(names(R_date_hosp), (R_delay_+1)) # add country_sim
        }
    
        
        # Get current hospitalization days and accumulate them -- Deaths
        if (sum(D_)>0){
            curr_hospD_date <- rev(foreach(n=1:sum(D_), .combine = c) %dopar% {
                D_date_hosp[n] + 0:D_delay_ })
            names(curr_hospD_date) <- rep(names(D_time_), (D_delay_+1))
        }
        
        # Get current ICU days and accumulate them -- ALL (add ICU eventually)
        if (sum(ICU_)>0){
            curr_icu_date <- rev(foreach(n=1:sum(ICU_), .combine = c) %dopar% {
                ICU_time_[n] + 0:ICU_dur_ })
            names(curr_icu_date) <- rep(names(ICU_time_), (ICU_dur_+1))
        }
        
        stopCluster(cl)
        
    } else {
        
        # Get current hospitalization days and accumulate them -- Recoveries
        if (sum(R_)>0){
            curr_hosp_date <- rev(unlist(
                sapply(1:sum(R_), function(x) R_date_hosp[x] + 0:R_delay_)))
            names(curr_hosp_date) <- rep(names(R_time_), (R_delay_+1)) # add country_sim
        }
        
        if (sum(D_)>0){
            # Get current hospitalization days and accumulate them -- Deaths
            curr_hospD_date <- rev(unlist(
                sapply(1:sum(D_), function(x) D_date_hosp[x] + 0:D_delay_)))
            names(curr_hospD_date) <- rep(names(D_time_), (D_delay_+1))
        }
        
        if (sum(ICU_)>0){
            # Get current ICU days and accumulate them -- ALL (add ICU eventually)
            curr_icu_date <- rev(unlist(
                sapply(1:sum(ICU_), function(x) ICU_time_[x] + 0:ICU_dur_)))
            names(curr_icu_date) <- rep(names(ICU_time_), (ICU_dur_+1))
        }
    }
    
    
    
    # ----------------------------------------------
    
    data_currhosp <- data_curricu <- NULL
    if (sum(H_)>0){
        # combine them & tabulate
        curr_hosp_date <- c(curr_hosp_date, curr_hospD_date)
        
        data_currhosp <- data.frame(time=curr_hosp_date, county_sim=names(curr_hosp_date))
        data_currhosp <- setDT(data_currhosp)[, .N, by = .(time, county_sim)]
        colnames(data_currhosp) <- c("time","county_sim","hosp_curr")
        
        data_curricu <- data.frame(time=curr_icu_date, county_sim=names(curr_icu_date))
        data_curricu <- setDT(data_curricu)[, .N, by = .(time, county_sim)]
        colnames(data_curricu) <- c("time","county_sim","icu_curr")
        
        curr_hosp_date <- curr_icu_date <- curr_hospD_date <- NULL
    }
    
    # Merge them all
    res <- full_join(data_H, data_ICU, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_Vent, by=c("time", "county_sim"="county_sim"))
    res <- full_join(res, data_D, by=c("time", "county_sim"="county_sim"))
    
    if (sum(H_)>0){
        res <- full_join(res, data_currhosp, by=c("time", "county_sim"="county_sim"))
        res <- full_join(res, data_curricu, by=c("time", "county_sim"="county_sim"))
    } else {
        res$hosp_curr <- 0
        res$icu_curr <- 0
    }
    
    # Add full dates if we want that -- leaving out to save memory
    # res <- full_join(data.frame(time=as.Date(date_tmp)), 
    #                  res, 
    #                  by=c("time"))
    
    # Add incidence for checking -- leaving out to save memory
    res <- full_join(res, 
                     data %>% select(time, county_sim, incidI), 
                     by=c("time", "county_sim"="county_sim"))
    
    
    data_ICU <- data_ICUend <- data_Vent <- data_D <- 
        data_currhosp <- data_curricu <- date_tmp <- data_H <- NULL
    
    
    res <- res %>% 
        replace_na(
            list(incidH = 0,
                 incidICU = 0,
                 incidVent = 0,
                 incidD = 0,
                 hosp_curr = 0,
                 icu_curr = 0)
        ) %>%
        mutate(geoid = substr(county_sim,1,length_geoid),
               sim_num= substr(county_sim,length_geoid+2,length_geoid+7)) %>%
        left_join(data %>% select(geoid, metrop_labels) %>% distinct(), by='geoid')
    
    
    # Summarization starts here
    
    res_metro <- res %>%
        filter(!is.na(county_sim) & !is.na(metrop_labels)) %>% 
        select(-county_sim) %>%
        mutate(time = as.Date(time)) %>%
        #filter(time <= as.Date(end_date)) %>%
        group_by(metrop_labels, sim_num) %>% 
        summarize(
            # nInf = sum(incidI, na.rm = TRUE), 
            nhosp = sum(incidH, na.rm = TRUE), 
            nICU = sum(incidICU, na.rm = TRUE), 
            nVent = sum(incidVent, na.rm = TRUE), 
            ndeath = sum(incidD, na.rm = TRUE),
            maxHospAdm = max(incidH, na.rm=TRUE),
            maxICUAdm = max(incidICU, na.rm=TRUE),
            maxHospCap = max(hosp_curr, na.rm = TRUE),
            maxICUCap = max(icu_curr, na.rm=TRUE)) %>%
        ungroup() %>% 
        group_by(metrop_labels) %>% 
        summarize(#nInf_final = mean(nInf),
            #nInf_lo = quantile(nInf, 0.25),
            #nInf_hi = quantile(nInf, 0.75),
            nhosp_final = mean(nhosp),
            nhosp_lo = quantile(nhosp, 0.25),
            nhosp_hi = quantile(nhosp, 0.75),
            phosp_final = mean(maxHospAdm),
            phosp_lo = quantile(maxHospAdm, 0.25),
            phosp_hi = quantile(maxHospAdm, 0.75),
            nICU_final = mean(nICU),
            nICU_lo = quantile(nICU, 0.25),
            nICU_hi = quantile(nICU, 0.75),
            pICU_final = mean(maxICUAdm),
            pICU_lo = quantile(maxICUAdm, 0.25),
            pICU_hi = quantile(maxICUAdm, 0.75),
            nVent_final = mean(nVent),
            nVent_lo = quantile(nVent, 0.25),
            nVent_hi = quantile(nVent, 0.75),
            ndeath_final = mean(ndeath),
            ndeath_lo = quantile(ndeath, 0.25),
            ndeath_hi = quantile(ndeath, 0.75),
            nhosp_curr_final = mean(maxHospCap),
            nhosp_curr_lo = quantile(maxHospCap, 0.25),
            nhosp_curr_hi = quantile(maxHospCap, 0.75),
            nicu_curr_final = mean(maxICUCap),
            nicu_curr_lo = quantile(maxICUCap, 0.25),
            nicu_curr_hi = quantile(maxICUCap, 0.75))
    
    res_total <- res %>% 
        filter(!is.na(county_sim)) %>% 
        select(-county_sim) %>%
        #filter(time <= as.Date(end_date)) %>%
        group_by(sim_num) %>% 
        summarize(#nInf = sum(incidI, na.rm = TRUE), 
            nhosp = sum(incidH, na.rm = TRUE), 
            nICU = sum(incidICU, na.rm = TRUE), 
            nVent = sum(incidVent, na.rm = TRUE), 
            ndeath = sum(incidD, na.rm = TRUE),
            maxHospAdm = max(incidH, na.rm=TRUE),
            maxICUAdm = max(incidICU, na.rm=TRUE),
            maxHospCap = max(hosp_curr, na.rm = TRUE),
            maxICUCap = max(icu_curr, na.rm=TRUE)) %>%
        ungroup() %>% 
        summarize(#nInf_final = mean(nInf),
            #nInf_lo = quantile(nInf, 0.25),
            #nInf_hi = quantile(nInf, 0.75),
            nhosp_final = mean(nhosp),
            nhosp_lo = quantile(nhosp, 0.25),
            nhosp_hi = quantile(nhosp, 0.75),
            phosp_final = mean(maxHospAdm),
            phosp_lo = quantile(maxHospAdm, 0.25),
            phosp_hi = quantile(maxHospAdm, 0.75),
            nICU_final = mean(nICU),
            nICU_lo = quantile(nICU, 0.25),
            nICU_hi = quantile(nICU, 0.75),
            pICU_final = mean(maxICUAdm),
            pICU_lo = quantile(maxICUAdm, 0.25),
            pICU_hi = quantile(maxICUAdm, 0.75),
            nVent_final = mean(nVent),
            nVent_lo = quantile(nVent, 0.25),
            nVent_hi = quantile(nVent, 0.75),
            ndeath_final = mean(ndeath),
            ndeath_lo = quantile(ndeath, 0.25),
            ndeath_hi = quantile(ndeath, 0.75),
            nhosp_curr_final = mean(maxHospCap),
            nhosp_curr_lo = quantile(maxHospCap, 0.25),
            nhosp_curr_hi = quantile(maxHospCap, 0.75),
            nicu_curr_final = mean(maxICUCap),
            nicu_curr_lo = quantile(maxICUCap, 0.25),
            nicu_curr_hi = quantile(maxICUCap, 0.75))
    
    out <- list(res_total = as.data.frame(res_total), res_metro = as.data.frame(res_metro))
    
    if(incl.county){
        res_geoid <- res %>% 
            filter(!is.na(county_sim)) %>% 
            select(-county_sim) %>%
            #filter(time <= as.Date(end_date)) %>%
            group_by(geoid, sim_num) %>% 
            summarize(#nInf = sum(incidI, na.rm = TRUE), 
                nhosp = sum(incidH, na.rm = TRUE), 
                nICU = sum(incidICU, na.rm = TRUE), 
                nVent = sum(incidVent, na.rm = TRUE), 
                ndeath = sum(incidD, na.rm = TRUE),
                maxHospAdm = max(incidH, na.rm=TRUE),
                maxICUAdm = max(incidICU, na.rm=TRUE),
                maxHospCap = max(hosp_curr, na.rm = TRUE),
                maxICUCap = max(icu_curr, na.rm=TRUE)) %>%
            ungroup() %>% 
            group_by(geoid) %>% 
            summarize(
                #nInf_final = mean(nInf),
                #     nInf_lo = quantile(nInf, 0.25),
                #    nInf_hi = quantile(nInf, 0.75),
                nhosp_final = mean(nhosp),
                nhosp_lo = quantile(nhosp, 0.25),
                nhosp_hi = quantile(nhosp, 0.75),
                phosp_final = mean(maxHospAdm),
                phosp_lo = quantile(maxHospAdm, 0.25),
                phosp_hi = quantile(maxHospAdm, 0.75),
                nICU_final = mean(nICU),
                nICU_lo = quantile(nICU, 0.25),
                nICU_hi = quantile(nICU, 0.75),
                pICU_final = mean(maxICUAdm),
                pICU_lo = quantile(maxICUAdm, 0.25),
                pICU_hi = quantile(maxICUAdm, 0.75),
                nVent_final = mean(nVent),
                nVent_lo = quantile(nVent, 0.25),
                nVent_hi = quantile(nVent, 0.75),
                ndeath_final = mean(ndeath),
                ndeath_lo = quantile(ndeath, 0.25),
                ndeath_hi = quantile(ndeath, 0.75),
                nhosp_curr_final = mean(maxHospCap),
                nhosp_curr_lo = quantile(maxHospCap, 0.25),
                nhosp_curr_hi = quantile(maxHospCap, 0.75),
                nicu_curr_final = mean(maxICUCap),
                nicu_curr_lo = quantile(maxICUCap, 0.25),
                nicu_curr_hi = quantile(maxICUCap, 0.75))
        
        out <- list(res_total = as.data.frame(res_total), res_metro = as.data.frame(res_metro), res_geoid = as.data.frame(res_geoid))
    }
    
    return(out)
}






