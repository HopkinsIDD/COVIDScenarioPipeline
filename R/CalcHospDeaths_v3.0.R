


# RUN THE CALCULATION -----------------------------------------------------

# will have to adjust to model output and time
# add detection and confirmation later
# 
# test_data <- data.frame(t = 0:8, S = c(1000, 999, 999, 990, 980, 960, 800, 700, 500), incidI = c(1, 1, 0, 10, 10, 20, 160, 100, 200))
# 
# 
# data <- test_data %>% select(t, incidI)
# res_data <- build_hospdeath(data, p_hosp=p_hosp[3], p_death=p_death[3], time_hosp_pars, time_death_pars, time_disch_pars)








create_delay_frame <- function(X, p_X, data_, X_pars, varname) {
        X_ <- rbinom(length(data_[[X]]),data_[[X]],p_X)
        data_X <- data.frame(time=data_$time,  uid=data_$uid, count=X_)
        X_delay_ <- round(exp(X_pars[1] + X_pars[2]^2 / 2))
        
        X_time_ <- rep(as.Date(data_X$time), data_X$count) + X_delay_
        names(X_time_) <- rep(data_$uid, data_X$count)
        
        data_X <- data.frame(time=X_time_, uid=names(X_time_))
        data_X <- data.frame(setDT(data_X)[, .N, by = .(time, uid)])
        colnames(data_X) <- c("time","uid",paste0("incid",varname))
  return(data_X)
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

build_hospdeath_par <- function(data, p_hosp, p_death, p_vent, p_ICU, p_hosp_type="gamma",
                                time_hosp_pars = c(1.23, 0.79), 
                                time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                time_death_pars = c(log(11.25), log(1.15)), 
                                time_disch_pars = c(log(11.5), log(1.22)),
                                time_ICUdur_pars = c(log(17.46), log(4.044)),
                                time_ventdur_pars = c(log(17.46-1), log(4.044)),
                                end_date = "2020-04-01",
                                length_geoid = 5,
                                incl.county=FALSE,
                                cores=8, 
                                run_parallel=FALSE){
  
  library(data.table)
  library(doParallel)
  
  # filter to earlier than the end_date
  data <- data %>% filter(time<=end_date, incidI>0)
  
  # Set up results data
  data$uid <- paste0(data$geoid, "-",data$sim_num)
  n_sim <- length(unique(data$sim_num))
  
  dat_final <- list()
  
  
  print(paste("Creating cluster with",cores,"cores"))
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  
  print(paste("Running over",n_sim,simulations))
  dat_final <- foreach(s=1:n_sim, .package=c("dplyr","readr","data.table","tidyr")) %dopar% {
    
    dat_ <- data %>% filter(sim_num==s) %>% 
      mutate(hosp_curr = 0, icu_curr = 0, vent_curr=0)
    dates_ <- as.Date(dat_$time)
    
    
    # Add hosp    
    dat_H <- create_delay_frame('incidI',p_hosp,dat_,time_hosp_pars,"H")
    
    
    # Add ICU
    data_ICU <- create_delay_frame('incidH',p_ICU,dat_H,time_ICU_pars,"ICU")
    
    
    # Add Vent
    data_Vent <- create_delay_frame('incidICU',p_vent,data_ICU,time_vent_pars,"Vent")
    
    # Add D
    data_D <- create_delay_frame('incidH',p_death,dat_H,time_death_pars,"D")
    
    
    # Add R
    #R_ <- dat_H$incidH - D_
    # Rate of Recovery
    #R_delay_ <- floor(rlnorm(sum(R_), meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))
    R_delay_ <- round(exp(time_disch_pars[1]))
    #R_time_ <- rep(as.Date(dat_H$time), R_) + R_delay_  
    #R_date_hosp <- rep(as.Date(dat_H$time), R_)
    #names(R_time_) <- rep(dat_H$uid, R_)
    #names(R_date_hosp) <- rep(dat_H$uid, R_)
    # dat__R <- data.frame(time=R_time_, uid=names(R_time_))
    # dat__R <- data.frame(setDT(dat__R)[, .N, by = .(time, uid)])
    # colnames(dat__R) <- c("time","uid","incidR")
    ICU_dur_ <- round(exp(time_ICUdur_pars[1]))
    Vent_dur_ <- round(exp(time_ventdur_pars[1]))
    
    
    # Get durations ....................
    
    
    # Merge them all
    
    #Some reason not working right
    # res <- full_join(dat_H %>% mutate(uid = as.character(uid)),
    #                  data_ICU %>% mutate(uid = as.character(uid)), by=c("time"="time", "uid"="uid"))
    # res <- full_join(res, data_D %>% mutate(uid = as.character(uid)), by=c("time", "uid"="uid"))
    # #res <- full_join(res, dat__R, by=c("time", "uid"="uid"))
    # res <- full_join(dat_ %>% mutate(uid = as.character(uid)),
    #                  res %>% mutate(uid = as.character(uid)), by=c("time"="time", "uid"="uid"))
    
    # Using `merge` instead     
    res <- merge(dat_H %>% mutate(uid = as.character(uid)), 
                 data_ICU %>% mutate(uid = as.character(uid)), all=TRUE)
    res <- merge(res, data_Vent %>% mutate(uid = as.character(uid)), all=TRUE)
    res <- merge(res, data_D %>% mutate(uid = as.character(uid)), all=TRUE)
    res <- merge(dat_ %>% mutate(uid = as.character(uid)), 
                 res %>% mutate(uid = as.character(uid)), all=TRUE)
    
    res <- res %>% 
      replace_na(
        list(incidI = 0,
             incidH = 0,
             incidICU = 0,
             incidVent = 0,
             incidD = 0,
             vent_curr = 0,
             hosp_curr = 0))
    
    # get sim nums
    res <- res %>% select(-geoid, -sim_num) %>%
      separate(uid, c("geoid", "sim_num"), sep="-", remove=FALSE)
    
    res <- res %>% mutate(date_inds = as.integer(time - min(time) + 1))
    n_sim <- length(unique(res$sim_num))
    
    
    
    for (x in 1:nrow(res)){
      res$hosp_curr <- res$hosp_curr + res$date_inds %in% (res$date_inds[x] + 0:R_delay_)*res$incidH[x]
      res$icu_curr <- res$icu_curr + res$date_inds %in% (res$date_inds[x] + 0:ICU_dur_)*res$incidICU[x]
      res$vent_curr <- res$vent_curr + res$date_inds %in% (res$date_inds[x] + 0:Vent_dur_)*res$incidVent[x]
    }
    res
    
  }
  print(paste("Parallel portion finished"))
  
  stopCluster(cl)
  
  
  
  res <- rbindlist(dat_final)
  
  
  res <- res %>% 
    replace_na(
      list(incidH = 0,
           incidICU = 0,
           incidVent = 0,
           incidD = 0,
           hosp_curr = 0,
           icu_curr = 0,
           vent_curr = 0)
    ) 
  
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

build_hospdeath_summarize <- function(res,
                                      end_date = "2020-10-01",
                                      incl.county = FALSE){
  
  # Summarization starts here
  
  res_metro <- res %>%
    filter(!is.na(time) & !is.na(metrop_labels)) %>% 
    mutate(time = as.Date(time)) %>%
    filter(time <= as.Date(end_date)) %>%
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
      maxICUCap = max(icu_curr, na.rm=TRUE),
      maxVentCap = max(vent_curr, na.rm=TRUE)
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
      ndeath_hi = quantile(ndeath, 0.75),
      nhosp_curr_final = mean(maxHospCap),
      nhosp_curr_lo = quantile(maxHospCap, 0.25),
      nhosp_curr_hi = quantile(maxHospCap, 0.75),
      nicu_curr_final = mean(maxICUCap),
      nicu_curr_lo = quantile(maxICUCap, 0.25),
      nicu_curr_hi = quantile(maxICUCap, 0.75),
      nvent_curr_final = mean(maxVentCap),
      nvent_curr_lo = quantile(maxVentCap, 0.25),
      nvent_curr_hi = quantile(maxVentCap, 0.75)
    )
  
  res_total <- res %>% 
    filter(!is.na(time)) %>% 
    filter(time <= as.Date(end_date)) %>%
    group_by(sim_num) %>% 
    summarize(#nInf = sum(incidI, na.rm = TRUE), 
      nhosp = sum(incidH, na.rm = TRUE), 
      nICU = sum(incidICU, na.rm = TRUE), 
      nVent = sum(incidVent, na.rm = TRUE), 
      ndeath = sum(incidD, na.rm = TRUE),
      maxHospAdm = max(incidH, na.rm=TRUE),
      maxICUAdm = max(incidICU, na.rm=TRUE),
      maxHospCap = max(hosp_curr, na.rm = TRUE),
      maxICUCap = max(icu_curr, na.rm=TRUE),
      maxVentCap = max(vent_curr, na.rm=TRUE)
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
      ndeath_hi = quantile(ndeath, 0.75),
      nhosp_curr_final = mean(maxHospCap),
      nhosp_curr_lo = quantile(maxHospCap, 0.25),
      nhosp_curr_hi = quantile(maxHospCap, 0.75),
      nicu_curr_final = mean(maxICUCap),
      nicu_curr_lo = quantile(maxICUCap, 0.25),
      nicu_curr_hi = quantile(maxICUCap, 0.75),
      nvent_curr_final = mean(maxVentCap),
      nvent_curr_lo = quantile(maxVentCap, 0.25),
      nvent_curr_hi = quantile(maxVentCap, 0.75))
  
  out <- list(res_total = as.data.frame(res_total), res_metro = as.data.frame(res_metro))
  
  if(incl.county){
    res_geoid <- res %>% 
      filter(!is.na(geoid)) %>% 
      filter(time <= as.Date(end_date)) %>%
      group_by(geoid, sim_num) %>% 
      summarize(#nInf = sum(incidI, na.rm = TRUE), 
        nhosp = sum(incidH, na.rm = TRUE), 
        nICU = sum(incidICU, na.rm = TRUE), 
        nVent = sum(incidVent, na.rm = TRUE), 
        ndeath = sum(incidD, na.rm = TRUE),
        maxHospAdm = max(incidH, na.rm=TRUE),
        maxICUAdm = max(incidICU, na.rm=TRUE),
        maxHospCap = max(hosp_curr, na.rm = TRUE),
        maxICUCap = max(icu_curr, na.rm=TRUE),
        maxVentCap = max(vent_curr, na.rm=TRUE)
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
        ndeath_hi = quantile(ndeath, 0.75),
        nhosp_curr_final = mean(maxHospCap),
        nhosp_curr_lo = quantile(maxHospCap, 0.25),
        nhosp_curr_hi = quantile(maxHospCap, 0.75),
        nicu_curr_final = mean(maxICUCap),
        nicu_curr_lo = quantile(maxICUCap, 0.25),
        nicu_curr_hi = quantile(maxICUCap, 0.75),
        nvent_curr_final = mean(maxVentCap),
        nvent_curr_lo = quantile(maxVentCap, 0.25),
        nvent_curr_hi = quantile(maxVentCap, 0.75)
      )
    
    out <- list(res_total = as.data.frame(res_total), res_metro = as.data.frame(res_metro), res_geoid = as.data.frame(res_geoid))
  }
  
  return(out)
}













##' Function 
##' Build a set of sampled hospitalizations, deaths, and recoveries 
##' from full hosp res objects at multiple p_deaths
##' 
##' Instantly summarize to save memory + run for multiple p_death
##'  
##' @param data list of res results for given secnario
##' @param p_death_vec vector probability of death, among infections (hospitalization is required for death)
##' @param end_date date at which to summarize hospitalizations or deaths
##' @param incl_county logical, whether to produce a table grouped by geoid in addition to state + metrop
##' 

build_hospdeath_summary_multiplePDeath <- function(data, p_death_vec, end_date, incl.county){
  
  
  tmp_out <- build_hospdeath_summarize(data[[1]], 
                                       end_date = end_date,
                                       incl.county = incl.county) 
  
  tmp_metro <- tmp_out[['res_metro']] %>% mutate(p_death = p_death[1])
  tmp_total <- tmp_out[['res_total']] %>% mutate(p_death = p_death[1])
  if(incl.county){ tmp_geoid <- tmp_out[['res_geoid']] %>% mutate(p_death = p_death[1]) }
  
  for(i in 2:length(p_death)){
    tmp_out <- build_hospdeath_summarize(data[[i]], 
                                         end_date = end_date,
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

build_hospdeath_summary_multiplePDeath_OLD <- function(data, 
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

build_hospdeath_SLOWVERSION <- function(data, p_hosp, p_death, p_vent, p_ICU,
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
    uid <- paste0(data$geoid, "-",sim_num)
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
    
    return(res)
}














