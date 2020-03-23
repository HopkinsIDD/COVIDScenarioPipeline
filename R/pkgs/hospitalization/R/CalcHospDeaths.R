
## Notes on package import from jwills
## These functions were derived from the CalcHospDeaths_v3.0.R
## file in the top-level source repo. I took that file and ripped
## out anything that was either old/unused or had been replaced
## by logic in another file (in this case, the hospdeath.R file
## that has the fast impl of build_hospdeath_par that we're currently
## using for most runs.) No changes to the remaining two functions
## have been made yet relative to their impls in CalcHospDeath_v3.0.R.

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

