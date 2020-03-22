
build_hospdeath_par <- function(p_hosp, p_death, p_vent, p_ICU, data_filename, scenario_name, target_geo_ids,
                                p_hosp_type="gamma",
                                time_hosp_pars = c(1.23, 0.79), 
                                time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                time_death_pars = c(log(11.25), log(1.15)), 
                                time_disch_pars = c(log(11.5), log(1.22)),
                                time_ICUdur_pars = c(log(17.46), log(4.044)),
                                end_date = "2020-04-01",
                                cores=8,
                                root_out_dir='hospitalization') {
  
  n_sim <- length(list.files(data_filename))
  print(paste("Creating cluster with",cores,"cores"))
  doParallel::registerDoParallel(cores)
  
  print(paste("Running over",n_sim,"simulations"))
  dat_final <- foreach::foreach(s=seq_len(n_sim), .packages=c("dplyr","readr","data.table","tidyr")) %dopar% {
    create_delay_frame <- function(X, p_X, data_, X_pars, varname) {
      X_ <- rbinom(length(data_[[X]]),data_[[X]],p_X)
      # nonzero_mask <- X_ > 0
      nonzero_mask <- rep(TRUE,length(X_))
      nonzero_time <- data_$time[nonzero_mask] 
      nonzero_uid <- data_$uid[nonzero_mask] 
      rc <- data.table(
        time = data_$time[nonzero_mask] + round(exp(X_pars[1] + X_pars[2]^2 / 2)),
        uid = data_$uid[nonzero_mask],
        count = X_[nonzero_mask]
      )
      names(rc)[3] <- paste0("incid",varname)

      return(rc)
    }

    load_scenario_sim <- function(scenario_dir,
                                   sim_id,
                                   keep_compartments=NULL,
                                   time_filter_low = -Inf,
                                   time_filter_high = Inf
    ){
      files <- dir(scenario_dir,full.names = TRUE)
      rc <- list()
      i = sim_id
      file <- files[i]
      if (is.null(keep_compartments)) {
        suppressMessages(tmp <- read_csv(file))
      } else {
        suppressMessages(
          tmp <-  read_csv(file) %>%
            filter(comp%in%keep_compartments)
        )
      }
    
      tmp <- #tmp[-1,] %>%
        tmp %>%
        filter(time <= time_filter_high & time >= time_filter_low) %>%
        pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>%
        mutate(sim_num = i)
      return(tmp)
    }

    dat_ <- load_scenario_sim(data_filename,s,keep_compartments = c("diffI","cumI")) %>%
    filter(geoid %in% target_geo_ids, time<=end_date, comp == "diffI") %>%
    mutate(hosp_curr = 0, icu_curr = 0, vent_curr = 0, uid = paste0(geoid, "-",sim_num)) %>%
    rename(incidI = N)
    dates_ <- as.Date(dat_$time)
    
    # Add time things
    dat_H <- create_delay_frame('incidI',p_hosp,dat_,time_hosp_pars,"H")
    data_ICU <- create_delay_frame('incidH',p_ICU,dat_H,time_ICU_pars,"ICU")
    data_Vent <- create_delay_frame('incidICU',p_vent,data_ICU,time_vent_pars,"Vent")
    data_D <- create_delay_frame('incidH',p_death,dat_H,time_death_pars,"D")
    R_delay_ <- round(exp(time_disch_pars[1]))
    ICU_dur_ <- round(exp(time_ICUdur_pars[1]))
    
    # Using `merge` instead of full_join for performance reasons    
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
    res$sim_num_good <- as.numeric(res$sim_num) 
    res$sim_num_good <- res$sim_num_good - min(res$sim_num_good) +1
    
    res$geo_ind <- as.numeric(as.factor(res$geoid))

     res <- res %>%
       arrange(geo_ind, date_inds) %>%
       group_by(geo_ind) %>%
       group_map(function(.x,.y){
         .x$hosp_curr <- cumsum(.x$incidH) - lag(cumsum(.x$incidH),n=R_delay_,default=0)
         .x$icu_curr <- cumsum(.x$incidICU) - lag(cumsum(.x$incidICU),n=ICU_dur_,default=0)
         .x$geo_ind <- .y$geo_ind
         return(.x)
       }) %>%
       do.call(what=rbind) %>%
       arrange(date_inds, geo_ind)

    outfile <- paste0(root_out_dir,'/', data_filename,'/',scenario_name,'-',s,'.csv')
    outdir <- gsub('/[^/]*$','',outfile)
    if(!dir.exists(outdir)){
      dir.create(outdir,recursive=TRUE)
    }
    write.csv(res,outfile)
  }
  print(paste("Parallel portion finished"))
  doParallel::stopImplicitCluster()
}
