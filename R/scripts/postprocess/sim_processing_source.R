


# GENERAL -----------------------------------------------------------------


fix_NAs <- function(x){
    x[which(is.na(x))] <- 0
    return(x)
}




# DATA LOADING ------------------------------------------------------------


combine_and_format_sims <- function(outcome_vars = "incid",
                                    scenario_dir = arguments$args,
                                    quick_run = FALSE,
                                    testing  = FALSE,
                                    outcomes_, 
                                    keep_variant_compartments = keep_variant_compartments,
                                    keep_vacc_compartments = keep_vacc_compartments,
                                    keep_all_compartments = keep_all_compartments,
                                    variants_ = variants_,
                                    vacc_ = NULL,
                                    county_level = FALSE, 
                                    forecast_date = opt$forecast_date,
                                    end_date = opt$end_date,
                                    geodata, 
                                    death_filter = opt$death_filter) {
    
    res_geoid_all <- arrow::open_dataset(sprintf("%shosp",scenario_dir), 
                                         partitioning = c("location", "scenario", "death_rate", "config", "lik_type", "is_final")) %>%
        select(time, geoid, death_rate, starts_with(outcome_vars)) %>%
        filter(time>=forecast_date & time<=end_date) %>%
        collect() %>%
        filter(stringr::str_detect(death_rate, death_filter)) %>%
        mutate(time=as.Date(time)) %>%
        group_by(time, geoid, death_rate) %>%
        dplyr::mutate(sim_num = as.character(seq_along(geoid))) %>%
        ungroup() 
    
    if (quick_run){
        res_geoid_all <- res_geoid_all %>% filter(sim_num %in% 1:20)
    }
    gc()
    
    # ~ Subset if testing
    if (testing){
        res_geoid_all <- res_geoid_all %>% filter(sim_num %in% sample(.$sim_num, 10, replace = FALSE))
    }
    
    
    # pull out just the total outcomes of interest
    cols_aggr <- expand_grid(a="incid",b=outcomes_) %>% mutate(d=paste0(a,b)) %>% pull(d)
    cols_aggr <- cols_aggr[cols_aggr %in% colnames(res_geoid_all)]
    
    if(!keep_all_compartments & !keep_variant_compartments & !keep_vacc_compartments){
        res_geoid_all <- res_geoid_all %>%
            select(time, geoid, death_rate, sim_num, all_of(cols_aggr))
        
    } else if (keep_variant_compartments){
        # pull out just the variant outcomes
        cols_vars <- expand_grid(a="incid",b=outcomes_, c=paste0("_", variants_)) %>% mutate(d=paste0(a,b,c)) %>% pull(d)
        cols_vars <- cols_vars[cols_vars %in% colnames(res_geoid_all)]
        res_geoid_all <- res_geoid_all %>%
            select(time, geoid, death_rate, sim_num, all_of(cols_vars))
    } else if (keep_all_compartments){
        # remove the aggregate outcomes 
        res_geoid_all <- res_geoid_all %>%
            select(-all_of(cols_vars), -all_of(cols_aggr))
    } else if (keep_vacc_compartments){
        # pull out just the variant outcomes
        cols_vars <- expand_grid(a="incid",b=outcomes_, c=paste0("_", vacc_)) %>% mutate(d=paste0(a,b,c)) %>% pull(d)
        cols_vars <- cols_vars[cols_vars %in% colnames(res_geoid_all)]
        res_geoid_all <- res_geoid_all %>%
            select(time, geoid, death_rate, sim_num, all_of(cols_vars))
    }
    
    
    # Merge in Geodata
    
    if(county_level){
        res_state <- res_geoid_all %>%
            inner_join(geodata%>%select(geoid, USPS)) %>%
            group_by_at(c("USPS", "time", "sim_num", compartment_types)) %>%
            summarise(across(starts_with("incid"), sum)) %>%
            as_tibble()
    } else {
        res_state <- res_geoid_all %>%
            inner_join(geodata %>% select(geoid, USPS))
    }
    rm(res_geoid_all)
    
    # ~ Add US totals
    res_us <- res_state %>% 
        group_by(time, sim_num, death_rate) %>%
        summarise(across(starts_with("incid"), sum)) %>%
        as_tibble() %>%
        mutate(USPS = "US")
    res_state <- res_state %>%
        bind_rows(res_us)
    rm(res_us)
    
    return(res_state)
}




load_simulations <- function(geodata,
                             sim_directory = arguments$args, 
                             forecast_date = opt$forecast_date,
                             end_date = opt$end_date,
                             death_filter = opt$death_filter,
                             compartment_types,
                             county_level = FALSE,
                             keep_compartments = TRUE,
                             testing = FALSE){
    
    res_geoid <- arrow::open_dataset(sprintf("%s/hosp", sim_directory), 
                                     partitioning =c("location", 
                                                     "scenario", 
                                                     "death_rate", 
                                                     "config", 
                                                     "lik_type", 
                                                     "is_final")) %>%
        select(time, geoid, starts_with("incid"), death_rate)%>%
        filter(time>=forecast_date & time<=end_date)%>%
        collect() %>%
        filter(stringr::str_detect(death_rate, death_filter))%>%
        mutate(time=as.Date(time)) %>%
        group_by(time, geoid, death_rate) %>%
        dplyr::mutate(sim_num = as.character(seq_along(geoid))) %>%
        ungroup() %>%
        pivot_longer(cols=starts_with("incid"), 
                     names_to = c("outcome",compartment_types), 
                     names_pattern = "(.*)_(.*)_(.*)_(.*)", values_to = "value") %>%
        filter(!is.na(outcome)) 
    
    res_geoid <- res_geoid %>%
        pivot_wider(names_from = outcome, values_from = value)
    
    # Subset for testing
    if(testing){
        res_geoid <- res_geoid %>% filter(sim_num %in% 1:10)
        res_geoid_long <- res_geoid_long %>% filter(sim_num %in% 1:10)
    }
    
    # res_geoid <- res_geoid %>%
    #   group_by(time, geoid, death_rate, variant, vacc, agestrat, sim_num)%>%
    #   #summarise(across(starts_with("incid"), sum)) %>%
    #   summarise(incidD=sum(incidD), incidH=sum(incidH), incidC=sum(incidC))%>%
    #   as_tibble()
    
    if(county_level){
        res_state <- res_geoid %>%
            inner_join(geodata%>%select(geoid, USPS)) %>%
            group_by_at(c("USPS", "time", "sim_num", compartment_types)) %>%
            # summarize(incidI=sum(incidI),
            #           incidD=sum(incidD),
            #           incidH=sum(incidH),
            #           incidC=sum(incidC)) %>%
            summarise(across(starts_with("incid"), sum)) %>%
            as_tibble()
    } else {
        res_state <- res_geoid %>%
            inner_join(geodata%>%select(geoid, USPS))
        
        if (keep_compartments){
            res_state_long <- res_geoid_long %>% 
                inner_join(geodata%>%select(geoid, USPS))
        }
        rm(res_geoid_long, res_geoid)
    }
    
    # ADD US TOTAL
    res_us <- res_state %>% 
        group_by_at(c("time", "sim_num", compartment_types)) %>%
        # summarize(incidI=sum(incidI),
        #           incidD=sum(incidD),
        #           incidH=sum(incidH),
        #           incidC=sum(incidC)) %>%
        summarise(across(starts_with("incid"), sum)) %>%
        as_tibble() %>%
        mutate(USPS = "US")
    res_state <- res_state %>%
        bind_rows(res_us)
    
    res_us_long <- res_state_long %>% 
        group_by_at(c("time", "sim_num", "outcome", compartment_types)) %>%
        # summarize(incidI=sum(incidI),
        #           incidD=sum(incidD),
        #           incidH=sum(incidH),
        #           incidC=sum(incidC)) %>%
        summarise(value = sum(value)) %>%
        as_tibble() %>%
        mutate(USPS = "US")
    res_state_long <- res_state_long %>%
        bind_rows(res_us_long)
    rm(res_us_long)
    
    
    return(res_state)
}









trans_sims_wide <- function(geodata,
                            sim_directory = arguments$args, 
                            forecast_date = opt$forecast_date,
                            end_date = opt$end_date,
                            death_filter = opt$death_filter,
                            county_level = FALSE,
                            keep_compartments = TRUE,
                            testing = FALSE){
    
    res_geoid_long <- res_geoid
    res_geoid <- res_geoid %>%
        pivot_wider(names_from = outcome, values_from = value)
    
    # Subset for testing
    if(testing){
        res_geoid <- res_geoid %>% filter(sim_num %in% 1:10)
        res_geoid_long <- res_geoid_long %>% filter(sim_num %in% 1:10)
    }
    
    # res_geoid <- res_geoid %>%
    #   group_by(time, geoid, death_rate, variant, vacc, agestrat, sim_num)%>%
    #   #summarise(across(starts_with("incid"), sum)) %>%
    #   summarise(incidD=sum(incidD), incidH=sum(incidH), incidC=sum(incidC))%>%
    #   as_tibble()
    
    if(county_level){
        res_state <- res_geoid %>%
            inner_join(geodata%>%select(geoid, USPS)) %>%
            group_by_at(c("USPS", "time", "sim_num", compartment_types)) %>%
            # summarize(incidI=sum(incidI),
            #           incidD=sum(incidD),
            #           incidH=sum(incidH),
            #           incidC=sum(incidC)) %>%
            summarise(across(starts_with("incid"), sum)) %>%
            as_tibble()
    } else {
        res_state <- res_geoid %>%
            inner_join(geodata%>%select(geoid, USPS))
        
        if (keep_compartments){
            res_state_long <- res_geoid_long %>% 
                inner_join(geodata%>%select(geoid, USPS))
        }
        rm(res_geoid_long, res_geoid)
    }
    
    # ADD US TOTAL
    res_us <- res_state %>% 
        group_by_at(c("time", "sim_num", compartment_types)) %>%
        # summarize(incidI=sum(incidI),
        #           incidD=sum(incidD),
        #           incidH=sum(incidH),
        #           incidC=sum(incidC)) %>%
        summarise(across(starts_with("incid"), sum)) %>%
        as_tibble() %>%
        mutate(USPS = "US")
    res_state <- res_state %>%
        bind_rows(res_us)
    
    res_us_long <- res_state_long %>% 
        group_by_at(c("time", "sim_num", "outcome", compartment_types)) %>%
        # summarize(incidI=sum(incidI),
        #           incidD=sum(incidD),
        #           incidH=sum(incidH),
        #           incidC=sum(incidC)) %>%
        summarise(value = sum(value)) %>%
        as_tibble() %>%
        mutate(USPS = "US")
    res_state_long <- res_state_long %>%
        bind_rows(res_us_long)
    rm(res_us_long)
    
    
    return(res_state)
}




load_simulations_orig <- function(geodata,
                                  sim_directory = arguments$args, 
                                  forecast_date = opt$forecast_date,
                                  end_date = opt$end_date,
                                  death_filter = opt$death_filter,
                                  county_level = FALSE,
                                  keep_compartments = TRUE,
                                  testing = FALSE){
    
    res_geoid <- arrow::open_dataset(sprintf("%s/hosp", sim_directory), 
                                     partitioning =c("location", 
                                                     "scenario", 
                                                     "death_rate", 
                                                     "config", 
                                                     "lik_type", 
                                                     "is_final")) %>%
        select(time, geoid, starts_with("incid"), death_rate)%>%
        filter(time>=forecast_date & time<=end_date)%>%
        collect() %>%
        filter(stringr::str_detect(death_rate, death_filter))%>%
        mutate(time=as.Date(time)) %>%
        group_by(time, geoid, death_rate) %>%
        dplyr::mutate(sim_num = as.character(seq_along(geoid))) %>%
        ungroup() %>%
        pivot_longer(cols=starts_with("incid"), 
                     names_to = c("outcome",compartment_types), 
                     names_pattern = "(.*)_(.*)_(.*)_(.*)", values_to = "value") %>%
        filter(!is.na(outcome))
    
    res_geoid_long <- res_geoid
    res_geoid <- res_geoid %>%
        pivot_wider(names_from = outcome, values_from = value)
    
    # Subset for testing
    if(testing){
        res_geoid <- res_geoid %>% filter(sim_num %in% 1:10)
        res_geoid_long <- res_geoid_long %>% filter(sim_num %in% 1:10)
    }
    
    # res_geoid <- res_geoid %>%
    #   group_by(time, geoid, death_rate, variant, vacc, agestrat, sim_num)%>%
    #   #summarise(across(starts_with("incid"), sum)) %>%
    #   summarise(incidD=sum(incidD), incidH=sum(incidH), incidC=sum(incidC))%>%
    #   as_tibble()
    
    if(county_level){
        res_state <- res_geoid %>%
            inner_join(geodata%>%select(geoid, USPS)) %>%
            group_by_at(c("USPS", "time", "sim_num", compartment_types)) %>%
            # summarize(incidI=sum(incidI),
            #           incidD=sum(incidD),
            #           incidH=sum(incidH),
            #           incidC=sum(incidC)) %>%
            summarise(across(starts_with("incid"), sum)) %>%
            as_tibble()
    } else {
        res_state <- res_geoid %>%
            inner_join(geodata%>%select(geoid, USPS))
        
        if (keep_compartments){
            res_state_long <- res_geoid_long %>% 
                inner_join(geodata%>%select(geoid, USPS))
        }
        rm(res_geoid_long, res_geoid)
    }
    
    # ADD US TOTAL
    res_us <- res_state %>% 
        group_by_at(c("time", "sim_num", compartment_types)) %>%
        # summarize(incidI=sum(incidI),
        #           incidD=sum(incidD),
        #           incidH=sum(incidH),
        #           incidC=sum(incidC)) %>%
        summarise(across(starts_with("incid"), sum)) %>%
        as_tibble() %>%
        mutate(USPS = "US")
    res_state <- res_state %>%
        bind_rows(res_us)
    
    res_us_long <- res_state_long %>% 
        group_by_at(c("time", "sim_num", "outcome", compartment_types)) %>%
        # summarize(incidI=sum(incidI),
        #           incidD=sum(incidD),
        #           incidH=sum(incidH),
        #           incidC=sum(incidC)) %>%
        summarise(value = sum(value)) %>%
        as_tibble() %>%
        mutate(USPS = "US")
    res_state_long <- res_state_long %>%
        bind_rows(res_us_long)
    rm(res_us_long)
    
    
    return(res_state)
}



get_ground_truth_revised <- function(config, scenario_dir, csp_path = "../COVIDScenarioPipeline") {
    
    Sys.setenv(CONFIG_PATH = config)
    Sys.setenv(COVID_PATH  = csp_path)
    # source(file.path(csp_path, "/R/scripts/build_US_setup.R"))
    source(file.path(csp_path, "/R/scripts/build_covid_data.R"))
    
    gt_data <- readr::read_csv(config$filtering$data_path)
    
    # Add cum and us
    
    gt_data <- gt_data %>% filter(source != "US")
    
    gt_long <- gt_data %>%
        pivot_longer(cols = -c(date, source, FIPS), names_to = "target", values_to = "incid")
    gt_long <- gt_long %>%
        group_by(source, FIPS, date, target)%>%
        summarise(incid=sum(incid))%>%
        ungroup()
    gt_long_tmp <- gt_long %>%
        as_tibble() %>%
        mutate(incid = fix_NAs(incid)) %>%
        group_by(source, FIPS, target) %>%
        arrange(date) %>%
        mutate(incid=cumsum(incid))%>%
        ungroup() %>%
        mutate(target = gsub("incid", "cum", target))
    gt_long <- gt_long %>% full_join(gt_long_tmp)
    rm(gt_long_tmp)
    
    gt_long_us <- gt_long %>%
        group_by(date, target)%>%
        summarise(incid=sum(incid, na.rm = TRUE)) %>%
        mutate(source="US")
    gt_long <- gt_long %>%
        bind_rows(gt_long_us)
    rm(gt_long_us)
    
    # pivot back wide now with cum
    gt_data <- gt_long %>%
        pivot_wider(names_from = target, values_from = incid)
    
    gt_long <- gt_long %>%
        rename(time=date, USPS=source)
    
    gt_data_clean <- gt_data %>%
        rename(geoid=FIPS, time=date, USPS=source)
    
    write_csv(gt_data_clean, file.path(scenario_dir, "gt_data_clean.csv"))
    file.remove(config$filtering$data_path)
    
    print(paste0("Created new groundtruth data in \n", 
                 file.path(scenario_dir, basename(config$filtering$data_path))))
    
    return(gt_data_clean)
}



# CALIBRATION -------------------------------------------------------------


calibrate_outcome <- function(outcome_calib = "incidH",
                              weekly_outcome = TRUE,
                              n_calib_days = 14,
                              gt_data, 
                              incid_sims_formatted, 
                              incid_sims, 
                              projection_date,
                              quick_run, testing,
                              keep_variant_compartments, keep_vacc_compartments, keep_all_compartments,
                              variants_=NULL, vacc_=NULL, death_filter=opt$death_filter,
                              opt,
                              scenario_dir) {
    
    calib_dates <- sort((lubridate::as_date(projection_date)) - c(1, n_calib_days))
    outcome_calib_base <- gsub("incid|cum", "", outcome_calib)
    
    # get gt to calibrate to
    if (weekly_outcome){
        gt_calib <- get_weekly_incid(gt_data %>% dplyr::select(time, geoid, USPS, !!sym(outcome_calib)) %>% mutate(sim_num = 0),
                                     outcomes = outcome_calib_base) 
    } else {
        gt_calib <- gt_data %>% dplyr::select(time, geoid, USPS, !!sym(outcome_calib)) %>% mutate(sim_num = 0)
    }
    
    gt_calib <- gt_calib %>% 
        dplyr::select(-sim_num) %>%
        as_tibble() %>%
        mutate(time = lubridate::as_date(time)) %>%
        arrange(USPS, time)  %>% 
        filter(time >= lubridate::as_date(calib_dates[1]) & time <= lubridate::as_date(calib_dates[2])) %>%
        dplyr::mutate(time_calib = lubridate::as_date(projection_date)-1) %>%
        dplyr::mutate(time = lubridate::as_date(ifelse(time == lubridate::as_date(calib_dates[2]), 
                                                       lubridate::as_date(projection_date)-1, time)))
    
    if (full_fit){
        inc_calib <- incid_sims_formatted %>% filter(outcome %in% outcome_calib)
    }else{
        # repull data with one week earlier to calibrate to if not full run
        res_geoid_all_calib <- combine_and_format_sims(
            outcome_vars = outcome_calib,
            scenario_dir = scenario_dir,
            quick_run = quick_run,
            testing  = testing,
            outcomes_ = outcome_calib_base, 
            keep_variant_compartments = keep_variant_compartments,
            keep_vacc_compartments = keep_vacc_compartments,
            keep_all_compartments = keep_all_compartments,
            variants_ = variants_,
            vacc_ = vacc_,
            county_level = county_level, 
            forecast_date = calib_dates[1],
            end_date = calib_dates[2],
            geodata = geodata,
            death_filter = death_filter)
        
        if (weekly_outcome) {
            inc_calib <- get_weekly_incid(res_geoid_all_calib, outcomes = outcome_calib_base)
            inc_calib <- format_weekly_outcomes(inc_calib, point_est = 0.5, opt)
        } else {
            inc_calib <- get_daily_incid(res_geoid_all_calib, outcomes = outcome_calib_base)
            inc_calib <- format_daily_outcomes(inc_calib, point_est = 0.5, opt)
        }
    }
    
    inc_calibrator <- inc_calib %>% 
        filter(target_end_date >= lubridate::as_date(calib_dates[1]) & target_end_date <= lubridate::as_date(calib_dates[2])) %>%
        select(-target) %>%
        filter(quantile == 0.5) %>%
        rename(outcome_name = outcome) %>%
        left_join(gt_calib %>% select(target_end_date=time, USPS, value_gt = outcome, outcome_name)) %>%
        mutate(inc_calib = value_gt / value) %>%
        group_by(USPS, location, outcome_name) %>%
        summarize(inc_calib = median(inc_calib, na.rm=TRUE)) %>%
        as_tibble() %>% select(USPS, location, outcome_name, inc_calib)
    
    # re-calibrate outcome after projection date
    # if (smh_or_fch=="smh"){
    incid_sims_recalib <- incid_sims %>% 
        filter(time >= calib_dates[1] & outcome_name %in% outcome_calib) %>%
        left_join(inc_calibrator) %>%
        mutate(inc_calib = replace_na(inc_calib, 1)) %>%
        mutate(outcome = outcome * inc_calib) %>% 
        select(-inc_calib) %>%
        filter(!is.na(time))
    
    incid_sims_recalib <- incid_sims %>%
        filter(!(time >= calib_dates[1] & outcome_name %in% outcome_calib)) %>%
        bind_rows(incid_sims_recalib) %>%
        arrange(USPS, sim_num, outcome_name, time) %>%
        select(-location) %>% as_tibble()
    # } else {
    #     date_start_calib <- (lubridate::as_date(projection_date)-1) - 7*n_calib_days
    #     date_start_calib <- lubridate::as_date(ifelse(any(daily_inc$target_end_date==date_start_calib), date_start_calib, lubridate::as_date(projection_date)-1))
    #     
    #     daily_inc_proj <- daily_inc %>% 
    #         filter(target_end_date>=date_start_calib) %>%
    #         full_join(inc_caibrator) %>%
    #         mutate(value = value * inc_calib) %>% 
    #         select(-inc_calib) %>%
    #         filter(!is.na(target_end_date))
    #     
    #     daily_inc <- daily_inc %>% 
    #         filter(target_end_date<date_start_calib) %>%
    #         bind_rows(daily_inc_proj) %>%
    #         arrange(USPS, location, target_end_date, quantile)
    # }
    return(list(incid_sims_recalib = incid_sims_recalib, inc_calibrator = inc_calibrator))
}







# MISC --------------------------------------------------------------------

# Assign point estimate
change_point_est <- function(dat, point_estimate=0.5){
    dat <- dat %>% 
        filter(type!="point") %>%
        bind_rows(dat %>% filter(quantile==point_estimate) %>%
                      mutate(type="point", quantile=NA)) %>%
        bind_rows(dat %>% filter(type=="point") %>%
                      mutate(type="point-mean", quantile=NA)) 
    return(dat)
}




reichify_cum_ests <- function(cum_ests, cum_var="cumH", 
                              reich_locs=read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv"),
                              point_est=0.5, opt){
    
    outcome_short <- recode(cum_var, "cumI"="inf", "cumC"="case", "cumH"="hosp", "cumD"="death")
    
    cum_ests <- cum_ests %>%
        filter(quantile!="data") %>%
        filter(time>opt$forecast_date) %>%
        mutate(forecast_date=opt$forecast_date) %>%
        rename(target_end_date=time) %>%
        mutate(location=as.character(cdlTools::fips(USPS))) %>%
        mutate(location = ifelse(USPS=="US", "US", location)) %>%
        mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
        rename(value=!!sym(cum_var)) %>%
        mutate(target=paste0(sprintf("%d day ahead cum ", steps_ahead), outcome_short)) %>%
        mutate(type="quantile") %>%
        mutate(type=replace(type, quantile=="mean", "point")) %>%
        mutate(quantile=suppressWarnings(readr::parse_number(quantile)/100)) %>%
        select(forecast_date, target, target_end_date,USPS, location,type, quantile, value)
    
    if (point_est!="mean"){
        cum_ests <- change_point_est(dat = cum_ests, point_estimate = point_est)
    }
    
    cum_ests <- cum_ests %>%
        mutate(day_of_week=lubridate::wday(target_end_date, label=T)) %>%
        filter(day_of_week=="Sat") %>%
        mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7)) %>%
        mutate(target=paste0(sprintf("%d wk ahead cum ", ahead), outcome_short)) %>%
        select(-day_of_week, -ahead)
    
    return(cum_ests)
}





# INCIDENT ESTIMATES ------------------------------------------------------

gen_quantiles <- function(outcome){
    list(enframe(c(quantile(outcome, probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), na.rm=TRUE),
                   mean=mean(outcome, na.rm=TRUE)), "quantile","outcome"))
}

##Incident Outcome daily
get_daily_incid <- function(res_state, outcomes){
    daily_inc_outcome <- res_state %>%
        dplyr::select(USPS, sim_num, time, starts_with(paste0("incid", outcomes))) %>%
        mutate(time=lubridate::as_date(time)) %>%
        mutate(week=lubridate::epiweek(time)) %>%
        as_tibble() %>%
        pivot_longer(cols = starts_with("incid"), names_to = "outcome_name", values_to = "outcome") %>%
        group_by(USPS, sim_num, time, week, outcome_name) %>%
        summarize(outcome = sum(outcome, na.rm=TRUE)) %>%
        as_tibble()
    
    return(daily_inc_outcome)
}

##Incident Outcome weekly
get_weekly_incid <- function(res_state, outcomes){
    weekly_inc_outcome <- res_state %>%
        dplyr::select(USPS, sim_num, time, starts_with(paste0("incid", outcomes))) %>%
        mutate(week=lubridate::epiweek(time), year = lubridate::epiyear(time)) %>%
        mutate(tmp_time = as.numeric(time)) %>%
        as_tibble() %>%
        pivot_longer(cols = starts_with("incid"), names_to = "outcome_name", values_to = "outcome") %>%
        group_by(USPS, sim_num, week, year, outcome_name) %>%
        summarize(outcome = sum(outcome, na.rm=TRUE),
                  time = max(tmp_time, na.rm=TRUE)) %>%
        as_tibble() %>%
        mutate(time=lubridate::as_date(time)) %>%
        dplyr::select(-year)
    
    return(weekly_inc_outcome)
}


reichify_inc_ests <- function(weekly_inc_outcome, opt){
    weekly_inc_outcome <- weekly_inc_outcome %>% 
        pivot_wider(names_from = quantile, names_prefix = "quant_", values_from = outcome) %>%
        mutate(forecast_date=opt$forecast_date) %>%
        rename(target_end_date=time) %>%
        mutate(location=as.character(cdlTools::fips(USPS))) %>%
        mutate(location = ifelse(USPS=="US", "US", location)) %>%
        mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
        mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7)) %>%
        mutate(target = recode(outcome_name, "incidI"="inf", "incidC"="case", "incidH"="hosp", "incidD"="death")) %>%
        mutate(target=sprintf(paste0("%d wk ahead inc ", target), ahead)) %>%
        pivot_longer(cols=dplyr::starts_with("quant_"), names_to = "quantile", values_to = "value") %>%
        mutate(type="quantile") %>%
        mutate(quantile2=suppressWarnings(readr::parse_number(quantile)/100)) %>%
        mutate(type=replace(type, grepl("mean", quantile),"point")) %>%
        as_tibble() %>%
        select(forecast_date, outcome = outcome_name, target, target_end_date, USPS, location, type, quantile=quantile2, value)
    
    if (point_est!="mean"){
        weekly_inc_outcome <- change_point_est(dat = weekly_inc_outcome, point_estimate = point_est)
    }
}




format_daily_outcomes <- function(daily_inc_outcome, point_est=0.5, opt){
    
    daily_inc_outcome <- daily_inc_outcome %>%
        group_by(time, USPS, outcome_name) %>%
        summarize(x=list(enframe(c(quantile(outcome, probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), na.rm=TRUE),
                                   mean=mean(outcome, na.rm=TRUE)), "quantile","outcome"))) %>%
        unnest(x)
    
    if(opt$reichify) {
        
        cum_outcomes <- any(grepl("cum", daily_inc_outcome$outcome_name))
        if (cum_outcomes){
            daily_inc_outcome <- daily_inc_outcome %>% mutate(outcome_name = gsub("cum", "incid", outcome_name))
        }
        
        daily_inc_outcome <- daily_inc_outcome %>% 
            pivot_wider(names_from = quantile, names_prefix = "quant_", values_from = outcome) %>%
            mutate(forecast_date = opt$forecast_date) %>%
            rename(target_end_date = time) %>%
            mutate(location=as.character(cdlTools::fips(USPS))) %>%
            mutate(location = ifelse(USPS=="US", "US", location)) %>%
            mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
            mutate(ahead = round(as.numeric(target_end_date - forecast_date))) %>%
            mutate(target = recode(outcome_name, "incidI"="inf", "incidC"="case", "incidH"="hosp", "incidD"="death")) %>%
            mutate(target = sprintf(paste0("%d day ahead inc ", target), ahead)) %>%
            pivot_longer(cols=dplyr::starts_with("quant_"), names_to = "quantile", values_to = "value") %>%
            mutate(type="quantile") %>%
            mutate(quantile2=suppressWarnings(readr::parse_number(quantile)/100)) %>%
            mutate(type=replace(type, grepl("mean", quantile),"point")) %>%
            as_tibble() %>%
            select(forecast_date, outcome = outcome_name, target, target_end_date, USPS, location, type, quantile=quantile2, value)
        
        if (point_est!="mean"){
            daily_inc_outcome <- change_point_est(dat = daily_inc_outcome, point_estimate = point_est)
        }
        if (cum_outcomes){
            daily_inc_outcome <- daily_inc_outcome %>% 
                mutate(outcome = gsub("incid", "cum", outcome),
                       target = gsub("inc", "cum", target))
        }
    } 
    
    return(daily_inc_outcome)
}



format_weekly_outcomes <- function(weekly_inc_outcome, point_est=0.5, opt){
    
    weekly_inc_outcome <- weekly_inc_outcome %>%
        group_by(time, USPS, outcome_name) %>%
        summarize(x=list(enframe(c(quantile(outcome, probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), na.rm=TRUE),
                                   mean=mean(outcome, na.rm=TRUE)), "quantile","outcome"))) %>%
        unnest(x)
    
    if(opt$reichify) {
        
        cum_outcomes <- any(grepl("cum", weekly_inc_outcome$outcome_name))
        if (cum_outcomes){
            weekly_inc_outcome <- weekly_inc_outcome %>% mutate(outcome_name = gsub("cum", "incid", outcome_name))
        }
        
        weekly_inc_outcome <- weekly_inc_outcome %>% 
            pivot_wider(names_from = quantile, names_prefix = "quant_", values_from = outcome) %>%
            mutate(forecast_date=opt$forecast_date) %>%
            rename(target_end_date=time) %>%
            mutate(location=as.character(cdlTools::fips(USPS))) %>%
            mutate(location = ifelse(USPS=="US", "US", location)) %>%
            mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
            mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7)) %>%
            mutate(target = recode(outcome_name, "incidI"="inf", "incidC"="case", "incidH"="hosp", "incidD"="death")) %>%
            mutate(target=sprintf(paste0("%d wk ahead inc ", target), ahead)) %>%
            pivot_longer(cols=dplyr::starts_with("quant_"), names_to = "quantile", values_to = "value") %>%
            mutate(type="quantile") %>%
            mutate(quantile2=suppressWarnings(readr::parse_number(quantile)/100)) %>%
            mutate(type=replace(type, grepl("mean", quantile),"point")) %>%
            as_tibble() %>%
            select(forecast_date, outcome = outcome_name, target, target_end_date, USPS, location, type, quantile=quantile2, value)
        
        if (point_est!="mean"){
            weekly_inc_outcome <- change_point_est(dat = weekly_inc_outcome, point_estimate = point_est)
        }
        if (cum_outcomes){
            weekly_inc_outcome <- weekly_inc_outcome %>% 
                mutate(outcome = gsub("incid", "cum", outcome),
                       target = gsub("inc", "cum", target))
        }
    } 
    
    return(weekly_inc_outcome)
}






get_weekly_incid2 <- function(res_state, point_est=0.5, outcome_var="incidI", opt){
    
    outcome_short <- recode(outcome_var, "incidI"="inf", "incidC"="case", "incidH"="hosp", "incidD"="death")
    
    ##Incident Outcome weekly
    weekly_inc_outcome <- res_state %>%
        mutate(week=lubridate::epiweek(time), year = lubridate::epiyear(time)) %>%
        mutate(tmp_time = as.numeric(time)) %>%
        rename(outcome = !!sym(outcome_var)) %>%
        as_tibble() %>%
        group_by(USPS, sim_num, week, year) %>%
        summarize(outcome = sum(outcome),
                  time=max(tmp_time)) %>%
        dplyr::select(-year) %>%
        mutate(time=lubridate::as_date(time)) %>%
        group_by(time, USPS, week) %>%
        summarize(x=list(enframe(c(quantile(outcome, probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), na.rm=TRUE),
                                   mean=mean(outcome, na.rm=TRUE)), "quantile","outcome"))) %>%
        unnest(x)
    colnames(weekly_inc_outcome)[colnames(weekly_inc_outcome)=="outcome"] <- outcome_var
    
    if(opt$reichify) {
        
        weekly_inc_outcome <- weekly_inc_outcome %>%
            pivot_wider(names_from = quantile, names_prefix = "quant_", values_from = !!sym(outcome_var)) %>%
            mutate(forecast_date=opt$forecast_date) %>%
            rename(target_end_date=time) %>%
            mutate(location=as.character(cdlTools::fips(USPS))) %>%
            mutate(location = ifelse(USPS=="US", "US", location)) %>%
            mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
            mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
            mutate(target=sprintf(paste0("%d wk ahead inc ", outcome_short), ahead)) %>%
            pivot_longer(cols=dplyr::starts_with("quant_"), names_to = "quantile", values_to = "value") %>%
            mutate(type="quantile") %>%
            mutate(quantile2=suppressWarnings(readr::parse_number(quantile)/100)) %>%
            mutate(type=replace(type, grepl("mean", quantile),"point")) %>%
            select(forecast_date, target, target_end_date,USPS,location,type, quantile=quantile2, value)
        
        if (point_est!="mean"){
            weekly_inc_outcome <- change_point_est(dat = weekly_inc_outcome, point_estimate = point_est)
        }
    } 
    
    return(weekly_inc_outcome)
}






# CUMULATIVE ESTIMATES ----------------------------------------------------


cum_sum_sims <- function (sim_data, start_date, cum_dat, loc_column, cmprt_column){
    rc <- sim_data %>% filter(time > start_date) %>% 
        mutate(outcome_abbr = gsub("incid","", outcome)) %>%
        mutate(outcome = paste0("cum", outcome_abbr)) %>%
        group_by(time, sim_num, !!sym(loc_column), outcome, !!sym(cmprt_column)) %>% 
        summarise(value = sum(value, na.rm = TRUE)) %>% 
        group_by(sim_num, !!sym(loc_column), outcome, !!sym(cmprt_column)) %>% 
        arrange(time) %>%
        mutate(value = cumsum(value)) %>% as_tibble()
    
    rc <- rc %>% left_join(
        cum_dat %>% rename(value_start = value)) %>%
        mutate(value_start = replace_na(value_start, 0)) %>%
        mutate(value = value + value_start) %>%
        select(-value_start)
    
    return(as_tibble(rc))
}



get_cum_sims <- function(sim_data, obs_data, forecast_date, aggregation = "day", use_obs_data = TRUE,
                         gt_cum_vars = "cumH", weights = NA, loc_column = "USPS", cmprt_column = "agestrat"){
    
    if ((nrow(obs_data)==FALSE | is.null(obs_data))){
        use_obs_data <- FALSE
    }
    if (use_obs_data){
        if(forecast_date > (max(obs_data$time) + 1)){
            print("Calculating cumulative from start of projections.")
            use_obs_data <- FALSE
        }
    }
    if ((forecast_date + 7) < min(sim_data$time) & aggregation == "week") {
        stop("no simulation support for first forecast date")
    }
    if (use_obs_data){
        if (forecast_date > (max(obs_data$time) + 1)) {
            stop("forecast date must be within one day after the range of observed times")
        } 
        
        # if (max(obs_data$time) == forecast_date) {
        #     print(glue::glue("Accumulate cases through {forecast_date}, typically for USA Facts aggregation after noon."))
        #     start_cases <- obs_data %>% filter(time == forecast_date) %>% select(!!sym(loc_column),outcome, value)
        #     cum_sims <- cum_ests_forecast(sim_data, forecast_date, start_cases, loc_column, cmprt_column)
        if (min(obs_data$time)==forecast_date){
            print(glue::glue("Accumulate cases through {forecast_date}, typically for CSSE aggregation."))
            start_cases <- obs_data %>% filter(time == forecast_date) %>% select(!!sym(loc_column), !!(gt_cum_vars)) %>%
                pivot_longer(cols=starts_with("cum"), names_to = "outcome", values_to = "value")
            cum_sims <- cum_sum_sims(sim_data, forecast_date, 
                                     cum_dat = start_cases, loc_column, cmprt_column)
        } else {
            print(glue::glue("Accumulate cases through {forecast_date-1}, typically for CSSE aggregation."))
            start_cases <- obs_data %>% filter(time == forecast_date - 1) %>% select(!!sym(loc_column), !!(gt_cum_vars)) %>%
                pivot_longer(cols=starts_with("cum"), names_to = "outcome", values_to = "value")
            cum_sims <- cum_sum_sims(sim_data, start_date = forecast_date - 1, 
                                     cum_dat = start_cases, loc_column, cmprt_column)
        }
    } else {
        start_cases <- obs_data %>% filter(time == forecast_date) %>% select(!!sym(loc_column), !!(gt_cum_vars)) %>%
            pivot_longer(cols=starts_with("cum"), names_to = "outcome_name", values_to = "outcome") %>%
            mutate(outcome = 0)
        cum_sims <- cum_sum_sims(sim_data, start_date = forecast_date - 1, 
                                 cum_dat = start_cases, loc_column, cmprt_column)
    }
    return(cum_sims)
}



format_weekly_cum_outcomes <- function(weekly_cum_sims){
    rc <- weekly_cum_sims %>% group_by(time, !!sym(loc_column)) %>% 
        summarize(x = list(enframe(c(quantile(cum_cases_corr, 
                                              probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 
                                                        0.975, 0.99)), mean = mean(cum_cases_corr)), 
                                   "quantile", "cumC"))) %>% unnest(x)
    rc <- dplyr::bind_rows(rc, obs_data %>% select(time, !!sym(loc_column), 
                                                   cumC) %>% mutate(quantile = "data"))
    rc <- rc %>% mutate(steps_ahead = as.numeric(time - forecast_date))
    return(rc)
}




create_cum_ests_forecast <- function(sim_data, obs_data, forecast_date, aggregation = "day", 
                                     quants = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99), 
                                     weights = NA, loc_column = "USPS", cmprt_column = "agestrat"){
    use_obs_data <- TRUE
    if (forecast_date > max(obs_data$time) + 1) {
        stop("forecast date must be within one day after the range of observed times")
    } else if (forecast_date > max(obs_data$time) + 1) {
        print("Calculating cumulative from start of projections.")
        use_obs_data <- FALSE
    }
    if ((forecast_date + 1) < min(sim_data$time)) {
        stop("no simulation support for first forecast date")
    }
    if (use_obs_data){
        if (max(obs_data$time) == forecast_date) {
            print(glue::glue("Accumulate cases through {forecast_date}, typically for USA Facts aggregation after noon."))
            start_cases <- obs_data %>% filter(time == forecast_date) %>% select(!!sym(loc_column),outcome, value)
            forecast_sims <- cum_ests_forecast(sim_data, forecast_date, start_cases, loc_column, cmprt_column)
        } else if (min(obs_data$time)==forecast_date){
            print(glue::glue("Accumulate cases through {forecast_date}, typically for CSSE aggregation."))
            start_cases <- obs_data %>% filter(time == forecast_date) %>% select(!!sym(loc_column), outcome, value)
            forecast_sims <- cum_ests_forecast(sim_data, forecast_date, start_cases, loc_column, cmprt_column)
        } else {
            print(glue::glue("Accumulate cases through {forecast_date-1}, typically for CSSE aggregation."))
            start_cases <- obs_data %>% filter(time == forecast_date - 1) %>% select(!!sym(loc_column), outcome, value)
            
            if(nrow(start_cases)==0){
                start_cases <- obs_data %>% select(!!sym(loc_column), outcome) %>% 
                    distinct() %>%
                    mutate(value = 0)
            }
            forecast_sims <- cum_ests_forecast(sim_data, forecast_date - 1, start_cases, loc_column, cmprt_column)
        }
    }
    if (aggregation == "day") {
    } else {
        stop("unknown aggregatoin period")
    }
    
    rc <- forecast_sims %>% group_by(time, !!sym(loc_column)) %>% 
        summarize(x = list(enframe(c(quantile(cum_cases_corr, 
                                              probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 
                                                        0.975, 0.99)), mean = mean(cum_cases_corr)), 
                                   "quantile", "cumC"))) %>% unnest(x)
    rc <- dplyr::bind_rows(rc, obs_data %>% select(time, !!sym(loc_column), 
                                                   cumC) %>% mutate(quantile = "data"))
    rc <- rc %>% mutate(steps_ahead = as.numeric(time - forecast_date))
    return(rc)
}



# PROCESS AND COMBINE FINAL -----------------------------------------------

combine_and_format_scenarios <- function(
        config,    
        round_num,
        round_directory,
        validation_date,
        scenarios,
        projection_date,
        forecast_date,
        scenario_ids,
        full_fit,
        forecast_date_name = "model_projection_date") {
    
    # COMBINE THEM ALL AND SAVE
    files_ <- list.files(round_directory, pattern = "JHU_IDD-CovidSP", full.names = TRUE, include.dirs = TRUE)
    files_ <- as.character(sapply(paste0(projection_date, "-JHU_IDD-CovidSP-", scenarios, ifelse(full_fit,"_FULL",""), ".parquet"), grep, files_, value=TRUE))
    
    data_comb  <- lapply(files_, arrow::read_parquet) %>%
        data.table::rbindlist() %>% 
        as_tibble() %>%
        filter(type!="point-mean")
    colnames(data_comb)[colnames(data_comb) == "forecast_date"] <- forecast_date_name
    
    # Save it
    readr::write_csv(data_comb, file.path(round_directory, paste0(projection_date, "-JHU_IDD-CovidSP", ifelse(full_fit,"_FULL",""), ".csv")))
    arrow::write_parquet(data_comb, file.path(round_directory, paste0(projection_date, "-JHU_IDD-CovidSP", ifelse(full_fit,"_FULL",""), ".parquet")))
    
    print(paste0("Final data saved in:  [  ", file.path(round_directory, paste0(projection_date, "-JHU_IDD-CovidSP", ifelse(full_fit,"_FULL",""), ".csv")), "  ]"))
    
    return(data_comb)
}




# RUN PROCESSING - All ----------------------------------------------------

process_sims <- function(
        scenario_num,
        scenarios_all,
        scenario_names,
        scenario_ids,
        proj_id,
        projection_date,
        forecast_date,
        end_date,
        smh_or_fch,
        round_num,
        subname_all,
        config_subname,
        round_directory,
        full_fit = FALSE,
        testing = FALSE,
        quick_run = FALSE,
        outcomes_ = c("I","C","H","D"),
        outcomes_time_ = c("weekly","weekly","weekly","weekly"),
        outcomes_cum_ = c(TRUE, TRUE, TRUE, TRUE),
        outcomes_cumfromgt = c(FALSE, FALSE, TRUE, FALSE),
        outcomes_calibrate = c(FALSE, FALSE, TRUE, FALSE),
        n_calib_days = 0,
        likelihood_prune = FALSE,
        keep_variant_compartments = keep_variant_compartments,
        keep_vacc_compartments = keep_vacc_compartments,
        keep_all_compartments = keep_all_compartments,
        variants_ = variants_,
        vacc_ = vacc_,
        geodata_file = "data/geodata_2019_statelevel.csv",
        death_filter = "med",
        plot_samp,
        gt_data,
        scenario_dir,
        summarize_peaks = FALSE,
        save_reps = FALSE) {
    
    
    
    # SETUP -------------------------------------------------------------------
    # print(scenarios_all)
    print(scenarios_all[scenario_num])
    
    opt <- list()
    errors <- list()
    scenario <- scenarios_all[scenario_num]  #"baseline_lowVac"
    scenario_name <- scenario_names[scenario_num]
    scenario_id <- scenario_ids[scenario_num]
    opt$scenario <- scenario
    opt$scenario_name <- scenario_name
    opt$projection_date <- projection_date
    opt$forecast_date <- opt$projection_date # same as projection date unless FULL fit, which gets fixed below
    opt$end_date <- end_date
    
    config_name <- paste0(paste(na.omit(c("config", toupper(smh_or_fch), paste0("R", round_num), scenario, subname_all[1], config_subname)), collapse="_"), ".yml")
    config <- covidcommon::load_config(config_name)
    
    if (smh_or_fch=="fch") {
        scenario <- proj_id
        opt$scenario <- proj_id
    }
    
    #......................................................
    
    print( opt$scenario )
    
    opt$args <- scenario_dir <- paste0(round_directory, "/", opt$scenario, "/")
    out_sub_dir <- NA
    
    if (testing)    out_sub_dir <- "testing"
    if (quick_run)  out_sub_dir <- "quick"
    if (full_fit)   opt$forecast_date <- forecast_date
    opt$projection_date <- lubridate::as_date(opt$projection_date)
    opt$forecast_date <- lubridate::as_date(opt$forecast_date)
    
    reich_locs <- read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
    
    
    if (full_fit){
        if(!(exists('forecast_date') & !is.na(forecast_date) & !is.null(forecast_date))){
            opt$forecast_date <- "2020-01-01"
        }else{
            opt$forecast_date <- forecast_date
        }
    } 
    
    opt$projection_date <- lubridate::as_date(opt$projection_date)
    opt$forecast_date <- lubridate::as_date(opt$forecast_date)
    
    variants_ <- opt$variants
    
    #......................................................
    
    opt$geodata <- "data/geodata_2019_statelevel.csv"   #geodata_territories_2019_statelevel.csv"
    opt$death_filter <- "med"
    opt$outfile <- paste0(opt$projection_date, "-JHU_IDD-CovidSP-", opt$scenario, ifelse(full_fit, "_FULL", ""),ifelse(likelihood_prune, "_LLprune",""), ".csv")
    opt$vaccfile <- paste0(opt$projection_date, "-JHU_IDD-CovidSP-", opt$scenario, "_vaccdata", ifelse(full_fit, "_FULL", ""), ".csv")
    opt$vaccsumm <- paste0(opt$projection_date, "-JHU_IDD-CovidSP-", opt$scenario, "_vaccsummary", ifelse(full_fit, "_FULL", ""), ".csv")
    opt$indiv_sims <- paste0(opt$projection_date, "-JHU_IDD-CovidSP-", opt$scenario, ifelse(full_fit, "_FULL", ""), ".parquet")
    
    opt$outdir <- ifelse(!is.na(out_sub_dir), paste0(round_directory, out_sub_dir), file.path(round_directory))
    opt$reichify <- TRUE
    dir.create(opt$outdir, recursive = TRUE, showWarnings = FALSE)
    print(opt$outdir)
    
    projections_file_path <- file.path(opt$outdir, opt$outfile)
    projections_file_path
    
    opt$forecast_date <- as.Date(opt$forecast_date)
    opt$end_date <- as.Date(opt$end_date)
    
    # Functions ---------------------------------------------------------------
    
    # Load Data ---------------------------------------------------------------
    
    # ~ Geodata 
    geodata <- suppressMessages(readr::read_csv(opt$geodata, col_types = readr::cols(geoid=readr::col_character())))
    
    # ~ Ground truth
    if (!exists("gt_data")){
        gt_data <- readr::read_csv(file.path(round_directory, "gt_data_clean.csv"))
    }
    
    
    # Projections -----------------------------------------------------------
    
    res_state <- combine_and_format_sims(outcome_vars = paste0("incid", outcomes_),
                                         scenario_dir = opt$args,
                                         quick_run = quick_run,
                                         testing  = testing,
                                         outcomes_ = outcomes_, 
                                         keep_variant_compartments = keep_variant_compartments,
                                         keep_vacc_compartments = keep_vacc_compartments,
                                         keep_all_compartments = keep_all_compartments,
                                         variants_ = variants_,
                                         vacc_ = vacc_,
                                         county_level=FALSE, 
                                         forecast_date = opt$forecast_date,
                                         end_date = opt$end_date,
                                         geodata = geodata, 
                                         death_filter = opt$death_filter)
    
    if(exists("res_state")){
        print(paste("Successfully combined sims for:", scenario))
    } else {
        errors <- append(errors, "res_state not created.")
        stop("res_state not created.")
    }
    
    
    # ~ Individual Sims & Likelihoods -----------------------------------------
    
    if (likelihood_prune) {
        
        # add sim_id to sims
        sim_ids <- tibble(filename = list.files(sprintf("%s/hosp",opt$args), recursive = TRUE)) 
        sim_ids <- sim_ids %>%
            separate(filename, into=c(letters), sep= "[/]", remove=FALSE) %>%
            mutate(sim_id = as.integer(substr(g, 1, 9))) %>%
            select(sim_id) %>%
            mutate(sim_num = seq_along(sim_id))
        
        res_state <- res_state %>% 
            mutate(sim_num=as.integer(sim_num)) %>%
            left_join(sim_ids)
        
        # Pull Likelihood for pruning runs
        res_llik <- arrow::open_dataset(sprintf("%s/llik",opt$args), 
                                        partitioning =c("location",
                                                        "scenario",
                                                        "death_rate",
                                                        "config",
                                                        "lik_type",
                                                        "is_final")) %>%
            select(filename, geoid, scenario, death_rate, ll)%>%
            collect() %>%
            distinct() %>%
            filter(stringr::str_detect(death_rate, opt$death_filter))%>%
            separate(filename, into=c(letters[1:9]), sep= "[/]", remove=FALSE) %>%
            mutate(sim_id = as.integer(substr(i, 1, 9))) %>%
            as_tibble()
        
        
        res_llik %>% filter(geoid=='06000') %>%
            ggplot(aes(x=sim_id, y=ll)) +
            geom_point()
        
        res_llik %>% filter(geoid=='06000') %>%
            ggplot(aes(y=ll)) +
            geom_histogram()
        
        res_llik %>% filter(geoid=='06000') %>%
            mutate(lik = log(-ll)) %>%
            ggplot(aes(y=lik)) +
            geom_histogram()
        
        res_lik_ests <- res_llik %>%
            mutate(lik = log(-ll)) %>%
            group_by(geoid) %>%
            mutate(mean_ll = mean(ll),
                   median_ll = median(ll),
                   low_ll = quantile(ll, 0.025),
                   high_ll = quantile(ll, 0.975)) %>%
            mutate(mean_lik = mean(lik),
                   median_lik = median(lik),
                   low_lik = quantile(lik, 0.025),
                   high_lik = quantile(lik, 0.975)) %>%
            mutate(below025_ll = ll<low_ll,
                   below025_lik = lik>high_lik)
        
        # to exclude the same number from each state, we will use quantile approximates
        n_excl <- ceiling(nrow(sim_ids)*(1-likelihood_prune_percentkeep))
        
        res_lik_ests <- res_lik_ests %>%
            group_by(geoid, scenario, death_rate) %>%
            arrange(ll) %>%
            mutate(rank = seq_along(geoid),
                   excl_rank = rank<=n_excl) %>%
            ungroup()
        
        # res_lik_ests %>% 
        #   group_by(geoid) %>%
        #   summarise(n_excl_ll = sum(below025_ll),
        #             n_excl_lik = sum(below025_lik)) %>% View
        # res_lik_ests %>% 
        #   group_by(sim_id) %>%
        #   summarise(n_excl_ll = sum(below025_ll),
        #             n_excl_lik = sum(below025_lik)) %>% View
        
        res_lik_excl <- res_lik_ests %>% 
            select(geoid, sim_id, exclude=excl_rank, ll, lik) 
        
        res_state <- res_state %>% left_join(res_lik_excl) #%>% select(-death_rate)
        
        # Save it
        # arrow::write_parquet(res_state_indivs, file.path(opt$outdir, opt$indiv_sims))
        # If pruning by LLik
        res_state <- res_state %>% 
            filter(!exclude) %>%
            select(-sim_id, -exclude) %>%
            group_by(time, geoid, USPS, death_rate) %>%
            dplyr::mutate(sim_num = as.character(seq_along(geoid))) %>%
            ungroup()
        
    }
    
    
    
    # ~ Plot some sims ------------------------------
    
    plot_samp = ifelse(smh_or_fch=="smh", plot_samp, FALSE)
    if (plot_samp) {
        
        gt_data_wUS <- gt_data %>%
            bind_rows(gt_data %>%
                          group_by())
        
        plot_sims <- function(state_ = "MD", res_state_long=res_state_long, gt_data = gt_data_wUS, samp_=NULL){
            
            if (is.null(samp_)){
                samp_ <- sample(unique(res_state_long$sim_num), 10, replace=FALSE)
            }
            
            print(
                cowplot::plot_grid(
                    ggplot() +
                        geom_line(data=res_state_long %>% filter(sim_num %in% samp_) %>%
                                      filter(USPS == state_) %>%
                                      filter(outcome == "incidD"),
                                  aes(x=time, y=value, color=sim_num)) + 
                        # geom_point(data = gt_data %>% filter(source == state_) %>% rename("value"=incidD, "USPS"=source, "time"=Update),
                        #            aes(x=time, y=value), alpha=.25, pch=20) +
                        ggtitle(paste0(state_, " - incidD")),
                    ggplot() +
                        geom_line(data=res_state_long %>% filter(sim_num %in% samp_) %>%
                                      filter(USPS == state_) %>%
                                      filter(outcome == "incidC"),
                                  aes(x=time, y=value, color=sim_num)) + 
                        # geom_point(data = gt_data %>% filter(source == state_) %>% rename("value"=incidC, "USPS"=source, "time"=Update),
                        #            aes(x=time, y=value), alpha=.25, pch=20) +
                        ggtitle(paste0(state_, " - incidC")),
                    res_state_long %>% filter(sim_num %in% samp_) %>%
                        filter(USPS == state_) %>%
                        filter(outcome == "incidI") %>%
                        ggplot(aes(x=time, y=value, color=sim_num)) +
                        geom_line() + ggtitle(paste0(state_, " - incidI")), 
                    align="hv", axis = "lr", nrow=3))
            
        }
        
        states_ <- sort(unique(res_state_long$USPS))
        pdf(file= paste0(opt$outdir, paste0("SampleSims_",opt$scenario,".pdf")))
        samp_ <- sample(unique(res_state_long$sim_num), 10, replace=FALSE)
        sapply(states_, plot_sims, res_state_long=res_state_long, gt_data = gt_data, samp_=samp_)
        dev.off()
        
        # samp_ <- sample(unique(res_state_long$sim_num), 10, replace=FALSE)
        # plot_sims(state_ = "US", res_state_long=res_state_long, gt_data = gt_data, samp_)
        plot_sims(state_ = "CA", res_state_long=res_state_long, gt_data = gt_data, samp_)
        # plot_sims(state_ = "MD", res_state_long=res_state_long, gt_data = gt_data, samp_)
    }
    
    
    
    # GET SIM OUTCOMES -------------------------------------------------------------------
    
    use_obs_data_forcum <- TRUE
    gt_data_2 <- gt_data
    # colnames(gt_data_2) <- gsub("cumI", "cumC", colnames(gt_data_2))
    gt_data_2 <- gt_data_2 %>% mutate(cumH = 0) # incidH is only cumulative from start of simulation
    
    
    # ~ Weekly Outcomes -----------------------------------------------------------
    
    if (any(outcomes_time_=="weekly")) {
        
        # Incident
        weekly_incid_sims <- get_weekly_incid(res_state, outcomes = outcomes_[outcomes_time_=="weekly"])
        weekly_incid_sims_formatted <- format_weekly_outcomes(weekly_incid_sims, point_est=0.5, opt)
        
        if(exists("weekly_incid_sims_formatted")){
            print(paste("Successfully created weekly incidence for:", scenario))
        } else {
            errors <- append(errors, "weekly incidence not created.")
            stop("res_state not created.")
        }
        
        
        # Calibrate
        outcomes_calib_weekly <- outcomes_[outcomes_calibrate & outcomes_time_=="weekly"]
        if (length(outcomes_calib_weekly)>0 & n_calib_days>0){
            weekly_incid_sims_calibrations <- calibrate_outcome(outcome_calib = paste0("incid", outcomes_calib_weekly),
                                                                weekly_outcome = TRUE,
                                                                n_calib_days = n_calib_days,
                                                                gt_data, 
                                                                incid_sims_formatted = weekly_incid_sims_formatted,
                                                                incid_sims = weekly_incid_sims, 
                                                                projection_date,
                                                                quick_run, testing,
                                                                keep_variant_compartments, keep_vacc_compartments, keep_all_compartments,
                                                                variants_=NULL, vacc_=NULL, death_filter=opt$death_filter,
                                                                opt,
                                                                scenario_dir)
            weekly_incid_sims <- weekly_incid_sims_calibrations$incid_sims_recalib
            
            weekly_incid_sims_recalib_formatted <- format_weekly_outcomes(
                weekly_inc_outcome = weekly_incid_sims %>% filter(outcome_name %in% paste0("incid", outcomes_calib_weekly)), 
                point_est=0.5, opt)
            weekly_incid_sims_formatted <- weekly_incid_sims_formatted %>% 
                filter(!(outcome %in% outcomes_calib_weekly)) %>% 
                bind_rows(weekly_incid_sims_recalib_formatted)
            rm(weekly_incid_sims_calibrations)
        }
        
        
        # Cumulative
        weekly_cum_outcomes_ <- outcomes_[outcomes_cum_ & outcomes_time_=="weekly"]
        if (length(weekly_cum_outcomes_)>0) {
            weekly_cum_sims <- get_cum_sims(sim_data = weekly_incid_sims %>%
                                                mutate(agestrat="age0to130") %>%
                                                rename(outcome = outcome_name, value = outcome) %>%
                                                filter(outcome %in% paste0("incid", weekly_cum_outcomes_)),
                                            obs_data = gt_data_2, 
                                            gt_cum_vars = paste0("cum", outcomes_[outcomes_cumfromgt]), # variables to get cum from GT
                                            forecast_date = lubridate::as_date(opt$forecast_date),
                                            aggregation="week",
                                            loc_column = "USPS", 
                                            use_obs_data = use_obs_data_forcum)
            
            weekly_cum_sims_formatted <- format_weekly_outcomes(
                weekly_cum_sims %>% rename(outcome_name = outcome, outcome = value), 
                point_est = 0.5, 
                opt = opt)
            
            if(exists("weekly_cum_sims_formatted")){
                print(paste("Successfully created weekly cumulative for:", scenario))
            } else {
                errors <- append(errors, "weekly cumulative not created.")
                stop("res_state not created.")
            }
        }
    }
    
    
    # ~ Daily Outcomes -----------------------------------------------------------
    
    daily_cum_outcomes_ <- outcomes_[outcomes_cum_ & outcomes_time_=="daily"]
    if (length(daily_cum_outcomes_)>0){
        
        # Incident
        daily_incid_sims <- get_daily_incid(res_state, outcomes = outcomes_[outcomes_time_=="daily"])
        daily_incid_sims_formatted <- format_daily_outcomes(daily_incid_sims, point_est=0.5, opt)
        
        if(exists("daily_incid_sims_formatted")){
            print(paste("Successfully created daily incidence for:", scenario))
        } else {
            errors <- append(errors, "daily incidence not created.")
            stop("res_state not created.")
        }
        
        # Calibrate
        outcomes_calib_daily <- outcomes_[outcomes_calibrate & outcomes_time_=="daily"]
        if (length(outcomes_calib_daily)>0 & n_calib_days>0){
            daily_incid_sims_calibrations <- calibrate_outcome(outcome_calib = paste0("incid", outcomes_calib_daily),
                                                               weekly_outcome = FALSE,
                                                               n_calib_days = n_calib_days,
                                                               gt_data, 
                                                               incid_sims_formatted = daily_incid_sims_formatted,
                                                               incid_sims = daily_incid_sims, 
                                                               projection_date,
                                                               quick_run, testing,
                                                               keep_variant_compartments, keep_vacc_compartments, keep_all_compartments,
                                                               variants_=NULL, vacc_=NULL, death_filter=opt$death_filter,
                                                               opt,
                                                               scenario_dir)
            daily_incid_sims <- daily_incid_sims_calibrations$incid_sims_recalib
            
            daily_incid_sims_recalib_formatted <- format_daily_outcomes(
                daily_inc_outcome = daily_incid_sims %>% filter(outcome_name %in% paste0("incid", outcomes_calib_daily)), 
                point_est=0.5, opt)
            daily_incid_sims_formatted <- daily_incid_sims_formatted %>% 
                filter(!(outcome %in% outcomes_calib_daily)) %>% 
                bind_rows(daily_incid_sims_recalib_formatted)
            rm(daily_incid_sims_calibrations)
        }
        
        # Cumulative
        if (any(outcomes_cumfromgt & outcomes_time_=="daily")){
            daily_cum_sims <- get_cum_sims(sim_data = daily_incid_sims  %>%
                                               mutate(agestrat="age0to130") %>%
                                               rename(outcome = outcome_name, value = outcome),
                                           obs_data = gt_data_2, 
                                           gt_cum_vars = paste0("cum", outcomes_[outcomes_cumfromgt]), # variables to get cum from GT
                                           forecast_date = lubridate::as_date(opt$forecast_date),
                                           aggregation="day",
                                           loc_column = "USPS", 
                                           use_obs_data = use_obs_data_forcum)
            
            daily_cum_sims_formatted <- format_daily_outcomes(
                daily_cum_sims %>% rename(outcome_name = outcome, outcome = value), 
                point_est=0.5, 
                opt = opt)
            
            if(exists("daily_cum_sims_formatted")){
                print(paste("Successfully created daily cumulative for:", scenario))
            } else {
                errors <- append(errors, "daily cumulative not created.")
                stop("res_state not created.")
            }
        }
    }
    
    
    
    # ~ Combine Daily, Weekly, Cum ----------------------------------------------
    
    all_sims_formatted <- mget(objects(pattern = "_sims_formatted$")) %>%
        data.table::rbindlist() %>%
        as_tibble()
    
    
    
    
    
    # SAVE REPLICATES -----------------------------------------------
    
    if (save_reps) {
        
        weekly_reps <- weekly_incid_sims %>%
            mutate(time = lubridate::as_date(time)) %>%
            filter(time >= lubridate::as_date(projection_date) & time <= lubridate::as_date(proj_end_date)) %>%
            filter(sim_num %in% sample(unique(weekly_incid_sims$sim_num), ifelse(quick_run, 20, 100), replace = FALSE)) %>%
            pivot_wider(names_from = sim_num, values_from = outcome, names_prefix = "sim_") %>%
            mutate(age_group = "0-130",
                   scenario_id = scenario_id, scenario_name=scenario_name) %>%
            mutate(model_projection_date=opt$forecast_date) %>%
            rename(target_end_date=time) %>%
            mutate(location=as.character(cdlTools::fips(USPS))) %>%
            mutate(location = ifelse(USPS=="US", "US", location)) %>%
            mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
            mutate(ahead=round(as.numeric(target_end_date - model_projection_date)/7)) %>%
            mutate(target = recode(outcome_name, "incidI"="inf", "incidC"="case", "incidH"="hosp", "incidD"="death")) %>%
            mutate(target=sprintf(paste0("%d wk ahead inc ", target), ahead)) %>%
            pivot_longer(cols=dplyr::starts_with("sim_"), names_to = "sample", values_to = "value") %>%
            mutate(sample = gsub("sim_", "", sample)) %>%
            as_tibble() %>%
            mutate(age_group = "0-130",
                   scenario_id = scenario_id, scenario_name=scenario_name, model_projection_date=projection_date) %>%
            select(scenario_id, scenario_name, model_projection_date, target,
                   target_end_date, sample, location=USPS, value, age_group)
        
        replicate_file <- paste0(opt$projection_date, "-JHU_IDD-CovidSP-", opt$scenario_name, "_100reps.parquet")
        arrow::write_parquet(weekly_reps, file.path(opt$outdir, replicate_file))
        
        if(exists("weekly_reps")) {
            print(paste("Successfully created 'weekly_reps' for:", scenario))
        } else {
            errors <- append(errors, "'weekly_reps' not created.")
            stop("'weekly_reps' not created.")
        }
    }
    
    
    
    
    
    # PEAK SUMMARY -------------------------------------------------------------
    # currently only incidH
    
    if (summarize_peaks) {
        peak_timing <- weekly_incid_sims %>%
            filter(outcome_name=="incidH") %>%
            rename(incidH = outcome) %>%
            group_by(USPS, sim_num) %>%
            mutate(sim_peak_size = max(incidH, na.rm=TRUE)) %>%
            mutate(is_peak = as.integer(incidH==sim_peak_size)) %>%
            ungroup() %>%
            group_by(USPS, time) %>%
            summarise(prob_peak = mean(is_peak, na.rm=TRUE)) %>%
            as_tibble() %>%
            group_by(USPS) %>%
            arrange(time) %>%
            mutate(cum_peak_prob = cumsum(prob_peak)) %>%
            ungroup()        
        
        peak_timing <- peak_timing %>%
            mutate(time = lubridate::as_date(time)) %>%
            filter(time >= lubridate::as_date(projection_date) & time <= lubridate::as_date(proj_end_date)) %>%
            mutate(age_group = "0-130",
                   quantile = NA, type = "point",
                   outcome_name = "incidH",
                   scenario_id = scenario_id, scenario_name=scenario_name) %>%
            mutate(model_projection_date=opt$forecast_date) %>%
            rename(target_end_date=time) %>%
            mutate(location=as.character(cdlTools::fips(USPS))) %>%
            mutate(location = ifelse(USPS=="US", "US", location)) %>%
            mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
            mutate(ahead=round(as.numeric(target_end_date - model_projection_date)/7)) %>%
            mutate(target = recode(outcome_name, "incidI"="inf", "incidC"="case", "incidH"="hosp", "incidD"="death")) %>%
            mutate(target=sprintf(paste0("%d wk ahead peak time ", target), ahead)) %>%
            as_tibble() %>%
            mutate(age_group = "0-130",
                   model_projection_date=projection_date,
                   forecast_date = forecast_date) %>%
            select(model_projection_date, target,
                   target_end_date, quantile, type,
                   location = USPS, value=cum_peak_prob, age_group)
        
        # PEAK SIZE
        
        peak_size <- weekly_incid_sims %>%
            filter(outcome_name=="incidH") %>%
            group_by(USPS, sim_num, outcome_name) %>%
            summarise(peak_size = max(outcome, na.rm=TRUE)) %>%
            as_tibble() %>%
            mutate(age_group = "0-130") %>%
            rename(outcome = peak_size) %>%
            group_by(USPS, outcome_name, age_group) %>%
            summarize(x=list(enframe(c(quantile(outcome, probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), na.rm=TRUE),
                                       mean=mean(outcome, na.rm=TRUE)), "quantile","outcome"))) %>%
            unnest(x) %>%
            pivot_wider(names_from = quantile, names_prefix = "quant_", values_from = outcome) %>%
            mutate(forecast_date=opt$forecast_date) %>%
            mutate(location=as.character(cdlTools::fips(USPS))) %>%
            mutate(location = ifelse(USPS=="US", "US", location)) %>%
            mutate(location=stringr::str_pad(location, width=2, side="left", pad="0")) %>%
            mutate(target = recode(outcome_name, "incidI"="inf", "incidC"="case", "incidH"="hosp", "incidD"="death")) %>%
            mutate(target = paste0("peak size ", target)) %>%
            pivot_longer(cols=dplyr::starts_with("quant_"), names_to = "quantile", values_to = "value") %>%
            mutate(type="quantile") %>%
            mutate(quantile2=suppressWarnings(readr::parse_number(quantile)/100)) %>%
            mutate(type=replace(type, grepl("mean", quantile),"point")) %>%
            as_tibble() %>%
            mutate(target_end_date=NA,
                   forecast_date = forecast_date,
                   model_projection_date = projection_date) %>%
            select(model_projection_date, target,
                   target_end_date, quantile = quantile2, type, location=USPS, value, age_group)
        
        if (point_est!="mean"){
            peak_size <- change_point_est(dat = peak_size, point_estimate = point_est)
        }
        
        peaks_ <- peak_timing %>%
            full_join(peak_size) %>%
            rename(USPS = location) %>%
            left_join(reich_locs %>% select(location, USPS = abbreviation)) %>%
            mutate(age_group = "0-130") %>%
            filter(location %in% reich_locs$location) %>%
            select(-USPS) %>%
            as_tibble() %>%
            mutate(forecast_date = forecast_date)
    }
    
    
    
    
    # PUT TOGETHER AND SAVE ---------------------------------------------------
    
    full_forecast <- all_sims_formatted %>% 
        as_tibble() %>%
        filter(target_end_date<=opt$end_date) %>%
        mutate(age_group = "0-130") %>%
        filter(location %in% reich_locs$location) %>%
        select(-USPS, -outcome) 
    
    if (!full_fit) {
        full_forecast <- full_forecast %>%
            filter(target_end_date >= lubridate::as_date(forecast_date) | (target == "peak size hosp"))
    }
    
    if (summarize_peaks){
        full_forecast <- full_forecast %>% full_join(peaks_)
    }
    
    full_forecast <- full_forecast %>%
        mutate(scenario_id = scenario_id, scenario_name = scenario_name, model_projection_date = projection_date) %>%
        select(scenario_id, scenario_name, model_projection_date, target,
               target_end_date, quantile, type, location, value, age_group) 
    
    
    # ---- Save it all
    
    dir.create(opt$outdir, recursive = TRUE, showWarnings = FALSE)
    print(file.path(opt$outdir, opt$outfile))
    opt$outfile <- gsub(".csv", ".parquet", opt$outfile)
    arrow::write_parquet(full_forecast, file.path(opt$outdir, opt$outfile))
    
    paste0("Outputs saved to : ", file.path(opt$outdir, opt$outfile))
    
    
    if (exists("full_forecast")) {
        print(paste("Successfully created 'full_forecast' for:", scenario))
    } else {
        errors <- append(errors, "'full_forecast' not created.")
        stop("'full_forecast' not created.")
    }
    
    return(errors)
}
