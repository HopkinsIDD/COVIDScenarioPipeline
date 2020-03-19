##'Function to load multiple simulations into a combine data_frame
##'
##'
##'@param scenario_dir the subdirectory containing this scenario
##'@param keep_compartments the compartmetns to keep for this run.
##'@param time_filter_low the low end of the time filter
##'@param time_filter_high the high end of the time filter
##'
##'
##'@return a long thin data frame with all of the simulations comined together
##'
load_scenario_sims <- function(scenario_dir,
                               keep_compartments=NULL,
                               time_filter_low = -Inf,
                               time_filter_high = Inf) {
  
  require(data.table)
  
  files <- dir(sprintf("model_output/%s", scenario_dir),full.names = TRUE)
  
  rc <- list()
  
  
  for (i in 1:length(files)) {
    file <- files[i]
    #print(i)
    if (is.null(keep_compartments)) {
      #tmp <- data.table::fread(file) %>% as.data.frame()
      suppressMessages(tmp <- read_csv(file))
    } else {
      suppressMessages(
        tmp <-  read_csv(file) %>%
          filter(comp%in%keep_compartments)
      )
    }
    
    #colnames(tmp) <- tmp[1,]
    tmp <- #tmp[-1,] %>%
      tmp %>%
      filter(time <= time_filter_high & time >= time_filter_low) %>%
      pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>%
      mutate(sim_num = i)
    
    rc[[i]] <- tmp
  }
  
  rc<- rbindlist(rc)
  
  return(rc)
}


##'Function to load multiple simulations into a combine data_frame
##'
##'
##'@param scenario_dir the subdirectory containing the hospitalization output
##'@param p_death "low", "med", "high" prefix to file names
##'
##'@return a long thin data frame with all of the simulations comined together for one model + p_death
##'
load_hosp_sims <- function(scenario_dir, pdeath = "low") {
  
  require(data.table)
  
  files <- dir(sprintf("hospitalization/model_output/%s", scenario_dir),full.names = TRUE)
  files <- files[grepl(pdeath,files)]
  
  rc <- list()
  
  
  for (i in 1:length(files)) {
    file <- files[i]
    suppressMessages(tmp <- read_csv(file))

    rc[[i]] <- tmp
  }
  
  rc<- rbindlist(rc)
  
  return(rc)
}

##'Function to load multiple simulations into a combine data_frame in parallele
##'
##'@param scenario_dir the subdirectory containing this scenario
##'@param keep_compartments the compartmetns to keep for this run.
##'@param time_filter_low the low end of the time filter
##'@param time_filter_high the high end of the time filter
##'@param cores number of cores
##'
##'@return a long thin data frame with all of the simulations comined together
##'
load_scenario_sims_par <- function(scenario_dir,
                               keep_compartments=NULL,
                               time_filter_low = -Inf,
                               time_filter_high = Inf,
                               cores = 10){
  require(data.table)
  files <- dir(sprintf("model_output/%s", scenario_dir),full.names = TRUE)
  rc <- list()
  cl <- makeCluster( cores )
  rc = foreach (i = 1:length(files)) %dopar% {
    file <- files[i]
    #print(i)
    if (is.null(keep_compartments)) {
      #tmp <- data.table::fread(file) %>% as.data.frame()
      suppressMessages(tmp <- read_csv(file))
    } else {
      suppressMessages(
        tmp <-  read_csv(file) %>%
          filter(comp%in%keep_compartments)
      )
    }
    #colnames(tmp) <- tmp[1,]
    tmp <- #tmp[-1,] %>%
      tmp %>%
      filter(time <= time_filter_high & time >= time_filter_low) %>%
      pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>%
      mutate(sim_num = i)
    tmp
  }
  rc<- rbindlist(rc)
  return(rc)
}


# library(microbenchmark)
#
# fn1 <- function(i){
#   tmp <- read_csv(file) %>%
#     pivot_longer(cols=`6001`:`6115`, names_to = "county", values_to="N") %>%
#     mutate(sim_num = i)
# }
#
# fn2 <- function(i){
#   tmp <- data.table::fread(file, header = TRUE) %>% as.data.frame() %>%
#     pivot_longer(cols=`6001`:`6115`, names_to = "county", values_to="N") %>%
#     mutate(sim_num = i)
# }
#
# microbenchmark(fn1(1), fn2(2), times=100)
#



##' Function to load county data within California
##' NO LONGER USED
##'@param ca shapefile of CA counties
##'
##'@return a long data frame with GEOID + cental coordinates of each county
##'
load_county_dat_CA <- function(ca, metrop_labels){
  county_dat <- read.csv("data/geodata.csv")
  county_dat$metrop_labels <- county_dat$new_metrop
  levels(county_dat$metrop_labels) <- metrop_labels
  return(county_dat)
}


##'Function to load commuting data within California
##'
##'@param county_dat data frame of county geo IDs and population
##'
##'@return a long data frame with flows between ij county pairs
##'
load_comm_dat <- function(county_dat){
  comm_dat <- read.delim("california-data-county/mobility.txt",
                         sep=" ", header=F)
  colnames(comm_dat) <- county_dat$GEOID
  comm_dat$dest <- county_dat$GEOID
  comm_dat_long <- comm_dat %>% gather(key = "origin", value = "wt", -dest)
  comm_dat_long <- inner_join(comm_dat_long, county_dat %>% select(GEOID, y=lat, x=long, metrop.orig=new_metrop), by=c("origin"="GEOID"))
  comm_dat_long <- inner_join(comm_dat_long, county_dat %>% select(GEOID, yend=lat, xend=long, metrop.dest=new_metrop), by=c("dest"="GEOID"))
  return(comm_dat_long)
}

##'Function to print CIs
##'
##'@param lo numeric or vector of lower bound of CI
##'@param hi numeric or vector of upper bound of CI
##'
##'@return formatted texted for CI: '\(lo, hi\)'
##'
## print CI from two vectors
make_CI <- function(lo, hi){
  if(is.numeric(lo)){
    paste0("(", conv_round(lo), ", ", conv_round(hi), ")") }else{
      paste0("(", format.Date(lo, format="%d %b"), ", ", format.Date(hi, format="%d %b"), ")") }
}


##'Function to create data frame of final size, attack rates for each county
##'
##'@param scenario_dat long data frame of results from all simulations
##'@param cdat data frame of county geo IDs and population
##'
##'@return final size and 60% PI for each county
##'
make_final_dat <- function(scenario_dat, cdat=county_dat, final_date = "2020-04-01"){
  final_dat <- scenario_dat %>%
    filter(time==final_date, comp=="cumI") %>%
    group_by(geoid, time) %>%
    summarize(mean=mean(N),
              median=median(N),
              pi_high=quantile(N,probs=.75),
              pi_low=quantile(N,probs=.25)) %>%
    left_join(cdat %>% select(geoid, new_pop, metrop_labels), by="geoid") %>%
    mutate(ar = mean / new_pop * 100000)
  return(final_dat)
}



##'Function to create create data frame of arrival time to each county
##'
##'@param scenario_dat long data frame of results from all simulations
##'@param cdat data frame of county geo IDs and population
##'
##'@return earliest arrival time and 60% PI for each county
##'
make_arrival_dat <- function(scenario_dat, cdat=county_dat){
  arrival_dat <- scenario_dat %>%
    filter(comp=="cumI") %>%
    filter(N>0) %>%
    group_by(geoid, sim_num) %>%
    summarize(time=min(time)) %>%
    group_by(geoid) %>%
    summarize(mean=mean.Date(time),
              pi_low = as.Date(quantile(unclass(time), probs=.25), origin = "1970-01-01"),
              pi_high=as.Date(quantile(unclass(time), .75), origin = "1970-01-01")) %>%
    left_join(cdat %>% select(geoid, new_pop, metrop_labels), by="geoid")
  return(arrival_dat)
}


##'Function to create data frame for statewide epi curve
##'
##'@param scenario_dat long data frame of results from all simulations
##'
##'@return incident infections and 60% PI at each time in simulation, summed across all counties
##'
make_inc_state_dat <- function(scenario_dat){
  state_dat <- scenario_dat %>%
    filter(comp=="diffI") %>%
    group_by(time, sim_num) %>%
    summarize(incidence = sum(N)) %>%
    group_by(time) %>%
    summarize(meanInc=mean(incidence),
              pi_low=quantile(incidence, probs=0.25),
              pi_high=quantile(incidence, probs=0.75))
  return(state_dat)
}

##'Function to create data frame for metro epi curve
##'
##'@param scenario_dat long data frame of results from all simulations with metro labels
##'
##'@return incident infections per county and 60% PI at each time in simulation
##'
make_inc_metro_dat <- function(scenario_dat){
  metro_dat <- scenario_dat %>%
    filter(comp=="diffI") %>%
    drop_na(metrop_labels) %>%
    group_by(time, metrop_labels, sim_num) %>%
    summarize(incidence=sum(N)) %>%
    group_by(time, metrop_labels) %>%
    summarize(mean=mean(incidence),
              pi_low=quantile(incidence, probs=.25),
              pi_high=quantile(incidence, probs=.75))
  return(metro_dat)
}


##'Function to create table of attack rates and arrival times for each metro
##'
##'@param final_dat date frame of final sizes for each county
##'@param arrival_dat data frame of earliest arrival time for each county
##'
##'@return data frame with metro area final size, attack rates, earliest arrival times
##'
make_ar_table <- function(final_dat, arrival_dat){
  tmp <-  final_dat %>%
    left_join(arrival_dat %>% select(geoid, arvl=mean, arvl_lo=pi_low, arvl_hi=pi_high), by="geoid") %>%
    filter(!is.na(metrop_labels)) %>%
    group_by(metrop_labels) %>%
    summarise(nfinal = sum(mean),
              nfinal_low = sum(pi_low),
              nfinal_high = sum(pi_high),
              npop = sum(new_pop),
              ar = nfinal /npop * 100000,
              ar_low = nfinal_low / npop * 100000,
              ar_high = nfinal_high / npop * 100000,
              arvl = min(arvl),
              arvl_low = min(arvl_lo),
              arvl_high = min(arvl_hi)) %>%
    arrange(desc(nfinal))
  return(tmp)
}


##' Function to create data frame of total hospitalizations and deaths in each county
##' NO LONGER USED - see build_hospdeath_summary
##'
##'@param hd_dat date frame of incident hospitalizations and deaths
##'@param cdat data frame of county geo IDs and population
##'
##'@return data frame of final hosps and deaths by county with 60% PI
##'
make_final_hosp_county_dat <- function(hd_dat, cdat=county_dat, end_date="2020-04-01"){
  tmp <- hd_dat %>%
    filter(time <= as.Date(end_date)) %>%
    group_by(geoid, sim_num) %>%
    summarize(nhosp = sum(incidH), ndeath = sum(incidD)) %>%
    ungroup() %>%
    group_by(geoid) %>%
    summarize(nhosp_final = mean(nhosp),
              nhosp_lo = quantile(nhosp, 0.25),
              nhosp_hi = quantile(nhosp, 0.75),
              ndeath_final = mean(ndeath),
              ndeath_lo = quantile(ndeath, 0.25),
              ndeath_hi = quantile(ndeath, 0.75)) %>%
    left_join(cdat %>% select(geoid, metrop_labels, new_pop) %>% mutate(geoid=as.character(geoid)), by=c("geoid"="geoid"))
  return(tmp)
}


##' Function to create data frame of total hospitalizations and deaths in each metro area
##' NO LONGER USED - see build_hospdeath_summary
##'
##'@param hd_dat date frame of incident hospitalizations and deaths
##'@param cdat data frame of county geo IDs and population
##'
##'@return data frame of final hosps and deaths by metro area with 60% PI
##'
make_final_hosp_metrop_dat <- function(hd_dat, cdat=county_dat, end_date="2020-04-01"){
  tmp <- hd_dat %>%
    filter(time <= as.Date(end_date)) %>%
    left_join(cdat %>% select(geoid, metrop_labels, new_pop), by=c("geoid"="geoid")) %>%
    group_by(metrop_labels, sim_num, p_death) %>%
    summarize(nhosp = sum(incidH),
              ndeath = sum(incidD),
              new_pop = new_pop[1]) %>%
    ungroup() %>%
    group_by(metrop_labels, p_death) %>%
    summarize(nhosp_final = mean(nhosp),
              nhosp_lo = quantile(nhosp, 0.25),
              nhosp_hi = quantile(nhosp, 0.75),
              ndeath_final = mean(ndeath),
              ndeath_lo = quantile(ndeath, 0.25),
              ndeath_hi = quantile(ndeath, 0.75))
  return(tmp)
}

##' Function to create data frame of total hospitalizations and deaths
##' NO LONGER USED - see build_hospdeath_summary
##'
##'@param hd_dat date frame of incident hospitalizations and deaths
##'@param cdat data frame of county geo IDs and population
##'
##'@return data frame of final hosps and deaths by metro area with 60% PI
##'
make_final_hosp_dat <- function(hd_dat, end_date="2020-04-01"){
  tmp <- hd_dat %>%
    filter(time <= as.Date(end_date)) %>%
    group_by(sim_num, p_death) %>%
    summarize(nhosp = sum(incidH),
              ndeath = sum(incidD)) %>%
    ungroup() %>%
    group_by(p_death) %>%
    summarize(nhosp_final = mean(nhosp),
              nhosp_lo = quantile(nhosp, 0.25),
              nhosp_hi = quantile(nhosp, 0.75),
              ndeath_final = mean(ndeath),
              ndeath_lo = quantile(ndeath, 0.25),
              ndeath_hi = quantile(ndeath, 0.75))
  return(tmp)
}


##' Function to format the hospitalization table
##'
##'@param final_hosp_dat total hospitalizations and deaths for geo area of interest
##'@param final_hosp_metrop_dat total hosps and deaths per metro
##'@param p_death IFRs used in this analysis
##'
make_hosp_table <- function(final_hosp_dat, final_hosp_metrop_dat, p_death){
  
  tmp_metro <- final_hosp_metrop_dat %>%
    mutate(hosp_est = paste0(conv_round(nhosp_final), " (", conv_round(nhosp_lo), "-", conv_round(nhosp_hi), ")"),
           peak_hosp = paste0(conv_round(phosp_final), " (", conv_round(phosp_lo), "-", conv_round(phosp_hi), ")"),
           #peak_hosp_cap = paste0(conv_round(nhosp_curr_final), " (", conv_round(nhosp_curr_lo), "-", conv_round(nhosp_curr_hi), ")"),
           ICU_est = paste0(conv_round(nICU_final), " (", conv_round(nICU_lo), "-", conv_round(nICU_hi), ")"),
           peak_ICU = paste0(conv_round(pICU_final), " (", conv_round(pICU_lo), "-", conv_round(pICU_hi), ")"),
           #peak_ICU_cap = paste0(round(nicu_curr_final, 1), " (", round(nicu_curr_lo, 1), "-", round(nicu_curr_hi, 1), ")"),
           vent_est = paste0(conv_round(nVent_final), " (", conv_round(nVent_lo), "-", conv_round(nVent_hi), ")"),
           death_est = paste0(conv_round(ndeath_final), " (", conv_round(ndeath_lo), "-", conv_round(ndeath_hi), ")")) %>%
    arrange(desc(nhosp_final)) %>%
    select(metrop_labels, p_death, hosp_est, ICU_est, vent_est, death_est) %>%
    filter(!is.na(metrop_labels)) %>%
    pivot_wider(id_cols=metrop_labels,
                names_from=p_death,
                values_from = c(hosp_est, ICU_est, vent_est, death_est))
  
  tmp_total <- final_hosp_dat %>%
    mutate(hosp_est = paste0(conv_round(nhosp_final), " (", conv_round(nhosp_lo), "-", conv_round(nhosp_hi), ")"),
           peak_hosp = paste0(conv_round(phosp_final), " (", conv_round(phosp_lo), "-", conv_round(phosp_hi), ")"),
           #peak_hosp_cap = paste0(conv_round(nhosp_curr_final), " (", conv_round(nhosp_curr_lo), "-", conv_round(nhosp_curr_hi), ")"),
           ICU_est = paste0(conv_round(nICU_final), " (", conv_round(nICU_lo), "-", conv_round(nICU_hi), ")"),
           peak_ICU = paste0(conv_round(pICU_final), " (", conv_round(pICU_lo), "-", conv_round(pICU_hi), ")"),
           #peak_ICU_cap = paste0(round(nicu_curr_final, 1), " (", round(nicu_curr_lo, 1), "-", round(nicu_curr_hi, 1), ")"),
           vent_est = paste0(conv_round(nVent_final), " (", conv_round(nVent_lo), "-", conv_round(nVent_hi), ")"),
           death_est = paste0(conv_round(ndeath_final), " (", conv_round(ndeath_lo), "-", conv_round(ndeath_hi), ")"),
           metrop_labels = "All Locations") %>%
    select(metrop_labels, p_death, hosp_est, ICU_est, vent_est, death_est) %>%
    pivot_wider(id_cols=metrop_labels,
                names_from=p_death,
                values_from = c(hosp_est, ICU_est, vent_est, death_est))
  
  var_to_report <- c("hosp_est_", "ICU_est_", "vent_est_", "death_est_")
  cnames <- paste0(var_to_report, rep(p_death, each=length(var_to_report)))
  
  tab <- bind_rows(tmp_total[,c("metrop_labels", cnames)],
                   tmp_metro[,c("metrop_labels", cnames)])
  
  return(tab)
}


##' Function to format the hospitalization table for county-level outcomes
##'
##'@param final_hosp_dat total hospitalizations and deaths for geo area of interest
##'@param final_hosp_geoid_dat total hosps and deaths per metro
##'@param county_dat geoid and geoid_labels object
##'@param p_death IFRs used in this analysis
##'
make_hosp_table_county <- function(final_hosp_dat, final_hosp_geoid_dat, county_dat, p_death){
  
  final_hosp_geoid_dat <- final_hosp_geoid_dat %>% left_join(county_dat %>% select(geoid, geoid_label), by="geoid")
  
  tmp_geoid <- final_hosp_geoid_dat %>%
    mutate(hosp_est = paste0(conv_round(nhosp_final), " (", conv_round(nhosp_lo), "-", conv_round(nhosp_hi), ")"),
           peak_hosp = paste0(conv_round(phosp_final), " (", conv_round(phosp_lo), "-", conv_round(phosp_hi), ")"),
           #peak_hosp_cap = paste0(conv_round(nhosp_curr_final), " (", conv_round(nhosp_curr_lo), "-", conv_round(nhosp_curr_hi), ")"),
           ICU_est = paste0(conv_round(nICU_final), " (", conv_round(nICU_lo), "-", conv_round(nICU_hi), ")"),
           peak_ICU = paste0(conv_round(pICU_final), " (", conv_round(pICU_lo), "-", conv_round(pICU_hi), ")"),
           #peak_ICU_cap = paste0(conv_round(nicu_curr_final), " (", conv_round(nicu_curr_lo), "-", conv_round(nicu_curr_hi), ")"),
           vent_est = paste0(conv_round(nVent_final), " (", conv_round(nVent_lo), "-", conv_round(nVent_hi), ")"),
           death_est = paste0(conv_round(ndeath_final), " (", conv_round(ndeath_lo), "-", conv_round(ndeath_hi), ")")) %>%
    arrange(desc(nhosp_final)) %>%
    select(geoid_label, p_death, hosp_est, peak_hosp, ICU_est, peak_ICU, vent_est, death_est) %>%
    filter(!is.na(geoid_label)) %>%
    pivot_wider(id_cols=geoid_label,
                names_from=p_death,
                values_from = c(hosp_est, peak_hosp, ICU_est, peak_ICU, vent_est, death_est))
  
  
  tmp_total <- final_hosp_dat %>%
    mutate(hosp_est = paste0(conv_round(nhosp_final), " (", conv_round(nhosp_lo), "-", conv_round(nhosp_hi), ")"),
           peak_hosp = paste0(conv_round(phosp_final), " (", conv_round(phosp_lo), "-", conv_round(phosp_hi), ")"),
          # peak_hosp_cap = paste0(conv_round(nhosp_curr_final), " (", conv_round(nhosp_curr_lo), "-", conv_round(nhosp_curr_hi), ")"),
           ICU_est = paste0(conv_round(nICU_final), " (", conv_round(nICU_lo), "-", conv_round(nICU_hi), ")"),
           peak_ICU = paste0(conv_round(pICU_final), " (", conv_round(pICU_lo), "-", conv_round(pICU_hi), ")"),
          # peak_ICU_cap = paste0(conv_round(nicu_curr_final), " (", conv_round(nicu_curr_lo), "-", conv_round(nicu_curr_hi), ")"),
           vent_est = paste0(conv_round(nVent_final), " (", conv_round(nVent_lo), "-", conv_round(nVent_hi), ")"),
           death_est = paste0(conv_round(ndeath_final), " (", conv_round(ndeath_lo), "-", conv_round(ndeath_hi), ")"),
           geoid_label = "All Locations") %>%
    select(geoid_label, p_death, hosp_est, peak_hosp, ICU_est, peak_ICU, vent_est, death_est) %>%
    pivot_wider(id_cols=geoid_label,
                names_from=p_death,
                values_from = c(hosp_est, peak_hosp, ICU_est, peak_ICU, vent_est, death_est))
  
  var_to_report <- c("hosp_est_", "ICU_est_", "vent_est_", "death_est_")
  cnames <- paste0(var_to_report, rep(p_death, each=length(var_to_report)))
  
  tab <- bind_rows(tmp_total[,c("geoid_label", cnames)],
                   tmp_geoid[,c("geoid_label", cnames)])
  
  return(tab)
}


##'Function to plot county final sizes
##'
##'@param ca_final_dat sf object with county final sizes
##'
##'@return ggplot object, final sizes with CI (y) by county (x)
##'
make_final_plot <- function(ca_final_dat){
  p <- ggplot(ca_final_dat,
              aes(x=reorder(county.name,-median), y=median,ymin=pi_low, ymax=pi_high )) +
    geom_pointrange() +
    ylab("Infections") +
    xlab("County") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}


##'Function to map county final sizes
##'
##'@param ca_final_dat sf object with county final sizes
##'
##'@return ggplot object, CA map with fill prop to final size
##'
make_final_map <- function(ca_final_dat){
  p <- ggplot(ca_final_dat) +
    geom_sf(aes(fill=median)) +
    theme_minimal() +
    scale_fill_viridis(option="plasma")
  return(p)
}


##'Function to plot county arrival times
##'
##'@param ca_final_dat sf object with county arrival times
##'
##'@return ggplot object, arrival times with CI (x) by county (y)
##'
make_arrvl_plot <- function(ca_arrival_dat){
  p <- ggplot(ca_arrival_dat,
              aes(x=reorder(county.name,-as.numeric(mean)), y=mean,ymin=pi_low, ymax=pi_high )) +
    geom_pointrange() +
    ylab("Arrival Date") +
    xlab("County") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
    coord_flip()
  return(p)
}


##'Function to map county arrival times
##'
##'@param ca_final_dat sf object with county arrival times
##'
##'@return ggplot object, CA map with fill prop to arrival time
##'
make_arrvl_map <- function(ca_arrival_dat,
                           breaks = as.numeric(as.Date(c("2020-02-01", "2020-02-14", "2020-03-01", "2020-03-15","2020-04-01"))),
                           labels = c("Feb 1", "Feb 15", "Mar 1", "Mar 15", "April 1")){
  p <- ggplot(ca_arrival_dat) +
    geom_sf(aes(fill=as.numeric(mean))) +
    theme_minimal() +
    scale_fill_viridis(direction=-1,
                       breaks=breaks,
                       labels=labels)
  return(p)
}


##'Function to plot statewide epi curve
##'
##'@param state_inc_dat data frame with statewide incidence by time
##'
##'@return ggplot object, incidence and 60% PI (y) by time
##'
make_state_inc_plot <- function(state_inc_dat){
  p <- ggplot(state_inc_dat, aes(x=time, y=meanInc)) +
    geom_bar(stat="identity", fill="red", alpha=.2) +
    geom_pointrange(aes(ymin=pi_low, ymax=pi_high), color="red", fill="white", alpha=.75, shape=21)
  return(p)
}


##'Function to plot metro epi curve
##'
##'@param metro_inc_dat data frame with incidence by time and metro area
##'
##'@return ggplot object, incidence and 60% PI (y) by time faceted by metro area
##'
make_metro_inc_plot <- function(metro_inc_dat){
  p <- ggplot(metro_inc_dat, aes(x=time, y=mean)) +
    facet_wrap(~metrop_labels, ncol=3) +
    geom_bar(stat="identity", fill="red", alpha=.2) +
    geom_pointrange(aes(ymin=pi_low, ymax=pi_high), color="red", fill="white", alpha=.75, shape=21)
  return(p)
}

##'Function to create summary table of hosp/deaths
##'specifically - columns for estimate + CI
##'with rows for hosp + deaths at each p_death used
##'
##'@param sim_hospdeath_dat summary hospitalization/death data frame
##'
##'@return df object of estimates + CIs for hosp/deaths at all p_death
##'
make_hospdeath_table1 <- function(sim_hospdeath_dat){
  tmp <- sim_hospdeath_dat$res_total %>%
    mutate(ci = make_CI(nhosp_lo, nhosp_hi),
           est = conv_round(nhosp_final),
           lvl = paste0("hosp", p_death)) %>%
    select(lvl, est, ci) %>%
    bind_rows(sim_hospdeath_dat$res_total %>%
                mutate(ci = make_CI(phosp_lo, phosp_hi),
                       est = conv_round(phosp_final),
                       lvl = paste0("peak hosp adm", p_death)) %>%
                select(lvl, est, ci)) %>%
    # bind_rows(sim_hospdeath_dat$res_total %>%
    #             mutate(ci = make_CI(nhosp_curr_lo, nhosp_curr_hi),
    #                    est = nhosp_curr_final,
    #                    lvl = paste0("peak hosp cap", p_death)) %>%
    #             select(lvl, est, ci)) %>%
    bind_rows(sim_hospdeath_dat$res_total %>%
                mutate(ci = make_CI(nICU_lo, nICU_hi),
                       est = conv_round(nICU_final),
                       lvl = paste0("ICU", p_death)) %>%
                select(lvl, est, ci)) %>%
    bind_rows(sim_hospdeath_dat$res_total %>%
                mutate(ci = make_CI(pICU_lo, pICU_hi),
                       est = conv_round(pICU_final),
                       lvl = paste0("peak ICU adm", p_death)) %>%
                select(lvl, est, ci)) %>%
    # bind_rows(sim_hospdeath_dat$res_total %>%
    #             mutate(ci = make_CI(nicu_curr_lo, nicu_curr_hi),
    #                    est = nicu_curr_final,
    #                    lvl = paste0("peak ICU cap", p_death)) %>%
    #             select(lvl, est, ci)) %>%
    bind_rows(sim_hospdeath_dat$res_total %>%
                mutate(ci = make_CI(nVent_lo, nVent_hi),
                       est = conv_round(nVent_final),
                       lvl = paste0("Vent", p_death)) %>%
                select(lvl, est, ci)) %>%
    bind_rows(sim_hospdeath_dat$res_total %>%
                mutate(ci = make_CI(ndeath_lo, ndeath_hi),
                       est = conv_round(ndeath_final),
                       lvl = paste0("death", p_death)) %>%
                select(lvl, est, ci))
  return(tmp)
}


##'Function to create summary table of final sizes
##'
##'@param scenario_dat scenario data output
##'@param final_date date at which to make table
##'
##'@return df object of estimates + CIs for final sizes
##'
make_finalsize_table1 <- function(scenario_dat, final_date = "2020-04-01"){
  tmp <- scenario_dat %>%
    filter(time==final_date, comp=="cumI") %>%
    group_by(sim_num) %>%
    summarize(N = sum(N)) %>%
    ungroup() %>%
    summarize(est=conv_round(mean(N)),
              ci = make_CI(quantile(N,probs=.25),quantile(N,probs=.75)))
  return(tmp)
}

##'Function to create metro labels in California
##'
##'@param data county_dat object of GEOIDs + STATE
##'
##'@return county_dat with metrop_labels added
##'
make_metrop_labels <- function(data=county_dat){
  
  LA <- c('6037', '6059', '6065', '6071', '6111')
  SF <- c('6001', '6013', '6075', '6081', '6041', '6085', '6069', 
          '6077', '6099', '6095', '6097', '6087', '6047', '6055')
  SD <- c('6073')
  FN <- c('6019','6031','6039')
  SC <- c('6067', '6061', '6113', '6017', '6101', '6115', '6057')
  RD <- c('6089', '6103')
  
  data$new_metrop <- NA
  data$new_metrop[data$geoid %in% LA] <- "LA"
  data$new_metrop[data$geoid %in% SF] <- "SF"
  data$new_metrop[data$geoid %in% SD] <- "SD"
  data$new_metrop[data$geoid %in% FN] <- "FN"
  data$new_metrop[data$geoid %in% SC] <- "SC"
  data$new_metrop[data$geoid %in% RD] <- "RD"
  
  ##Update the labels
  data$metrop_labels <- NA
  data$metrop_labels[data$new_metrop=="LA"] <- "Los Angeles"
  data$metrop_labels[data$new_metrop=="SF"] <- "San Francisco"
  data$metrop_labels[data$new_metrop=="SD"] <- "San Diego"
  data$metrop_labels[data$new_metrop=="FN"] <- "Fresno"
  data$metrop_labels[data$new_metrop=="SC"] <- "Sacremento"
  data$metrop_labels[data$new_metrop=="RD"] <- "Redding"
  data$metrop_labels <- as.factor(data$metrop_labels)
  
  return(data)
}


##'Function to round cleanly
##'
##'@param x single number to round
##'
##'@return rounded x
##'
conv_round <- function(x){
  if(x>50){x <- round(x, -2)}
  if(x<50){x <- round(x, -1)}
  return(x)
}


##'Function to print formatted numbers
##'
##'@param x single number to round
##'
##'@return rounded x
##'
print_num <- function(x){
  x <- as.numeric(x)
  if(x>50){format(round(x, -2), scientific=FALSE, big.mark=",")}
  if(x<50){format(round(x, -1), scientific=FALSE, big.mark=",")}
}


