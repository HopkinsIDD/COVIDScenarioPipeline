##'Function to load multiple simulations into a combine data_frame
##'
##'
##'@param scenario_dir the subdirectory containing this scenario
##'
##'
##'@return a long thin data frame with all of the simulations comined together 
##'
load_scenario_sims <- function(scenario_dir) {
  
  require(data.table)
  
  files <- dir(sprintf("model_output/%s", scenario_dir),full.names = TRUE)
  
  rc <- list()
  
  
  for (i in 1:length(files)) {
    file <- files[i]
    #print(i)
    tmp <- data.table::fread(file) %>% as.data.frame()
    colnames(tmp) <- tmp[1,]
    tmp <- tmp[-1,] %>%
      pivot_longer(cols=`6001`:`6115`, names_to = "county", values_to="N") %>% 
      mutate(sim_num = i) 
    
    rc[[i]] <- tmp
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



##'Function to load county data within California
##'
##'@param ca shapefile of CA counties
##'
##'@return a long data frame with GEOID + cental coordinates of each county
##'
load_county_dat <- function(ca, metrop_labels){
  county_dat <- read.csv("data/geodata.csv")
  county_dat$GEOID <- sprintf("0%s", county_dat$geoid)
  county_dat$long <- as.numeric(as.character(ca$INTPTLON[match(county_dat$GEOID, ca$GEOID)]))
  county_dat$lat <- as.numeric(as.character(ca$INTPTLAT[match(county_dat$GEOID, ca$GEOID)]))
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
    paste0("(", sprintf("%.1f", lo), ", ", sprintf("%.1f", hi), ")") }else{
      paste0("(", format.Date(lo, format="%d %b"), ", ", format.Date(hi, format="%d %b"), ")") }
}


##'Function to create data frame of final size, attack rates for each county
##'
##'@param scenario_dat long data frame of results from all simulations
##'@param cdat data frame of county geo IDs and population
##'
##'@return final size and 60% PI for each county
##'
make_final_dat <- function(scenario_dat, cdat=county_dat){
  final_dat <- scenario_dat %>% 
    filter(time=="2020-04-01", comp=="cumI") %>% 
    mutate(county = sprintf("0%s",county)) %>% 
    group_by(county, time) %>% 
    summarize(mean=mean(N), 
              pi_high=quantile(N,probs=.8),
              pi_low=quantile(N,probs=.2)) %>% 
    left_join(cdat %>% select(county=GEOID, new_pop, metrop_labels), by="county") %>%
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
    mutate(county = sprintf("0%s",county)) %>% 
    filter(comp=="cumI") %>% 
    filter(N>0) %>% 
    group_by(county, sim_num) %>% 
    summarize(time=min(time)) %>% 
    group_by(county) %>% 
    summarize(mean=mean.Date(time), 
              pi_low = as.Date(quantile(unclass(time), probs=.2), origin = "1970-01-01"),
              pi_high=as.Date(quantile(unclass(time), .8), origin = "1970-01-01")) %>% 
    left_join(cdat %>% select(county=GEOID, new_pop, metrop_labels), by="county") 
  return(arrival_dat)
}


##'Function to create data frame for statewide epi curve
##'
##'@param scenario_dat long data frame of results from all simulations
##'
##'@return incident infections and 60% PI at each time in simulation, summed across all counties
##'
make_inc_state_dat <- function(scenario_dat){
  state_dat <- scn_dat_mid %>%
    filter(comp=="diffI") %>% 
    group_by(time, sim_num) %>% 
    summarize(incidence = sum(N)) %>% 
    group_by(time) %>% 
    summarize(meanInc=mean(incidence), 
              pi_low=quantile(incidence, probs=0.2), 
              pi_high=quantile(incidence, probs=0.8))
  return(state_dat)
}

##'Function to create data frame for metro epi curve
##'
##'@param scenario_dat long data frame of results from all simulations
##'@param cdat data frame of county geo IDs and population
##'
##'@return incident infections per county and 60% PI at each time in simulation
##'
make_inc_metro_dat <- function(scenario_dat, cdat=county_dat){
  metro_dat <- scenario_dat %>%
    inner_join(cdat %>% select(geoid, metrop_labels) %>% 
                 mutate(geoid = as.character(geoid)), by=c("county"="geoid")) %>% 
    filter(comp=="diffI") %>% 
    drop_na(metrop_labels) %>% 
    group_by(time, metrop_labels, sim_num) %>% 
    summarize(incidence=sum(N)) %>% 
    group_by(time, metrop_labels) %>% 
    summarize(mean=mean(incidence), 
              pi_low=quantile(incidence, probs=.2), 
              pi_high=quantile(incidence, probs=.8))
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
    left_join(arrival_dat %>% select(county, arvl=mean, arvl_lo=pi_low, arvl_hi=pi_high), by="county") %>%
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

##'Function to create data frame of total hospitalizations and deaths in each county
##'
##'@param hd_dat date frame of incident hospitalizations and deaths
##'@param cdat data frame of county geo IDs and population
##'
##'@return data frame of final hosps and deaths by county with 60% PI
##'
make_final_hosp_county_dat <- function(hd_dat, cdat=county_dat, end_date="2020-04-01"){
  tmp <- hd_dat %>%
    filter(time <= as.Date(end_date)) %>%
    group_by(county, sim_num) %>% 
    summarize(nhosp = sum(incidH), ndeath = sum(incidD)) %>%
    ungroup() %>% group_by(county) %>% 
    summarize(nhosp_final = mean(nhosp),
              nhosp_lo = quantile(nhosp, 0.2),
              nhosp_hi = quantile(nhosp, 0.8),
              ndeath_final = mean(ndeath),
              ndeath_lo = quantile(ndeath, 0.2),
              ndeath_hi = quantile(ndeath, 0.8)) %>%
    left_join(cdat %>% select(geoid, metrop_labels, new_pop) %>% mutate(geoid=as.character(geoid)), by=c("county"="geoid"))
  return(tmp)
}

##'Function to create data frame of total hospitalizations and deaths in each metro area
##'
##'@param hd_dat date frame of incident hospitalizations and deaths
##'@param cdat data frame of county geo IDs and population
##'
##'@return data frame of final hosps and deaths by metro area with 60% PI
##'
make_final_hosp_metrop_dat <- function(hd_dat, cdat=county_dat, end_date="2020-04-01"){
  tmp <- hd_dat %>%
         filter(time <= as.Date(end_date)) %>%
         left_join(cdat %>% select(geoid, metrop_labels, new_pop) %>% mutate(geoid=as.character(geoid)), by=c("county"="geoid")) %>%
         group_by(metrop_labels, sim_num, p_death) %>% 
         summarize(nhosp = sum(incidH), 
                   ndeath = sum(incidD),
                   new_pop = new_pop[1]) %>%
         ungroup() %>% 
         group_by(metrop_labels, p_death) %>% 
         summarize(nhosp_final = mean(nhosp),
                   nhosp_lo = quantile(nhosp, 0.2),
                   nhosp_hi = quantile(nhosp, 0.8),
                   ndeath_final = mean(ndeath),
                   ndeath_lo = quantile(ndeath, 0.2),
                   ndeath_hi = quantile(ndeath, 0.8))
  return(tmp)
}

##'Function to create data frame of total hospitalizations and deaths
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
              nhosp_lo = quantile(nhosp, 0.2),
              nhosp_hi = quantile(nhosp, 0.8),
              ndeath_final = mean(ndeath),
              ndeath_lo = quantile(ndeath, 0.2),
              ndeath_hi = quantile(ndeath, 0.8))
  return(tmp)
}

##'Function to format the hospitalization table
##'
##'@param final_hosp_dat total hospitalizations and deaths for geo area of interest
##'@param final_hosp_metrop_dat total hosps and deaths per metro 
##'@param p_death IFRs used in this analysis
##'
make_hosp_table <- function(final_hosp_dat, final_hosp_metrop_dat, p_death){
  
  tmp_metro <- final_hosp_metrop_dat %>% 
               mutate(hosp_est = paste0(round(nhosp_final, 1), " (", round(nhosp_lo, 1), "-", round(nhosp_hi, 1), ")"),
                      death_est = paste0(round(ndeath_final, 1), " (", round(ndeath_final, 1), "-", round(ndeath_final, 1), ")")) %>%
               arrange(desc(nhosp_final)) %>%
               select(metrop_labels, p_death, hosp_est, death_est) %>%
               filter(!is.na(metrop_labels)) %>%
               pivot_wider(id_cols=metrop_labels, names_from=p_death, values_from = c(hosp_est, death_est))
  
  tmp_total <- final_hosp_dat %>%
               mutate(hosp_est = paste0(round(nhosp_final, 1), " (", round(nhosp_lo, 1), "-", round(nhosp_hi, 1), ")"),
                      death_est = paste0(round(ndeath_final, 1), " (", round(ndeath_final, 1), "-", round(ndeath_final, 1), ")"),
                      metrop_labels = "All Locations") %>%
               select(metrop_labels, p_death, hosp_est, death_est) %>%
               pivot_wider(id_cols=metrop_labels, names_from=p_death, values_from = c(hosp_est, death_est))
  
  cnames <- paste0(c("hosp_est_", "death_est_"), rep(p_death, each=2))

  tab <- bind_rows(tmp_total[,c("metrop_labels", cnames)], 
                   tmp_metro[,c("metrop_labels", cnames)])
  
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
              aes(x=reorder(NAME,-mean), y=mean,ymin=pi_low, ymax=pi_high )) +
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
    geom_sf(aes(fill=mean)) +
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
              aes(x=reorder(NAME,-as.numeric(mean)), y=mean,ymin=pi_low, ymax=pi_high )) +
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
make_arrvl_map <- function(ca_arrival_dat){
  p <- ggplot(ca_arrival_mid) +
    geom_sf(aes(fill=as.numeric(mean))) +
    theme_minimal() +
    scale_fill_viridis(direction=-1, 
                       breaks=as.numeric(as.Date(c("2020-02-01", "2020-02-14", "2020-03-01", "2020-03-15","2020-04-01"))),
                       labels=c("Feb 1", "Feb 15", "Mar 1", "Mar 15", "April 1"))
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
