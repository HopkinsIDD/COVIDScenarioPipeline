###PREAMBLE
library(inference)
library(tidyverse)
setwd("/home/jhsph/COVIDWorking/Forecast_Hub")
opt <- arguments <- list()

# MODIFY THIS SECTION .................................
opt$scenario <- "counterfactual"
opt$projection_date <- "2020-01-01"
opt$forecast_date <- "2020-01-01"
opt$end_date <- "2021-11-06"
#arguments$args <- paste0(opt$projection_date, "_Scenariohub_proj/", opt$scenario, "/")
arguments$args <- "rd8_test_2021-11-22"

opt$outfile <- "rd8_2021-11-22-JHU_IDD-CovidSP_full.csv"
opt$outdir <- paste0(".")
#......................................................


opt$jobs <- 2
opt$geodata <- "geodata_territories_2019_statelevel.csv"
opt$death_filter <- "med"
opt$num_simulations <- 50
# opt$outfile <- paste0(opt$projection_date, "-JHU_IDD-CovidSP-", opt$scenario,".csv")
# opt$outdir <- paste0(opt$projection_date, "_Scenariohub_Proj")
opt$include_hosp <- TRUE
opt$reichify <-TRUE



##SCRIPT
scenarios <- arguments$args

suppressMessages(geodata <- readr::read_csv(opt$geodata, col_types = readr::cols(geoid=readr::col_character())))


res_geoid <- arrow::open_dataset(sprintf("%s/hosp",arguments$args), 
                                 partitioning =c("location", 
                                                 "scenario", 
                                                 "death_rate", 
                                                 "date", 
                                                 "lik_type", 
                                                 "is_final"))%>%
  select(time, geoid, incidD, incidH, incidC, death_rate, p_comp)%>%
  filter(time>=lubridate::as_date(opt$forecast_date) & time<=lubridate::as_date(opt$end_date))%>%
  collect() %>%
  filter(stringr::str_detect(death_rate, opt$death_filter))%>%
  mutate(time=as.Date(time)) %>%
  group_by(time, geoid, death_rate, p_comp) %>%
  dplyr::mutate(sim_num = as.character(seq_along(geoid))) %>%
  ungroup()%>%
  group_by(time, geoid, death_rate, sim_num)%>%
  summarise(incidD=sum(incidD), incidH=sum(incidH), incidC=sum(incidC))%>%
  ungroup()
  


res_state <- res_geoid %>%
  filter(time>=opt$forecast_date& time<=opt$end_date) %>%
  inner_join(geodata%>%select(geoid, USPS)) %>%
  group_by(USPS, time, sim_num) %>%
  summarize(incidD=sum(incidD),
            incidH=sum(incidH),
            incidC=sum(incidC)) %>%
  ungroup()
  
# ##Load all of the sims
# ## Register the parallel backend
# cl <- parallel::makeCluster(opt$jobs)
# doParallel::registerDoParallel(cl)
# 
opt$forecast_date <- as.Date(opt$forecast_date)
opt$end_date <- as.Date(opt$end_date)
# 

csse_deaths <- covidcommon::get_groundtruth_from_source(source = "csse", scale = "US county") %>%
  dplyr::select(Update, Deaths, incidDeath, FIPS, source)


jhu_csse_deaths <- csse_deaths %>%
  rename(geoid=FIPS, time=Update, cumD=Deaths, USPS=source)%>%
  group_by(USPS,time)%>%
  summarize(cumD=sum(cumD))%>%
  ungroup()

##Make the forecast for daily cumlative cases
state_cum_deaths<- create_cum_death_forecast(res_state,
                                          jhu_csse_deaths,
                                          opt$forecast_date,
                                          aggregation="day",
                                          loc_column = "USPS")


##reichify if needed
if (opt$reichify) {
  state_daily_cum_deaths<- state_cum_deaths%>%
    filter(quantile!="data")%>%
    filter(time>opt$forecast_date)%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location=as.character(cdlTools::fips(USPS)))%>%
    mutate(location=stringr::str_pad(location, width=2, side="left", pad="0"))%>%
    rename(value=cumD)%>%
    mutate(target=sprintf("%d day ahead cum death", steps_ahead))%>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)
  
  state_week_cum_deaths <- state_daily_cum_deaths%>%
    mutate(day_of_week=lubridate::wday(target_end_date, label=T))%>%
    filter(day_of_week=="Sat")%>%
    mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
    mutate(target=sprintf("%d wk ahead cum death", ahead))%>%
  select(-day_of_week, -ahead)
  
  state_cum_deaths <- dplyr::bind_rows(state_week_cum_deaths)
}
  

##Make the cumdeaths for nation
res_us <- res_state%>%
  group_by(time, sim_num)%>%
  summarize(incidD=sum(incidD),
            incidH=sum(incidH),
              incidC=sum(incidC))%>%
  ungroup()%>%
  mutate(location="US")
  
jhu_csse_deaths_us <- jhu_csse_deaths %>%
  group_by(time)%>%
  summarize(cumD=sum(cumD))%>%
  ungroup()%>%
  mutate(location="US")


us_cum_deaths<- create_cum_death_forecast(res_us,
                                          jhu_csse_deaths_us,
                                          opt$forecast_date,
                                          aggregation="day",
                                          loc_column = "location")
if (opt$reichify) {
  us_daily_cum_deaths<- us_cum_deaths%>%
    filter(quantile!="data")%>%
    filter(time>opt$forecast_date)%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    rename(value=cumD)%>%
    mutate(target=sprintf("%d day ahead cum death", steps_ahead))%>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)

us_week_cum_deaths <- us_daily_cum_deaths%>%
    mutate(day_of_week=lubridate::wday(target_end_date, label=T))%>%
    filter(day_of_week=="Sat")%>%
    mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
    mutate(target=sprintf("%d wk ahead cum death", ahead)) %>%
  select(-day_of_week, -ahead)
  
  us_cum_deaths <- dplyr::bind_rows(us_week_cum_deaths)
}


cum_deaths <- dplyr::bind_rows(us_cum_deaths, state_cum_deaths)


## Incident Deaths Daily
# state_daily_incident_deaths <- res_state%>%
#   group_by(time, USPS)%>% 
#   summarize(x=list(enframe(c(quantile(incidD, probs=c(0.01, 0.025,
#                                                                seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
#                              mean=mean(incidD)),
#                            "quantile","incidD"))) %>%
#   unnest(x)
# 
# us_daily_incident_deaths <- res_us %>%
#   group_by(time) %>%
#   summarize(x=list(enframe(c(quantile(incidD, probs=c(0.01, 0.025,
#                                                       seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
#                              mean=mean(incidD)),
#                            "quantile","incidD"))) %>%
#   unnest(x)
# 
# 
# if (opt$reichify)  {
#   tmp_state_daily_incident_deaths<- state_daily_incident_deaths%>%
#     filter(time>opt$forecast_date)%>%
#     mutate(forecast_date=opt$forecast_date)%>%
#     rename(target_end_date=time)%>%
#     mutate(location=as.character(cdlTools::fips(USPS)))%>%
#     mutate(location=stringr::str_pad(location, width=2, side="left", pad="0"))%>%
#     rename(value=incidD)%>%
#     mutate(target=sprintf("%d day ahead inc death", as.numeric(target_end_date-forecast_date)))%>%
#     mutate(type="quantile")%>%
#     mutate(type=replace(type, quantile=="mean","point"))%>%
#     mutate(quantile=readr::parse_number(quantile)/100)%>%
#     select(forecast_date, target, target_end_date,location,type, quantile, value)
#   
#   tmp_us_daily_incident_deaths<- us_daily_incident_deaths%>%
#     filter(time>opt$forecast_date)%>%
#     mutate(forecast_date=opt$forecast_date)%>%
#     rename(target_end_date=time)%>%
#     mutate(location="US")%>%
#     rename(value=incidD)%>%
#     mutate(target=sprintf("%d day ahead inc death", as.numeric(target_end_date-forecast_date)))%>%
#     mutate(type="quantile")%>%
#     mutate(type=replace(type, quantile=="mean","point"))%>%
#     mutate(quantile=readr::parse_number(quantile)/100)%>%
#     select(forecast_date, target, target_end_date,location,type, quantile, value)
#   
#   
#   daily_inc_deaths <-dplyr::bind_rows(tmp_state_daily_incident_deaths, tmp_us_daily_incident_deaths)
# }



## Incident Deaths Weekly
weekly_state_incident_deaths <- res_state %>%
  mutate(week=lubridate::epiweek(time), year=lubridate::epiyear(time)) %>%
  mutate(tmp_time = as.numeric(time))%>%
  group_by(USPS, sim_num, week, year)%>%
  summarize(incidD=sum(incidD),
         time=max(tmp_time))%>%
  ungroup() %>%
  mutate(time=lubridate::as_date(time))%>%
  group_by(time, USPS, week) %>%
  summarize(x=list(enframe(c(quantile(incidD, probs=c(0.01, 0.025,
                                                      seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                             mean=mean(incidD)),
                           "quantile","incidD"))) %>%
  unnest(x)



weekly_us_incident_deaths <- res_us %>%
  mutate(week=lubridate::epiweek(time), year = lubridate::epiyear(time)) %>%
  group_by(sim_num, week, year) %>%
  summarize(incidD=sum(incidD),
            time=max(time))%>%
  ungroup() %>%
  dplyr::select(-year) %>%
  group_by(time,week) %>%
  summarize(x=list(enframe(c(quantile(incidD, probs=c(0.01, 0.025,
                                                      seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                             mean=mean(incidD)),
                           "quantile","incidD"))) %>%
  unnest(x)


if(opt$reichify) {
  tmp_weekly_state_incident_deaths <-
    weekly_state_incident_deaths%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location=as.character(cdlTools::fips(USPS)))%>%
    mutate(location=stringr::str_pad(location, width=2, side="left", pad="0"))%>%
    rename(value=incidD)%>%
    mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
    mutate(target=sprintf("%d wk ahead inc death", ahead)) %>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)
  
  tmp_weekly_us_incident_deaths <-
    weekly_us_incident_deaths%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location="US")%>%
    rename(value=incidD)%>%
    mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
    mutate(target=sprintf("%d wk ahead inc death", ahead)) %>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)

  
  weekly_inc_deaths <-dplyr::bind_rows(tmp_weekly_state_incident_deaths, tmp_weekly_us_incident_deaths)
}

if (opt$include_hosp) {
  
  state_daily_incident_hosps <- res_state%>%
    group_by(time, USPS)%>% 
    summarize(x=list(enframe(c(quantile(incidH, probs=c(0.01, 0.025,
                                                        seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                               mean=mean(incidH)),
                             "quantile","incidH"))) %>%
    unnest(x)
  
  us_daily_incident_hosps <- res_us %>%
    group_by(time) %>%
    summarize(x=list(enframe(c(quantile(incidH, probs=c(0.01, 0.025,
                                                        seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                               mean=mean(incidH)),
                             "quantile","incidH"))) %>%
    unnest(x)
  
  
  if (opt$reichify)  {
    tmp_state_daily_incident_hosps<- state_daily_incident_hosps%>%
      filter(time>opt$forecast_date)%>%
      mutate(forecast_date=opt$forecast_date)%>%
      rename(target_end_date=time)%>%
      mutate(location=as.character(cdlTools::fips(USPS)))%>%
      mutate(location=stringr::str_pad(location, width=2, side="left", pad="0"))%>%
      rename(value=incidH)%>%
      mutate(target=sprintf("%d day ahead inc hosp", as.numeric(target_end_date-forecast_date)))%>%
      mutate(type="quantile")%>%
      mutate(type=replace(type, quantile=="mean","point"))%>%
      mutate(quantile=readr::parse_number(quantile)/100)%>%
      select(forecast_date, target, target_end_date,location,type, quantile, value)
    
    tmp_us_daily_incident_hosps<- us_daily_incident_hosps%>%
      filter(time>opt$forecast_date)%>%
      mutate(forecast_date=opt$forecast_date)%>%
      rename(target_end_date=time)%>%
      mutate(location="US")%>%
      rename(value=incidH)%>%
      mutate(target=sprintf("%d day ahead inc hosp", as.numeric(target_end_date-forecast_date)))%>%
      mutate(type="quantile")%>%
      mutate(type=replace(type, quantile=="mean","point"))%>%
      mutate(quantile=readr::parse_number(quantile)/100)%>%
      select(forecast_date, target, target_end_date,location,type, quantile, value)
    
    
    daily_inc_hosps <-dplyr::bind_rows(tmp_state_daily_incident_hosps, tmp_us_daily_incident_hosps)
  }
}  


##Incident Cases weekly
weekly_state_incident_cases <- res_state %>%
  mutate(week=lubridate::epiweek(time), year = lubridate::epiyear(time))%>%
  mutate(tmp_time = as.numeric(time))%>%
  group_by(USPS, sim_num, week, year)%>%
  summarize(incidC=sum(incidC),
            time=max(tmp_time))%>%
  ungroup() %>%
  dplyr::select(-year) %>%
  mutate(time=lubridate::as_date(time))%>%
  group_by(time, USPS, week) %>%
  summarize(x=list(enframe(c(quantile(incidC, probs=c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)),
                             mean=mean(incidC)),
                           "quantile","incidC"))) %>%
  unnest(x)



weekly_us_incident_cases <- res_us %>%
  mutate(week=lubridate::epiweek(time), year=lubridate::epiyear(time))%>%
  group_by(sim_num, week, year)%>%
  summarize(incidC=sum(incidC),
            time=max(time))%>%
  ungroup() %>%
  group_by(time,week) %>%
  summarize(x=list(enframe(c(quantile(incidC, probs=c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)),
                             mean=mean(incidC)),
                           "quantile","incidC"))) %>%
  unnest(x)


weekly_geoid_incident_cases <- res_geoid %>%
  mutate(week=lubridate::epiweek(time), year=lubridate::epiyear(time)) %>%
  group_by(sim_num, week, geoid, year) %>%
  summarize(incidC=sum(incidC),
            time=max(time))%>%
  ungroup() %>%
  group_by(time,week,geoid) %>%
  summarize(x=list(enframe(c(quantile(incidC, probs=c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)),
                             mean=mean(incidC)),
                           "quantile","incidC"))) %>%
  unnest(x)

if(opt$reichify) {
  tmp_weekly_state_incident_cases <-
    weekly_state_incident_cases%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location=as.character(cdlTools::fips(USPS)))%>%
    mutate(location=stringr::str_pad(location, width=2, side="left", pad="0"))%>%
    rename(value=incidC)%>%
    mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
    mutate(target=sprintf("%d wk ahead inc case", ahead)) %>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)
  
  tmp_weekly_us_incident_cases <-
    weekly_us_incident_cases%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location="US")%>%
    rename(value=incidC)%>%
    mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
    mutate(target=sprintf("%d wk ahead inc case", ahead)) %>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)
  
  tmp_weekly_geoid_incident_cases <-
    weekly_geoid_incident_cases%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location=geoid)%>%
    rename(value=incidC)%>%
    mutate(ahead=round(as.numeric(target_end_date - forecast_date)/7))%>%
    mutate(target=sprintf("%d wk ahead inc case", ahead)) %>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)
  
  weekly_inc_cases <-dplyr::bind_rows(tmp_weekly_state_incident_cases, tmp_weekly_us_incident_cases, 
                                      tmp_weekly_geoid_incident_cases)%>%
    select(-week)
}



if (opt$reichify) {
  full_forecast <- dplyr::bind_rows( weekly_inc_deaths,
                                     cum_deaths,
                                     weekly_inc_cases)
  
  reich_locs <- read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
  
  full_forecast <- full_forecast%>%
    filter(location%in%reich_locs$location)
  
  if(opt$include_hosp) {
    full_forecast<- dplyr::bind_rows(full_forecast,
                     daily_inc_hosps)
    
  }
  
  full_forecast<- full_forecast%>%
    ungroup()%>%
    filter(target_end_date<=opt$end_date)%>%
    select(-USPS)
  
  dir.create(opt$outdir, recursive = TRUE, showWarnings = FALSE)
  write_csv(full_forecast, file.path(opt$outdir, opt$outfile))
}

projections_file_path <- file.path(opt$outdir, opt$outfile)
print(paste0("Forecast file saved to ",projections_file_path))


# PLOT IT ALL
# source("../COVID19_USA/R/scripts/state_checker_scenariohub.R")


