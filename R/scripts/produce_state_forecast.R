###PREAMBLE
library(inference)
library(tidyverse)

setwd("~/COVIDWorking")


opt <- list()

opt$jobs <- 20
opt$forecast_date <- "2020-06-14"
opt$end_date <- "2020-07-11"
opt$geodata <- "COVID19_USA/data/geodata_territories.csv"
opt$name_filter <- "low"
opt$num_simulationsulations <- 2000
opt$outfile <- "2020-06-14-JHU_IDD-CovidSP_low.csv"
opt$include_hosp <- TRUE

arguments<- list()
arguments$args <- "USRun-2020-6-15"


opt$reichify <-TRUE

##SCRIPT
scenarios <- arguments$args

suppressMessages(geodata <- readr::read_csv(opt$geodata, col_types = readr::cols(geoid=readr::col_character())))


##Load all of the sims
## Register the parallel backend
cl <- parallel::makeCluster(opt$jobs)
doParallel::registerDoParallel(cl)

opt$forecast_date <- as.Date(opt$forecast_date)
opt$end_date <- as.Date(opt$end_date)


##Per file filtering code
post_proc <- function(x,geodata,opt) {
    x%>%
    filter(time>=opt$forecast_date& time<=opt$end_date) %>%
    inner_join(geodata%>%select(geoid, USPS)) %>%
    group_by(USPS, time) %>%
    summarize(incidD=sum(incidD),
              incidH=sum(incidH)) %>%
    ungroup()
}


##Run over scenarios and death rates as appropriate. Note that
##Final results will average accross whatever is included
res_state <-list()
#setup_name <- config$spatial_setup$setup_name
for (i in 1:length(scenarios)) {
  scenario_dir = scenarios[i]
  res_state[[i]] <- report.generation::load_hosp_sims_filtered(scenario_dir,
                                                               name_filter = opt$name_filter,
                                                               num_files = opt$num_simulations,
                                                               post_process = post_proc,
                                                               geodata=geodata,
                                                               opt=opt)%>%
    mutate(scenario=scenarios[i])
  
  
}

##Put in one data frame
res_state<-dplyr::bind_rows(res_state)

##deregister backend
parallel::stopCluster(cl)

##For now get the USA Facts data
usa_facts_deaths <- read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv") %>%
  #  select(-X119)%>%
  pivot_longer(c(-countyFIPS, -`County Name`, -State, -stateFIPS), names_to = "date", values_to="cumDeaths")%>%
  mutate(date=lubridate::mdy(date))%>%
  mutate(geoid=stringr::str_pad(countyFIPS, width=5,pad="0"))%>%
  rename(USPS=State, county=`County Name`)%>%
  group_by(USPS, date)%>%
  summarize(cumDeaths=sum(cumDeaths))%>%
  ungroup()%>%
  rename(time=date)


##Make the forecast for daily cumlative cases
state_cum_deaths<- create_cum_death_forecast(res_state,
                                          usa_facts_deaths,
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
    rename(value=cumDeaths)%>%
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
  
  state_cum_deaths <- dplyr::bind_rows(state_daily_cum_deaths, state_week_cum_deaths)
}
  

##Make the cumdeaths for nation
res_us <- res_state%>%
  group_by(time, sim_num)%>%
  summarize(incidD=sum(incidD),
            incidH=sum(incidH))%>%
  ungroup()%>%
  mutate(location="US")
  
usa_facts_deaths_us <- usa_facts_deaths %>%
  group_by(time)%>%
  summarize(cumDeaths=sum(cumDeaths))%>%
  ungroup()%>%
  mutate(location="US")


us_cum_deaths<- create_cum_death_forecast(res_us,
                                          usa_facts_deaths_us,
                                          opt$forecast_date,
                                          aggregation="day",
                                          loc_column = "location")
if (opt$reichify) {
  us_daily_cum_deaths<- us_cum_deaths%>%
    filter(quantile!="data")%>%
    filter(time>opt$forecast_date)%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    rename(value=cumDeaths)%>%
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
  
  us_cum_deaths <- dplyr::bind_rows(us_daily_cum_deaths, us_week_cum_deaths)
}


cum_deaths <- dplyr::bind_rows(us_cum_deaths, state_cum_deaths)


## Incident Deaths Daily
state_daily_incident_deaths <- res_state%>%
  group_by(time, USPS)%>% 
  summarize(x=list(enframe(c(quantile(incidD, probs=c(0.01, 0.025,
                                                               seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                             mean=mean(incidD)),
                           "quantile","incidD"))) %>%
  unnest(x)

us_daily_incident_deaths <- res_us %>%
  group_by(time) %>%
  summarize(x=list(enframe(c(quantile(incidD, probs=c(0.01, 0.025,
                                                      seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                             mean=mean(incidD)),
                           "quantile","incidD"))) %>%
  unnest(x)


if (opt$reichify)  {
  tmp_state_daily_incident_deaths<- state_daily_incident_deaths%>%
    filter(time>opt$forecast_date)%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location=as.character(cdlTools::fips(USPS)))%>%
    mutate(location=stringr::str_pad(location, width=2, side="left", pad="0"))%>%
    rename(value=incidD)%>%
    mutate(target=sprintf("%d day ahead inc death", as.numeric(target_end_date-forecast_date)))%>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)
  
  tmp_us_daily_incident_deaths<- us_daily_incident_deaths%>%
    filter(time>opt$forecast_date)%>%
    mutate(forecast_date=opt$forecast_date)%>%
    rename(target_end_date=time)%>%
    mutate(location="US")%>%
    rename(value=incidD)%>%
    mutate(target=sprintf("%d day ahead inc death", as.numeric(target_end_date-forecast_date)))%>%
    mutate(type="quantile")%>%
    mutate(type=replace(type, quantile=="mean","point"))%>%
    mutate(quantile=readr::parse_number(quantile)/100)%>%
    select(forecast_date, target, target_end_date,location,type, quantile, value)
  
  
  daily_inc_deaths <-dplyr::bind_rows(tmp_state_daily_incident_deaths, tmp_us_daily_incident_deaths)
}



## Incident Deaths Weekly
weekly_state_incident_deaths <- res_state %>%
  mutate(week=lubridate::epiweek(time))%>%
  mutate(tmp_time = as.numeric(time))%>%
  group_by(USPS, sim_num, week)%>%
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
  mutate(week=lubridate::epiweek(time))%>%
  group_by(sim_num, week)%>%
  summarize(incidD=sum(incidD),
            time=max(time))%>%
  ungroup() %>%
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
  
if (opt$reichify) {
  full_forecast <- dplyr::bind_rows( weekly_inc_deaths,
                                     daily_inc_deaths,
                                     cum_deaths)
  
  if(opt$include_hosp) {
    full_forecast<- dplyr::bind_rows(full_forecast,
                     daily_inc_hosps)
    
  }
  
  full_forecast<- full_forecast%>%
    ungroup()%>%
    filter(target_end_date<=opt$end_date)%>%
    select(-USPS)
  
  write_csv(full_forecast, opt$outfile)
  }
