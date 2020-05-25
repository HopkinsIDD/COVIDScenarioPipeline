###PREAMBLE
library(inference)

setwd("~/COVIDWorking")


opt <- list()

opt$jobs <- 20
opt$forecast_date <- "2020-05-17"
opt$end_date <- "2020-06-30"
opt$geodata <- "COVID19_USA/data/geodata_territories.csv"
opt$name_filter <- ""
opt$num_simulations <- 100

arguments<- list()
arguments$args <- "USTest-5-24"


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
    summarize(incidD=sum(incidD)) %>%
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
  summarize(incidD=sum(incidD))%>%
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