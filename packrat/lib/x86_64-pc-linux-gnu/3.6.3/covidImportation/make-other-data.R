

library(tidyverse)


## make underreporting data
source("https://raw.githubusercontent.com/salauer/CFR_calculation/master/global_estimates/scripts/main_script_clean.R")
underreporting <- reportDataFinal; rm(reportDataFinal)
#save(underreporting, file="data/underreporting.rda")
usethis::use_data(underreporting, overwrite = TRUE)





# Travel Restrictions Data ------------------------------------------------


# TSA travel reductions
# Source: https://www.tsa.gov/coronavirus/passenger-throughput
tsa <- readr::read_csv("data/tsa_throughput.csv") %>% 
  mutate(date = lubridate::as_date(as.Date(date, "%m/%d/%Y")),
         p_travel = round(tsa_2020 / tsa_2019, 3)) %>%
  arrange(date)
ggplot2::ggplot(tsa, aes(date, p_travel)) + geom_line()


# Scrape this table from web
#--- TO DO ------







hubei_shutdown <- lubridate::as_date(c("2020-01-24", "2020-09-01"))
data("pop_data", package = "covidImportation")
# this requires the pop_data to have all of the correct source locations

## make travel_restrictions data
travel_restrictions <- 
  
  # Reduce travel from all Chinese sources to 10%
  tibble(loc=unique((pop_data %>% filter(country=="CHN"))$source),
             min=hubei_shutdown[1], 
             max=hubei_shutdown[2],
             p_travel=.1) %>% filter(loc!="Hubei") %>%
  
  # Reduce travel from Hubei to 0
  bind_rows(tibble(loc="Hubei",
                       min=hubei_shutdown[1],
                       max=hubei_shutdown[2],
                       p_travel=0)) %>%
  
  # Reduce travel from all US sources based on TSA numbers
  bind_rows(  expand_grid(loc=unique((pop_data %>% filter(country=="USA"))$source),
                        min=tsa$date) %>% 
              left_join(
                tibble(min=tsa$date,
                        max=tsa$date,
                        p_travel=tsa$p_travel), by="min")) %>%
  
  # Reduce travel from all US sources to 10%
  bind_rows(tibble(loc=unique((pop_data %>% filter(country=="USA"))$source),
                       min=lubridate::as_date(max(tsa$date)+1),
                       max=lubridate::as_date("2020-09-01"),
                       p_travel=tsa$p_travel[nrow(tsa)])) %>%
  
  # # Reduce travel from all US sources to 60%
  # bind_rows(data.frame(loc=unique((pop_data %>% filter(country=="USA"))$source),
  #                      min="2020-03-02",
  #                      max="2020-03-08",
  #                      p_travel=.6)) %>%
  # 
  # # Reduce travel from all US sources to 30%
  # bind_rows(data.frame(loc=unique((pop_data %>% filter(country=="USA"))$source),
  #                      min="2020-03-09", 
  #                      max="2020-03-16", 
  #                      p_travel=.3)) %>%
  # 
  # # Reduce travel from all US sources to 10%
  # bind_rows(data.frame(loc=unique((pop_data %>% filter(country=="USA"))$source),
  #                      min="2020-03-17", 
  #                      max="2020-06-16", 
  #                      p_travel=.1)) %>%
  
  # Reduce travel from non-China to US to 30%
  bind_rows(data.frame(loc=unique((pop_data %>% filter(country!="CHN" & country!="USA"))$source),
                       min=lubridate::as_date("2020-03-02"),
                       max=lubridate::as_date("2020-03-08"),
                       p_travel=.3)) %>%
  
  # Reduce travel from all other sources to 10%
  bind_rows(data.frame(loc=unique((pop_data %>% filter(country!="CHN" & country!="USA"))$source),
                       min=lubridate::as_date("2020-03-09"),
                       max=lubridate::as_date("2020-03-16"),
                       p_travel=.1)) %>%
  
  # Reduce travel from all US sources to 4% (matching overall us travel)
  bind_rows(data.frame(loc=unique((pop_data %>% filter(country!="CHN" & country!="USA"))$source),
                       min=lubridate::as_date("2020-03-17"),
                       max=lubridate::as_date("2020-09-01"),
                       p_travel=.04))

usethis::use_data(travel_restrictions, overwrite = TRUE)




# Airport Attributions - China ------------------------------------------

## make airport_attributions data
airport_attribution <- read_csv("data-raw/airport_attribution.csv") %>%
  mutate(Province = gsub(" Province", "", Province)) %>%
  mutate(Province = gsub(" province", "", Province)) %>%
  mutate(Province = gsub(" Special Administrative Region", "", Province)) %>%
  mutate(Province = gsub(" Autonomous Region", "", Province)) %>%
  mutate(Province = gsub(" Municipality", "", Province)) %>%
  mutate(Province = ifelse(grepl("Xinjiang", Province), "Xinjiang", Province)) %>%
  mutate(Province = ifelse(grepl("Guangxi", Province), "Guangxi", Province)) %>%
  mutate(Province = ifelse(grepl("Ningxia", Province), "Ningxia", Province)) %>%
  mutate(Province = ifelse(grepl("Inner Mongolia", Province), "Nei Mongol", Province)) %>%
  mutate(Province = ifelse(grepl("Macao", Province), "Macau", Province)) %>%
  # Attribute travel according to normalizaed attribution score,
  #  weighted by population
  left_join(read_csv("data-raw/pop_data.csv") %>% dplyr::select(source, pop),
            by=c("Province"="source")) %>%
  group_by(airport_iata) %>%
  mutate(attribution = attribution*pop / sum(attribution*pop)) %>%
  ungroup()
#save(airport_attribution, file="data/airport_attribution.rda")
usethis::use_data(airport_attribution, overwrite = TRUE)




# JHUCSSE Case Data ------------------------------------------------------

## Generate combined JHU CSSE data for packages (so users dont have to create the full data)
update_jhucsse_package_data <- function(){
  
  # # pull the data from github
  # pull_JHUCSSE_github_data(case_data_dir = "data/case_data", repull_all=TRUE)
  # # read and merge data
  # jhucsse_case_data <- read_JHUCSSE_cases(last_date=Sys.Date(), 
  #                                         append_wiki=TRUE, 
  #                                         case_data_dir = "data/case_data", 
  #                                         print_file_path=FALSE) 
  # New method
  jhucsse_case_data <- update_JHUCSSE_github_data(case_data_dir = "data/case_data",
                                         last_date=Sys.time(),
                                         check_saved_data=FALSE,
                                         save_data=TRUE)
  
  #save(jhucsse_case_data, file="data/jhucsse_case_data.rda")
  usethis::use_data(jhucsse_case_data, overwrite = TRUE)
}
update_jhucsse_package_data()




# Wikipedia Case Data -----------------------------------------------------

wikipedia_cases <- readr::read_csv("data-raw/WikipediaWuhanPre1-20-2020.csv",
                                   col_types=readr::cols(Update = readr::col_datetime("%m/%d/%Y")))
usethis::use_data(wikipedia_cases, overwrite = TRUE)





# OAG Travel Data ---------------------------------------------------------

##' Get OAG travel data
##'
##' Get subsetted and cleaned OAG data for a specific destination
##'
##' @param destination destination of interest; can be a vector.
##' @param destination_type options: "airport", "city", "state", "country"
##' @param dest_0 default=NULL; change to specify higher level destination (i.e. dest_0="USA")
##' @param dest_0_type default=NULL; must specify if specifying a `dest_0` option.
##' @param dest_aggr_level level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)

make_aggr_oag_travel <- function(){
  # Read full data
  # these data are clean in  `oag_data_cleaning.R`
  data_travel_all <- read_csv(file.path("data_other", "complete_OAG_data.csv"), na=c(""," ", "NA"),
                              col_types = list(
                                `Dep Airport Code` = col_character(),
                                `Dep City Name` = col_character(),
                                `Dep State Code` = col_character(),
                                `Dep Country Code` = col_character(),
                                `Arr Airport Code` = col_character(),
                                `Arr City Name` = col_character(),
                                `Arr State Code` = col_character(),
                                `Arr Country Code` = col_character(),
                                `Total Est. Pax` = col_double(),
                                `Time Series` = col_double()))
  
  
  # Re-assign city, state, country from airport codes (cuz OAG sucks and these data have tons of errors)
  data(airport_data)
  airport_data <- airport_data %>%
    filter(type!="closed") %>% 
    mutate(iso_country = ifelse(iso_country=="XK", "KOS",
                                countrycode::countrycode(iso_country,
                                                         origin = "iso2c",
                                                         destination = "iso3c"))) %>%
    mutate(state = substr(iso_region, 4,6)) %>%
    dplyr::select(-c(type, name, continent, gps_code, local_code, coordinates, elevation_ft, ident, iso_region)) %>%
    filter(!is.na(iata_code))
  airport_data <- airport_data %>% distinct() %>% 
    filter(!duplicated(iata_code))
  
  
  ## Fix arrival codes
  data_travel_all <- left_join(data_travel_all, 
                               airport_data, by = c("Arr Airport Code"="iata_code"))
  data_travel_all <- data_travel_all %>%
    mutate(`Arr City Name`   = ifelse(!is.na(municipality), municipality, `Arr City Name`),
           `Arr State Code`  = ifelse(!is.na(state), state, `Arr State Code`),
           `Arr Country Code`= ifelse(!is.na(iso_country), iso_country, `Arr Country Code`)) %>%
    dplyr::select(-c(municipality, state, iso_country))
  
  ## Fix Departure codes
  data_travel_all <- left_join(data_travel_all, 
                               airport_data, by = c("Dep Airport Code"="iata_code"))
  data_travel_all <- data_travel_all %>%
    mutate(`Dep City Name`   = ifelse(!is.na(municipality), municipality, `Dep City Name`),
           `Dep State Code`  = ifelse(!is.na(state), state, `Dep State Code`),
           `Dep Country Code`= ifelse(!is.na(iso_country), iso_country, `Dep Country Code`)) %>%
    dplyr::select(-c(municipality, state, iso_country))
    
    
    
  # data_dups <- data_travel_all %>% group_by(`Arr Airport Code`) %>%
  #   summarise(unique_cities = length(unique(`Arr City Name`)))
  # data_dups <- data_travel_all %>% group_by(`Dep Airport Code`) %>%
  #   summarise(unique_cities = length(unique(`Dep City Name`)))
  # View(data_travel_all %>% filter(`Arr Airport Code`== "AUS"))
  
  
  data('pop_data', package = 'covidImportation')
  
  # Give Chinese airports the provinces
  data(airport_attribution)
  
  # merge with travel data
  dest_data <- left_join(data_travel_all,
                         airport_attribution,
                         by=c("Dep Airport Code"="airport_iata"))
  rm(data_travel_all)
  gc()
  # Adjust travel volume based on attribution
  dest_data <- dest_data %>%
    replace_na(list(attribution=1)) %>%
    mutate(`Total Est. Pax` = `Total Est. Pax` * attribution) %>%
    dplyr::select(-attribution, pop)
  
  
  # # Get us State codes for departures
  # airport_data_us <- airport_data %>%
  #   dplyr::filter(iso_country=="USA")
  # 
  dest_data <- dest_data %>%
    rename(dep_airport = `Dep Airport Code`,
           dep_state = `Dep State Code`,
           dep_country = `Dep Country Code`,
           dep_city = `Dep City Name`,
           arr_airport = `Arr Airport Code`,
           arr_city = `Arr City Name`,
           arr_state = `Arr State Code`,
           arr_country = `Arr Country Code`,
           travelers = `Total Est. Pax`,
           yr_month = `Time Series`,
           dep_province = Province)
  
  
  # make aggregate source variable, and get mean across 3 years 
  dest_data <- dest_data %>%
    mutate(dep_loc_aggr = ifelse(dep_country=="CHN", dep_province, ifelse(dep_country=="USA", dep_state, dep_country)),
           t_year = substr(yr_month, 1,4),
           t_month = as.character(substr(yr_month, 5,6)))    # Get year and month variables
  
  
  # aggregation levels for destination
  aggr_levels <- factor(c("airport", "city", "metro", "state", "country"), levels=c("airport", "city", "metro", "state", "country"), ordered = TRUE)
  loc_vars_aggr <- c("arr_airport", "arr_city","arr_metro", "arr_state", "arr_country")[aggr_levels>="airport"]
  loc_vars_aggr <- loc_vars_aggr[loc_vars_aggr %in% colnames(dest_data)]
  other_vars_aggr <- c("yr_month", "t_year", "t_month", "dep_loc_aggr", "dep_country")
  
  dest_data_aggr <- dest_data %>% group_by(.dots = c(other_vars_aggr, loc_vars_aggr)) %>%
    summarise(travelers = sum(travelers, na.rm = TRUE))
  rm(dest_data)
  gc()
  
  # Get Monthly means across the 3 year (using geometric means)
  other_vars_aggr <- c("t_month", "dep_loc_aggr", "dep_country")
  dest_data_aggr <- dest_data_aggr %>%
    group_by(.dots = c(other_vars_aggr, loc_vars_aggr)) %>%
    summarise(travelers_sd = sd(travelers, na.rm = TRUE),
              travelers_mean = exp(mean(log(travelers+1), na.rm = TRUE))-1)
  
  dest_data_aggr <- dest_data_aggr %>% mutate(travelers_sd = ifelse(is.nan(travelers_sd), travelers_mean/1.96, travelers_sd)) # for those with only 1 value for travel, just use that /2 for the SD
  
  # Save it
  write_csv(dest_data_aggr, paste0("data_other/", "complete_oag_aggr.csv"))
  
  return(dest_data_aggr)
}





# ~ Make USA Data ---------------------------------------------------------

dest_data_aggr_orig <- make_aggr_oag_travel()
dest_data_aggr <- dest_data_aggr_orig %>% 
  as.data.frame() %>%
  # Increase travel for Chinese New Year
  mutate(travelers=ifelse(t_month == "01" & dep_country=="CHN", travelers_mean*1.6, travelers_mean)) %>%
  select(-arr_city) %>% 
  mutate(travelers_sd=round(travelers_sd, 1), travelers_mean=round(travelers_mean, 1))

format(object.size(dest_data_aggr), "Mb")
write_csv(dest_data_aggr, paste0("data_other/", "complete_oag_aggr_lite.csv"))
save(dest_data_aggr, file=paste0("data_other/", "complete_oag_aggr_lite.rda"))


usa_oag_aggr_travel <- dest_data_aggr %>% filter(arr_country=="USA")
format(object.size(usa_oag_aggr_travel), "Mb")
usethis::use_data(usa_oag_aggr_travel, overwrite = TRUE)
#write_csv(dest_data_aggr_usa, paste0("data_other/", "usa_oag_aggr_lite.csv"))
#save(dest_data_aggr_usa, file=paste0("data_other/", "usa_oag_aggr_lite.rda"))



# ~ Make Territory Data ---------------------------------------------------------

dest_data_aggr <- readr::read_csv(paste0("data_other/", "complete_oag_aggr_lite.csv"))

us_terr_oag_aggr_travel <- dest_data_aggr %>% filter(arr_country %in% c("GUM","VIR","PRI","ASM","MNP"))
format(object.size(us_terr_oag_aggr_travel), "Mb")
#usethis::use_data(us_terr_oag_aggr_travel, overwrite = TRUE)
write_csv(us_terr_oag_aggr_travel, paste0("data_other/", "us_terr_oag_aggr_travel.csv"))
#save(dest_data_aggr_usa, file=paste0("data_other/", "usa_oag_aggr_lite.rda"))





# USA Counties ------------------------------------------------------------

us_counties <- readr::read_csv("data-raw/us_counties.csv")
us_counties <- us_counties %>% rowwise() %>% 
  mutate(FIPS = as.character(FIPS)) %>%
  mutate(FIPS = ifelse(stringr::str_length(FIPS)==4, paste0(0, FIPS), FIPS))
usethis::use_data(us_counties, overwrite = TRUE)
