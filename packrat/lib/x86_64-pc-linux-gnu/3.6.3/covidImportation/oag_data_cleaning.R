##' Merge CDC-provided OAG data 
##' 
##' Shaun Truelove (shauntruelove@jhu.edu)
##' 27 February 2020
##' 




# OPTIONS -----------------------------------------------------------------

# whether to save region data individually
save_regions <- FALSE

# whether to save full data as one file
save_full <- TRUE




# SETUP -------------------------------------------------------------------

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('countrycode')) install.packages('countrycode'); library(countrycode)



# this is set up to use data in "data/OAG_travel_data"
data_dir <- file.path("data", "OAG_travel_data")
data_files <- list.files(data_dir, pattern = ".zip")

# get region info 
region_info <- read_csv(file.path(data_dir, "OAG Region Codes.csv"), na="")
region_info$iso3 <- countrycode::countrycode(region_info$`Country Code`, origin = "iso2c", destination = "iso3c")
region_info$iso3[region_info$`Country Code`=="AN"] <- "ANT"
region_info <- region_info %>% filter(!is.na(iso3))
regions_ <- unique(region_info$`Region Code`)
n_region <- length(regions_)




# READ AND EXTRACT ZIPS ---------------------------------------------------
# we will combine them by region first and see how big those are


# Loop through the regions
for (i in 1:n_region){
    
    # Get files specific to this region
    files_region_ <- data_files[grep(regions_[i], data_files)]
    
    # if no files, skip
    if (length(files_region_)==0) next
    
    # Loop through files for this region, read them, and merge them
    # data_merged_ <- NULL
    data_merged_ <- list()
    for (f in seq_along(files_region_)){
        data_ <- readr::read_csv(file.path(data_dir, files_region_[f]), na="")
        # data_merged_ <- bind_rows(data_merged_, data_)
        data_merged_[[f]] <- data_
    }
    data_merged_ <- rbindlist(data_merged_)
    
    # change iso2 to iso3 (the "NA" for Namibia keeps getting translated as not available)
    data_merged_$`Dep Country Code` <- countrycode::countrycode(data_merged_$`Dep Country Code`, origin = "iso2c", destination = "iso3c")
    data_merged_$`Arr Country Code` <- countrycode::countrycode(data_merged_$`Arr Country Code`, origin = "iso2c", destination = "iso3c")
    
    
    # Save regional data if desired
    if (save_regions){
        write_csv(data_merged_, file.path("data", paste0(regions_, "_OAG_data.csv")))
    }
    
    
    # Assign region name to object
    assign(paste0("data_",regions_[i]), data_merged_)
    # Remove temp data
    rm(data_merged_, data_)
    
    #Check size of the data object
    print(paste0("data_",regions_[i], " is ", format(object.size(get(paste0("data_",regions_[i]))), "Mb")))
}



# They are not too big (biggest is 500Mb), so let's just merge them to save

data_travel_all <- list()
for (i in 1:n_region){
    if (!exists(paste0("data_",regions_[i]))) next
    data_travel_all[[i]] <- get(paste0("data_",regions_[i]))
    #data_travel_all <- bind_rows(data_travel_all, get(paste0("data_",regions_[i])))
}
data_travel_all <- rbindlist(data_travel_all)

print(paste0("Full data is ", format(object.size(data_travel_all), "Mb")))




# Save full data if desired
if (save_full){
    write_csv(data_travel_all, file.path("data", "complete_OAG_data.csv"))
}








# GET SUMMARY OF LOCATIONS ------------------------------------------------

dest_options <- data_travel_all %>% group_by(`Arr Airport Code`, `Arr City Name`, `Arr State Code`, `Arr Country Code`) %>% 
                                        summarise(n_occur = n())

write_csv(dest_options, "data/oag_destination_combos.csv")
    
    
get_dest_options <- function(){
    return(readr::read_csv("data/oag_destination_combos.csv"))
}



#














# GET A SENSE OF THE DATA -------------------------------------------------

us_airports <- unique((data_travel_all %>% filter(`Dep Country Code`=="USA" & `Arr Country Code`=="USA"))$`Arr Airport Code`)









# 
# 
# 
# data_travel_all <- read_csv(file.path("data", "complete_OAG_data.csv"), na=c(""," ", "NA"),
#                             col_types = list(
#                                 `Dep Airport Code` = col_character(),
#                                 `Dep City Name` = col_character(),
#                                 `Dep State Code` = col_character(),
#                                 `Dep Country Code` = col_character(),
#                                 `Arr Airport Code` = col_character(),
#                                 `Arr City Name` = col_character(),
#                                 `Arr State Code` = col_character(),
#                                 `Arr Country Code` = col_character(),
#                                 `Total Est. Pax` = col_double(),
#                                 `Time Series` = col_double()))
# 
# cali_data <- data_travel_all %>% filter(`Arr State Code`=="CA")
# write_csv(cali_data, "data/cali_oag_20172019.csv")
# 
# 
# 
# # SAVE INBOUND TO CALIFORNIA ----------------------------------------------
# 
# cali_data <- read_csv("data/cali_oag_20172019.csv", na=c(""," ","NA"),
#                       col_types = list(
#                           `Dep Airport Code` = col_character(),
#                           `Dep City Name` = col_character(),
#                           `Dep State Code` = col_character(),
#                           `Dep Country Code` = col_character(),
#                           `Arr Airport Code` = col_character(),
#                           `Arr City Name` = col_character(),
#                           `Arr State Code` = col_character(),
#                           `Arr Country Code` = col_character(),
#                           `Total Est. Pax` = col_double(),
#                           `Time Series` = col_double()))
# 
# 
# # Give Chinese airports the provinces 
# airport_attribution <- read_csv(file ='data/airport_attribution.csv')
# 
# cali_data <- left_join(cali_data, 
#                        airport_attribution %>% mutate(Province = gsub(" Province", "", Province)) %>% 
#                            select(-attribution),
#                        by=c("Dep Airport Code"="airport_iata"))
# 
# 
# # Get us State codes for departures
# airport_data <- read_csv("data/airport-codes.csv")
# airport_data <- airport_data %>% mutate(iso_country = ifelse(iso_country=="XK", "KOS",
#                                                              countrycode::countrycode(iso_country, origin = "iso2c", destination = "iso3c")))
# 
# airport_data_us <- airport_data %>% filter(iso_country=="USA")
# 
# cali_data <- left_join(cali_data,
#                        airport_data_us %>% mutate(state = substr(iso_region, 4,5)) %>%
#                            select(state, iata_code),
#                        by=c("Dep Airport Code"="iata_code"))
# cali_data <- cali_data %>% mutate(`Dep State Code`=ifelse(is.na(`Dep State Code`) & !is.na(state), state, `Dep State Code`))
# 
# 
# 
# # Aggregate to province (China) or state (US) or country (all others) for source
# cali_data <- cali_data %>% rename(dep_airport = `Dep Airport Code`,
#                                        dep_state = `Dep State Code`,
#                                        dep_country = `Dep Country Code`,
#                                        dep_city = `Dep City Name`,
#                                        arr_airport = `Arr Airport Code`,
#                                        arr_city = `Arr City Name`,
#                                        arr_state = `Arr State Code`,
#                                        arr_country = `Arr Country Code`,
#                                        arr_city = `Arr City Name`,
#                                        travelers = `Total Est. Pax`,
#                                        yr_month = `Time Series`,
#                                        dep_province = Province)
# # Get aggr location
# cali_data_aggr <- cali_data %>%
#     mutate(dep_loc_aggr = ifelse(dep_country=="CHN", dep_province, ifelse(dep_country=="USA", dep_state, dep_country)))
# cali_data_aggr <- cali_data_aggr %>% group_by(dep_loc_aggr, dep_country, arr_city, arr_state, arr_country, yr_month) %>% 
#     summarise(travelers = sum(travelers, na.rm=TRUE))
# 
# # Get Monthly means across the 3 year (will do geometric means)
# cali_data_aggr <- cali_data_aggr %>% 
#     mutate(t_year = substr(yr_month, 1,4), t_month = as.character(substr(yr_month, 5,6)))
# cali_data_aggr <- cali_data_aggr %>%
#     group_by(dep_loc_aggr, dep_country, arr_city, arr_state, arr_country, t_month) %>% 
#     summarise(travelers_sd = sd(travelers),
#               travelers_mean = exp(mean(log(travelers+1)))-1)
# 
# cali_data_aggr <- cali_data_aggr %>% mutate(travelers_sd = ifelse(is.nan(travelers_sd), travelers_mean/1.96, travelers_sd)) # for those with only 1 value for travel, just use that /2 for the SD
# 
# 
#     
# # Save it
# write_csv(cali_data_aggr, "data/cali_oag_20172019_aggr.csv")
# 
# 
# 
# 
# 
# 
# 
# # SHENZHEN INBOUND --------------------------------------------------------
# 
# 
# data_travel_all <- read_csv(file.path("data", "complete_OAG_data.csv"), na=c(""," ", "NA"),
#                             col_types = list(
#                                 `Dep Airport Code` = col_character(),
#                                 `Dep City Name` = col_character(),
#                                 `Dep State Code` = col_character(),
#                                 `Dep Country Code` = col_character(),
#                                 `Arr Airport Code` = col_character(),
#                                 `Arr City Name` = col_character(),
#                                 `Arr State Code` = col_character(),
#                                 `Arr Country Code` = col_character(),
#                                 `Total Est. Pax` = col_double(),
#                                 `Time Series` = col_double()))
# 
# 
# # check all locations
# # china_data <- data_travel_all %>% filter(`Arr Country Code`=="CHN")
# # Guangzhou
# 
# 
# shenzhen_data <- data_travel_all %>% filter(`Arr Airport Code`=="SZX")
# write_csv(shenzhen_data, "data/shenzhen_oag_20172019.csv")
# 
# 
# 
# # SAVE INBOUND TO shenzhenFORNIA ----------------------------------------------
# 
# shenzhen_data <- read_csv("data/shenzhen_oag_20172019.csv", na=c(""," ","NA"),
#                       col_types = list(
#                           `Dep Airport Code` = col_character(),
#                           `Dep City Name` = col_character(),
#                           `Dep State Code` = col_character(),
#                           `Dep Country Code` = col_character(),
#                           `Arr Airport Code` = col_character(),
#                           `Arr City Name` = col_character(),
#                           `Arr State Code` = col_character(),
#                           `Arr Country Code` = col_character(),
#                           `Total Est. Pax` = col_double(),
#                           `Time Series` = col_double()))
# 
# 
# # Give Chinese airports the provinces 
# airport_attribution <- read_csv(file ='data/airport_attribution.csv')
# 
# shenzhen_data <- left_join(shenzhen_data, 
#                        airport_attribution %>% mutate(Province = gsub(" Province", "", Province)) %>% 
#                            select(-attribution),
#                        by=c("Dep Airport Code"="airport_iata"))
# 
# 
# # Get us State codes for departures
# airport_data <- read_csv("data/airport-codes.csv")
# airport_data <- airport_data %>% mutate(iso_country = ifelse(iso_country=="XK", "KOS",
#                                                              countrycode::countrycode(iso_country, origin = "iso2c", destination = "iso3c")))
# 
# airport_data_us <- airport_data %>% filter(iso_country=="USA")
# 
# shenzhen_data <- left_join(shenzhen_data,
#                        airport_data_us %>% mutate(state = substr(iso_region, 4,5)) %>%
#                            select(state, iata_code),
#                        by=c("Dep Airport Code"="iata_code"))
# shenzhen_data <- shenzhen_data %>% mutate(`Dep State Code`=ifelse(is.na(`Dep State Code`) & !is.na(state), state, `Dep State Code`))
# 
# 
# 
# # Aggregate SOURCE/Departure Location to province (China) or state (US) or country (all others) for source
# shenzhen_data <- shenzhen_data %>% rename(dep_airport = `Dep Airport Code`,
#                                   dep_state = `Dep State Code`,
#                                   dep_country = `Dep Country Code`,
#                                   dep_city = `Dep City Name`,
#                                   arr_airport = `Arr Airport Code`,
#                                   arr_city = `Arr City Name`,
#                                   arr_state = `Arr State Code`,
#                                   arr_country = `Arr Country Code`,
#                                   arr_city = `Arr City Name`,
#                                   travelers = `Total Est. Pax`,
#                                   yr_month = `Time Series`,
#                                   dep_province = Province)
# 
# # Get Aggr Departure location
# shenzhen_data_aggr <- shenzhen_data %>%
#     mutate(dep_loc_aggr = ifelse(dep_country=="CHN", dep_province, ifelse(dep_country=="USA", dep_state, dep_country)))
# shenzhen_data_aggr <- shenzhen_data_aggr %>% group_by(dep_loc_aggr, dep_country, arr_city, arr_state, arr_country, yr_month) %>% 
#     summarise(travelers = sum(travelers, na.rm=TRUE))
# 
# 
# 
# # Get Monthly means across the 3 year (will do geometric means)
# shenzhen_data_aggr <- shenzhen_data_aggr %>% 
#     mutate(t_year = substr(yr_month, 1,4), t_month = as.character(substr(yr_month, 5,6)))
# shenzhen_data_aggr <- shenzhen_data_aggr %>%
#     group_by(dep_loc_aggr, dep_country, arr_city, arr_state, arr_country, t_month) %>% 
#     summarise(travelers_sd = sd(travelers),
#               travelers_mean = exp(mean(log(travelers+1)))-1)
# 
# shenzhen_data_aggr <- shenzhen_data_aggr %>% mutate(travelers_sd = ifelse(is.nan(travelers_sd), travelers_mean/1.96, travelers_sd)) # for those with only 1 value for travel, just use that /2 for the SD
# 
# 
# 
# # Save it
# write_csv(shenzhen_data_aggr, "data/shenzhen_oag_20172019_aggr.csv")
# 
# 
# 
# 
# 





# CHECKS ------------------------------------------------------------------
 
# View(cali_data_aggr %>% filter(is.na(dep_loc_aggr)))
# View(cali_data %>% filter(is.na(dep_state) & dep_country=="USA"))
# tmp <- cali_data_aggr %>% filter(is.na(dep_loc_aggr))
# 
# 
# View(cali_data %>% filter(is.na(Province) & dep_country=="CHN"))
# View(cali_data %>% filter(is.na(Province) & dep_country=="CHN"))
# 






