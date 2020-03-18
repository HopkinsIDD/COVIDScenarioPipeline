# source("census_api_key.R") ## user needs to make a census API key and put in a separate R file assigned to variable "census_api_key". This file will not be committed to git

# SETUP ------------------------------------------------------

# library(tidyverse)
# library(tidycensus)
# library(sf)

options(tigris_use_cache = TRUE)

# Change these settings ---------------------------------------------------

# states_of_interest <- sort(c("CA","NV","WA","OR","AZ"))
# region <- "west-coast"
# yr <- 2010 ## 2010 Census was the last time county commuting data was released

## 2006-2010 5-year ACS
census_api_key(census_api_key)

# Pull data ---------------------------------------------------

county_pops <- map(states_of_interest, 
                      ~get_acs(geography = "county",
                        variables = "B01003_001", ## total population data
                        state = .x, 
                        year = yr, 
                        keep_geo_vars = TRUE,
                        geometry = TRUE,
                        show_call = TRUE)) %>%
              map2(states_of_interest, ~mutate(.x, id = .y))
county_pops2 <- reduce(county_pops, rbind)

## write populations dataframe only
county_pops_df <- sf::st_drop_geometry(county_pops2)
write_csv(county_pops_df, paste0("data/", regioncode, "/county_pops_", yr, ".csv"))

## write shapefiles for counties in region of interest
county_pops_sf <- county_pops2 %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAME.x, id) %>%
  dplyr::rename(NAME = NAME.x)
  
if(!file.exists(paste0("data/", regioncode, "/shp/counties_", yr, "_", regioncode, ".shp"))){

  sf::st_write(county_pops_sf, paste0("data/", regioncode, "/shp/counties_", yr, "_", regioncode, ".shp"))
}
