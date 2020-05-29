



# SETUP -------------------------------------------------------------------

country_code <- "ZMB"
country_name <- "Zambia"
census_year <- 2020
cores <- 4
geoid_len <- 5
shp <- "data/geodata/Districts2017_shapefiles/Zambian_Districts.shp" #"data/geodata/gadm36_ZMB_shp/gadm36_ZMB_2.shp"
shp_loc_var <- "NAME"  # Admin2 variable name in the shapefile
download_worldpop <- TRUE # Set to TRUE for the first run. Otherwise false as this takes a while

# OD matrix of mobility with row
mobility_od_matrix <- "data/geodata/mobility_data_counts.txt" 




library(tidyverse)
library(covidSeverity)
library(sf)




# SEVERITY ----------------------------------------------------------


#devtools::install_github(repo='HopkinsIDD/covidSeverity')
library(covidSeverity)
dir.create(file.path("data", country_code), recursive = TRUE)


# Download the geotiffs
if (download_worldpop){
    filenames <- download_worldpop_agetifs(country=country_code, 
                                           year=census_year, 
                                           save_dir="data/worldpop", 
                                           cores=cores)
}



# Combine data and consolidate to districts
age_pop_data <- load_worldpop_age(shp=shp, 
                                  country=country_code, 
                                  year=census_year, 
                                  loc_var=shp_loc_var,
                                  save_dir="data/worldpop", 
                                  add_pop_to_shapefile = TRUE,
                                  cores=cores)


# Setup geoids
age_pop_data <- age_pop_data %>% 
    tibble::as_tibble() %>%
    dplyr::rename(setNames(shp_loc_var, "adm2")) %>% # Rename 
    dplyr::mutate(geoid = as.character(stringr::str_pad(as.integer(as.factor(adm2)), geoid_len, "left", 0)))

# Convert to 10-year age groups
age_pop_10yr <- convert_wp_10yr(age_pop_data) 

# Get proportions
age_pop_10yr <- age_pop_10yr %>% 
    dplyr::group_by(adm2) %>%
    dplyr::mutate(prop = pop / sum(pop)) %>%
    dplyr::ungroup() 

readr::write_csv(age_pop_10yr, file.path("data", country_code, paste0(country_code, "_age_pop_10yr.csv")))
readr::write_csv(age_pop_data, file.path("data", country_code, paste0(country_code, "_age_pop_data.csv")))


# Estimate parameters
geo_age_params <- get_ageadjustments(age_pop_10yr,
                                     cores=cores,
                                     n_sims=40,
                                     n_preds=1000,
                                     age_grps=c(seq(0,80,by=10),100),
                                     googlesheet_access=FALSE,
                                     output_dir="data",
                                     pop_name=NULL)




# POPULATION_DATA.CSV -----------------------------------------------------
# Use WorldPop data to get total population by geounit -- this is used by the mobility component

age_pop_data <- readr::read_csv(file.path("data", country_code, paste0(country_code, "_age_pop_data.csv")))

population_data <- age_pop_data %>% 
    dplyr::group_by(geoid, adm2) %>%
    dplyr::summarise(POP = round(sum(pop))) %>%
    dplyr::mutate(ADMIN0 = country_code) %>%
    dplyr::select(ADMIN0, ADMIN2=adm2, GEOID=geoid, POP)

dir.create("data/geodata", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(population_data, "data/geodata/population_data.csv")



# SHAPEFILE ---------------------------------------------------------------
# This section verifies that the shapefile has a variable `GEOID` 
#  that matches with the other data

# Read 
adm2 <- sf::read_sf(shp) %>% dplyr::rename(setNames(shp_loc_var, "ADMIN2"))
population_data <- readr::read_csv("data/geodata/population_data.csv")

# Add population
adm2 <- adm2 %>% 
    dplyr::select(-pop) %>% 
    dplyr::left_join(population_data %>% dplyr::select(POP, GEOID, ADMIN2), by="ADMIN2")

adm2 <- adm2 %>% dplyr::rename(pop=POP, geoid=GEOID, province=PROVINCE) 

# Add country name and iso3 code
if (!is.na(country_name)){
    adm2 <- adm2 %>%
        dplyr::mutate(country=country_name, 
                      iso3=country_code)
}

# Save it back in the same name
sf::st_write(adm2, shp, delete_layer=TRUE)




# Get names and geoids for the next step
geoid_info <- adm2 %>% 
    dplyr::select(geoid, ADMIN2) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::arrange(geoid)




# mobility_data.csv ------------------------------------------------------------
# - input data that will be further formatted
# - this is an example 

mobility <- read.table(mobility_od_matrix, stringsAsFactors = FALSE, row.names = 1)

mobility <- mobility %>% 
    tibble::as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column(var="origin") %>%
    tidyr::pivot_longer(cols = -origin, names_to = "destination", values_to = "FLOW")

mobility <- mobility %>%
    dplyr::full_join(population_data %>% dplyr::select(ADMIN2, POP, GEOID), by = c("origin"="ADMIN2")) %>%
    rename(pop_orig = POP,
           OGEOID = GEOID) %>%
    dplyr::full_join(population_data %>% dplyr::select(ADMIN2, POP, GEOID), by = c("destination"="ADMIN2")) %>%
    rename(pop_dest = POP, 
           DGEOID = GEOID)

readr::write_csv(mobility, "data/geodata/mobility_data.csv")    
    


