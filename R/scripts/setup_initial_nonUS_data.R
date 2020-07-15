##
# @file
# @brief Creates a filter file
#
# @details
#
# 
# ## Configuration Items
# 
# ```yaml
# dynfilter_path: <path to file>
# start_date: <date>
# end_date: <date>
#
# spatial_setup:
#   setup_name: <string>
#   base_path: <path to directory>
#   geodata: <path to file>
#   nodenames: <string>
# ```
#
# ## Input Data
#
# * <b>{spatial_setup::base_path}/{spatial_setup::geodata}</b> is a csv with column {spatial_setup::nodenames} that denotes the geoids
#
# ## Output Data
#
# * <b>importation/{spatial_setup::setup_name}/case_data/jhucsse_case_data.csv</b> is a csv with case data from JHU CSSE
# * <b>{dynfilter_path}</b>: filter file
#

## @cond

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(covidSeverity)
library(sf)
library(globaltoolboxlite)
library(foreach)



#.................................................................

# Pulling arguments from command line (comment out if using the next section)

option_list = list(
    optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
    optparse::make_option(c("-w", "--worldpop"), action="store", default=FALSE, type='logical', help="whether to download worldpop data"),   # should be set to TRUE for first run, then FALSE after that
    optparse::make_option(c("-v", "--varname"), action="store", default="ADMIN2", type='character', help="variable name of geounits in shapefile. this is changed by this script to 'ADMIN2'"),
    optparse::make_option(c("-j", "--jobs"), action="store", default="4", type='integer', help="Number of cores (jobs) to run in parallel")
)
opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))

# Variables defined by the command line arguments 
config <- covidcommon::load_config(opts$config)
cores <- opts$jobs
shp_loc_var <- opts$varname  # Admin2 variable name in the shapefile
download_worldpop <- opts$worldpop # Set to TRUE for the first run. Otherwise false as this takes a while


#.................................................................

# ## IF RUNNING FROM THE SCRIPT AND NOT COMMAND LINE, UNCOMMENT THIS SECTION, COMMENT OUT THE ABOVE SECTION, AND SET THESE PARAMETERS
# config <- covidcommon::load_config("config.yml")
# cores <- 4
# shp_loc_var <- "ADMIN2"  # Admin2 variable name in the shapefile
# download_worldpop <- TRUE # Set to TRUE for the first run. Otherwise false as this takes a while

#.................................................................



if (length(config) == 0) {
    stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}




# Pull variables from config
base_data_path <- config$spatial_setup$base_path
country_code <- config$spatial_setup$modeled_states
country_name <- globaltoolboxlite::get_country_name_ISO3(country_code)
# check that the codes is valid
if(any(is.na(country_name))){
    # In case we are using fake countries
    if (exists(config$spatial_setup$modeled_states_names) & exists(config$spatial_setup$modeled_states_nums)){
        country_name <- config$spatial_setup$modeled_states_names
        country_num  <- stringr::str_pad(config$spatial_setup$modeled_states_nums, 3, "left", 0)
    } else {
        stop(paste0("Country code(s): [", paste(country_code, collapse = ", "), "] not recognized as an official ISO-3 code. Please revise in the config."))
    }
} else {
    country_num  <- stringr::str_pad(globaltoolboxlite::get_UNcode_from_ISO3(country_code), 3, "left", 0)
}

census_year <- config$spatial_setup$census_year
geoid_len <- config$spatial_setup$geoid_len
shp <- file.path(base_data_path, config$spatial_setup$shapefile) 

# OD matrix of mobility with row
mobility_od_matrix <- file.path(base_data_path, "geodata/mobility_data_counts.csv")




# SEVERITY ----------------------------------------------------------

#devtools::install_github(repo='HopkinsIDD/covidSeverity')

# Download the geotiffs
if (download_worldpop){
    filenames <- NULL
    for (c in seq_len(length(country_code))){
        filenames_ <- globaltoolboxlite::download_worldpop_agetifs(country=country_code[c], 
                                               year=census_year, 
                                               save_dir=file.path(base_data_path, "worldpop"), 
                                               cores=cores)
        filenames_ <- c(filenames, filenames_) 
    }
} else {
    # if not downloading, check that these files exist
    all_wp_files_downloaded <- NULL
    for (c in seq_len(length(country_code))){
        all_wp_files_downloaded_ <- length(list.files(file.path(base_data_path, "worldpop", country_code[c]))) == 36 # should be 36 files for every country
        all_wp_files_downloaded <- c(all_wp_files_downloaded, all_wp_files_downloaded_)
    }
    if (!all(all_wp_files_downloaded)){
        stop(cat(paste(
            paste0("ERROR: Worldpop geotiffs were not completely downloaded for: ", paste(country_code[!all_wp_files_downloaded], collapse = ", "), "."),
            "Please set command line argument of '-w TRUE' and rerun to download all files", sep="\n")))
    }
}


# Combine data and consolidate to districts

age_pop_data <- NULL
age_pop_10yr <- NULL


for (c in seq_len(length(country_code))){
        
    age_pop_data_ <- globaltoolboxlite::load_worldpop_age(shp=shp, 
                                      country=country_code[c], 
                                      year=census_year, 
                                      loc_var=shp_loc_var,
                                      save_dir=file.path(base_data_path,"worldpop"), 
                                      add_pop_to_shapefile = TRUE,
                                      cores=cores)
    
    # Setup geoids
    age_pop_data_ <- age_pop_data_ %>% 
        tibble::as_tibble() %>%
        dplyr::rename(setNames(shp_loc_var, "adm2")) %>% # Rename 
        dplyr::arrange(as.numeric(age_l), adm2) %>%
        dplyr::mutate(geoid = as.character(paste0(country_num, stringr::str_pad(as.integer(as.factor(adm2)), geoid_len-3, "left", 0))),
                      adm0 = country_code[c])
    
    # Convert to 10-year age groups
    age_pop_10yr_ <- globaltoolboxlite::convert_wp_10yr(age_pop_data_) %>%
        mutate(adm0 = country_code[c]) %>%
        left_join(age_pop_data_ %>% dplyr::select(adm2, geoid, adm0) %>%
                                    dplyr::distinct(), 
                  by=c("adm2", "adm0"))
    
    # Get proportions
    age_pop_10yr_ <- age_pop_10yr_ %>% 
        dplyr::group_by(adm2, adm0, geoid) %>%
        dplyr::mutate(prop = pop / sum(pop)) %>%
        dplyr::ungroup() 
    
    ## Save individual countries (not sure if needed right now)
    # dir.create(file.path(base_data_path, country_code[c]), recursive = TRUE)
    # readr::write_csv(age_pop_10yr_, file.path(base_data_path, country_code[c], paste0(country_code[c], "_age_pop_10yr.csv")))
    # readr::write_csv(age_pop_data_, file.path(base_data_path, country_code[c], paste0(country_code[c], "_age_pop_data.csv")))

    age_pop_data <- dplyr::bind_rows(age_pop_data, age_pop_data_)
    age_pop_10yr <- dplyr::bind_rows(age_pop_10yr, age_pop_10yr_)

}

readr::write_csv(age_pop_10yr, file.path(base_data_path, paste0("age_pop_10yr.csv")))
readr::write_csv(age_pop_data, file.path(base_data_path, paste0("age_pop_data.csv")))


# Estimate parameters - All included countries done at once
geo_age_params <- covidSeverity::get_ageadjustments(age_pop_10yr,
                                                     cores=cores,
                                                     n_sims=40,
                                                     n_preds=1000,
                                                     age_grps=c(seq(0,80,by=10),100),
                                                     googlesheet_access=FALSE,
                                                     output_dir=base_data_path,
                                                     pop_name=NULL)




# POPULATION_DATA.CSV -----------------------------------------------------
# Use WorldPop data to get total population by geounit -- this is used by the mobility component

age_pop_data <- readr::read_csv(file.path(base_data_path, paste0("age_pop_data.csv")))

population_data <- age_pop_data %>% 
    dplyr::group_by(geoid, adm2, adm0) %>%
    dplyr::summarise(POP = round(sum(pop))) %>%
    dplyr::select(ADMIN0=adm0, ADMIN2=adm2, GEOID=geoid, POP)

dir.create(file.path(base_data_path, "geodata"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(population_data, file.path(base_data_path, "geodata/population_data.csv"))





# SHAPEFILE ---------------------------------------------------------------
# This section verifies that the shapefile has a variable `GEOID` 
#  that matches with the other data

# Read 
adm2 <- sf::read_sf(shp) %>% dplyr::rename(setNames(shp_loc_var, "ADMIN2"))
population_data <- readr::read_csv(file.path(base_data_path, "geodata/population_data.csv"))

# Add population
if ("pop" %in% colnames(adm2)){
    adm2 <- adm2 %>% dplyr::select(-pop)
}
adm2 <- adm2 %>% 
    dplyr::left_join(population_data %>% dplyr::select(POP, GEOID, ADMIN0, ADMIN2), 
                     by=c("ADMIN2"))
adm2 <- adm2 %>% dplyr::rename(pop=POP, geoid=GEOID) 


# Save it back in the same name
sf::st_write(adm2, shp, delete_layer=TRUE)



# mobility_data.csv ------------------------------------------------------------

mobility <- readr::read_csv(mobility_od_matrix) %>% as.data.frame()
rownames_ <- as.character(mobility[,1])
mobility <- as.matrix(mobility[,-1])
row.names(mobility) <- rownames_

# Get names and geoids from the shapefile
geoid_info <- adm2 %>% 
    dplyr::select(geoid, ADMIN0, ADMIN2) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::arrange(geoid, ADMIN0)

# first check that the names match those of the population data
if(all.equal(sort(colnames(mobility)), sort(geoid_info$ADMIN2))==TRUE){
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
    
    readr::write_csv(mobility, file.path(base_data_path, "geodata/mobility_data.csv"))    
    
    print(paste0("SUCCESS: Mobility data saved to ", file.path(base_data_path, "geodata/mobility_data.csv")))
    
} else {
    
    print(paste0("ERROR: Location names not equal between ", mobility_od_matrix, " and population_data.csv. Please check and re-run MOBILITY setup."))
    
}






# Setup hospitalization parquet file ---------------------------------------------
# if using the newer hospitalization module, need to generate a parquet file of parameters
# THis only gets run if the output file name/location for the parquet file are specified

if (!is.null(config$outcomes$param_place_file)){
    geoid_params <- readr::read_csv(config$spatial_setup$geoid_params_file)
    
    rc <- list()
    names <- c('hosp_inf','icu_hosp','vent_icu','death_inf')
    
    for(name in names){
        p_name <- paste('p',name,sep='_')
        rr_name <- paste('rr',name,sep='_')
        rc[[name]]<- geoid_params[,'geoid']
        rc[[name]]$parameter <- paste('p',name,sep='_')
        if(name == 'hosp_inf'){
            rc[[name]]$value <- geoid_params[[rr_name]] * .05
        }
        if(name == 'death_inf'){
            rc[[name]]$value <- geoid_params[[rr_name]] * .005
        }
        if(name == 'icu_hosp'){
            rc[[name]]$value <- geoid_params[[p_name]]
        }
        if(name == 'vent_icu'){
            rc[[name]]$value <- geoid_params[[p_name]]
        }
    }
    
    name <- 'confirmed_inf'
    rc[[name]] <- geoid_params[,'geoid']
    rc[[name]]$parameter <- paste('p',name,sep='_')
    rc[[name]]$value <- ifelse(!is.null(config$spatial_setup$detect_rate), config$spatial_setup$detect_rate, 0.1)
    
    print(paste0("Using a detection rate of: ", round(rc[[name]]$value[1] * 100,1), "%"))
    
    rc <- do.call(rbind,rc)
    ##rc <- dplyr::filter(rc,gsub('...$','',geoid) == '36')
    arrow::write_parquet(rc, config$outcomes$param_place_file)
}

