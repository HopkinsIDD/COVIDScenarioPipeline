##
# @file
# @brief Creates mobility and geodata for US
#
# @details
#
# ## Configuration Items
#
# ```yaml
# spatial_setup:
#   base_path: <path to directory>
#   modeled_states: <list of state postal codes> e.g. MD, CA, NY
#   mobility: <path to file relative to base_path> optional; default is 'mobility.csv'
#   geodata: <path to file relative to base_path> optional; default is 'geodata.csv'
#   popnodes: <string> optional; default is 'population'
# 
# importation:
#   census_api_key: <string, optional> default is environment variable CENSUS_API_KEY. Environment variable is preferred so you don't accidentally commit your key.
# ```
#
# ## Input Data
#
# None
#
# ## Output Data
#
# * {spatial_setup::base_path}/{spatial_setup::mobility}
# * {spatial_setup::base_path}/{spatial_setup::geodata}
#

## @cond

library(dplyr)
library(tidyr)
library(tidycensus)


option_list = list(
    optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
    optparse::make_option(c("-p", "--path"), action="store", default=Sys.getenv("COVID_PATH", "COVIDScenarioPipeline"), type='character', help="path to the COVIDScenarioPipeline directory"),
    optparse::make_option(c("-w", "--wide_form"), action="store",default=FALSE,type='logical',help="Whether to generate the old wide format mobility or the new long format")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opt$c)
if (length(config) == 0) {
    stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

outdir <- config$spatial_setup$base_path
filterUSPS <- config$spatial_setup$modeled_states
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
# commute_data <- readr::read_csv(paste(opt$p,"sample_data","united-states-commutes","commute_data.csv",sep='/'))
# census_data <- readr::read_csv(paste(opt$p,"sample_data","united-states-commutes","census_tracts_2010.csv", sep = '/'))


# Get census key
census_key = Sys.getenv("CENSUS_API_KEY")
if(length(config$importation$census_api_key) != 0){
    census_key = config$importation$census_api_key
}
if(census_key == ""){
    stop("no census key found -- please set CENSUS_API_KEY environment variable or specify importation::census_api_key in config file")
}
tidycensus::census_api_key(key = census_key)


# CENSUS DATA -------------------------------------------------------------


# Pull stored data unless it does not contain desired states or year, then pull from census API

geodata_us <- readr::read_csv("data/geodata_territories_2019_statelevel.csv")
USPSall <- geodata_us$USPS

if (all(filterUSPS %in% USPSall) & config$spatial_setup$census_year==2019){
    census_data <- arrow::read_parquet("data/census_data_us.parquet") %>%
        dplyr::as_tibble() %>% 
        dplyr::filter(USPS %in% filterUSPS)
} else {
    census_data <- tidycensus::get_acs(geography="county", state=filterUSPS, 
                                       variables="B01003_001", year=config$spatial_setup$census_year, 
                                       keep_geo_vars=TRUE, geometry=FALSE, show_call=TRUE)
    census_data <- census_data %>%
        dplyr::rename(population=estimate, geoid=GEOID) %>%
        dplyr::select(geoid, population) %>%
        dplyr::mutate(geoid = substr(geoid,1,5))
    
    # Add USPS column
    data(fips_codes)
    fips_geoid_codes <- dplyr::mutate(fips_codes, geoid=paste0(state_code,county_code)) %>% 
        dplyr::group_by(geoid) %>% 
        dplyr::summarize(USPS=unique(state))
    
    census_data <- dplyr::left_join(census_data, fips_geoid_codes, by="geoid")
}




# Make each territory one county.
# Puerto Rico is the only one in the 2018 ACS estimates right now. Aggregate it.
# Keeping the other territories in the aggregation just in case they're there in the future.
name_changer <- setNames(
    unique(census_data$geoid),
    unique(census_data$geoid)
)
name_changer[grepl("^60",name_changer)] <- "60000" # American Samoa
name_changer[grepl("^66",name_changer)] <- "66000" # Guam
name_changer[grepl("^69",name_changer)] <- "69000" # Northern Mariana Islands
name_changer[grepl("^72",name_changer)] <- "72000" # Puerto Rico
name_changer[grepl("^78",name_changer)] <- "78000" # Virgin Islands

census_data <- census_data %>%
    dplyr::mutate(geoid = name_changer[geoid]) %>%
    dplyr::group_by(geoid) %>%
    dplyr::summarize(USPS = unique(USPS), population = sum(population))


# Territory populations (except Puerto Rico) taken from from https://www.census.gov/prod/cen2010/cph-2-1.pdf
terr_census_data <- readr::read_csv(file.path(opt$p,"sample_data","united-states-commutes","census_tracts_island_areas_2010.csv"))

census_data <- terr_census_data %>% 
    dplyr::filter(length(filterUSPS) == 0 | ((USPS %in% filterUSPS) & !(USPS %in% census_data)))%>%
    rbind(census_data)


# State-level aggregation if desired ------------------------------------------
if (state_level){
    census_data <- census_data %>%
        dplyr::mutate(geoid = as.character(paste0(substr(geoid,1,2), "000"))) %>%
        dplyr::group_by(USPS, geoid) %>%
        dplyr::summarise(population=sum(population, na.rm=TRUE)) %>%
        tibble::as_tibble()
}


# Sort by population
census_data <- census_data %>%
    dplyr::arrange(population)

if (!is.null(config$spatial_setup$popnodes)) {
    names(census_data)[names(census_data) == "population"] <- config$spatial_setup$popnodes
}

if (length(config$spatial_setup$geodata) > 0) {
    geodata_file <- config$spatial_setup$geodata
} else {
    geodata_file <- 'geodata.csv'
}
write.csv(file = file.path(outdir, geodata_file), census_data, row.names=FALSE)
print(paste("Wrote geodata file:", file.path(outdir, geodata_file)))




# COMMUTE DATA ------------------------------------------------------------
if(state_level & !file.exists(paste0(config$spatial_setup$base_path, "/", config$spatial_setup$mobility))){
    
    warning(paste("State-level mobility files must be created manually because `build_US_setup.R` does not generate a state-level mobility file automatically. No valid mobility file named", paste0(config$spatial_setup$base_path, "/", config$spatial_setup$mobility), "(specified in the config) currently exists. Please check again."))
    
} else if(state_level & file.exists(paste0(config$spatial_setup$base_path, "/", config$spatial_setup$mobility))){
    
    warning(paste("Using existing state-level mobility file named", paste0(config$spatial_setup$base_path, "/", config$spatial_setup$mobility)))
    
} else{
    
    commute_data <- readr::read_csv(paste(opt$p,"sample_data","united-states-commutes","commute_data.csv",sep='/'))
    commute_data <- commute_data %>%
        dplyr::mutate(OFIPS = substr(OFIPS,1,5), DFIPS = substr(DFIPS,1,5)) %>%
        dplyr::mutate(OFIPS = name_changer[OFIPS], DFIPS = name_changer[DFIPS]) %>%
        dplyr::filter(OFIPS %in% census_data$geoid, DFIPS %in% census_data$geoid) %>%
        dplyr::group_by(OFIPS,DFIPS) %>%
        dplyr::summarize(FLOW = sum(FLOW)) %>%
        dplyr::filter(OFIPS != DFIPS)
    
    if(opt$w){
        mobility_file <- 'mobility.txt'
    } else if (length(config$spatial_setup$mobility) > 0) {
        mobility_file <- config$spatial_setup$mobility
    } else {
        mobility_file <- 'mobility.csv'
    }
    
    if(endsWith(mobility_file, '.txt')) {
        
        # Pads 0's for every geoid and itself, so that nothing gets dropped on the pivot
        padding_table <- tibble::tibble(
            OFIPS = census_data$geoid,
            DFIPS = census_data$geoid,
            FLOW = 0
        )
        
        rc <- dplyr::bind_rows(padding_table, commute_data) %>% 
            dplyr::arrange(match(OFIPS, census_data$geoid), match(DFIPS, census_data$geoid)) %>% 
            tidyr::pivot_wider(OFIPS,names_from=DFIPS,values_from=FLOW, values_fill=c("FLOW"=0),values_fn = list(FLOW=sum))
        if(!isTRUE(all(rc$OFIPS == census_data$geoid))){
            print(rc$OFIPS)
            print(census_data$geoid)
            stop("There was a problem generating the mobility matrix")
        }
        write.table(file = file.path(outdir, mobility_file), as.matrix(rc[,-1]), row.names=FALSE, col.names = FALSE, sep = " ")
        
    } else if(endsWith(mobility_file, '.csv')) {
        
        rc <- commute_data
        names(rc) <- c("ori","dest","amount")
        
        rc <- rc[rc$ori != rc$dest,]
        write.csv(file = file.path(outdir, mobility_file), rc, row.names=FALSE)
        
    } else {
        stop("Only .txt and .csv extensions supported for mobility matrix. Please check config's spatial_setup::mobility.")
    }
    
    print(paste("Wrote mobility file:", file.path(outdir, mobility_file)))
}



## @endcond
