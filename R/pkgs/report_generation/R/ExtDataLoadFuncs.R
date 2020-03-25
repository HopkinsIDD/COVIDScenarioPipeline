##' Function to load shapefile for report location.
##' 
##' @param scenario_dir the subdirectory containing this scenario
##' @param post_process function that does processing to sims before loading 
##' @param post_process function that does processing after 
##' 
##' @return shapefile for region of interest with appropriate administrative units
##' 
##' 
##' @author Elizabeth Lee
##' 
##' @export
load_shapefile <- function(region_code, state_fips, shapefile_fn) {
  
  require(sf)
  
  file <- sprintf("COVIDScenarioPipeline/data/%s/shp/%s", region_code, shapefile_fn)
  
  rc <- st_read(file) %>%
    dplyr::filter(STATEFP == state_fips)
  
  return(rc)
}


##' Function to load dataframe of county-geoid crosswalk
##' 
##' @param region_code 
##' @param state_fips
##' @param shapefile_fn
##' 
##' @return Grab county names and population with geoid
##' 
##' @author Elizabeth Lee
##' 
##' @export
load_county_pops <- function(region_code, state_fips, shapefile_fn) {
    
  file <- sprintf("COVIDScenarioPipeline/data/%s/county_pops_%s.csv", region_code, shapefile_fn, pop_year)
  
  rc <- read_csv(file) %>%
    dplyr::mutate(STATEFP = ifelse(nchar(STATEFP)==1, paste0("0", STATEFP))) %>%
    dplyr::filter(STATEFP == state_fips) %>%
    dplyr::select(GEOID, NAME, estimate) %>%
    dplyr::rename(geoid = GEOID, county = NAME, population = estimate)
  
  ## testcases to add: 
  ## check variable column represents Census population variable
  ## check columns exist
  ## check state is in dataset

  return(rc)
}