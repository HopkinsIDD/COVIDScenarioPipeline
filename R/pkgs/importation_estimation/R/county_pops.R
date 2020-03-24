
# Query the census API to get the county populations for the states of interest, assigned
# to the given region code.
# 
# Assumes census_api_key is already called by the calling client and
# that `options(tigris_use_cache = TRUE)`.
get_county_pops <- function(states_of_interest, regioncode, yr=2010, local_dir="data/") {
    county_pops <- purrr::map(states_of_interest,
                       ~tidycensus::get_acs(geography = "county",
                                            variables = "B01003_001", ## total population data
                                            state = .x, 
                                            year = yr,
                                            keep_geo_vars = TRUE,
                                            geometry = TRUE,
                                            show_call = TRUE)) %>%
                   purrr::map2(states_of_interest, ~mutate(.x, id = .y))
    county_pops2 <- purrr::reduce(county_pops, rbind)

    ## write populations dataframe only
    county_pops_df <- sf::st_drop_geometry(county_pops2)
    write_csv(county_pops_df, paste0(local_dir, regioncode, "/county_pops_", yr, ".csv"))

    ## write shapefiles for counties in region of interest
    county_pops_sf <- county_pops2 %>%
        dplyr::select(STATEFP, COUNTYFP, GEOID, NAME.x, id) %>%
        dplyr::rename(NAME = NAME.x)

    shp_path <- paste0(local_dir, regioncode, "/shp/counties_", yr, "_", regioncode, ".shp")  
    if (!file.exists(shp_path)) {
        #TODO: jwills, consider forced overwrite, perhaps?
        sf::st_write(county_pops_sf, shp_path)
    }
    return(shp_path)
}
