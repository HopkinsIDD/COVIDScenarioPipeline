
library(tidyverse)

#' @name set_region_paths
#' @param region string for region
#' @return 
set_region_paths <- function(region = "around_md"){ 
  
  year <- 2010
  airport_attribution_fname = paste0("data/", region, "/airport_attribution_", year, "_", region, ".csv")
  county_pops_fname = paste0("data/", region, "/county_pops_", year, "_", region, ".csv")
  county_risk_by_airport_fname = paste0("data/", region, "/county_risk_by_airport_", year, "_", region, ".csv")
  importation_params_fname = paste0("data/", region, "/import_nb_params_", region, ".csv")

  return(list(airport_attribution_fname=airport_attribution_fname, 
              county_pops_fname=county_pops_fname, 
              county_risk_by_airport_fname=county_risk_by_airport_fname, 
              importation_params_fname=importation_params_fname))
}


#' @name calculate_county_risk_by_airport
#' @description 
#' @param airport_attribution_fname a long-form dataframe that attributes county populations to surrounding airports according to Voronoi tessellation (attribution sums to 1 for a given county) 
#' @param county_pops_fname
#' @return 
calculate_county_risk_by_airport <- function(region){

  paths_ls <- set_region_paths(region)
  airport_attribution_fn = paths_ls[[1]]
  county_pops_fn = paths_ls[[2]]
  county_risk_by_airport_fn = paths_ls[[3]]

  if(!file.exists(county_risk_by_airport_fn)){

    ## import 2018 population data for counties served by all airports
    county_pops <- read_csv(county_pops_fn) %>%
      dplyr::mutate(fips_cty = ifelse(nchar(GEOID)==4, paste0("0", GEOID), GEOID)) %>%
      dplyr::rename(pop2018 = estimate, countyname = NAME.x) %>%
      dplyr::select(fips_cty, pop2018)

    ## import airport attributions to counties, according to Voronoi population
    airport_attribution <- read_csv(airport_attribution_fn) %>%
      dplyr::mutate(fips_cty = ifelse(nchar(county)==4, paste0("0", county), county)) %>%
      dplyr::left_join(county_pops, by = c("fips_cty")) %>%
      dplyr::mutate(pop_attribution = round(attribution*pop2018))

    airport_catchment_populations <- airport_attribution %>%
      group_by(airport_iata) %>%
      summarise(pop_airport_catchment = sum(pop_attribution))

    county_risk_by_airport <- left_join(airport_attribution, airport_catchment_populations, by = c("airport_iata")) %>%
      dplyr::mutate(cty_risk = pop_attribution/pop_airport_catchment) %>%
      dplyr::select(airport_iata, fips_cty, countyname, pop_attribution, pop_airport_catchment, cty_risk)

    write_csv(county_risk_by_airport, county_risk_by_airport_fn)

  }

  return(read_csv(county_risk_by_airport_fn))
  
}

## This cannot be in a function

# #' @name distribute_airport_importations_to_counties
# #' @description 
# #' @param region
# #' @return 
# distribute_airport_importations_to_counties <- function(region){

  paths_ls <- set_region_paths(region)
  airport_attribution_fn = paths_ls[[1]]
  importation_params_fn = paths_ls[[4]]
  
  importation_params <- read_csv(importation_params_fn) %>%
    dplyr::rename(airport_iata = airport)
  
  ## how many importations per airport per day?
  importation_draws_airport <- purrr::map_dfr(seq_len(nrow(importation_params)), function(i){

    row <- importation_params[i,]
    num_importations_airport <- rnbinom(1, mu = row$mu, size = row$size)
    data.frame(date = row$date, airport_iata = row$airport_iata, num_importations = num_importations_airport)
  })

  county_risk_by_airport <- calculate_county_risk_by_airport(region) %>%
      dplyr::select(airport_iata, fips_cty, cty_risk) %>%
      dplyr::arrange(airport_iata)

  county_importations_by_airport <- purrr::map_dfr(seq_len(nrow(importation_draws_airport)), function(i){

    row <- importation_draws_airport[i,]
    county_risk_by_1airport <- county_risk_by_airport %>%
      dplyr::filter(airport_iata == row$airport_iata) 
    uq_counties_1airport <- county_risk_by_1airport %>% 
      distinct(fips_cty)
    
    if(row$num_importations>0){
      importations_draws <- data.frame(fips_cty = sample(county_risk_by_1airport$fips_cty, 
          size = row$num_importations, 
          replace = TRUE,
          prob = county_risk_by_1airport$cty_risk)) %>%
        group_by(fips_cty) %>%
        count %>%
        ungroup
      county_importations <- full_join(importations_draws, uq_counties_1airport) %>%
        dplyr::mutate(date = row$date, airport_iata = row$airport_iata) %>%
        dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% ## fill 0s for counties that had no importations
        dplyr::rename(importations = n) 
    } else{
      county_importations <- uq_counties_1airport %>%
        dplyr::mutate(importations = 0, date = row$date, airport_iata = row$airport_iata) 
    } 

    return(county_importations)

  })

  county_importations_total <- county_importations_by_airport %>%
    group_by(fips_cty, date) %>%
    summarise(importations = sum(importations)) %>%
    ungroup %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::arrange(date, fips_cty) 

#   return(county_importations_total)

# }


