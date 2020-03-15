
library(tidyverse)

#' @name set_region_settings
#' @param region string for region
#' @return 
set_region_settings <- function(region = "around_md"){ 
  
  if(region == "around_md"){
    year <- 2010
    airport_attribution_fname = paste0("COVIDScenarioPipeline/data/", region, "/airport_attribution_", year, "_", region, ".csv")
    county_pops_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_pops_", year, "_", region, ".csv")
    county_risk_by_airport_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_risk_by_airport_", year, "_", region, ".csv")
    importation_params_fname = paste0("COVIDScenarioPipeline/data/", region, "/import_nb_params_", region, ".csv")
    necessary_geoids = c("24025", "24031", "34003", "34017", "42045", "42091", "42127", "51059") ## fips codes that should have at least one importation by importation_data_upto
    importation_data_upto = "2020-03-01"
    distribute_airport_importations_to_counties <- generate_fixedseeds_distribution_function()
  
  } else{ ## settings for other regions can be modified
    year <- 2010
    airport_attribution_fname = paste0("COVIDScenarioPipeline/data/", region, "/airport_attribution_", year, "_", region, ".csv")
    county_pops_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_pops_", year, "_", region, ".csv")
    county_risk_by_airport_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_risk_by_airport_", year, "_", region, ".csv")
    importation_params_fname = paste0("COVIDScenarioPipeline/data/", region, "/import_nb_params_", region, ".csv")
    necessary_geoids = c()
    importation_data_upto = NA
    distribute_airport_importations_to_counties <- generate_stoch_distribution_function()
  }

  
  return(list(airport_attribution_fname=airport_attribution_fname, 
              county_pops_fname=county_pops_fname, 
              county_risk_by_airport_fname=county_risk_by_airport_fname, 
              importation_params_fname=importation_params_fname,
              necessary_geoids = necessary_geoids,
              importation_data_upto = importation_data_upto,
              distribution_fxn = distribute_airport_importations_to_counties))
}


#' @name calculate_county_risk_by_airport
#' @description 
#' @param airport_attribution_fname a long-form dataframe that attributes county populations to surrounding airports according to Voronoi tessellation (attribution sums to 1 for a given county) 
#' @param county_pops_fname
#' @return 
calculate_county_risk_by_airport <- function(region){

  settings_ls <- set_region_settings(region)
  airport_attribution_fn = settings_ls[[1]]
  county_pops_fn = settings_ls[[2]]
  county_risk_by_airport_fn = settings_ls[[3]]

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
      dplyr::mutate(pop_attribution = round(attribution*pop2018));

    airport_catchment_populations <- airport_attribution %>%
      group_by(airport_iata) %>%
      summarise(pop_airport_catchment = sum(pop_attribution));

    county_risk_by_airport <- left_join(airport_attribution, airport_catchment_populations, by = c("airport_iata")) %>%
      dplyr::mutate(cty_risk = pop_attribution/pop_airport_catchment) %>%
      dplyr::select(airport_iata, fips_cty, countyname, pop_attribution, pop_airport_catchment, cty_risk);

    write_csv(county_risk_by_airport, county_risk_by_airport_fn)

  }

  return(read_csv(county_risk_by_airport_fn))
  
}

generate_stoch_distribution_function <- function(){

  stoch_distribution_function <- function(region){

    settings_ls <- set_region_settings(region)
    airport_attribution_fn = settings_ls[[1]]
    importation_params_fn = settings_ls[[4]]
    
    importation_params <- read_csv(importation_params_fn) %>%
      dplyr::rename(airport_iata = airport)

    print("Stochastic importation seed procedure engaged.")
    
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
        county_importations <- full_join(importations_draws, uq_counties_1airport, by = c("fips_cty")) %>%
          dplyr::mutate(date = row$date, airport_iata = row$airport_iata) %>%
          dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% ## fill 0s for counties that had no importations
          dplyr::rename(importations = n) 
      } else{
        county_importations <- uq_counties_1airport %>%
          dplyr::mutate(importations = 0, date = row$date, airport_iata = row$airport_iata) 
      } 

      return(county_importations)

    })

    county_importations_stoch <- county_importations_by_airport %>%
      group_by(fips_cty, date) %>%
      summarise(importations = sum(importations)) %>%
      ungroup %>%
      dplyr::mutate(date = as.character(date)) %>%
      dplyr::arrange(date, fips_cty) 

      return(county_importations_stoch)
  }

  return(stoch_distribution_function)
}


generate_fixedseeds_distribution_function <- function(){

  fixedseeds_distribution_function <- function(region){

    settings_ls <- set_region_settings(region)
    necessary_geoids = settings_ls[[5]]
    importation_data_upto = settings_ls[[6]]
    stoch_importations_to_counties <- generate_stoch_distribution_function()

    county_importations_stoch <- stoch_importations_to_counties(region) %>%
      dplyr::mutate(date = lubridate::as_date(date))

    print("Importation seed swapping procedure engaged.")

    included_fixedseeds <- county_importations_stoch %>%
      dplyr::filter(date < importation_data_upto & importations > 0) %>%
      dplyr::distinct(fips_cty) %>%
      dplyr::filter(fips_cty %in% necessary_geoids) %>%
      unlist %>% unname
    excluded_fixedseeds <- necessary_geoids[!(necessary_geoids %in% included_fixedseeds)]

    if(length(excluded_fixedseeds)>0){
      ## randomly choose from among importations to swap
      swappedseeds <- county_importations_stoch %>%
        dplyr::mutate(ix = -1*seq_along(date)) %>%
        dplyr::filter(date < importation_data_upto & importations > 0) %>%
        dplyr::filter(!(fips_cty %in% necessary_geoids)) %>%
        dplyr::sample_n(length(excluded_fixedseeds), replace = FALSE)

      ## perform swapping
      new_fixedseeds <- swappedseeds %>%
        dplyr::mutate(fips_cty = sample(excluded_fixedseeds, length(excluded_fixedseeds), replace = FALSE)) %>%
        dplyr::mutate(fips_cty = as.character(fips_cty)) %>%
        dplyr::select(-ix)
      new_nonseeds <- swappedseeds %>%
        dplyr::mutate(fips_cty = as.character(fips_cty)) %>%
        dplyr::mutate(importations = 0) %>%
        dplyr::select(-ix)

      ## remove swaps from original county importations set
      county_importations_wo_swappedseeds <- county_importations_stoch %>%
        dplyr::slice(swappedseeds$ix) %>%
        dplyr::mutate(fips_cty = as.character(fips_cty))
      for (i in 1:nrow(new_fixedseeds)){
        row = new_fixedseeds[i,]
        county_importations_wo_swappedseeds <- county_importations_wo_swappedseeds %>%
          dplyr::filter(!(fips_cty == row$fips_cty & date == row$date))
      }

      county_importations_tot <- bind_rows(county_importations_wo_swappedseeds, new_fixedseeds, new_nonseeds) %>%
        dplyr::mutate(date = as.character(date))

      if(!(dim(county_importations_tot)==dim(county_importations_stoch) & sum(county_importations_tot$importations)==sum(county_importations_stoch$importations))){
        warning("An error occurred in the swapping procedure. Please check the code.")
      }
      print(paste("All necessary counties are now included in the seeding process prior to", importation_data_upto))
    
    } else{
      print("All necessary counties were included in the original random seeding process.")
      county_importations_tot <- county_importations_stoch
    } 

    return(county_importations_tot)
    
  }
}

#########################
## The final output object needs to be able to be accessed directly
region_settings <- set_region_settings(region)
distribute_airport_importations_to_counties <- region_settings$distribution_fxn
county_importations_total <- distribute_airport_importations_to_counties(region)