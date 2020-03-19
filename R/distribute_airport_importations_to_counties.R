
library(tidyverse)

#' @name set_region_settings
#' @param region string for region
#' @return 
set_region_settings <- function(region = "around_md"){ 
  
  if(region == "around_md"){
    year <- 2010
    airport_attribution_fname = paste0("COVIDScenarioPipeline/data/", region, "/airport_attribution_", year, "_", region, ".csv")
    county_pops_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_pops_", year, ".csv")
    county_risk_by_airport_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_risk_by_airport_", year, "_", region, ".csv")
    importation_params_fname = paste0("COVIDScenarioPipeline/data/", region, "/import_nb_params_", region, ".csv")
    necessary_geoids = c("24025", "24031", "34003", "34017", "42045", "42091", "42127", "51059") ## fips codes that should have at least one importation by importation_data_upto
    importations_to_geoids_fixed = c(1,4,4,1,1,5,1,2) ## number of importations to necessary geoids
    importation_data_upto = "2020-03-01"
    distribute_airport_importations_to_counties <- generate_fixedseeds_distribution_function()
  
  } else if(region == "west-coast-AZ-NV"){
    year <- 2010
    airport_attribution_fname = paste0("COVIDScenarioPipeline/data/", region, "/airport_attribution_", year, ".csv")
    county_pops_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_pops_", year, ".csv")
    county_risk_by_airport_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_risk_by_airport_", year, ".csv")
    importation_params_fname = paste0("COVIDScenarioPipeline/data/", region, "/import_nb_params.csv")
    necessary_geoids = c("04013", "04021", "06001", "06013", "06019", "06023", "06037", "06039", "06059", "06061", "06065", "06067", "06069", "06073", "06075", "06081", "06085", "06089", "06097", "06113", "32003", "32031", "41019", "41029", "41035", "41047", "41059", "41067", "53011", "53025", "53031", "53033", "53037", "53053", "53061") ## fips codes that should have at least one importation by importation_data_upto
    importations_to_geoids_fixed = c(2.,  2.,  2.,  9.,  1.,  1., 14.,  1.,  4.,  7.,  1.,  2.,  2., 3.,  9.,  2., 38.,  1.,  3.,  1.,  2.,  2.,  1.,  2.,  1.,  1., 1.,  8.,  1.,  1.,  1., 82.,  1.,  4., 31.) ## number of importations to necessary geoids
    importation_data_upto = "2020-03-07"
    distribute_airport_importations_to_counties <- generate_stoch_distribution_function()

  } else{ ## settings for other regions can be modified
    year <- 2010
    airport_attribution_fname = paste0("COVIDScenarioPipeline/data/", region, "/airport_attribution_", year, ".csv")
    county_pops_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_pops_", year, ".csv")
    county_risk_by_airport_fname = paste0("COVIDScenarioPipeline/data/", region, "/county_risk_by_airport_", year, ".csv")
    importation_params_fname = paste0("COVIDScenarioPipeline/data/", region, "/import_nb_params.csv")
    necessary_geoids = c()
    importations_to_geoids_fixed = c()
    importation_data_upto = NA
    distribute_airport_importations_to_counties <- generate_stoch_distribution_function()
  }

  
  return(list(airport_attribution_fname=airport_attribution_fname, 
              county_pops_fname=county_pops_fname, 
              county_risk_by_airport_fname=county_risk_by_airport_fname, 
              importation_params_fname=importation_params_fname,
              necessary_geoids = necessary_geoids,
              importation_data_upto = importation_data_upto,
              distribution_fxn = distribute_airport_importations_to_counties,
              fixed_importations_to_necessary_geoids = importations_to_geoids_fixed))
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

#' @name generate_stoch_distribution_function
#' @description seed counties according to importation-to-airport parameters
#' @return function for stochastic seeding
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
      dplyr::mutate(date = as.character(date), importations = as.numeric(importations)) %>%
      dplyr::arrange(date, fips_cty) 

      return(county_importations_stoch)
  }

  return(stoch_distribution_function)
}

#' @name generate_swappedseeds_distribution_function
#' @description seed counties according to importation-to-airport parameters, then swap in locations that should have at least one seed prior to a certain date
#' @return function for seeding where fixed locations have at least 1 seed
generate_swappedseeds_distribution_function <- function(){

  swappedseeds_distribution_function <- function(region){

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

      county_importations_swap <- bind_rows(county_importations_wo_swappedseeds, new_fixedseeds, new_nonseeds) %>%
        dplyr::mutate(date = as.character(date), importations = as.numeric(importations))

      if(!(dim(county_importations_swap)==dim(county_importations_stoch) & sum(county_importations_swap$importations)==sum(county_importations_stoch$importations))){
        warning("An error occurred in the swapping procedure. Please check the code.")
      }
      print(paste("All necessary counties are now included in the seeding process prior to", importation_data_upto))
    
    } else{
      print("All necessary counties were included in the original random seeding process.")
      county_importations_swap <- county_importations_stoch
    } 

    return(county_importations_swap)
    
  }
}

#' @name generate_fixedseeds_distribution_function
#' @description set a minimum number of importations in specific locations, then seed counties according to importation-to-airport parameters for the remaining number of importations 
#' @return function for seeding where fixed locations have a minimum set number of seeds
generate_fixedseeds_distribution_function <- function(){

  fixedseeds_distribution_function <- function(region){

    settings_ls <- set_region_settings(region)
    airport_attribution_fn = settings_ls[[1]]
    importation_params_fn = settings_ls[[4]]
    necessary_geoids = settings_ls[[5]]
    fixed_importations = settings_ls[[8]]
    importation_data_upto = settings_ls[[6]]

    importation_params <- read_csv(importation_params_fn) %>%
      dplyr::rename(airport_iata = airport)

    print("Fixed seed importation procedure engaged.")
    
    ## how many importations per airport per day?
    importation_draws_airport <- purrr::map_dfr(seq_len(nrow(importation_params)), function(i){

      row <- importation_params[i,]
      num_importations_airport <- rnbinom(1, mu = row$mu, size = row$size)
      data.frame(date = row$date, airport_iata = row$airport_iata, num_importations = num_importations_airport)
    })

    ## set fixed seeds first
    fixed_seeds_df <- data.frame(fips_cty = necessary_geoids, fixed_imports = fixed_importations)
    fixed_importations_draws <- map_dfr(seq_len(nrow(fixed_seeds_df)), function(i){
      row <- fixed_seeds_df[i,]
      potential_importation_dates <- sort(unique(importation_params$date))
      
      data.frame(date = sample(potential_importation_dates, 
                              size = row$fixed_imports, 
                              replace = TRUE)) %>%
        group_by(date) %>%
        count %>%
        ungroup %>%
        dplyr::mutate(fips_cty = row$fips_cty) %>%
        dplyr::rename(importations = n) %>%
        dplyr::select(date, fips_cty, importations)

    }) %>%
      group_by(date, fips_cty) %>%
      summarise(importations = sum(importations)) %>%
      ungroup

    #### randomly set the remainder of seeds ####

    if (sum(importation_draws_airport$num_importations) > sum(fixed_importations)){

      ## first modify the remaining number of importations to randomly assign
      importation_draws_airport_remaining <- importation_draws_airport
      fixed_importations_to_remove <- sum(fixed_importations)
      while (fixed_importations_to_remove > 0){
          
        ix <- sample(seq_len(nrow(importation_draws_airport_remaining)), 1)
        while(importation_draws_airport_remaining[ix,]$num_importations == 0){
          ix <- sample(seq_len(nrow(importation_draws_airport_remaining)), 1)
        }      
        
        importation_draws_airport_remaining[ix,]$num_importations <- (importation_draws_airport_remaining[ix,]$num_importations)-1
        fixed_importations_to_remove = fixed_importations_to_remove-1
      }

      ## calculate county risk by airport
      county_risk_by_airport <- calculate_county_risk_by_airport(region) %>%
          dplyr::select(airport_iata, fips_cty, cty_risk) %>%
          dplyr::arrange(airport_iata)

      county_importations_by_airport <- purrr::map_dfr(
        seq_len(nrow(importation_draws_airport_remaining)), function(i){

        row <- importation_draws_airport_remaining[i,]
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
    
      }) %>%
        dplyr::mutate(fips_cty = as.character(fips_cty))

      county_importations_fixed <- county_importations_by_airport %>%
        group_by(fips_cty, date) %>%
        summarise(importations = sum(importations)) %>%
        ungroup %>%
        bind_rows(fixed_importations_draws) %>%
        group_by(fips_cty, date) %>%
        summarise(importations = sum(importations)) %>%
        ungroup %>%
        dplyr::mutate(date = as.character(date), importations = as.numeric(importations)) %>%
        dplyr::arrange(date, fips_cty) 
    
    } else{
      warning("The number of fixed seed importations is equal to or exceeds the number of estimated importations. Including only fixed seed importations in the output. Warning: This simulation may be seeded with more importations than expected.")
      county_importations_fixed <- fixed_importations_draws
    }

   return(county_importations_fixed)
  
  }

  return(fixedseeds_distribution_function)
}

#########################
## The final output object needs to be able to be accessed directly
region_settings <- set_region_settings(region)
distribute_airport_importations_to_counties <- region_settings$distribution_fxn
county_importations_total <- distribute_airport_importations_to_counties(region)