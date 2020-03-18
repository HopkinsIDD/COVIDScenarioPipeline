
library(tidyverse)


distribute_dummy_importations <- function(){
  cty <- read_csv("data/west-coast/geodata.csv") %>%
    dplyr::select(geoid, pop2010) %>%
    rowwise %>%
    dplyr::mutate(importations = rpois(1, pop2010/1E7)) %>% 
    ungroup
  dates <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-07-01"), by = "day")
  dummy_imports <- expand.grid(geoid = cty$geoid, date = dates) %>%
    dplyr::left_join(cty, by = c("geoid")) %>%
    dplyr::rename(fips_cty = geoid) %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::select(date, fips_cty, importations)
  return(dummy_imports)
}
#########################
## The final output object needs to be able to be accessed directly
county_importations_total <- distribute_dummy_importations()
