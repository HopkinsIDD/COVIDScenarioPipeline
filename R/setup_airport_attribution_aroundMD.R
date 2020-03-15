

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(maptools)
library(rgdal)
library(ggvoronoi)
library(raster)
library(sf)
#if (!require(gpclib)) install.packages("gpclib", type="source")
#gpclibPermit()         # Seems wacky, shouldn't require this.

# The projection is adjusted but may need additional checks because there was
# a problem with FIPS 10001 that had to be fixed manually 
# see below `counties_with_errors`


# OPTIONS -----------------------------------------------------------------
states_of_interest <- sort(c("NJ","DE","MD","VA","DC","PA"))
regions_of_interest <- paste("US", states_of_interest, sep = "-")
year <- "2010"
shapefile_path <- paste0("data/around_md/shp_around_md/counties_", year, "_around_md.shp")

plot = TRUE
travelers_threshold <- 60000


# DATA --------------------------------------------------------------------

## All airport data
airport_data <- read_csv("data/airport-codes.csv", na=c(""," "))

## filter from all airports in region based on number of travelers
big_airports_around_md <- read_csv("data/around_md/airport_monthlymeantravelers_around_md.csv") %>%
  dplyr::rename(iata_code = dest) %>%
  full_join(airport_data, by = c("iata_code")) %>%
  dplyr::filter(travelers > travelers_threshold) %>%
  dplyr::select(iata_code) %>% unlist

if(!file.exists(paste0("data/around_md/airports_", year, "_around_md.csv"))){
  airports_to_consider <- airport_data %>%
    dplyr::filter(iso_region %in% regions_of_interest) %>%
    dplyr::filter(iata_code %in% big_airports_around_md) %>%
    tidyr::separate(coordinates, sep = ',', c('coor_lat', 'coor_lon'), convert = TRUE) 
  write_csv(airports_to_consider, paste0("data/around_md/airports_", year, "_around_md.csv"))
}

airports_to_consider <- read_csv(paste0("data/around_md/airports_", year, "_around_md.csv"))


# ~ Get Shapefile ---------------------------------------------------------

# shape file at adm1 and adm0 level
loc_map <- rgdal::readOGR(shapefile_path) 
adm0_loc = unionSpatialPolygons(loc_map, loc_map@data$STATEFP)
adm1_loc = unionSpatialPolygons(loc_map, loc_map@data$GEOID)
plot(loc_map)

# Voronoi tesselation by airports
voronoi_tess <- voronoi_polygon(airports_to_consider, x = "coor_lon", y = "coor_lat",
                                outline = adm0_loc)

## change projections of voronoi tesselation to match county shapefiles
crs_shp <- crs(adm1_loc)
tri_loc = unionSpatialPolygons(voronoi_tess, voronoi_tess@data$iata_code)
tri_loc <- rgeos::gBuffer(tri_loc, byid=TRUE, width=0)
#adm1_loc <- rgeos::gBuffer(adm1_loc, byid=TRUE, width=0)

projection(tri_loc) <- crs_shp

airport_attribution <- tribble(~county, ~airport_iata, ~attribution)

for (co in levels(loc_map@data$GEOID)) {
  cksum = 0      # to test if there is no error
  for (iata in voronoi_tess@data$iata_code) {
    if (!is.na(iata)){
      inter <- tryCatch({ 
        raster::intersect(tri_loc[iata], adm1_loc[co])
      }, error =function(err){
        NULL
      })
      if(!is.null(inter)){
        if(length(inter@polygons)>0){
          percent_to_iata = raster::area(inter)/raster::area(adm1_loc[co])
          cksum  = cksum + percent_to_iata
          airport_attribution <-add_row(airport_attribution, county = co, 
                                        airport_iata = iata, attribution = percent_to_iata)
          #print(paste(co, "intersect with",iata, "at", percent_to_iata))
        }
      }
    }
  }
  if (cksum < 0.999) print(paste("ERROR ATTRIBUTION -", cksum, co))
  if (cksum > 1.001) print(paste("ERROR ATTRIBUTION +", cksum, co)) ## all errors are positive and less than 5

}


## for loop over voronoi_tess@data$iata_code introduced duplicates (1 part of a county per adjacenet airport)
if(nrow(distinct(airport_attribution, county, airport_iata))!=nrow(distinct(airport_attribution, county, airport_iata, attribution))){
  warning("There are duplicate county-airport pairs. Please check the data again.")
} 

airport_attribution <- distinct(airport_attribution)
airport_attribution <- left_join(airport_attribution, data.frame(GEOID=loc_map$GEOID, countyname=loc_map$NAME), by=c("county"="GEOID"))

counties_with_errors <- airport_attribution %>% 
  group_by(county) %>%
  summarise(check = sum(attribution)) %>%
  dplyr::filter(round(check,2) != 1)

if (nrow(counties_with_errors)>0){
  warning(paste("county fips", counties_with_errors$county, "may have errors"))
  
  ## For some reason the remainder of FIPS 10001 is not being appropriately assigned to the BWI catchment area, so do it manually
  airport_attribution <- bind_rows(airport_attribution, data.frame(county = "10001", airport_iata = "BWI", attribution = 1-0.915, countyname = "Kent")) %>%
    dplyr::arrange(county)

  counties_with_errors_v2 <- airport_attribution %>% 
    group_by(county) %>%
    summarise(check = sum(attribution)) %>%
    dplyr::filter(round(check,2) != 1)
  
  if (nrow(counties_with_errors_v2)>0){
    warning(paste("county fips", counties_with_errors$county, "were not fixed"))
  }
}

# Save it

write.csv(airport_attribution, file =paste0("data/around_md/airport_attribution_", year, "_around_md.csv"), row.names=FALSE)


if (plot) {
  airport_map <- ggplot() + 
    geom_point(data = airports_to_consider, aes(coor_lon, coor_lat)) +
    geom_polygon(data = fortify(tri_loc), 
                 aes(long, lat, group = group),
                 alpha = .4, size = .5,  colour = 'red') +
    geom_polygon(data = adm1_loc["10001"] , aes(long, lat, group = group),
                alpha = .4,size = .2,colour = 'blue') + ## the problem county should be attributed to BWI & PHL
    theme(legend.position = "none")
  print(airport_map)
}

