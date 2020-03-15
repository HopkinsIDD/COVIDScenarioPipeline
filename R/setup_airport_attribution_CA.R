

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(maptools)
library(rgdal)
library(ggvoronoi)
#if (!require(gpclib)) install.packages("gpclib", type="source")
#gpclibPermit()         # Seems wacky, shouldn't require this.

# The projection is not adjusted but the error is minor.


# OPTIONS -----------------------------------------------------------------

shapefile_path <- 'data/ca/california-counties-shp/california-counties.shp' #CA
#shapefile_path <- 'data/cn_admbnda_adm1/chn_admbnda_adm1_ocha.shp' #China

plot = TRUE


# DATA --------------------------------------------------------------------

# Airport data
airport_data <- read_csv("data/airport-codes.csv", na=c(""," "))
airports_to_consider <- read_csv("data/ca/ca_airports.csv")

airport_data %<>% filter(!is.na(iata_code)) %>%
  separate(coordinates, sep = ',', c('coor_lat', 'coor_lon'), convert = TRUE) %>%
  filter(iso_country=="US" & type!="closed")

airport_missing <- airports_to_consider %>% 
  filter(!(.$arr_airport %in% airport_data$iata_code))

print(paste('WARNING, missing ', nrow(airport_missing),  'airports'))



# ~ Get Shapefile ---------------------------------------------------------


# shape file at adm1 and adm0 level
loc_map <- rgdal::readOGR(shapefile_path) # California
adm0_loc = unionSpatialPolygons(loc_map, loc_map@data$STATEFP)
adm1_loc = unionSpatialPolygons(loc_map, loc_map@data$GEOID)
plot(loc_map)


# Voronoi tesselation by airports
voronoi_tess <- voronoi_polygon(airport_data, x = "coor_lon", y = "coor_lat",
                                outline = adm0_loc)

tri_loc = unionSpatialPolygons(voronoi_tess, voronoi_tess@data$iata_code)

airport_attribution <- tribble(~county, ~airport_iata, ~attribution)

for (co in levels(loc_map@data$GEOID)) {
  cksum = 0      # to test if there is no error
  for (iata in voronoi_tess@data$iata_code) {
    if (!is.na(iata)){
      inter <- raster::intersect(tri_loc[iata], adm1_loc[co])
      if (!is.null(inter)){
        percent_to_iata = raster::area(inter)/raster::area(adm1_loc[co])
        cksum  = cksum + percent_to_iata
        airport_attribution <-add_row(airport_attribution, county = co, 
                                      airport_iata = iata, attribution = percent_to_iata)
        #print(paste(co, "intersect with",iata, "at", percent_to_iata))
      }
    }
  }
  if (cksum < 0.999) print(paste("ERROR ATTRIBUTION -", cksum, co))
  if (cksum > 1.001) print(paste("ERROR ATTRIBUTION +", cksum, co))
}


airport_attribution <- left_join(airport_attribution, data.frame(GEOID=loc_map$GEOID, countyname=loc_map$NAME), by=c("county"="GEOID"))

# Remove extra 0 at beginning of county
airport_attribution$county <- substr(airport_attribution$county,2,6)

# Get metro area from counties
airport_attribution <- get_metro_labels(airport_attribution %>% mutate(county = as.character(substr(county,3,6))))

# Save it
write.csv(airport_attribution, file ='data/ca/airport_attribution.csv', row.names=FALSE)


if (plot) {
  airport_cn_map <- ggplot() + 
    geom_point(data = airport_data, aes(coor_lon, coor_lat)) +
    geom_polygon(data = fortify(voronoi_tess), 
                 aes(long, lat, group = group),
                 alpha = .4, size = .5,  colour = 'red') +
    #geom_polygon(data = adm1_loc, aes(long, lat, group = group),
    #             alpha = .4,size = .2,colour = 'blue') + 
  geom_polygon(data = adm1_loc["Beijing Municipality"], aes(long, lat, group = group),
               alpha = .4,size = .2, fill = 'darkred') +
    #geom_polygon(data = adm1_loc["Xinjiang Uygur Autonomous Region"], aes(long, lat, group = group),
    #             alpha = .4,size = .2, fill = 'brown') +
    geom_polygon(data = tri_loc['CDE'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_loc['PEK'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_loc['PKX'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_loc['NAY'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    theme(legend.position = "none")
  print(airport_cn_map)
}

