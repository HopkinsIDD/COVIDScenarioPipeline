library(tidyverse)
library(magrittr)
library(maptools)
library(rgdal)
library(ggvoronoi)
#if (!require(gpclib)) install.packages("gpclib", type="source")
#gpclibPermit()         # Seems wacky, shouldn't require this.

# The projection is not adjusted but the error is minor.

plot = FALSE
airport_cn_data <- read_csv("data/airport-codes.csv")
airports_to_consider <- read_csv("data/china_airports_intravel.csv")

airport_cn_data %<>% filter(iata_code %in% airports_to_consider$`Dep Airport Code`) %>%
  separate(coordinates, sep = ',', c('coor_lat', 'coor_lon'), convert = TRUE)

airport_missing <- airports_to_consider %>% 
  filter(!(.$`Dep Airport Code` %in% airport_cn_data$iata_code))

print(paste('WARNING, missing ', nrow(airport_missing),  'airports'))

# China shape file at adm1 and adm0 level
china_map <- rgdal::readOGR('data/cn_admbnda_adm1/chn_admbnda_adm1_ocha.shp')
adm0_china = unionSpatialPolygons(china_map, china_map@data$ADM0_EN)
adm1_china = unionSpatialPolygons(china_map, china_map@data$ADM1_EN)

# Voronoi tesselation by airports
voronoi_tess <- voronoi_polygon(airport_cn_data,x = "coor_lon", y = "coor_lat",
                                outline = adm0_china)

tri_china = unionSpatialPolygons(voronoi_tess, voronoi_tess@data$iata_code)

if (plot){
  airport_cn_map <- ggplot() + 
    geom_polygon(data = fortify(voronoi_tess), aes(long, lat, group = group),
                 alpha = .3, size = .7,  colour = 'brown1', fill = 'beige') +
    geom_polygon(data = adm1_china, aes(long, lat, group = group),
                 alpha = .4,size = .4,colour = 'darkmagenta', fill=NA) +
    geom_polygon(data = adm0_china, aes(long, lat, group = group),
                 alpha = 1,size = .7,colour = 'black', fill=NA) +
    geom_point(data = airport_cn_data, aes(coor_lon, coor_lat), color = 'darkblue') +
    theme(legend.position = "none")
  print(airport_cn_map)
}

airport_attribution <- tribble(~Province, ~airport_iata, ~attribution)

for (prov in levels(china_map@data$ADM1_EN)) {
  cksum = 0      # to test if there is no error
  for (iata in voronoi_tess@data$iata_code) {
    if (!is.na(iata)){
      inter <- raster::intersect(tri_china[iata], adm1_china[prov])
      if (!is.null(inter)){
        percent_to_iata = raster::area(inter)/raster::area(adm1_china[prov])
        cksum  = cksum + percent_to_iata
        airport_attribution <-add_row(airport_attribution, Province = prov, 
                                      airport_iata = iata, attribution = percent_to_iata)
        #print(paste(prov, "intersect with",iata, "at", percent_to_iata))
      }
    }
  }
  if (cksum < 0.999) print(paste("ERROR ATTRIBUTION -", cksum, prov))
  if (cksum > 1.001) print(paste("ERROR ATTRIBUTION +", cksum, prov))
}

write.csv(airport_attribution, file ='data/airport_attribution.csv', row.names=FALSE)


if (plot) {
  airport_cn_map <- ggplot() + 
    geom_point(data = airport_cn_data, aes(coor_lon, coor_lat)) +
    geom_polygon(data = fortify(voronoi_tess), 
                 aes(long, lat, group = group),
                 alpha = .4, size = .5,  colour = 'red') +
    #geom_polygon(data = adm1_china, aes(long, lat, group = group),
    #             alpha = .4,size = .2,colour = 'blue') + 
  geom_polygon(data = adm1_china["Beijing Municipality"], aes(long, lat, group = group),
               alpha = .4,size = .2, fill = 'darkred') +
    #geom_polygon(data = adm1_china["Xinjiang Uygur Autonomous Region"], aes(long, lat, group = group),
    #             alpha = .4,size = .2, fill = 'brown') +
    geom_polygon(data = tri_china['CDE'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_china['PEK'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_china['PKX'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_china['NAY'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    theme(legend.position = "none")
  print(airport_cn_map)
}

