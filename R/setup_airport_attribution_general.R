

# SETUP -------------------------------------------------------------------

# library(tidyverse)
# library(magrittr)
# library(maptools)
# library(rgdal)
# library(ggvoronoi)
# library(raster)
# library(sf)
# library(igraph)
# library(geosphere)
# library(rlist)
#if (!require(gpclib)) install.packages("gpclib", type="source")
#gpclibPermit()         # Seems wacky, shouldn't require this.

# The map projection may not adjusted correctly but the error is minor.


# # Settings -----------------------------------------------------------------
# states_of_interest <- sort(c("CA","NV","WA","OR","AZ"))
# regions_of_interest <- paste("US", states_of_interest, sep = "-")
# year <- "2010"
# regioncode <- "west-coast"
# shapefile_path <- paste0('data/', regioncode, '/shp/','counties_2010_', regioncode, '.shp')

# plot = TRUE
# travelers_threshold <- 60000
# airport_cluster_threshold <- 160 # km


# DATA --------------------------------------------------------------------

# Airport data
airport_data <- read_csv("data/airport-codes.csv", na=c(""," "))

## filter from all airports in region based on number of travelers
big_airports_region <- read_csv(paste0("data/", regioncode, "/airport_monthlymeantravelers.csv")) %>%
  dplyr::rename(iata_code = dest) %>%
  full_join(airport_data, by = c("iata_code")) %>%
  dplyr::filter(travelers > travelers_threshold) %>%
  dplyr::select(iata_code) %>% unlist %>% unname

airports_to_consider <- airport_data %>%
  dplyr::filter(iso_region %in% regions_of_interest) %>%
  dplyr::filter(iata_code %in% big_airports_region) %>%
  tidyr::separate(coordinates, sep = ',', c('coor_lat', 'coor_lon'), convert = TRUE) %>%
  dplyr::mutate(id = seq_along(iata_code))


# Cluster Airports in close proximity -------------------------

## airport edgelist to start dataframe
airnet <- make_full_graph(nrow(airports_to_consider), directed = FALSE, loops = FALSE) %>%
  set_vertex_attr("name", value = airports_to_consider$iata_code)
airedgelist <- data.frame(as_edgelist(airnet, names = TRUE), stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  dplyr::rename(iata1 = X1, iata2 = X2) %>%
  dplyr::left_join(airports_to_consider %>% dplyr::select(iata_code, coor_lat, coor_lon), by = c("iata1" = "iata_code")) %>%
  dplyr::rename(lat1 = coor_lat, lon1 = coor_lon) %>%
  dplyr::left_join(airports_to_consider %>% dplyr::select(iata_code, coor_lat, coor_lon), by = c("iata2" = "iata_code")) %>%
  dplyr::rename(lat2 = coor_lat, lon2 = coor_lon) 

lonlat1 <- airedgelist %>% dplyr::select(lon1, lat1) %>% as.matrix
lonlat2 <- airedgelist %>% dplyr::select(lon2, lat2) %>% as.matrix
dist_km <- distHaversine(lonlat1, lonlat2)/1000

airdf <- airedgelist %>%
  dplyr::mutate(dist_km = dist_km)

clusters <- airdf %>% 
  dplyr::filter(dist_km <= airport_cluster_threshold)

## aggregate clusters -- first pass
clusters_ls <- list()  
clusters_ls <- lapply(unique(clusters$iata1), function(airport){
  proposed_cluster <- clusters %>%
    dplyr::filter(iata1 == airport) 
  proposed_cluster2 <- clusters %>%
    dplyr::filter(iata1 %in% proposed_cluster$iata2)
  proposed_cluster3 <- clusters %>%
    dplyr::filter(iata1 %in% proposed_cluster2$iata2)

  cluster <- sort(unique(c(airport, proposed_cluster$iata2, proposed_cluster2$iata2, proposed_cluster3$iata2)))

  return(cluster)
})

## aggregate clusters -- second pass, and clean up
## 1) in case the clusters are expansive and are more than 2x removed from each other, combine clusters with any overlapping airports; 2) then remove clusters that are subsets of others
rm_ix <- c()

joint_ls <- lapply(1:length(clusters_ls), function(i){
  ix <- c()
  subi <- i:length(clusters_ls)
  for (j in subi[-1]){
    ix <- c(ix, ifelse(any(clusters_ls[[i]] %in% clusters_ls[[j]]), j, 0))
  }
  ix <- ix[ix!=0]

  cluster <- sort(unique(c(clusters_ls[[i]], 
                unlist(flatten(map(ix, function(k){ 
                        return(clusters_ls[[k]])
                      }))))))
  rm_ix <- c(rm_ix, ix)
  return(list(cluster, rm_ix))
})

## identify indexes of clusters that are subsets of others
clusters_ls_cl <- lapply(1:length(joint_ls), 
                        function(i){joint_ls[[i]][[1]]})
remove_indexes <- sort(unique(unlist(lapply(1:length(joint_ls),
                        function(i){joint_ls[[i]][[2]]}))))

## remove clusters that are subsets of others
clusters_ls_cl <- list.remove(clusters_ls_cl, range = remove_indexes)


# Get centroid of airport coordinate clusters -------------------------
cluster_ids <- map_dfr(1:length(clusters_ls_cl), function(i){
  data.frame(iata_code = clusters_ls_cl[[i]], c_id = i, stringsAsFactors = FALSE)
})
clustered_airports <- airports_to_consider %>%
  dplyr::right_join(cluster_ids, by = c("iata_code")) %>%
  dplyr::select(iata_code, c_id, coor_lat, coor_lon) %>%
  group_by(c_id) %>%
  dplyr::summarise(iata_code = paste(iata_code, collapse = "_"), coor_lat = mean(coor_lat), coor_lon = mean(coor_lon)) %>%
  ungroup %>% 
  dplyr::select(iata_code, coor_lat, coor_lon)

## remerge clustered airports with other airports
airports_to_consider_cl <- airports_to_consider %>%
  dplyr::filter(!(iata_code %in% cluster_ids$iata_code)) %>%
  dplyr::select(iata_code, coor_lat, coor_lon) %>%
  bind_rows(clustered_airports)


# ~ Get Shapefile ---------------------------


# shape file at adm1 and adm0 level
loc_map <- rgdal::readOGR(shapefile_path) 
adm0_loc = unionSpatialPolygons(loc_map, loc_map@data$STATEFP)
adm1_loc = unionSpatialPolygons(loc_map, loc_map@data$GEOID)
plot(loc_map)


# Voronoi tesselation by airports
voronoi_tess <- voronoi_polygon(airports_to_consider_cl, x = "coor_lon", y = "coor_lat",
                                outline = adm0_loc)

## change projections of voronoi tesselation to match county shapefiles
crs_shp <- crs(adm1_loc)
reg_loc = unionSpatialPolygons(voronoi_tess, voronoi_tess@data$iata_code)
reg_loc <- rgeos::gBuffer(reg_loc, byid=TRUE, width=0)
#adm1_loc <- rgeos::gBuffer(adm1_loc, byid=TRUE, width=0)

projection(reg_loc) <- crs_shp

airport_attribution <- tribble(~county, ~airport_iata, ~attribution)

for (co in levels(loc_map@data$GEOID)) {
  cksum = 0      # to test if there is no error
  for (iata in voronoi_tess@data$iata_code) {
    if (!is.na(iata)){
      inter <- tryCatch({ 
        raster::intersect(reg_loc[iata], adm1_loc[co])
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
  if (cksum > 1.001) print(paste("ERROR ATTRIBUTION +", cksum, co)) ## positive errors add people. small positive errors are okay

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
  
  if(regioncode == "around_md"){
    warning(paste0("Special warning for airport attribution in ", regioncode, "Manual fix for Kent County, DE was implemented."))

    ## For some reason the remainder of FIPS 10001 is not being appropriately assigned to the BWI catchment area, so do it manually
    airport_attribution <- bind_rows(airport_attribution, data.frame(county = "10001", airport_iata = "BWI", attribution = 1-0.915, countyname = "Kent")) %>%
      dplyr::arrange(county)
  }
  

  counties_with_errors_v2 <- airport_attribution %>% 
    group_by(county) %>%
    summarise(check = sum(attribution)) %>%
    dplyr::filter(round(check,2) != 1)
  
  if (nrow(counties_with_errors_v2)>0){
    warning(paste("county fips", counties_with_errors$county, "were not fixed"))
  }
}

# Save it
write.csv(airport_attribution, file =paste0("data/", regioncode, "/airport_attribution_", yr, ".csv"), row.names=FALSE)


if (plot) {
  airport_map <- ggplot() + 
    geom_point(data = airports_to_consider_cl, aes(coor_lon, coor_lat)) +
    geom_polygon(data = fortify(reg_loc), 
                 aes(long, lat, group = group),
                 alpha = .4, size = .5,  colour = 'red') + 
    theme(legend.position = "none")
  print(airport_map)
}

