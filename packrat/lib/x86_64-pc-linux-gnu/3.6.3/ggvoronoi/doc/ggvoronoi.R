## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggmap)
library(mapproj)

## ----1a------------------------------------------------------------------
library(ggvoronoi)
set.seed(45056)
x <- sample(1:200,100)
y <- sample(1:200,100)
points <- data.frame(x, y,
                     distance = sqrt((x-100)^2 + (y-100)^2))
circle <- data.frame(x = 100*(1+cos(seq(0, 2*pi, length.out = 2500))),
                     y = 100*(1+sin(seq(0, 2*pi, length.out = 2500))),
                     group = rep(1,2500))

ggplot(points) +
  geom_point(aes(x,y,color=distance)) +
  geom_path(data=circle,aes(x,y,group=group))

## ----1b------------------------------------------------------------------
ggplot(points) +
  geom_voronoi(aes(x,y,fill=distance))

## ----1c------------------------------------------------------------------
ggplot(points,aes(x,y)) +
  stat_voronoi(geom="path") +
  geom_point()

## ----1d------------------------------------------------------------------
ggplot(data=points, aes(x=x, y=y, fill=distance)) + 
  geom_voronoi(outline = circle)

## ----1e------------------------------------------------------------------
ggplot(points,aes(x,y)) +
  geom_voronoi(aes(fill=distance),outline=circle,
               color="#4dffb8",size=.125) +
  scale_fill_gradient(low="#4dffb8",high="black",guide=F) +
  theme_void() +
  coord_fixed()

## ----2a,message=F,eval=F-------------------------------------------------
#  library(ggmap)
#  
#  oxford_map <- get_googlemap(center = c(-84.7398373,39.507306),zoom = 15,key="your_api_key")

## ----2b,message=F--------------------------------------------------------
bounds <- as.numeric(attr(oxford_map,"bb"))

map <-
  ggmap(oxford_map,base_layer = ggplot(data=oxford_bikes,aes(x,y))) +
        xlim(-85,-84)+ylim(39,40)+
        coord_map(ylim=bounds[c(1,3)],xlim=bounds[c(2,4)]) +
        theme_minimal() +
        theme(axis.text=element_blank(),
              axis.title=element_blank())

## ----2c------------------------------------------------------------------
map + geom_path(stat="voronoi",alpha=.085,size=.25) +
      geom_point(color="blue",size=.25)

## ----2d------------------------------------------------------------------
ox_diagram <- voronoi_polygon(oxford_bikes,x="x",y="y")

## ----2e------------------------------------------------------------------
library(sp)
mac_joes <- SpatialPointsDataFrame(cbind(long=-84.7418,lat=39.5101),
                                   data=data.frame(name="Mac & Joes"))

## ----2f------------------------------------------------------------------
mac_joes %over% ox_diagram

## ----2g,message=FALSE----------------------------------------------------
map + geom_path(data=fortify_voronoi(ox_diagram),aes(x,y,group=group),alpha=.1,size=1) +
      coord_map(xlim=c(-84.746,-84.739),ylim=c(39.508,39.514)) +
      geom_point(data=data.frame(mac_joes),aes(long,lat),color="red",size=2) +
      geom_point(size=1.5,stroke=1, shape=21,color="black",fill="white") +
      geom_point(data=mac_joes %over% ox_diagram,aes(x,y),color="blue",size=2)

## ----3a,message=FALSE----------------------------------------------------
library(dplyr)

california <- map_data("state") %>% filter(region == "california")
ncdc.cali <- ncdc_locations %>% filter(state=="CA")

## ----3a2-----------------------------------------------------------------
cali_map <-
  ggplot(data=ncdc.cali,aes(x=long,y=lat)) +
      scale_fill_gradientn("Elevation", 
          colors=c("seagreen","darkgreen","green1","yellow","gold4", "sienna"),
          values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
      scale_color_gradientn("Elevation", 
          colors=c("seagreen","darkgreen","green1","yellow","gold4", "sienna"),
          values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
      coord_quickmap() + 
      theme_minimal() +
      theme(axis.text=element_blank(),
            axis.title=element_blank())

cali_map +
      geom_point(aes(color=elev),size=.01) +
      geom_path(data=california,aes(long,lat,group=group),color="black")

## ----3b------------------------------------------------------------------
cali_map +
  geom_voronoi(aes(fill=elev),outline=california)

## ----3c------------------------------------------------------------------
california <- map_data("state") %>% filter(region == "california")

ncdc.cali <- ncdc_locations %>% filter(state=="CA")

cali.voronoi <- voronoi_polygon(data=ncdc.cali,
                                x="long",y="lat",
                                outline=california)

## ----3d------------------------------------------------------------------
cali.voronoi <- fortify_voronoi(cali.voronoi)

ggplot(cali.voronoi) +
  geom_polygon(aes(x=long.x, y=lat.x ,fill=elev,
                   group=group, color=elev), size=0) + 
  scale_fill_gradientn("Elevation", 
                       colors=c("seagreen","darkgreen","green1",
                                "yellow","gold4", "sienna"), 
                       values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  scale_color_gradientn("Elevation", 
                        colors=c("seagreen","darkgreen","green1",
                                 "yellow","gold4", "sienna"), 
                        values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  coord_quickmap() + 
  theme_minimal() +
  theme(axis.text=element_blank(),
        axis.title=element_blank())

