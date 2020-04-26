## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=3, fig.height=3, fig.path='figures/map-', warning=FALSE)

## ---- message = FALSE---------------------------------------------------------
library(mapdata)
library(ggplot2)

jp <- ggplot2::map_data('world2', 'japan')
class(jp)
head(jp)
ggplot(jp, aes(x = long, y = lat, group = group)) +
  geom_polygon()

## ---- message = FALSE---------------------------------------------------------
library(ggfortify)
jp <-  map('world2', 'japan', plot = FALSE, fill = TRUE)
class(jp)
autoplot(jp)

p <- autoplot(jp, geom = 'polygon', fill = 'subregion') + 
  theme(legend.position="none")
p

## ---- message = FALSE---------------------------------------------------------
cities <- get('world.cities')
cities <- cities[cities$country.etc == 'Japan', ]
head(cities)

p + geom_point(data = cities, aes(x = long, y = lat),
               colour = 'blue', size = 0.1)

## ---- message = FALSE---------------------------------------------------------
p + geom_point(data = cities, colour = 'blue', size = 0.1)

## ---- message = FALSE---------------------------------------------------------
library(sp)
df <- data.frame(long = c(139.691704, 135.519711),
                 lat = c(35.689521, 34.686316),
                 label = c('Tokyo', 'Osaka'),
                 population = c(1335, 886))
coordinates(df) <- ~ long + lat
class(df)
autoplot(df, p = p, colour = 'red', size = 10)

## ---- message = FALSE---------------------------------------------------------
autoplot(df, p = p, colour = 'red', size = 'population') +
  scale_size_area()

## ---- message = FALSE, eval = FALSE-------------------------------------------
#  library(ggmap)
#  bbox <- c(130.0, 30.0, 145.0, 45.0)
#  map <- get_openstreetmap(bbox = bbox, scale = 47500000)
#  p <- ggmap(map)
#  autoplot(df, p = p, colour = 'red', size = 'population') +
#    scale_size_area() +
#    theme(legend.justification = c(1, 0), legend.position = c(1, 0))

