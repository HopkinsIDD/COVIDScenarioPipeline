## main code for code to distribute importations to airports

## The purpose of this code is to take in determine how air importations to a specific US region will be distributed to surrounding counties to seed the SEIR epidemic model. 

source("R/census_api_key.R") ## user needs to make a census API key and put in a separate R file assigned to variable "census_api_key". This file will not be committed to git

# Setup -------------------------
library(tidyverse)
library(tidycensus)
library(sf)
library(tidyverse)
library(magrittr)
library(maptools)
library(rgdal)
library(ggvoronoi)
library(raster)
library(igraph)
library(geosphere)
library(rlist)

# Change these settings -------------------------

## run from COVIDScenarioPipeline/

states_of_interest <- sort(c("CA","NV","WA","OR","AZ"))
regions_of_interest <- paste("US", states_of_interest, sep = "-")
regioncode <- "west-coast-AZ-NV"
yr <- 2010 ## 2010 Census was the last time county commuting data was released. This data is used to do a population-weighted distribution of importations to airports into surrounding counties.
shapefile_path <- paste0('data/', regioncode, '/shp/','counties_2010_', regioncode, '.shp')

plot = TRUE
travelers_threshold <- 60000 ## airports must meet this average number of monthly travelers to be included as a potential importer of infection
airport_cluster_threshold <- 160 # units: km. Airports that are separated by Haversine distance (distance as the crow flies, essentially) under this threshold are clustered together and their centroid is used to determine the area where air importations may be distributed

# MAIN ------------------------------------------

## pull population data and shapefiles for region
source(paste0("R/pull_county_pops_", yr, "_general.R"))

## identify the counties that may receive importations into a given airport
source("R/setup_airport_attribution_general.R")

## modify importation parameters to account for airport clusters
source("R/setup_nb_params_airport_clustering.R")
