

source("R/DataUtils.R")
source("R/CalcHospDeaths_v2.0.R")

library(sf)
library(tidyverse)


##Load the shape file data
md <- st_read("data/maryland/Maryland_Physical_Boundaries__County_Boundaries_Generalized/Maryland_Physical_Boundaries__County_Boundaries_Generalized.shp")
md$county.name <- md$county

##Load the county data
county_dat <- read.csv("data/maryland/geodata.csv")
county_dat$geoid <- as.character(county_dat$geoid)
county_dat$new_pop <- county_dat$pop2010
county_dat$metrop <- as.numeric( substr(county_dat$geoid, 3, 5) )
county_dat$metrop_labels <- md$county.name[match(county_dat$metrop, md$county_fip)]

md$geoid <- county_dat$geoid[match(md$county_fip, county_dat$metrop)]

## Load in the data for each scenario:
# MD mid, Baltimore introduction 1 March 2020
# scn_dat_mid <- load_scenario_sims(scenario_dir="mid-east-coast_NoNPI_sub", 
#                                   keep_compartments = "diffI")

#scn_dat_mid <- load_scenario_sims("AroundMaryland_20200310")
sim_res_name <- "mid-east-coast_NoNPI_sub"
#sim_res_name <- "mid-east-coast_NoNPI"
#sim_res_name <- "WestCoast_test_sims"
scn_dat_mid <- load_scenario_sims(sim_res_name,
                                  keep_compartments = "diffI",
                                  time_filter_high = "2020-05-01")

## Add metro labels (here, county names) to scenario data
scn_dat_mid$metrop_labels <- county_dat$metrop_labels[match(scn_dat_mid$geoid, county_dat$geoid)]
incid_data_mid <- scn_dat_mid %>% rename(incidI=N)
# incid_data_scl <- scn_dat_scl %>% filter(comp=="diffI") %>% rename(incidI=N)
# incid_data_npi <- scn_dat_npi %>% filter(comp=="diffI") %>% rename(incidI=N)

# set parameters for time to hospitalization, time to death, time to discharge
# all are log-normally distributed
time_hosp_pars <- c(1.23, 0.79)
time_disch_pars <- c(log(11.5), log(1.22))
time_death_pars <- c(log(11.25), log(1.15))
time_ICU_pars = c(log(10.5), log((10.5-7)/1.35))
time_ICUdur_pars = c(log(17.46), log(4.044))
time_vent_pars = c(log(10.5), log((10.5-8)/1.35))
mean_inc <- 5.2
dur_inf_shape <- 2
dur_inf_scale <- 3

# set death + hospitalization parameters

p_death <- c(.0025, .005, .01)
p_ICU=0.264
p_vent=0.15
p_hosp_vec=(p_death*10)

p_hosp = p_hosp_vec[3]

#incid_data_mid$time <- as.Date(lubridate::mdy(incid_data_mid$time))
data <- incid_data_mid
sims <- unique(incid_data_mid$sim_num)

time_ <- Sys.time()
hosp_death <- build_hospdeath_par(incid_data_mid, p_hosp, p_death[3], p_vent, p_ICU,
                            time_hosp_pars = c(1.23, 0.79), 
                            time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                            time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                            time_death_pars = c(log(11.25), log(1.15)), 
                            time_disch_pars = c(log(11.5), log(1.22)),
                            time_ICUdur_pars = c(log(17.46), log(4.044)),
                            end_date = "2020-05-01",
                            length_geoid = 5,
                            incl.county=FALSE,
                            cores=5, 
                            run_parallel=TRUE)
time_dur_ <- Sys.time() - time_


source("R/CalcHospDeaths_v2.0.R")
time_ <- Sys.time()
hosp_death2 <- build_hospdeath_par(incid_data_mid, p_hosp, p_death[3], p_vent, p_ICU,
                                  time_hosp_pars = c(1.23, 0.79), 
                                  time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                  time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                  time_death_pars = c(log(11.25), log(1.15)), 
                                  time_disch_pars = c(log(11.5), log(1.22)),
                                  time_ICUdur_pars = c(log(17.46), log(4.044)),
                                  end_date = "2020-05-01",
                                  length_geoid = 5,
                                  incl.county=FALSE,
                                  cores=5, 
                                  run_parallel=TRUE)
time_dur_ <- Sys.time() - time_





write_csv(hosp_death, paste0("model_output/",sim_res_name,"_hosp.csv"))
ob


