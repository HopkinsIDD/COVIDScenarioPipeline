library(devtools)
devtools::install_local("./R/pkgs/hospitalization", force=TRUE)
library(hospitalization)
library(readr)

set.seed(123456789)

# set parameters for time to hospitalization, time to death, time to discharge
time_hosp_pars <- c(1.23, 0.79)
time_disch_pars <- c(log(11.5), log(1.22))
time_death_pars <- c(log(11.25), log(1.15))
time_ICU_pars <- c(log(8.25), log(2.2))
time_ICUdur_pars <- c(log(17.46), log(4.044))
time_vent_pars <- c(log(10.5), log((10.5-8)/1.35))
mean_inc <- 5.2
dur_inf_shape <- 2
dur_inf_scale <- 3

# set death + hospitalization parameters
p_death <- c(.0025, .005, .01)
p_ICU <- 0.264
p_vent <- 0.15

args <- commandArgs(trailingOnly=TRUE)
data_filename <- args[1]
cmd <- args[2]
ncore = as.numeric(args[3])
if(length(args) == 0){
  data_filename <- "model_output/mid-west-coast-AZ-NV_NoNPI"
  cmd <- "high"
  ncore <- 1
}

#TODO (jwills): make this geodata file into a CLI arg
county_dat <- read.csv("data/west-coast-AZ-NV/geodata.csv")
county_dat$geoid <- as.character(county_dat$geoid)
county_dat$new_pop <- county_dat$pop2010
#county_dat <- make_metrop_labels(county_dat)
target_geo_ids <- county_dat$geoid[county_dat$stateUSPS=="CA"]

cat(paste(data_filename,"\n"))
res_npi3 <- build_hospdeath_par(p_hosp = p_death[3]*10,
                                p_death = .1,
                                p_vent = p_vent,
                                p_ICU = p_ICU,
                                p_hosp_type = "gamma",
                                time_hosp_pars=time_hosp_pars,
                                time_death_pars=time_death_pars,
                                time_disch_pars=time_disch_pars,
                                time_ICU_pars = time_ICU_pars,
                                time_vent_pars = time_vent_pars,
                                time_ICUdur_pars = time_ICUdur_pars,
                                end_date = "2020-10-01",
                                cores = ncore,
                                data_filename = data_filename,
                                scenario_name = paste(cmd,"death",sep="_"),
                                target_geo_ids=target_geo_ids
)
