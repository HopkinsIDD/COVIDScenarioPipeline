library(devtools)
library(covidcommon)
library(hospitalization)
library(readr)

set.seed(123456789)

# set parameters for time to hospitalization, time to death, time to discharge
time_hosp_pars <- as_evaled_expression(config$hospitalization$parameters$time_hosp)
time_disch_pars <- as_evaled_expression(config$hospitalization$parameters$time_disch)
time_death_pars <- as_evaled_expression(config$hospitalization$parameters$time_death)
time_ICU_pars <- as_evaled_expression(config$hospitalization$parameters$time_ICU)
time_ICUdur_pars <- as_evaled_expression(config$hospitalization$parameters$time_ICUdur)
time_vent_pars <- as_evaled_expression(config$hospitalization$parameters$time_vent)
mean_inc <- as_evaled_expression(config$hospitalization$parameters$mean_inc)
dur_inf_shape <- as_evaled_expression(config$hospitalization$parameters$inf_shape)
dur_inf_scale <- as_evaled_expression(config$hospitalization$parameters$inf_scale)
end_date = config$hospitalization$parameters$end

# set death + hospitalization parameters
p_death <- as_evaled_expression(config$hospitalization$parameters$p_death)
p_death_rate <- as_evaled_expression(config$hospitalization$parameters$p_death_rate)
p_ICU <- as_evaled_expression(config$hospitalization$parameters$p_ICU)
p_vent <- as_evaled_expression(config$hospitalization$parameters$p_vent)

args <- commandArgs(trailingOnly=TRUE)
data_filename <- args[1]
cmd <- args[2]
ncore = as.numeric(args[3])
if(length(args) == 0){
  data_filename <- "model_output/mid-west-coast-AZ-NV_NoNPI"
  cmd <- "high"
  ncore <- 1
}
names(p_death) = c('low','med','high')

#TODO (jwills): make this geodata file into a CLI arg
county_dat <- read.csv(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata))
county_dat$geoid <- as.character(county_dat$geoid)
county_dat$new_pop <- county_dat$pop2010
#county_dat <- make_metrop_labels(county_dat)
target_geo_ids <- county_dat$geoid[county_dat$stateUSPS=="CA"]

cat(paste(data_filename,"\n"))
res_npi3 <- build_hospdeath_par(p_hosp = p_death[cmd]*10,
                                p_death = p_death_rate,
                                p_vent = p_vent,
                                p_ICU = p_ICU,
                                time_hosp_pars=time_hosp_pars,
                                time_death_pars=time_death_pars,
                                time_disch_pars=time_disch_pars,
                                time_ICU_pars = time_ICU_pars,
                                time_vent_pars = time_vent_pars,
                                time_ICUdur_pars = time_ICUdur_pars,
                                end_date = end_date,
                                cores = ncore,
                                data_filename = data_filename,
                                scenario_name = paste(cmd,"death",sep="_"),
                                target_geo_ids=target_geo_ids
)
