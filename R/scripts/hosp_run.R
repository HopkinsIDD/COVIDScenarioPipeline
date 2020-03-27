library(devtools)
library(covidcommon)
library(hospitalization)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(hospitalization)
library(data.table)

set.seed(123456789)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default='config.yml', type='character', help="path to the config file"),
  optparse::make_option(c("-d", "--deathrate"), action="store", default='all', type='character', help="name of the death scenario to run, or 'all' to run all of them"),
  optparse::make_option(c("-s", "--scenario"), action="store", default='None', type='character', help="name of the intervention to run"),
  optparse::make_option(c("-j", "--jobs"), action="store", default='8', type='numeric', help="number of cores used")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opt$c)


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
end_date = lubridate::ymd(config$hospitalization$parameters$end)

# set death + hospitalization parameters
p_death <- as_evaled_expression(config$hospitalization$parameters$p_death)
names(p_death) = config$hospitalization$parameters$p_death_names
p_death_rate <- as_evaled_expression(config$hospitalization$parameters$p_death_rate)
p_ICU <- as_evaled_expression(config$hospitalization$parameters$p_ICU)
p_vent <- as_evaled_expression(config$hospitalization$parameters$p_vent)

data_filename <- paste0("model_output/",config$spatial_setup$setup_name,"_",opt$s)
# config$hospitalization$paths$output_path
cmd <- opt$d
ncore <- opt$j

# Verify that the cmd maps to a known p_death value
if (cmd == "all") {
  cmd <- names(p_death) # Run all of the configured hospitalization scenarios
} else if (is.na(p_death[cmd]) || is.null(p_death[cmd]) || p_death[cmd] == 0) {
  message(paste("Invalid cmd argument:", cmd, "did not match any of the named args in", p_death, "\n"))
  quit("yes", status=1)
}

county_dat <- read.csv(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata))
county_dat$geoid <- as.character(county_dat$geoid)
county_dat$new_pop <- county_dat$pop2010
#county_dat <- make_metrop_labels(county_dat)
target_geo_ids <- county_dat$geoid[county_dat$include_in_report]

cat(paste(data_filename, "\n"))
for (cmd0 in cmd) {
  p_hosp <- p_death[cmd0]*10
  cat(paste("Running hospitalization scenario: ", cmd0, "with p_hosp", p_hosp, "\n"))
  res_npi3 <- build_hospdeath_par(p_hosp = p_hosp,
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
                                  scenario_name = paste(cmd0,"death",sep="_"),
                                  target_geo_ids=target_geo_ids
  )
}
