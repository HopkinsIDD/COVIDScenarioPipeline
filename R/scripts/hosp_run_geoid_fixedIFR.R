library(devtools)
library(covidcommon)
library(hospitalization)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)
library(parallel)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-d", "--deathrate"), action="store", default='all', type='character', help="name of the death scenario to run, or 'all' to run all of them"),
  optparse::make_option(c("-s", "--scenario"), action="store", default='all', type='character', help="name of the intervention to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=detectCores(), type='numeric', help="number of cores used"),
  optparse::make_option(c("-p", "--path"), action="store", default="COVIDScenarioPipeline", type='character', help="path to the COVIDScenarioPipeline directory")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opt$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

# set parameters for time to hospitalization, time to death, time to discharge
time_symp_pars <- as_evaled_expression(config$hospitalization$parameters$time_symp)
time_hosp_pars <- as_evaled_expression(config$hospitalization$parameters$time_hosp)
time_disch_pars <- as_evaled_expression(config$hospitalization$parameters$time_disch)
time_death_pars <- as_evaled_expression(config$hospitalization$parameters$time_death)
time_ICU_pars <- as_evaled_expression(config$hospitalization$parameters$time_ICU)
time_ICUdur_pars <- as_evaled_expression(config$hospitalization$parameters$time_ICUdur)
time_vent_pars <- as_evaled_expression(config$hospitalization$parameters$time_vent)

# set death + hospitalization parameters
# read in file
prob_dat <- readr::read_csv(paste(opt$p,"data","geoid-params.csv",sep='/'))

#removing leading 0s for merge with simulation data (this is hacky...)
prob_dat$geoid <- ifelse(substr(prob_dat$geoid, 1, 1)=="0", substr(prob_dat$geoid, 2, 5), prob_dat$geoid)

p_death <- as_evaled_expression(config$hospitalization$parameters$p_death)
names(p_death) = config$hospitalization$parameters$p_death_names
p_hosp_inf <- as_evaled_expression(config$hospitalization$parameters$p_hosp_inf)
names(p_hosp_inf) = config$hospitalization$parameters$p_death_names

if (length(p_death)!=length(p_hosp_inf)) {
  stop("Number of IFR and p_hosp_inf values do not match")
}

config$hospitalization$paths$output_path
cmd <- opt$d
scenario <- opt$s
ncore <- opt$j

# Verify that the cmd maps to a known scaling factor value
if (cmd == "all") {
   cmd <- names(p_death) # Run all of the scaling factor scenarios
 } else if (is.na(p_death[cmd]) || is.null(p_death[cmd]) || p_death[cmd] == 0) {
   message(paste("Invalid cmd argument:", cmd, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
   quit("yes", status=1)
 }
 if (scenario == "all" ) {
   scenario <- config$interventions$scenarios
 } else if (!(scenario %in% config$interventions$scenarios)) {
   message(paste("Invalid scenario argument:", scenario, "did not match any of the named args in", paste(config$interventions$scenario, collapse = ", ") , "\n"))
   quit("yes", status=1)
 }

print(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata))
county_dat <- read.csv(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata))
print(county_dat)
county_dat$geoid <- as.character(county_dat$geoid)
county_dat$new_pop <- county_dat[[config$spatial_setup$popnodes]]
#county_dat <- make_metrop_labels(county_dat)

for (scn0 in scenario) {
  for (cmd0 in cmd) {
    data_filename <- paste0("model_output/",config$name,"_",scn0)
    cat(paste(data_filename, "\n"))
    cat(paste("Running hospitalization scenario: ", cmd0, "with IFR", p_death[cmd0], "\n"))
    res_npi3 <- build_hospdeath_geoid_fixedIFR_par(prob_dat=prob_dat,
                                                   p_death= p_death[cmd0],
                                                   p_hosp_inf = p_hosp_inf[cmd0],
                                                   time_hosp_pars=time_hosp_pars,
                                                   time_death_pars=time_death_pars,
                                                   time_disch_pars=time_disch_pars,
                                                   time_ICU_pars = time_ICU_pars,
                                                   time_vent_pars = time_vent_pars,
                                                   time_ICUdur_pars = time_ICUdur_pars,
                                                   cores = ncore,
                                                   data_filename = data_filename,
                                                   scenario_name = paste(cmd0,"death",sep="_")
    )
  }
}

