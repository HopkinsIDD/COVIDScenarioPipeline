
library(tidyverse)
source("R/DataUtils.R")
source("R/CalcHospDeaths.R")




# PROBABILITIES -----------------------------------------------------------

# Probs
p_death <- c(.001, .0025, .01)
p_hosp <- p_death*10



# TIME TO EVENTS ----------------------------------------------------------

# Time to Hospitalization
time_hosp_pars <- c(1.23, 0.79)
plot(density(rlnorm(1000, meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2])), main="Time to Hospitalization")
qlnorm(c(.025, .5, .975), time_hosp_pars[1], time_hosp_pars[2])
mean(rlnorm(1000, meanlog=time_hosp_pars[1], sdlog=time_hosp_pars[2]))

# Time to Discharge
time_disch_pars <- c(log(11.5), log(1.22))
plot(density(rlnorm(1000, meanlog=time_disch_pars[1], sdlog=time_disch_pars[2])), main="Time to Discharge")
qlnorm(c(.025, .5, .975), time_disch_pars[1], time_disch_pars[2])
mean(rlnorm(1000, meanlog=time_disch_pars[1], sdlog=time_disch_pars[2]))

# Time to Death
time_death_pars <- c(log(11.25), log(1.15))
plot(density(rlnorm(1000, meanlog=time_death_pars[1], sdlog=time_death_pars[2])), main="Time to Death")
qlnorm(c(.025, .5, .975), time_death_pars[1], time_death_pars[2])
mean(rlnorm(1000, meanlog=time_death_pars[1], sdlog=time_death_pars[2]))

# 11.2 / 25
# time_death_pars <- c(25, )
# plot(density(rgamma(1000, shape=time_death_pars[1], scale=time_death_pars[2])), main="Time to Death")
# qgamma(c(.025, .5, .975), shape=time_death_pars[1], scale=time_death_pars[2])
# mean(rgamma(1000, shape=time_death_pars[1], scale=time_death_pars[2]))



# SAN FRAN ----------------------------------------------------------------



# SAN FRANCISCO RESULTS - LOW ---------------------------------------------------

sims_data <- load_scenario_sims(scenario_dir="SanFrancisco/low") %>% 
    mutate(time=parse_date(time)) %>% 
    mutate(comp=recode(comp, R="I3", cumI="R", diffI="cumI", `7.0`="diffI"))
incid_data <- sims_data %>% filter(comp=="diffI") %>% rename(incidI=N)


# Get San Fran - low - low hosp/mortality
sim_hospdeath_lowlow <- build_hospdeath_fullsim(incid_data, p_hosp=(p_death[1]*10), p_death=p_death[1], time_hosp_pars, time_death_pars, time_disch_pars)

# Get San Fran - low  - mid hosp/mortality
sim_hospdeath_lowmid <- build_hospdeath_fullsim(incid_data, p_hosp=(p_death[2]*10), p_death=p_death[2], time_hosp_pars, time_death_pars, time_disch_pars)

# Get San Fran - low  - high hosp/mortality
sim_hospdeath_lowhigh <- build_hospdeath_fullsim(incid_data, p_hosp=(p_death[3]*10), p_death=p_death[3], time_hosp_pars, time_death_pars, time_disch_pars)

sanfran_hosp_low <- bind_rows(sim_hospdeath_lowlow %>% mutate(p_death=p_death[1], scenario="B"), 
                              sim_hospdeath_lowmid %>% mutate(p_death=p_death[2], scenario="B"),
                              sim_hospdeath_lowhigh %>% mutate(p_death=p_death[3], scenario="B"))
dir.create("model_output/SanFrancisco/hosp_death")

write_csv(sim_hospdeath_lowhigh, file.path("model_output/SanFrancisco/hosp_death","scenario_B_high.csv"))
write_csv(sim_hospdeath_lowlow, file.path("model_output/SanFrancisco/hosp_death","scenario_B_low.csv"))
write_csv(sim_hospdeath_lowmid, file.path("model_output/SanFrancisco/hosp_death","scenario_B_mid.csv"))



# SAN FRANCISCO RESULTS - MID ---------------------------------------------------

sims_data <- load_scenario_sims(scenario_dir="SanFrancisco/mid") %>% 
    mutate(time=parse_date(time)) %>% 
    mutate(comp=recode(comp, R="I3", cumI="R", diffI="cumI", `7.0`="diffI"))
incid_data <- sims_data %>% filter(comp=="diffI") %>% rename(incidI=N)

# Get San Fran - mid - low hosp/mortality
sim_hospdeath_midlow <- build_hospdeath_fullsim(incid_data, p_hosp=(p_death[1]*10), p_death=p_death[1], time_hosp_pars, time_death_pars, time_disch_pars)

# Get San Fran - mid - mid hosp/mortality
sim_hospdeath_midmid <- build_hospdeath_fullsim(incid_data, p_hosp=(p_death[2]*10), p_death=p_death[2], time_hosp_pars, time_death_pars, time_disch_pars)

# Get San Fran - mid - high hosp/mortality
sim_hospdeath_midhigh <- build_hospdeath_fullsim(incid_data, p_hosp=(p_death[3]*10), p_death=p_death[3], time_hosp_pars, time_death_pars, time_disch_pars)


sanfran_hosp_mid <- bind_rows(sim_hospdeath_midlow %>% mutate(p_death=p_death[1], scenario="A"), 
                                                         sim_hospdeath_midmid %>% mutate(p_death=p_death[2], scenario="A"),
                                                         sim_hospdeath_midhigh %>% mutate(p_death=p_death[3], scenario="A"))

write_csv(sim_hospdeath_midhigh, file.path("model_output/SanFrancisco/hosp_death","scenario_A_high.csv"))
write_csv(sim_hospdeath_midlow, file.path("model_output/SanFrancisco/hosp_death","scenario_A_low.csv"))
write_csv(sim_hospdeath_midmid, file.path("model_output/SanFrancisco/hosp_death","scenario_A_mid.csv"))




