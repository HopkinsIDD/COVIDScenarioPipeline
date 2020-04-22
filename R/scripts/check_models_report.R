## checks for updated mobility matrix 4/7/2020

library(report.generation)
library(covidcommon)
library(tidyverse)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-s", "--scenario"), action="store", default='all', type='character', help="name of the intervention to check, or 'all' to check all of them")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon:::load_config(opt$c)
scenarios <- ifelse(opt$s == "all", config$interventions$scenarios, opt$s)

scn_dirs <- paste(config$name, scenarios, sep='_')
geodata <- load_geodata_file(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),
                             geoid_len=5)
included_geoids <- (geodata %>% dplyr::rename(targeted = config$spatial_setup$include_in_report) %>% dplyr::filter(targeted))$geoid
pdeathnames <- config$hospitalization$parameters$p_death_names ## c("med")

nfiles = 15
j = 1


###########################################
### check total population is constant
pre <- function(x){
  x %>% dplyr::filter(comp %in% c("S", "E", "I1", "I2", "I3", "R")) 
}
post <- function(x){
  x %>%
    ungroup %>%
    dplyr::filter(!is.na(time), geoid %in% included_geoids) %>%
    group_by(time) %>%
    summarise(N = sum(N)) %>%
    ungroup %>%
    mutate(time = as.Date(time))
}

###########################################
## check sim output
pre2 <- function(x){
  
  x %>%
    dplyr::filter(comp %in% c("diffI")) 
}

###########################################
## load hosp data from wrapper
state_hosp_totals <- list()
state_hosp_totals[[1]] <- load_hosp_geocombined_totals(scn_dirs,
                                                       num_files = nfiles,
                                                       scenariolabels = scn_dirs,
                                                       name_filter= pdeathnames[j],
                                                       incl_geoids = included_geoids,
                                                       geoid_len = 5,
                                                       file_extension = 'auto') %>%
  dplyr::mutate(pdeath=pdeathnames[j])

state_hosp_totals <- dplyr::bind_rows(state_hosp_totals)

hosp_plt <- state_hosp_totals %>% 
  dplyr::filter(pdeath == pdeathnames[j]) %>%
  dplyr::mutate(sim_num = factor(sim_num))

###########################################
## load pop data
pop <- load_scenario_sims_filtered(scn_dirs, num_files = nfiles, post_process = post, pre_process = pre, geoid_len = 5, file_extension = 'auto')
pop_toplt <- pop %>%
  dplyr::mutate(sim_num = factor(sim_num))



###########################################
## load simulation outputs diffI
sim_diffI <- list()
for (i in 1:length(scn_dirs)){
  
  sim_diffI[[i]] <- load_scenario_sims_filtered(scn_dirs[i], num_files = nfiles, post_process = post, pre_process = pre2, geoid_len = 5, file_extension = 'auto')
  sim_diffI[[i]]$scenario_num <- i
  sim_diffI[[i]]$scenario_name <- scn_dirs[i]  
}
sim_diffI <- dplyr::bind_rows(sim_diffI)

###########################################
## mean diff I outputs
sim_diffI_mn <- sim_diffI %>%
  group_by(time, scenario_name) %>%
  summarise(N = mean(N))

###########################################
## output checks to pdf file

pdf(paste0(config$name, "_check_models_report.pdf"))

ggplot(hosp_plt, aes(x = time, y = NincidInf, group = sim_num)) +
  geom_line(aes(colour = sim_num)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total incident infections (NincidInf)", labels = scales::comma) +
  guides(colour = "none") +
  facet_wrap(~scenario_name, ncol=1) + 
  ggtitle(paste("Checks for hosp outcomes:", pdeathnames[i], ",", nfiles, "sims \n"))

ggplot(hosp_plt, aes(x = time, y = NhospCurr, group = sim_num)) +
  geom_line(aes(colour = sim_num)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total hospital beds occupied", labels = scales::comma) +
  guides(colour = "none") +
  facet_wrap(~scenario_name, ncol=1)

ggplot(hosp_plt, aes(x = time, y = NincidHosp, group = sim_num)) +
  geom_line(aes(colour = sim_num)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total hospital admissions", labels = scales::comma) +
  guides(colour = "none") +
  facet_wrap(~scenario_name, ncol=1)

ggplot(hosp_plt, aes(x = time, y = NincidICU, group = sim_num)) +
  geom_line(aes(colour = sim_num)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total ICU admissions", labels = scales::comma) +
  guides(colour = "none") +
  facet_wrap(~scenario_name, ncol=1)

ggplot(hosp_plt, aes(x = time, y = NincidVent, group = sim_num)) +
  geom_line(aes(colour = sim_num)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total vent admissions", labels = scales::comma) +
  guides(colour = "none") +
  facet_wrap(~scenario_name, ncol=1)

ggplot(hosp_plt, aes(x = time, y = NVentCurr, group = sim_num)) +
  geom_line(aes(colour = sim_num)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total vent curr", labels = scales::comma) +
  guides(colour = "none") +
  facet_wrap(~scenario_name, ncol=1)


## check population size
ggplot(pop_toplt, aes(x = time, y = N, group = sim_num)) +
  geom_line(aes(colour = sim_num)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total population (hosp outcome)", labels = scales::comma) +
  guides(colour = "none")


## check simulation diffI
ggplot(sim_diffI, aes(x = time, y = N)) +
  geom_line(aes(colour = as.character(sim_num))) +
  facet_wrap(~scenario_name, nrow=2, scales = "free_y") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Total incident infections (diffI, sim output)", labels = scales::comma) +
  theme(legend.position = "bottom") +
  guides(colour = "none") +
  ggtitle(paste("Checks for sim outputs:", nfiles, "sims \n"))


## check mean simulation diffI mean
ggplot(sim_diffI_mn, aes(x = time, y = N)) +
  geom_line(aes(colour = as.character(scenario_name))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous("Mean total incident infections (diffI, sim output)", labels = scales::comma) +
  theme(legend.position = "bottom")

## check cumulative numbers (sim output)
total_inf_sim <- sim_diffI %>%
  group_by(sim_num, scenario_name) %>%
  summarise(cumI = sum(N)) %>%
  group_by(scenario_name) %>%
  summarise(meanCumI = mean(cumI))


## check cumulative numbers (hosp output)
total_inf <- state_hosp_totals %>%
  dplyr::filter(pdeath == pdeathnames[j]) %>%
  dplyr::mutate(sim_num = factor(sim_num)) %>%
  group_by(sim_num, scenario_name) %>%
  summarise(cumI = sum(NincidInf)) %>%
  group_by(scenario_name) %>%
  summarise(meanCumI = mean(cumI))

total_inf_toplt <- bind_rows(total_inf_sim %>% dplyr::mutate(type = "sim"), total_inf %>% dplyr::mutate(type = "hosp"))

ggplot(total_inf_toplt, aes(x = scenario_name, y = meanCumI)) +
  geom_col(aes(fill = type), position = "dodge") +
  scale_y_continuous("Mean Cumulative Infections", labels = scales::comma) +
  geom_text(aes(label = meanCumI)) +
  theme(legend.position = "bottom")

total_dh <- state_hosp_totals %>%
  dplyr::filter(pdeath == pdeathnames[j]) %>%
  dplyr::mutate(sim_num = factor(sim_num)) %>%
  group_by(sim_num, scenario_name) %>%
  summarise(incidD = sum(NincidDeath), incidH = sum(NincidHosp)) %>%
  group_by(scenario_name) %>%
  summarise(meanCumD = mean(incidD), meancumH = mean(incidH)) %>%
  pivot_longer(cols = contains("meanCum"), names_to = "metric", values_to = "value")

ggplot(total_dh, aes(x = scenario_name, y = value)) +
  geom_col(aes(fill = metric), position = "dodge") +
  geom_text(aes(label = value)) +
  scale_y_continuous("Deaths/Hosp", labels = scales::comma) +
  theme(legend.position = "bottom")

dev.off()


