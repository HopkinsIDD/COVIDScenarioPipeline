#..............................................................................................................

# This script gets run by 
# - `run_sim_processing.R`


# Do not modify unless changing the plotting or saving structures

#..............................................................................................................

library(tidyverse)

# Whether to compare to Baseline
include_baseline <- compare_to_baseline

# Carry over variables
opt$projection_date <- lubridate::as_date(opt$projection_date)
incl_hosp <- opt$include_hosp
incl_inf <- opt$include_inf

# if (is.null(scenario_name)){
#     scenarios <- c("baseline","optimistic","moderate","fatigue","counterfactual")
#     scenario_name <- scenarios[sapply(scenarios, grepl, projections_file_path)]
# } 

center_line <- ifelse(point_est==0.5, "median", "mean") ## mean or median model line
center_line_var <- ifelse(point_est==0.5, "point", "point-mean")
proj_data <- data_comb


#### Which valid locations are missing from our submission? 

# locs <- read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
# mismatched <- unique(proj_data$location)[which(!(unique(proj_data$location) %in% locs$location))]
# missing_from_fc <- unique(locs$location)[which(!(locs$location %in% unique(proj_data$location)))]
# 
# locs %>% filter(location %in% missing_from_fc) 


# STATE DATA --------------------------------------------------------------

# State Data #
state_cw <- cdlTools::census2010FIPS %>%
  distinct(State, State.ANSI) %>%
  dplyr::rename(USPS = State, location = State.ANSI) %>%
  dplyr::mutate(location = str_pad(location, 2, side = "left", pad = "0")) %>%
  distinct(location, USPS) %>%
  dplyr::mutate(location = as.character(location), USPS = as.character(USPS)) %>%
  bind_rows(tibble(USPS = "US", location = "US"))



# GROUND TRUTH ------------------------------------------------------------

# loaded from runs
#case_data_runs <- read_csv("data/ScenarioHub/R10/us_data.csv")

# gt_data <- read_csv(file.path(opt$outdir, "gt_data.csv")) %>%
#   rename(date = time) %>%
#   mutate(pre_gt_end = date < lubridate::as_date(proj_data[[1]]$model_projection_date)[1])

# loaded from runs
# scenario_dir <- file.path(opt$outdir, scenario_name)
# gt_data <- readr::read_csv(file.path(scenario_dir, "gt_data_clean.csv")) # loads gt_data
gt_data <- gt_data %>% 
  mutate(time = lubridate::as_date(time)) %>% mutate(date = time)

cum_dat_st <- gt_data %>%
  select(date, USPS, contains("cum")) %>%
  mutate(day_of_week=lubridate::wday(date, label=T))%>%
  ungroup %>%
  filter(day_of_week=="Sat") %>%
  select(-day_of_week)

inc_dat_st <- gt_data %>%
  select(date, USPS, contains("inc")) %>%
  mutate(week = lubridate::epiweek(date), year = lubridate::epiyear(date)) %>%
  mutate(tmp_time = as.numeric(date)) %>%
  group_by(USPS, week, year) %>%
  summarise(tmp_time = max(tmp_time), 
            across(starts_with("inc"), sum)) %>%
  ungroup %>%
  mutate(date = lubridate::as_date(tmp_time)) %>%
  mutate(pre_gt_end = date<=validation_date) %>%
  select(date, USPS, starts_with("incid"), pre_gt_end)

# inc_dat_st_vars <- dat_st_vars_long %>%
#   mutate(week = lubridate::epiweek(date), year = lubridate::epiyear(date)) %>%
#   mutate(tmp_time = as.numeric(date)) %>%
#   group_by(USPS, week, year, outcome, variant) %>%
#   summarise(tmp_time = max(tmp_time), value = sum(value, na.rm = TRUE)) %>%
#   ungroup %>%
#   mutate(date = lubridate::as_date(tmp_time)) %>%
#   mutate(pre_gt_end = date<=validation_date) %>%
#   select(-tmp_time)


# Combine cum, inc, and hosp ground truth #
dat_st_cl <- full_join(cum_dat_st, inc_dat_st, by = c("date", "USPS")) 
rm(cum_dat_st, inc_dat_st)

# Remove incomplete weeks from ground truth #
if(!((max(dat_st_cl$date)-lubridate::days(7)) %in% unique(dat_st_cl$date))){
  dat_st_cl <- dat_st_cl %>% filter(date != max(date))
}
# if(!((max(inc_dat_st_vars$date)-lubridate::days(7)) %in% unique(inc_dat_st_vars$date))){
#   inc_dat_st_vars <- inc_dat_st_vars %>% filter(date != max(date))
# }

colnames(dat_st_cl) <- gsub("cumI", "cumC", colnames(dat_st_cl)) 
colnames(dat_st_cl) <- gsub("incidI", "incidC", colnames(dat_st_cl)) 

dat_st_cl2 <- dat_st_cl %>%
  pivot_longer(cols=-c(date, USPS, pre_gt_end), names_to = "target", values_to = "value") %>%
  mutate(incid_cum = ifelse(grepl("inc", target), "inc", "cum")) %>%
  mutate(aggr_target = !grepl('_', target)) %>%
  mutate(outcome = substr(gsub("cum|incid", "", target), 1,1))





# <> OVERALL --------------------------------------------------------------

# PRIMARY FORECAST DATA ----------------------------------------------------

forecast_st <- proj_data %>%
  filter(nchar(location)==2 & (quantile %in% c(quant[1], 0.5, quant[2]) | is.na(quantile))) %>%
  left_join(state_cw, by = c("location")) 

# filter out incid or cum
if (!plot_incid) {  forecast_st <- forecast_st %>% filter(!grepl(" inc ", target)) }
if (!plot_cum) {  forecast_st <- forecast_st %>% filter(!grepl(" cum ", target)) }

# filter to keep only outcomes of interest
outcomes_name <- recode(outcomes_, "I"="inf", "C"="case", "H"="hosp", "D"="death")
cum_outcomes_name <- paste0("cum ", recode(cum_wk_outcomes_, "I"="inf", "C"="case", "H"="hosp", "D"="death"))
forecast_st <- forecast_st %>% filter(grepl(paste0(c(paste0("inc ", outcomes_name), cum_outcomes_name), collapse = "|"), target))

# create cat variables
forecast_st_plt <- forecast_st %>% 
  mutate(incid_cum = ifelse(grepl("inc ", target), "inc", "cum")) %>%
  mutate(outcome = stringr::word(target, 5)) %>%
  mutate(outcome = recode(outcome, "inf"="I", "case"="C", "hosp"="H", "death"="D")) %>%
  dplyr::mutate(quantile_cln = ifelse(!is.na(quantile), paste0("q", sub("0.", "", as.character(quantile))), 
                                      ifelse(type=="point-mean", paste0("mean"), 
                                             ifelse(type=="point", paste0("median"), NA)))) %>%
  dplyr::mutate(quantile_cln2 = ifelse(!is.na(quantile), paste0(incid_cum, outcome, "_q", sub("0.", "", as.character(quantile))), 
                                       ifelse(type=="point-mean", paste0(incid_cum, outcome,"_mean"), 
                                              ifelse(type=="point", paste0(incid_cum, outcome,"_median"), NA))))

forecast_st_plt <- forecast_st_plt %>%
  mutate(quantile_cln = gsub(substr(as.character(quant[1]),3,10), "low", quantile_cln),
         quantile_cln = gsub(substr(as.character(quant[2]),3,10), "high", quantile_cln)) %>%
  mutate(quantile_cln2 = gsub(substr(as.character(quant[1]),3,10), "low", quantile_cln2),
         quantile_cln2 = gsub(substr(as.character(quant[2]),3,10), "high", quantile_cln2)) %>%
  mutate(target_type = paste0(incid_cum, outcome))

pltdat_truth <- dat_st_cl2 %>% 
  filter(aggr_target) %>% rename(gt = value) %>%
  mutate(target = gsub("incid", "inc", target)) %>%
  rename(target_type = target) %>%
  filter(USPS %in% unique(forecast_st_plt$USPS)) %>%
  filter(target_type %in% unique(forecast_st_plt$target_type))

if(center_line == "mean"){
  forecast_st_plt <- forecast_st_plt %>% mutate(quantile_cln = gsub("mean", "ctr", quantile_cln))
} else{
  forecast_st_plt <- forecast_st_plt %>% mutate(quantile_cln = gsub("q5", "ctr", quantile_cln))
}

forecast_st_plt <- forecast_st_plt %>% 
  select(scenario_name, scenario_id, target = target_type, incid_cum, outcome, date = target_end_date, USPS, quantile_cln, value) %>%
  pivot_wider(names_from = quantile_cln, values_from = value) %>%
  mutate(type = "projection") %>%
  full_join(pltdat_truth %>% 
              mutate(type = "gt", scenario_name = ifelse(pre_gt_end, "gt-pre-projection", "gt-post-projection")) %>% 
              select(date, USPS, target = target_type, incid_cum, type, scenario_name, ctr=gt)) %>%
  filter(date >= trunc_date & date <= sim_end_date)




# PRODUCE PDF OF ALL LOCATIONS --------------------------------------------

# if (y_sqrt){
#     scale_y_funct <- scale_y_sqrt
# } else {
#     scale_y_funct <- scale_y_continuous
# }


# set up colors
scenarios_plot <- unique(forecast_st_plt$scenario_name)
scenarios_plot <- scenarios_plot[!grepl('gt', scenarios_plot)]
cols <- c("black", "orange", c("green", "coral", "blue", "purple")[1:length(scenarios_plot)])
names(cols) <- c("gt-pre-projection", "gt-post-projection", scenarios_plot)

options(scipen = 999)
scale_y_funct <- scale_y_continuous

pdf(stplot_fname, width=7, height=11)
for(usps in unique(forecast_st_plt$USPS)){
  
  print(paste0("Plotting: ", usps))
  cols_tmp <- cols[names(cols) %in% unique(forecast_st_plt$scenario_name)]
  
  inc_st_plt <- forecast_st_plt %>% 
    filter(USPS == usps) %>% 
    filter(incid_cum=="inc") %>%
    mutate(scenario_name = factor(scenario_name)) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(data = . %>% filter(type=="projection"), aes(ymin = qlow, ymax = qhigh, fill = factor(scenario_name)), alpha = 0.25) +
    geom_line(data = . %>% filter(type=="projection"), aes(y = ctr, color = factor(scenario_name)), linewidth = 1.25) +
    geom_point(data = . %>% filter(type=="gt"), aes(y = ctr, color = factor(scenario_name)), size = 1.5, pch=21, fill=NA) +
    geom_vline(xintercept = projection_date, color="red", alpha =0.5) +
    scale_color_manual(values = cols_tmp, aesthetics = c("color", "fill")) +
    scale_y_funct(glue::glue("Weekly Incident, {usps}")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    theme_bw() + xlab(NULL) +
    guides(color=guide_legend(title = NULL, nrow=2,byrow=TRUE), fill = "none") +
    coord_cartesian(xlim=lubridate::as_date(c(trunc_date, sim_end_date))) +
    facet_wrap(~target, ncol = 1, scales = "free_y") +
    theme(legend.position = "bottom", legend.text = element_text(size=10),
          axis.text.x = element_text(size=6, angle = 45))
  plot(inc_st_plt)
  
  
  if (plot_cum) {
    
    cum_st_plt <- forecast_st_plt %>% 
      filter(USPS == usps) %>% 
      filter(incid_cum=="cum") %>%
      mutate(scenario_name = factor(scenario_name)) %>%
      ggplot(aes(x = date)) +
      geom_ribbon(data = . %>% filter(type=="projection"), aes(ymin = qlow, ymax = qhigh, fill = factor(scenario_name)), alpha = 0.25) +
      geom_line(data = . %>% filter(type=="projection"), aes(y = ctr, color = factor(scenario_name)), linewidth = 1.25) +
      geom_point(data = . %>% filter(type=="gt"), aes(y = ctr, color = factor(scenario_name)), size = 1.5, pch=21, fill=NA) +
      geom_vline(xintercept = projection_date, color="red", alpha =0.5) +
      scale_color_manual(values = cols_tmp, aesthetics = c("color", "fill")) +
      scale_y_funct(glue::glue("Weekly Incident, {usps}")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
      theme_bw() + xlab(NULL) +
      guides(color=guide_legend(title = NULL, nrow=2,byrow=TRUE), fill = "none") +
      coord_cartesian(xlim=lubridate::as_date(c(trunc_date, sim_end_date))) +
      facet_wrap(~target, ncol = 1, scales = "free_y") +
      theme(legend.position = "bottom", legend.text = element_text(size=10),
            axis.text.x = element_text(size=6, angle = 45))
    
    plot(cum_st_plt)
  }
}
dev.off()


stplot_fname_sqrt <- gsub(".pdf", "_sqrt.pdf", stplot_fname)
scale_y_funct <- scale_y_sqrt

pdf(stplot_fname_sqrt, width=7, height=11)
for(usps in unique(forecast_st_plt$USPS)){
  
  print(paste0("Plotting: ", usps))
  cols_tmp <- cols[names(cols) %in% unique(forecast_st_plt$scenario_name)]
  
  inc_st_plt <- forecast_st_plt %>% 
    filter(USPS == usps) %>% 
    filter(incid_cum=="inc") %>%
    mutate(scenario_name = factor(scenario_name)) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(data = . %>% filter(type=="projection"), aes(ymin = qlow, ymax = qhigh, fill = factor(scenario_name)), alpha = 0.25) +
    geom_line(data = . %>% filter(type=="projection"), aes(y = ctr, color = factor(scenario_name)), linewidth = 1.25) +
    geom_point(data = . %>% filter(type=="gt"), aes(y = ctr, color = factor(scenario_name)), size = 1.5, pch=21, fill=NA) +
    geom_vline(xintercept = projection_date, color="red", alpha =0.5) +
    scale_color_manual(values = cols_tmp, aesthetics = c("color", "fill")) +
    scale_y_funct(glue::glue("Weekly Incident, {usps}")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    theme_bw() + xlab(NULL) +
    guides(color=guide_legend(title = NULL, nrow=2,byrow=TRUE), fill = "none") +
    coord_cartesian(xlim=lubridate::as_date(c(trunc_date, sim_end_date))) +
    facet_wrap(~target, ncol = 1, scales = "free_y") +
    theme(legend.position = "bottom", legend.text = element_text(size=10),
          axis.text.x = element_text(size=6, angle = 45))
  plot(inc_st_plt)
  
  if (plot_cum) {
    
    cum_st_plt <- forecast_st_plt %>% 
      filter(USPS == usps) %>% 
      filter(incid_cum=="cum") %>%
      mutate(scenario_name = factor(scenario_name)) %>%
      ggplot(aes(x = date)) +
      geom_ribbon(data = . %>% filter(type=="projection"), aes(ymin = qlow, ymax = qhigh, fill = factor(scenario_name)), alpha = 0.25) +
      geom_line(data = . %>% filter(type=="projection"), aes(y = ctr, color = factor(scenario_name)), linewidth = 1.25) +
      geom_point(data = . %>% filter(type=="gt"), aes(y = ctr, color = factor(scenario_name)), size = 1.5, pch=21, fill=NA) +
      geom_vline(xintercept = projection_date, color="red", alpha =0.5) +
      scale_color_manual(values = cols_tmp, aesthetics = c("color", "fill")) +
      scale_y_funct(glue::glue("Weekly Incident, {usps}")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
      theme_bw() + xlab(NULL) +
      guides(color=guide_legend(title = NULL, nrow=2,byrow=TRUE), fill = "none") +
      coord_cartesian(xlim=lubridate::as_date(c(trunc_date, sim_end_date))) +
      facet_wrap(~target, ncol = 1, scales = "free_y") +
      theme(legend.position = "bottom", legend.text = element_text(size=10),
            axis.text.x = element_text(size=6, angle = 45))
    
    plot(cum_st_plt)
  }
}
dev.off()



# WHERE SAVED
print(paste0("Plots created in ", stplot_fname))
