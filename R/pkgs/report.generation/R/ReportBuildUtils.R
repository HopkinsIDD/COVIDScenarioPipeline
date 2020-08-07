
##' Utility function for easily referencing chunks inluded as part of
##' the package as part of the report.
##'
##' @param chunkname the file name of the chunk that is referenced
##'
##' @return the path to the chunk
##'
##' @author Justin Lessler
##'
##'
##' @export
reference_chunk <- function(chunkname) {
  return(system.file("rmarkdown","chunks",chunkname,package="report.generation"))
}

##'
##' List the chunks to chose from
##'
##' @return a list of chunks
##'
##' @export
##'
list_chunks <- function() {
  return(list.files(system.file("rmarkdown","chunks",package="report.generation")))
}


##'
##' Pretty print date for text
##'
##' @return a function that formats full pretty dates
##'
##' @export
##'
print_pretty_date <- function(date_string) {

  return(format(as.Date(date_string), "%B %d, %Y"))
}


##'
##' Pretty print short date for text and tables
##'
##' @return a function that formats short pretty dates
##'
##' @export
##'
print_pretty_date_short <- function(date_string) {

  return(format(as.Date(date_string), "%b %d"))
}


##
##'Function to round cleanly
##'
##'@param x single number to round
##'
##'@return rounded x
##'
##'@export
##'
conv_round <- function(x){
  if(x>50){x <- round(x, -2)}
  if(x<50){x <- round(x, -1)}
  return(x)
}


##'
##'Function to print CIs
##'
##'@param lo numeric or vector of lower bound of CI
##'@param hi numeric or vector of upper bound of CI
##'
##'@return formatted texted for CI: '\(lo, hi\)'
##'
##'@export
##'
make_CI <- function(lo, hi){
  if(is.numeric(lo)){
    paste0("(", format(conv_round(lo), big.mark = ","), "-", format(conv_round(hi), big.mark=","), ")") }else{
      paste0("(", format.Date(lo, format="%d %b"), ", ", format.Date(hi, format="%d %b"), ")") }
}

##'
##' Plot figure showing 15 random sims of hospitalization occupancy
##'
##' TODO ADD OPTION TO CHANGE VARIABLE THAT IS PLOTTED TO ANYTHING IN HOSP OUTCOMES DATASET
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param varname character string of variable name to plot from hosp data
##' @param varlabel character string of varialbe name label for plot
##' @param num_sims the number of simulations to show
##' @param pdeath_level level of IFR for filtering hospitalization data - TODO: Move our of functions
##' @param scenario_labels names of the scenarios to include- TODO: give a default
##' @param scenario_cols colors to plot each scenario in  - TODO: give a default
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param sim_end_date simulation end date as character string
##' @param plot_intervention logical indicating whether to plot grey box over a single intervention period -- will need to adapt if we want to show multiple intervention periods
##' @param interv_start_date intervention start date as character string
##' @param interv_end_date intervention end date as character string
##'
##' @return plot with N random simulations of hospital occupancy
##'
##' @export
##'
plot_ts_hosp_state_sample <- function (hosp_state_totals,
                                       varname = "NhospCurr",
                                       varlabel = "Daily hospital occupancy",
                                       num_sims = 15,
                                       pdeath_level = "high",
                                       scenario_labels,
                                       scenario_cols,
                                       sim_start_date,
                                       sim_end_date,
                                       plot_intervention = FALSE, ## may not want to plot if it is too complicated
                                       interv_start_date = NA,
                                       interv_end_date = NA) {

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  sampled_sims <- sample(unique(hosp_state_totals$sim_num), 
                         min(num_sims, length(unique(hosp_state_totals$sim_num))),
                         replace=FALSE) 
  
  to_plt <- hosp_state_totals %>%
    dplyr::filter(pdeath==pdeath_level,
                  sim_num %in% sampled_sims) %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_labels,
                                         labels = scenario_labels),
                  sim_num = factor(sim_num)) %>%
    dplyr::rename(pltvar = !!varname)

  rc <- ggplot(data=to_plt,
               aes(x=time, colour = scenario_name,
                   group = interaction(sim_num, scenario_name))) +
    geom_line(aes(y = pltvar), alpha=0.3, size=.75) +
    scale_y_continuous(varlabel, labels = scales::comma) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 limits = c(lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date))) +
    scale_color_manual("Scenario",
                      labels = scenario_labels,
                      values = scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
         legend.position = "bottom",
         legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)))


  if(plot_intervention){
    interv_dates <- data.frame(xmin = lubridate::ymd(interv_start_date),
                               xmax = lubridate::ymd(interv_end_date),
                               ymin = 0,
                               ymax = 1.05*max(to_plt$NincidHosp))

    rc <- rc +
      geom_rect(data = interv_dates, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                color = "grey", fill = "grey", alpha = 0.33)
  }


  return(rc)

}



##'
##' Plot figure showing 15 random sims of incident infections
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data
##' @param num_sims the number of simulations to show
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param sim_end_date simulation end date as character string
##' @param plot_intervention logical indicating whether to plot grey box over a single intervention period -- will need to adapt if we want to show multiple intervention periods
##' @param interv_start_date intervention start date as character string
##' @param interv_end_date intervention end date as character string
##'
##' @return plot with 15 random simulations of incident infection
##'
##' @export
##'
plot_ts_incid_inf_state_sample <- function (hosp_state_totals,
                                            num_sims = 15,
                                            pdeath_level = "high", ## doesn't really matter since data should be the same for infections
                                            scenario_labels,
                                            scenario_cols,
                                            sim_start_date,
                                            sim_end_date,
                                            plot_intervention = FALSE, ## may not want to plot if it is too complicated
                                            interv_start_date = NA,
                                            interv_end_date = NA) {


  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_state_totals %>%
    dplyr::filter(pdeath==pdeath_level,
                  sim_num %in% sample(unique(sim_num),
                                      min(num_sims, length(unique(sim_num))),
                                      replace=FALSE)) %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_labels,
                                         labels = scenario_labels),
                  sim_num = factor(sim_num))

  rc <- ggplot(data=to_plt,
               aes(x = time, color = scenario_name,
                   group = interaction(sim_num, scenario_name))) +
    geom_line(aes(y = NincidInf), alpha=0.2, size=.75) +
    scale_y_continuous("Daily incident infections", labels = scales::comma) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 limits = c(lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date))) +
    scale_color_manual("Scenario",
                       labels = scenario_labels,
                       values = scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)))


  if(plot_intervention){
    interv_dates <- data.frame(xmin = lubridate::ymd(interv_start_date),
                               xmax = lubridate::ymd(interv_end_date),
                               ymin = 0,
                               ymax = 1.05*max(to_plt$NincidInf))

    rc <- rc +
      geom_rect(data = interv_dates, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                color = "grey", fill = "grey", alpha = 0.33)
  }

  return(rc)

}



##'
##' Plot figure showing 15 random sims of incident infections
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data
##' @param num_sims the number of simulations to show
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param sim_end_date simulation end date as character string
##' @param plot_intervention logical indicating whether to plot grey box over a single intervention period -- will need to adapt if we want to show multiple intervention periods
##' @param interv_start_date intervention start date as character string
##' @param interv_end_date intervention end date as character string
##'
##' @return plot with 15 random simulations of incident deaths
##'
##' @export
##'
plot_ts_incid_death_state_sample_allPdeath <- function (hosp_state_totals,
                                                        num_sims = 15,
                                                        scenario_labels,
                                                        scenario_cols,
                                                        pdeath_level = "high",
                                                        pdeath_labels,
                                                        sim_start_date,
                                                        sim_end_date,
                                                        plot_intervention = FALSE, ## may not want to plot if it is too complicated
                                                        interv_start_date = NA,
                                                        interv_end_date = NA) {


  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_state_totals %>%
    dplyr::filter(sim_num %in% sample(unique(sim_num),
                                      min(num_sims, length(unique(sim_num))),
                                      replace=FALSE)) %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_labels),
                  pdeath = factor(pdeath,
                                  levels = pdeath_level,
                                  labels = pdeath_labels),
                  sim_num = factor(sim_num))

  rc <- ggplot(data=to_plt,
               aes(x=time, color=scenario_name,
                   group=interaction(sim_num, scenario_name))) +
    geom_line(aes(y=NincidDeath), alpha=0.2, size=.75) +
    scale_y_continuous("Daily incident deaths", labels = scales::comma) +
    scale_x_date(date_breaks = "2 months",
                 date_labels = "%b",
                 limits = c(lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date))) +
    scale_color_manual("Scenario",
                       labels = scenario_labels,
                       values = scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    facet_wrap(~pdeath, nrow = 1) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)))


  if(plot_intervention){
    interv_dates <- data.frame(xmin = lubridate::ymd(interv_start_date),
                               xmax = lubridate::ymd(interv_end_date),
                               ymin = 0,
                               ymax = 1.05*max(to_plt$NincidDeath))

    rc <- rc +
      geom_rect(data = interv_dates, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                color = "grey", fill = "grey", alpha = 0.33)
  }

  return(rc)

}



##'
##' Plot figure showing histogram of cumulative hospitalizations (or other incident outcome) by a certain date
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param var_name variable name for final size distribution
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param summary_date date at which to present cumulative summary of hospitalizations
##' @param scenario_levels to order scenario_name
##' @param scenario_labels to label scenario_name
##' @param scenario_cols to add scenario colors
##'
##' @return plot of cum hosp for state across simulations
##'
##' @export
##'
plot_hist_incidHosp_state <- function(hosp_state_totals,
                                      var_name,
                                      pdeath_level = "high",
                                      scenario_levels,
                                      scenario_labels,
                                      scenario_cols,
                                      sim_start_date,
                                      summary_date) {

  sim_start_date <- lubridate::ymd(sim_start_date)
  summary_date <- lubridate::ymd(summary_date)

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_state_totals %>%
    dplyr::rename(pltVar = !!var_name) %>%
    dplyr::filter(pdeath==pdeath_level) %>%
    dplyr::filter(time >= sim_start_date & time <= summary_date) %>%
    group_by(scenario_name, sim_num) %>%
    dplyr::summarise(pltVar = sum(pltVar)) %>%
    ungroup %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_levels,
                                         labels = scenario_labels))

  rc <- ggplot(data=to_plt,
               aes(x = pltVar, fill = scenario_name, color = scenario_name)) +
    geom_histogram(binwidth = 2000) +
    facet_wrap(scenario_name~., ncol = 1) +
    scale_fill_manual(values = scenario_cols,
                      labels = scenario_labels,
                      aesthetics = c("colour", "fill")) +
    scale_x_continuous(paste("by",
                             print_pretty_date_short(summary_date)),
                       labels = scales::comma,
                       ) +
    ylab("Number of simulations") +
    theme_bw() +
    guides("none") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, vjust =1))


  return(rc)

}



##'
##' Plot figure showing histogram of peak hospital occupancy by a certain date
##'
##' @param hosp_cty_peaks totals for hospitalization related data for state for all pdeath
##' @param cty_names Dataframe with county geoid and county name
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data
##' @param scenario_labels scenario names from config
##' @param scenario_cols scenario colors from config
##' @param start_date date to filter to start search for peak timing (character string)
##' @param end_date date to filter to end search for peak timing (character string)
##'
##' @return plot of distribution of peak timing across simulations by county
##'
##' @export
##'
plot_line_hospPeak_time_county <- function (hosp_cty_peaks,
                                            cty_names,
                                            pdeath_level = "high", ## choose one to display peak
                                            scenario_labels,
                                            scenario_cols,
                                            start_date,
                                            end_date) {
  pdeath_level <- match.arg(pdeath_level)
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_cty_peaks %>%
    dplyr::filter(pdeath %in% pdeath_level,
                  scenario_name %in% scenario_labels) %>%
    group_by(geoid, scenario_name) %>%
    dplyr::summarise(mean_pkTime = as.Date(mean(time)),
                     median_pkTime = as.Date(median(time)),
                     low_pkTime = as.Date(quantile(time, probs=.25, type=1)),
                     hi_pkTime = as.Date(quantile(time, probs=.75, type=1))) %>%
    ungroup %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_labels,
                                         labels = scenario_labels)) %>%
    dplyr::left_join(cty_names, by = c("geoid"))

  if(length(scenario_labels)==1){
    rc <- ggplot(data=to_plt,
                 aes(x = reorder(countyname, -as.numeric(mean_pkTime)),
                     y = mean_pkTime, ymin = low_pkTime, ymax = hi_pkTime)) +
      geom_pointrange() +
      scale_y_date("Date of peak hospital occupancy",
                   date_breaks = "1 months",
                   date_labels = "%b",
                   # limits = c(as.Date(start_date), as.Date(end_date))
                   ) +
      xlab("County") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")  +
      coord_flip()
  } else{
    rc <- ggplot(data=to_plt,
                 aes(x = reorder(countyname, -as.numeric(mean_pkTime)),
                     y = mean_pkTime, ymin = low_pkTime, ymax = hi_pkTime,
                     color = scenario_name)) +
      geom_pointrange() +
      scale_y_date("Date of peak hospital occupancy",
                   date_breaks = "1 months",
                   date_labels = "%b",
                   # limits = c(as.Date(start_date), as.Date(end_date))
                   ) +
      xlab("County") +
      scale_color_manual("Scenario",
                         labels = scenario_labels,
                         values = scenario_cols) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")  +
      coord_flip()
  }


  return(rc)

}


##'
##' Plot map showing infections per 10K on a specific date for one scenario
##'
##' @param cum_inf_geounit_dates dataframe with cumulative infections up through a specific date, produced by load_cum_inf_geounit_dates, perhaps
##' @param geodata as loaded by skeleton
##' @param shp shapefile with geounits
##' @param scenariolabel scenario label character string
##' @param popnodes name of pop variable in geodata
##' @param display_datecharacter string of display date for map
##' @param viridis_palette character string of viridis palette
##'
##' @return plot of cumulative infections per 10K by a specific date by geounit for a single scenario
##'
##' @export
##'
plot_geounit_attack_rate_map <- function (cum_inf_geounit_dates,
                                           geodata,
                                           shp,
                                           scenariolabel = config$report$formatting$scenario_labels[1],
                                           popnodes = config$spatial_setup$popnodes,
                                           display_date,
                                           viridis_palette = "plasma") {

  display_date <- as.Date(display_date)
  shp$geoid <- as.character(shp$geoid)

  to_plt <- cum_inf_geounit_dates %>%
    dplyr::filter(scenario_name == scenariolabel,
                  time == display_date) %>%
    left_join(geodata) %>%
    dplyr::rename(pop = !!popnodes) %>%
    group_by(geoid) %>%
    dplyr::summarise(attack_rate=mean(N/pop)*10000) %>%
    ungroup

  plot_shp <- left_join(shp, to_plt, by="geoid")

  rc <- ggplot(plot_shp) +
    geom_sf(aes(fill=attack_rate)) +
    theme_minimal() +
    scale_fill_viridis_c("Infections\nper 10K", option=viridis_palette, labels = scales::comma) +
    ggtitle(print_pretty_date_short(display_date)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y=element_blank())
    return(rc)

}

##'
##' Plot map showing chosen variable per 10K on a specific date for one scenario (blue/green color scale)
##'
##' @param cum_inf_geounit_dates dataframe produced by load_cum_inf_geounit_dates, with cumulative totals/averages on a given date
##' @param plot_var name of the variable of which to plot intensity
##' @param geodata as loaded by skeleton
##' @param shp shapefile with geounits
##' @param scenariolabel scenario label character string
##' @param popnodes name of pop variable in geodata
##' @param display_date string of display date for map
##' @param viridis_palette character string of viridis palette
##'
##' @return map by geounit, filled by variable per 10K at a specific date for a single scenario
##'
##' @export
##'
plot_geounit_map <- function(cum_inf_geounit_dates, 
                             plot_var,
                             geodata, 
                             shp, 
                             legend_name = "Value per 10K",
                             scenariolabel = config$report$formatting$scenario_labels[1], 
                             popnodes = config$spatial_setup$popnodes, 
                             display_date,
                             clims = NULL){
  
  display_date <- as.Date(display_date)
  
  shp$geoid <- as.character(shp$geoid)
  to_plt <- cum_inf_geounit_dates %>% 
    dplyr::filter(scenario_name == scenariolabel, 
                  time == display_date) %>% 
    left_join(geodata) %>% 
    dplyr::rename(pop = !!popnodes, plot_var = !!plot_var) %>% group_by(geoid) %>% 
    dplyr::summarise(geoid_rate = mean(plot_var/pop) * 10000) %>% 
    ungroup
  
  plot_shp <- left_join(shp, to_plt, by = "geoid")
  
  if(is.null(clims)){
    clims = range(plot_shp$geoid_rate, na.rm=TRUE)
  }
  
  rc <- ggplot(plot_shp) + 
    geom_sf(aes(fill = geoid_rate)) + 
    theme_minimal() + 
    scale_fill_gradientn(name=legend_name, colors = RColorBrewer::brewer.pal(9, "YlGnBu"), limits=clims) + 
    ggtitle(paste0(scenariolabel, ": ", print_pretty_date_short(display_date))) + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), axis.title.y = element_blank(), 
          axis.text.y = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), panel.border = element_blank(), 
          axis.ticks.y = element_blank())
  return(rc)
}   


##'
##' Plot figure showing median and IQR time series for the state for one scenario
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param geodata geodata object from load_config
##' @param incl_geoids vector with geoids
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data -- choose 1
##' @param scenario_label which scenario name from config
##' @param scenario_col which scenario color from config
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param sim_end_date simulation end date as character string
##' @param plot_intervention logical indicating whether to plot grey box over a single intervention period -- will need to adapt if we want to show multiple intervention periods
##' @param interv_start_date intervention start date as character string
##' @param interv_end_date intervention end date as character string
##'
##' @return plot state time series median and IQR
##'
##' @export
##'
plot_ts_incid_ar_state <- function (hosp_state_totals,
                                    geodata,
                                    incl_geoids,
                                    pdeath_level = "high", ## doesn't really matter since data should be the same for infections
                                    scenario_label,
                                    scenario_col,
                                    sim_start_date,
                                    sim_end_date,
                                    plot_intervention = FALSE, ## may not want to plot if it is too complicated
                                    interv_start_date = NA,
                                    interv_end_date = NA) {

  geopop <- geodata %>%
    dplyr::filter(geoid %in% incl_geoids) %>%
    dplyr::select(pop2010) %>%
    unlist %>% unname

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_state_totals %>%
    dplyr::filter(pdeath==pdeath_level) %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_label,
                                         labels = scenario_label)) %>%
    dplyr::filter(scenario_name == scenario_label) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(NincidInf_med = median(NincidInf), NincidInf_mean = mean(NincidInf), NincidInf_lo = quantile(NincidInf, .25), NincidInf_hi = quantile(NincidInf, .75)) %>%
    dplyr::mutate(pop = sum(geopop)) %>%
    dplyr::mutate(AR_med = NincidInf_med/pop*100000,
                  AR_mean = NincidInf_mean/pop*100000,
                  AR_lo = NincidInf_lo/pop*100000,
                  AR_hi = NincidInf_hi/pop*100000)

  rc <- ggplot(data=to_plt, aes(x = time)) +
    geom_pointrange(aes(y = AR_med, ymin = AR_lo, ymax = AR_hi), colour = scenario_col) +
    scale_y_continuous("Infections per 100K", labels = scales::comma) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 limits = c(lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date))) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          legend.position = "bottom",
          legend.title = element_blank())


  if(plot_intervention){
    interv_dates <- data.frame(xmin = lubridate::ymd(interv_start_date),
                               xmax = lubridate::ymd(interv_end_date),
                               ymin = 0,
                               ymax = 1.05*max(to_plt$AR_hi))

    rc <- rc +
      geom_rect(data = interv_dates, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                color = "grey", fill = "grey", alpha = 0.33)
  }

  return(rc)

}


# #### This function isn't working quite right
# ##'
# ##' Plot modeling assumption parameter distributions
# ##'
# ##' @param name parameter name
# ##' @param config config file
# ##' @return plot of distribution of peak timing across simulations by county
# ##'
# ##' @export
# ##'
# plot_model_parameter_distributions <- function(name, config){
#   dist_plot_config <- config$report$plot_settings$parameters_to_display
#   local_config <- dist_plot_config[[name]]
#   value <- config[[local_config$type]][['parameters']][[name]]
#   if((length(value) > 1) & ("distribution" %in% names(value))){
#     if(value$distribution == 'uniform' ){
#       value <- runif(1e5,covidcommon::as_evaled_expression(value$low) , covidcommon::as_evaled_expression(value$high))
#     }
#   } else {
#     value <- covidcommon::as_evaled_expression(value)
#   }
#   if('transform' %in% names(dist_plot_config[[name]]) ){
#     if(dist_plot_config[[name]][['transform']] == 'invert'){
#       value = 1 / value
#     }
#   }
#   rval <- NaN
#   if(local_config$distribution == "lnormal"){
#     rval <- (rlnorm(1e5,meanlog= value[1], sdlog = value[2]))
#   }
#   if(local_config$distribution == "exp"){
#     rval <- rexp(1e5,rate=value)
#   }
#   if(local_config$distribution == "gamma"){
#     all_compartments <<- unique(report.generation::load_scenario_sims_filtered('mid-west-coast-AZ-NV_CaliforniaMild/',pre_process=function(x){return(x[x$time==x$time[1], ])})$comp)
#     number_compartments <<- sum(grepl("I[[:digit:]]+",all_compartments))
#     if(length(value)==1){
#       value[2] <- 1/value[1]
#     }
#     rval <- rgamma(1e5,shape=value/number_compartments,scale=number_compartments)
#   }
#   if('xlim' %in% names(local_config)){
#     plt <- plot(density(rval),main = local_config$formal_name, xlab = local_config$xlab, bty='n', xlim=as.numeric(local_config$xlim))
#   } else {
#     plt <- plot(density(rval),main = local_config$formal_name, xlab = local_config$xlab, bty='n')
#   }
#   return(plt)
# }




##'
##' Make statewide table of infections, hosp, ICU, deaths for given scenario
##'
##' @param current_scenario text string of scenario label for which to build table
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param table_dates formatted table_dates object
##' @param pdeath_labels pdeath formatted labels
##' @param pdeath_filecode pdeath file column codes
##'
##' @return state scenario table
##'
##' @export
##'
make_scn_state_table <- function(current_scenario,
                                 hosp_state_totals,
                                 table_dates,
                                 pdeath_labels,
                                 pdeath_filecode){

ci_lo = 0.025
ci_hi = 0.975

if (length(pdeath_filecode)==1) {
  stop("Currently does not support single values of pdeath")
}

tmp <- data.frame(name=c("INFECTIONS",
                         "HOSPITALIZATIONS\n  total", "", "",
                         "  daily peak admissions", "", "",
                         "  daily peak capacity", "", "",
                         "ICU \n  total", "", "",
                         "  daily peak admissions", "", "",
                         "  daily peak capacity", "", "",
                         "DEATHS\n  total", "", ""))
tmp$name <- as.character(tmp$name)
table_dates <- as.Date(table_dates)

for(i in 1:length(table_dates)){
  xx <- hosp_state_totals %>%
    filter(!is.na(time) & scenario_name==current_scenario) %>%
    filter(time <= table_dates[i]) %>%
    group_by(scenario_name, pdeath, sim_num) %>%
    summarize(
      TotalIncidInf = sum(NincidInf, na.rm = TRUE),
      TotalIncidHosp = sum(NincidHosp, na.rm = TRUE),
      TotalIncidICU = sum(NincidICU, na.rm = TRUE),
      TotalIncidDeath = sum(NincidDeath, na.rm = TRUE),
      maxHospAdm = max(NincidHosp, na.rm=TRUE),
      maxICUAdm = max(NincidICU, na.rm=TRUE),
      maxHospCap = max(NhospCurr, na.rm = TRUE),
      maxICUCap = max(NICUCurr, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    group_by(scenario_name, pdeath) %>%
    summarize(
      nIncidInf_final = mean(TotalIncidInf),
      nIncidInf_lo = quantile(TotalIncidInf, ci_lo),
      nIncidInf_hi = quantile(TotalIncidInf, ci_hi),
      nIncidHosp_final = mean(TotalIncidHosp),
      nIncidHosp_lo = quantile(TotalIncidHosp, ci_lo),
      nIncidHosp_hi = quantile(TotalIncidHosp, ci_hi),
      pIncidHosp_final = mean(maxHospAdm),
      pIncidHosp_lo = quantile(maxHospAdm, ci_lo),
      pIncidHosp_hi = quantile(maxHospAdm, ci_hi),
      nIncidICU_final = mean(TotalIncidICU),
      nIncidICU_lo = quantile(TotalIncidICU, ci_lo),
      nIncidICU_hi = quantile(TotalIncidICU, ci_hi),
      pIncidICU_final = mean(maxICUAdm),
      pIncidICU_lo = quantile(maxICUAdm, ci_lo),
      pIncidICU_hi = quantile(maxICUAdm, ci_hi),
      nIncidDeath_final = mean(TotalIncidDeath),
      nIncidDeath_lo = quantile(TotalIncidDeath, ci_lo),
      nIncidDeath_hi = quantile(TotalIncidDeath, ci_hi),
      nCurrHosp_final = mean(maxHospCap),
      nCurrHosp_lo = quantile(maxHospCap, ci_lo),
      nCurrHosp_hi = quantile(maxHospCap, ci_hi),
      nCurrICU_final = mean(maxICUCap),
      nCurrICU_lo = quantile(maxICUCap, ci_lo),
      nCurrICU_hi = quantile(maxICUCap, ci_hi)) %>%
    ungroup() %>%
    mutate(pdeath = pdeath_labels[match(pdeath, pdeath_filecode)])


  tmp <- bind_cols(tmp,
                   xx %>% filter(pdeath==pdeath_labels[1]) %>%
                     mutate(ci = make_CI(nIncidInf_lo, nIncidInf_hi),
                            est = conv_round(nIncidInf_final),
                            lvl = paste0("total inc infections"),
                            pdeath = "") %>% ## infections have no pdeath
                     select(lvl, est, ci, pdeath) %>%
                     bind_rows(xx %>%
                                 mutate(ci = make_CI(nIncidHosp_lo, nIncidHosp_hi),
                                        est = conv_round(nIncidHosp_final),
                                        lvl = paste0("total inc hosp", pdeath)) %>%
                                 select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                     bind_rows(xx %>%
                                 mutate(ci = make_CI(pIncidHosp_lo, pIncidHosp_hi),
                                        est = conv_round(pIncidHosp_final),
                                        lvl = paste0("peak inc hosp", pdeath)) %>%
                                 select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                     bind_rows(xx %>%
                                 mutate(ci = make_CI(nCurrHosp_lo, nCurrHosp_hi),
                                        est = conv_round(nCurrHosp_final),
                                        lvl = paste0("peak hosp cap", pdeath)) %>%
                                 select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                     bind_rows(xx %>%
                                 mutate(ci = make_CI(nIncidICU_lo, nIncidICU_hi),
                                        est = conv_round(nIncidICU_final),
                                        lvl = paste0("total inc ICU", pdeath)) %>%
                                 select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                     bind_rows(xx %>%
                                 mutate(ci = make_CI(pIncidICU_lo, pIncidICU_hi),
                                        est = conv_round(pIncidICU_final),
                                        lvl = paste0("peak inc ICU", pdeath)) %>%
                                 select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                     bind_rows(xx %>%
                                 mutate(ci = make_CI(nCurrICU_lo, nCurrICU_hi),
                                        est = conv_round(nCurrICU_final),
                                        lvl = paste0("peak ICU cap", pdeath)) %>%
                                 select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                     bind_rows(xx %>%
                                 mutate(ci = make_CI(nIncidDeath_lo, nIncidDeath_hi),
                                        est = conv_round(nIncidDeath_final),
                                        lvl = paste0("total inc death", pdeath)) %>%
                                 select(lvl, est, ci, pdeath) %>% arrange(pdeath))
  )
}


tlabels <- c(" ", "IFR")
nlabels <- c("name", "pdeath", "est", "ci")

for(i in 1:length(table_dates)){
  tlabels <- c(tlabels,
               paste0(print_pretty_date_short(table_dates[i]), "\nmean    "),
               "\n    95% PI")
  if(i>1){nlabels <- c(nlabels, paste0("est", i-1), paste0("ci", i-1))}
}
names(tlabels) <- nlabels

flextable::flextable(tmp[,nlabels]) %>%
  flextable::set_header_labels(values=tlabels) %>%
  flextable::valign(valign="bottom") %>%
  flextable::colformat_num(digits=0) %>%
  flextable::autofit()

}



##'
##' Make statewide table of infections, hosp, ICU, deaths, vents for given scenario
##'
##' @param current_scenario text string of scenario label for which to build table
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param table_dates formatted table_dates object
##' @param pdeath_labels pdeath formatted labels
##' @param pdeath_filecode pdeath file column codes
##'
##' @return state scenario table
##'
##' @export
##'
make_scn_state_table_withVent <- function(current_scenario,
                                 hosp_state_totals,
                                 table_dates,
                                 pdeath_labels,
                                 pdeath_filecode){
  
  ci_lo = 0.025
  ci_hi = 0.975
  
  if (length(pdeath_filecode)==1) {
    stop("Currently does not support single values of pdeath")
  }
  
  tmp <- data.frame(name=c("INFECTIONS",
                           "HOSPITALIZATIONS\n  total", "", "",
                           "  daily peak admissions", "", "",
                           "  daily peak capacity", "", "",
                           "ICU \n  total", "", "",
                           "  daily peak admissions", "", "",
                           "  daily peak capacity", "", "",
                           "VENTILATIONS \n total", "", "",
                           "   daily peak incident ventilations", "", "",
                           "   daily peak currently ventilated", "", "",
                           "DEATHS\n  total", "", ""))
  tmp$name <- as.character(tmp$name)
  table_dates <- as.Date(table_dates)
  
  for(i in 1:length(table_dates)){
    xx <- hosp_state_totals %>%
      filter(!is.na(time) & scenario_name==current_scenario) %>%
      filter(time <= table_dates[i]) %>%
      group_by(scenario_name, pdeath, sim_num) %>%
      summarize(
        TotalIncidInf = sum(NincidInf, na.rm = TRUE),
        TotalIncidHosp = sum(NincidHosp, na.rm = TRUE),
        TotalIncidICU = sum(NincidICU, na.rm = TRUE),
        TotalIncidVent = sum(NincidVent, na.rm=TRUE),
        TotalIncidDeath = sum(NincidDeath, na.rm = TRUE),
        maxHospAdm = max(NincidHosp, na.rm=TRUE),
        maxICUAdm = max(NincidICU, na.rm=TRUE),
        maxVentAdm = max(NincidVent, na.rm=TRUE),
        maxHospCap = max(NhospCurr, na.rm = TRUE),
        maxICUCap = max(NICUCurr, na.rm=TRUE),
        maxVentCap = max(NVentCurr, na.rm=TRUE)
      ) %>%
      ungroup() %>%
      group_by(scenario_name, pdeath) %>%
      summarize(
        nIncidInf_final = mean(TotalIncidInf),
        nIncidInf_lo = quantile(TotalIncidInf, ci_lo),
        nIncidInf_hi = quantile(TotalIncidInf, ci_hi),
        nIncidHosp_final = mean(TotalIncidHosp),
        nIncidHosp_lo = quantile(TotalIncidHosp, ci_lo),
        nIncidHosp_hi = quantile(TotalIncidHosp, ci_hi),
        pIncidHosp_final = mean(maxHospAdm),
        pIncidHosp_lo = quantile(maxHospAdm, ci_lo),
        pIncidHosp_hi = quantile(maxHospAdm, ci_hi),
        nIncidICU_final = mean(TotalIncidICU),
        nIncidICU_lo = quantile(TotalIncidICU, ci_lo),
        nIncidICU_hi = quantile(TotalIncidICU, ci_hi),
        pIncidICU_final = mean(maxICUAdm),
        pIncidICU_lo = quantile(maxICUAdm, ci_lo),
        pIncidICU_hi = quantile(maxICUAdm, ci_hi),
        nIncidVent_final = mean(TotalIncidVent),
        nIncidVent_lo = quantile(TotalIncidVent, ci_lo),
        nIncidVent_hi = quantile(TotalIncidVent, ci_hi),
        pIncidVent_final = mean(maxVentAdm),
        pIncidVent_lo = quantile(maxVentAdm, ci_lo),
        pIncidVent_hi = quantile(maxVentAdm, ci_hi),
        nIncidDeath_final = mean(TotalIncidDeath),
        nIncidDeath_lo = quantile(TotalIncidDeath, ci_lo),
        nIncidDeath_hi = quantile(TotalIncidDeath, ci_hi),
        nCurrHosp_final = mean(maxHospCap),
        nCurrHosp_lo = quantile(maxHospCap, ci_lo),
        nCurrHosp_hi = quantile(maxHospCap, ci_hi),
        nCurrICU_final = mean(maxICUCap),
        nCurrICU_lo = quantile(maxICUCap, ci_lo),
        nCurrICU_hi = quantile(maxICUCap, ci_hi),
        nCurrVent_final = mean(maxVentCap),
        nCurrVent_lo = quantile(maxVentCap, ci_lo),
        nCurrVent_hi = quantile(maxVentCap, ci_hi)) %>%
      ungroup() %>%
      mutate(pdeath = pdeath_labels[match(pdeath, pdeath_filecode)])
    
    
    tmp <- bind_cols(tmp,
                     xx %>% filter(pdeath==pdeath_labels[1]) %>%
                       mutate(ci = make_CI(nIncidInf_lo, nIncidInf_hi),
                              est = conv_round(nIncidInf_final),
                              lvl = paste0("total inc infections"),
                              pdeath = "") %>% ## infections have no pdeath
                       select(lvl, est, ci, pdeath) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(nIncidHosp_lo, nIncidHosp_hi),
                                          est = conv_round(nIncidHosp_final),
                                          lvl = paste0("total inc hosp", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(pIncidHosp_lo, pIncidHosp_hi),
                                          est = conv_round(pIncidHosp_final),
                                          lvl = paste0("peak inc hosp", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(nCurrHosp_lo, nCurrHosp_hi),
                                          est = conv_round(nCurrHosp_final),
                                          lvl = paste0("peak hosp cap", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(nIncidICU_lo, nIncidICU_hi),
                                          est = conv_round(nIncidICU_final),
                                          lvl = paste0("total inc ICU", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(pIncidICU_lo, pIncidICU_hi),
                                          est = conv_round(pIncidICU_final),
                                          lvl = paste0("peak inc ICU", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(nCurrICU_lo, nCurrICU_hi),
                                          est = conv_round(nCurrICU_final),
                                          lvl = paste0("peak ICU cap", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(nIncidVent_lo, nIncidVent_hi),
                                          est = conv_round(nIncidVent_final),
                                          lvl = paste0("total inc Vent", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(pIncidVent_lo, pIncidVent_hi),
                                          est = conv_round(pIncidVent_final),
                                          lvl = paste0("peak inc Vent", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(nCurrVent_lo, nCurrVent_hi),
                                          est = conv_round(nCurrVent_final),
                                          lvl = paste0("peak Vent cap", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath)) %>%
                       bind_rows(xx %>%
                                   mutate(ci = make_CI(nIncidDeath_lo, nIncidDeath_hi),
                                          est = conv_round(nIncidDeath_final),
                                          lvl = paste0("total inc death", pdeath)) %>%
                                   select(lvl, est, ci, pdeath) %>% arrange(pdeath))
    )
  }
  
  
  tlabels <- c(" ", "IFR")
  nlabels <- c("name", "pdeath", "est", "ci")
  
  for(i in 1:length(table_dates)){
    tlabels <- c(tlabels,
                 paste0(print_pretty_date_short(table_dates[i]), "\nmean    "),
                 "\n    95% PI")
    if(i>1){nlabels <- c(nlabels, paste0("est", i-1), paste0("ci", i-1))}
  }
  names(tlabels) <- nlabels
  
  flextable::flextable(tmp[,nlabels]) %>%
    flextable::set_header_labels(values=tlabels) %>%
    flextable::valign(valign="bottom") %>%
    flextable::colformat_num(digits=0) %>%
    flextable::autofit()
  
}

##'
##' Function makes a summary table for an entire state.
##'
##' @param hosp_state_totals contains the relevant hospital data
##' @param period_breaks the dates to break up the display periods.
##' @param pi_low low side of the prediction interval
##' @param pi_high high side of the prediction interval
##' @param round_digit what level to round to
##' 
##' @export
##'
make_scn_time_summary_table <- function(hosp_state_totals,
                                        period_breaks,
                                        pi_low = 0.025,
                                        pi_high = 0.975,
                                        round_digit=-2) {
    ##Make the period ranges and labels 
    period_breaks <- sort(as.Date(period_breaks)) #out of order leads to bad things....
    period_breaks <- c(min(hosp_state_totals$time)-1, as.Date(period_breaks), max(hosp_state_totals$time)+1)
   
    len <- length(period_breaks)
    lbls <- sprintf("%s-%s", format(period_breaks[1:(len-1)], "%b %d"),
                    format(period_breaks[2:len], "%b %d"))
    
    ## Build the table with summaries of all of the periods in it. 
    tbl_df <- hosp_state_totals %>% 
      mutate(period = cut(time, period_breaks, labels=lbls)) %>%
      group_by(period, scenario_name, sim_num) %>% #summarize totals in periods by scenario
      summarize(PeriodInf = sum(NincidInf),
                PeriodDeath = sum(NincidDeath),
                PeriodHosp = sum(NincidHosp),
                PeriodPkHosp = max(NhospCurr),
                PeriodICU = sum(NincidICU),
                PeriodPkICU = max(NICUCurr)) %>%
      ungroup %>%
      group_by(period, scenario_name) %>%  #now get means and prediction intervals
      summarize(PeriodInfPILow = round(quantile(PeriodInf, probs = c(pi_low)),digits = round_digit),
                PeriodDeathPILow = round(quantile(PeriodDeath, probs = c(pi_low)),digits = round_digit),
                PeriodHospPILow = round(quantile(PeriodHosp, probs = c(pi_low)),digits = round_digit),
                PeriodPkHospPILow = round(quantile(PeriodPkHosp, probs = c(pi_low)),digits = round_digit),
                PeriodICUPILow = round(quantile(PeriodICU, probs = c(pi_low)),digits = round_digit),
                PeriodPkICUPILow = round(quantile(PeriodPkICU, probs = c(pi_low)),digits = round_digit),
                PeriodInfPIHigh = round(quantile(PeriodInf, probs = c(pi_high)),digits = round_digit),
                PeriodDeathPIHigh = round(quantile(PeriodDeath, probs = c(pi_high)),digits = round_digit),
                PeriodHospPIHigh = round(quantile(PeriodHosp, probs = c(pi_high)),digits = round_digit),
                PeriodPkHospPIHigh = round(quantile(PeriodPkHosp, probs = c(pi_high)),digits = round_digit),
                PeriodICUPIHigh = round(quantile(PeriodICU, probs = c(pi_high)),digits = round_digit),
                PeriodPkICUPIHigh = round(quantile(PeriodPkICU, probs = c(pi_high)),digits = round_digit),
                PeriodInf = round(mean(PeriodInf),digits = round_digit),
                PeriodDeath = round(mean(PeriodDeath),digits = round_digit),
                PeriodHosp = round(mean(PeriodHosp),digits = round_digit),
                PeriodPkHosp = round(mean(PeriodPkHosp),digits = round_digit),
                PeriodICU = round(mean(PeriodICU),digits = round_digit),
                PeriodPkICU = round(mean(PeriodPkICU), digits = round_digit)) %>%
      ungroup() %>% ##make hi/low into CIs
      mutate(PeriodInfPI = paste(format(PeriodInfPILow,big.mark=","), format(PeriodInfPIHigh,big.mark=","), sep="-"),
             PeriodDeathPI = paste(format(PeriodDeathPILow,big.mark=","), format(PeriodDeathPIHigh,big.mark=","), sep="-"),
             PeriodHospPI = paste(format(PeriodHospPILow,big.mark=","), format(PeriodHospPIHigh,big.mark=","), sep="-"),
             PeriodPkHospPI = paste(format(PeriodPkHospPILow,big.mark=","), format(PeriodPkHospPIHigh,big.mark=","), sep="-"),
             PeriodICUPI = paste(format(PeriodICUPILow,big.mark=","), format(PeriodICUPIHigh,big.mark=","), sep="-"),
             PeriodPkICUPI = paste(format(PeriodPkICUPILow,big.mark=","), format(PeriodPkICUPIHigh,big.mark=","), sep="-"),) %>%
      select(-PeriodInfPILow, -PeriodInfPIHigh,
             -PeriodDeathPILow, -PeriodDeathPIHigh,
             -PeriodHospPILow, -PeriodHospPIHigh,
             -PeriodPkHospPILow, -PeriodPkHospPIHigh,
             -PeriodICUPILow, -PeriodICUPIHigh,
             -PeriodPkICUPILow, -PeriodPkICUPIHigh) 
    
  
    tmp<-sprintf("%s_%s", rep(lbls, each=2),c("mean","95% PI"))
   
   
    ##inellegant but should work
    tbl_df <- 
     bind_rows(tbl_df%>%select(period,scenario_name, PeriodInf, PeriodInfPI)%>%mutate(outcome="Infections in Period")%>%
                 rename(mean=PeriodInf,`95% PI`=PeriodInfPI),
               tbl_df%>%select(period,scenario_name, PeriodDeath, PeriodDeathPI)%>%mutate(outcome="Deaths in Period")%>%
                 rename(mean=PeriodDeath,`95% PI`=PeriodDeathPI),
               tbl_df%>%select(period,scenario_name, PeriodHosp, PeriodHospPI)%>%mutate(outcome="Hospital Admissions in Period")%>%
                 rename(mean=PeriodHosp,`95% PI`=PeriodHospPI),
               tbl_df%>%select(period,scenario_name, PeriodPkHosp, PeriodPkHospPI)%>%mutate(outcome="Peak Hospital Occupancy in Period")%>%
                 rename(mean=PeriodPkHosp,`95% PI`=PeriodPkHospPI),
               tbl_df%>%select(period,scenario_name, PeriodICU, PeriodICUPI)%>%mutate(outcome="ICU Admissions in Period")%>%
                 rename(mean=PeriodICU,`95% PI`=PeriodICUPI),
               tbl_df%>%select(period,scenario_name, PeriodPkICU, PeriodPkICUPI)%>%mutate(outcome="Peak ICU Occupancy in Period")%>%
                 rename(mean=PeriodPkICU,`95% PI`=PeriodPkICUPI)) %>%
      mutate(period=as.character(period)) %>%
      pivot_wider(names_from=period, values_from = c(mean,`95% PI`), names_sep=".")%>%
      setNames(nm = sub("(.*)\\.(.*)", "\\2_\\1", names(.)))%>%
      select(outcome,scenario_name,all_of(tmp))

    #tells how to group columns
    tbl_df <- flextable::as_grouped_data(tbl_df,groups="outcome")
    tmp <- is.na(tbl_df$scenario_name)
    tbl_df$scenario_name[tmp] <-tbl_df$outcome[tmp]
    tbl_df <- tbl_df%>%select(-outcome)
    typology<-data_frame(col_keys=colnames(tbl_df),
                         colA=c("",rep(lbls,each=2)),
                         colB=c("",rep(c("mean","95% PI"),length(lbls))))
 

    flx <- flextable::flextable(tbl_df)  %>%
      flextable::colformat_num(digits=0)%>%
      #flextable::merge_v(j="outcome")%>%
      flextable::autofit(add_w=.05)%>%
      flextable::valign(valign="top") %>%
      flextable::set_header_df(mapping = typology, key = "col_keys" )%>%
      #flextable::merge_h(part="header")%>%
      flextable::bold(j=sprintf("%s_mean",lbls))%>%
      flextable::bold(part="header",bold=TRUE)%>%
      flextable::bold(j = 1, i =which(tmp), bold = TRUE, part = "body" )%>%
      flextable::align(i=1,align = "center", part="header") %>% 
      flextable::align(i=2,j=which(typology$colB=="mean"), align = "right", part="header") %>%
      flextable::hline(i=2, part="header",  border = officer::fp_border())%>%
      flextable::hline_top(part="header",  border = officer::fp_border(width=2))%>%
      flextable::border(i=which(tmp),  border.top = officer::fp_border(col="grey"))
    
    return(flx)

}


##'
##' Function makes a summary table for an entire state.
##'
##' @param hosp_state_totals contains the relevant hospital data
##' @param period_breaks the dates to break up the display periods.
##' @param pi_low low side of the prediction interval
##' @param pi_high high side of the prediction interval
##' @param round_digit what level to round to
##' 
##' @export
##'
make_scn_time_summary_table_withVent <- function(hosp_state_totals,
                                                 period_breaks,
                                                 pi_low = 0.025,
                                                 pi_high = 0.975,
                                                 round_digit=-2) {
  ##Make the period ranges and labels 
  period_breaks <- sort(as.Date(period_breaks)) #out of order leads to bad things....
  period_breaks <- c(min(hosp_state_totals$time)-1, as.Date(period_breaks), max(hosp_state_totals$time)+1)
  
  len <- length(period_breaks)
  lbls <- sprintf("%s-%s", format(period_breaks[1:(len-1)], "%b %d"),
                  format(period_breaks[2:len], "%b %d"))
  
  ## Build the table with summaries of all of the periods in it. 
  tbl_df <- hosp_state_totals %>% 
    mutate(period = cut(time, period_breaks, labels=lbls)) %>%
    group_by(period, scenario_name, sim_num) %>% #summarize totals in periods by scenario
    summarize(PeriodInf = sum(NincidInf),
              PeriodDeath = sum(NincidDeath),
              PeriodHosp = sum(NincidHosp),
              PeriodPkHosp = max(NhospCurr),
              PeriodICU = sum(NincidICU),
              PeriodPkICU = max(NICUCurr),
              PeriodVent = sum(NincidVent),
              PeriodPkVent = max(NVentCurr)) %>%
    ungroup %>%
    group_by(period, scenario_name) %>%  #now get means and prediction intervals
    summarize(PeriodInfPILow = round(quantile(PeriodInf, probs = c(pi_low)),digits = round_digit),
              PeriodDeathPILow = round(quantile(PeriodDeath, probs = c(pi_low)),digits = round_digit),
              PeriodHospPILow = round(quantile(PeriodHosp, probs = c(pi_low)),digits = round_digit),
              PeriodPkHospPILow = round(quantile(PeriodPkHosp, probs = c(pi_low)),digits = round_digit),
              PeriodICUPILow = round(quantile(PeriodICU, probs = c(pi_low)),digits = round_digit),
              PeriodPkICUPILow = round(quantile(PeriodPkICU, probs = c(pi_low)),digits = round_digit),
              PeriodVentPILow = round(quantile(PeriodVent, probs = c(pi_low)),digits = round_digit),
              PeriodPkVentPILow = round(quantile(PeriodPkVent, probs = c(pi_low)),digits = round_digit),
              PeriodInfPIHigh = round(quantile(PeriodInf, probs = c(pi_high)),digits = round_digit),
              PeriodDeathPIHigh = round(quantile(PeriodDeath, probs = c(pi_high)),digits = round_digit),
              PeriodHospPIHigh = round(quantile(PeriodHosp, probs = c(pi_high)),digits = round_digit),
              PeriodPkHospPIHigh = round(quantile(PeriodPkHosp, probs = c(pi_high)),digits = round_digit),
              PeriodICUPIHigh = round(quantile(PeriodICU, probs = c(pi_high)),digits = round_digit),
              PeriodPkICUPIHigh = round(quantile(PeriodPkICU, probs = c(pi_high)),digits = round_digit),
              PeriodVentPIHigh = round(quantile(PeriodICU, probs = c(pi_high)),digits = round_digit),
              PeriodPkVentPIHigh = round(quantile(PeriodPkVent, probs = c(pi_high)),digits = round_digit),              
              PeriodInf = round(mean(PeriodInf),digits = round_digit),
              PeriodDeath = round(mean(PeriodDeath),digits = round_digit),
              PeriodHosp = round(mean(PeriodHosp),digits = round_digit),
              PeriodPkHosp = round(mean(PeriodPkHosp),digits = round_digit),
              PeriodICU = round(mean(PeriodICU),digits = round_digit),
              PeriodPkICU = round(mean(PeriodPkICU), digits = round_digit),
              PeriodVent = round(mean(PeriodVent),digits = round_digit),
              PeriodPkVent = round(mean(PeriodPkVent), digits = round_digit)) %>%
    ungroup() %>% ##make hi/low into CIs
    mutate(PeriodInfPI = paste(format(PeriodInfPILow,big.mark=","), format(PeriodInfPIHigh,big.mark=","), sep="-"),
           PeriodDeathPI = paste(format(PeriodDeathPILow,big.mark=","), format(PeriodDeathPIHigh,big.mark=","), sep="-"),
           PeriodHospPI = paste(format(PeriodHospPILow,big.mark=","), format(PeriodHospPIHigh,big.mark=","), sep="-"),
           PeriodPkHospPI = paste(format(PeriodPkHospPILow,big.mark=","), format(PeriodPkHospPIHigh,big.mark=","), sep="-"),
           PeriodICUPI = paste(format(PeriodICUPILow,big.mark=","), format(PeriodICUPIHigh,big.mark=","), sep="-"),
           PeriodPkICUPI = paste(format(PeriodPkICUPILow,big.mark=","), format(PeriodPkICUPIHigh,big.mark=","), sep="-"),
           PeriodVentPI = paste(format(PeriodVentPILow,big.mark=","), format(PeriodVentPIHigh,big.mark=","), sep="-"),
           PeriodPkVentPI = paste(format(PeriodPkVentPILow,big.mark=","), format(PeriodPkVentPIHigh,big.mark=","), sep="-")) %>%
    select(-PeriodInfPILow, -PeriodInfPIHigh,
           -PeriodDeathPILow, -PeriodDeathPIHigh,
           -PeriodHospPILow, -PeriodHospPIHigh,
           -PeriodPkHospPILow, -PeriodPkHospPIHigh,
           -PeriodICUPILow, -PeriodICUPIHigh,
           -PeriodPkICUPILow, -PeriodPkICUPIHigh,
           -PeriodVentPILow, -PeriodVentPIHigh,
           -PeriodPkVentPILow, -PeriodPkVentPIHigh,) 
  
  
  tmp<-sprintf("%s_%s", rep(lbls, each=2),c("mean","95% PI"))
  
  
  ##inellegant but should work
  tbl_df <- 
    bind_rows(tbl_df%>%select(period,scenario_name, PeriodInf, PeriodInfPI)%>%mutate(outcome="Infections in Period")%>%
                rename(mean=PeriodInf,`95% PI`=PeriodInfPI),
              tbl_df%>%select(period,scenario_name, PeriodDeath, PeriodDeathPI)%>%mutate(outcome="Deaths in Period")%>%
                rename(mean=PeriodDeath,`95% PI`=PeriodDeathPI),
              tbl_df%>%select(period,scenario_name, PeriodHosp, PeriodHospPI)%>%mutate(outcome="Hospital Admissions in Period")%>%
                rename(mean=PeriodHosp,`95% PI`=PeriodHospPI),
              tbl_df%>%select(period,scenario_name, PeriodPkHosp, PeriodPkHospPI)%>%mutate(outcome="Peak Hospital Occupancy in Period")%>%
                rename(mean=PeriodPkHosp,`95% PI`=PeriodPkHospPI),
              tbl_df%>%select(period,scenario_name, PeriodICU, PeriodICUPI)%>%mutate(outcome="ICU Admissions in Period")%>%
                rename(mean=PeriodICU,`95% PI`=PeriodICUPI),
              tbl_df%>%select(period,scenario_name, PeriodPkICU, PeriodPkICUPI)%>%mutate(outcome="Peak ICU Occupancy in Period")%>%
                rename(mean=PeriodPkICU,`95% PI`=PeriodPkICUPI),
              tbl_df%>%select(period,scenario_name, PeriodVent, PeriodVentPI)%>%mutate(outcome="Incident Ventilations in Period")%>%
                rename(mean=PeriodVent,`95% PI`=PeriodVentPI),
              tbl_df%>%select(period,scenario_name, PeriodPkVent, PeriodPkVentPI)%>%mutate(outcome="Peak Ventilators in Use in Period")%>%
                rename(mean=PeriodPkVent,`95% PI`=PeriodPkVentPI)
              ) %>%
    mutate(period=as.character(period)) %>%
    pivot_wider(names_from=period, values_from = c(mean,`95% PI`), names_sep=".")%>%
    setNames(nm = sub("(.*)\\.(.*)", "\\2_\\1", names(.)))%>%
    select(outcome,scenario_name,all_of(tmp))
  
  #tells how to group columns
  tbl_df <- flextable::as_grouped_data(tbl_df,groups="outcome")
  tmp <- is.na(tbl_df$scenario_name)
  tbl_df$scenario_name[tmp] <-tbl_df$outcome[tmp]
  tbl_df <- tbl_df%>%select(-outcome)
  typology<-data_frame(col_keys=colnames(tbl_df),
                       colA=c("",rep(lbls,each=2)),
                       colB=c("",rep(c("mean","95% PI"),length(lbls))))
  
  
  flx <- flextable::flextable(tbl_df)  %>%
    flextable::colformat_num(digits=0)%>%
    #flextable::merge_v(j="outcome")%>%
    flextable::autofit(add_w=.05)%>%
    flextable::valign(valign="top") %>%
    flextable::set_header_df(mapping = typology, key = "col_keys" )%>%
    #flextable::merge_h(part="header")%>%
    flextable::bold(j=sprintf("%s_mean",lbls))%>%
    flextable::bold(part="header",bold=TRUE)%>%
    flextable::bold(j = 1, i =which(tmp), bold = TRUE, part = "body" )%>%
    flextable::align(i=1,align = "center", part="header") %>% 
    flextable::align(i=2,j=which(typology$colB=="mean"), align = "right", part="header") %>%
    flextable::hline(i=2, part="header",  border = officer::fp_border())%>%
    flextable::hline_top(part="header",  border = officer::fp_border(width=2))%>%
    flextable::border(i=which(tmp),  border.top = officer::fp_border(col="grey"))
  
  return(flx)
  
}

##'
##' Plot figure showing when event time by geoid
##'
##' @param hosp_county_peak hosp geounit peak data
##' @param shapefile object with geoid and name
##' @param scenario_labels character vector of scenario labels from config
##' @param scenario_colors character vector of colors from config
##' @param time_caption label for time axis
##' @param geoid_caption label for geoid axis
##' @param value_name name of secondary axis value
##' @param value_label secondary axis character label
##' @param exclude_zeroes logical indicating whether to exclude geounits with no beds
##' @param start_date start date as character string "2020-01-01"
##' @param end_date end date as character string
##'
##' @return plot state time series median and IQR
##'
##' @export
##'
plot_event_time_by_geoid <- function(hosp_county_peaks,
                                     shapefile,
                                     scenario_labels, # TODO provide default arguments
                                     scenario_colors, # TODO provide default arguments
                                     time_caption,
                                     geoid_caption,
                                     value_name,
                                     value_label,
                                     start_date,      # TODO provide default arguments
                                     end_date) {
 
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)

  if(is.null(value_name) & (length(scenario_labels) == 1)){stop("Value name must be provided if only one scenario is plotted.")}

  value_name <- rlang::sym(value_name)

  hosp_county_peaks$time[is.na(hosp_county_peaks$time)] <- end_date+1
  hosp_county_peaks$time[hosp_county_peaks$time > end_date] <- end_date+1
  hosp_county_peaks$time[hosp_county_peaks$time < start_date] <- start_date-1

  to_plt <- hosp_county_peaks %>%
    filter(scenario_label %in% scenario_labels) %>%
    group_by(geoid, scenario_label) %>%
    dplyr::summarise(mean_time = mean(time),
                     median_time = median(time),
                     low_time = quantile(time, probs=.25, type=1),
                     hi_time = quantile(time, probs=.75, type=1),
                     value = round(mean(!!value_name,na.rm=T),0)) %>%
    ungroup %>%
    dplyr::mutate(scenario_label = factor(scenario_label,
                                         levels = scenario_labels,
                                         labels = scenario_labels)) %>%
    dplyr::inner_join(shapefile, by = c("geoid")) %>%
    mutate(
      name = reorder(name, -as.numeric(median_time)),
    )

  if(length(scenario_labels)==1){
    rc <- ggplot(data=to_plt,
                 aes(x = as.numeric(name),
                     y = median_time, ymin = low_time, ymax = hi_time)) +
      geom_pointrange() +
      scale_x_continuous(
        labels=levels(to_plt$name),
        breaks=seq_len(nrow(to_plt)),
        sec.axis = sec_axis(~.,labels = to_plt$value[order(as.numeric(to_plt$name))], breaks = seq_len(nrow(to_plt)), name = value_label)
      ) +
      scale_y_date(time_caption,
                   date_breaks = "1 week",
                   date_labels = "%b %d"
                   ) +
      xlab(geoid_caption) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")  +
      coord_flip()
  } else{
    rc <- ggplot(data=to_plt,
                 aes(x = reorder(name, -as.numeric(median_time)),
                     y = median_time, ymin = low_time, ymax = hi_time,
                     color = scenario_label)) +
      geom_pointrange() +
      scale_y_date(time_caption,
                   date_breaks = "1 week",
                   date_labels = "%b %d"
                   ) +
      xlab(geoid_caption) +
      scale_color_manual("Scenario",
                         labels = scenario_labels,
                         values = scenario_colors) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")  +
      coord_flip()
  }


  return(rc)

}


##'
##' Returns a ggplot object giving a boxplot of the range of N 
##' within each time period. 
##' 
##' @param hosp_state_totals,
##' 
##' @param df a data frame with columns time, sim_num, scenario_name and N
##' @param period_breaks the dates to break up the display periods.
##' @param stat  either "identity" or "peak" [TODO:extend options]
##' 
##' @export
boxplot_by_timeperiod <- function(df,
                                  period_breaks,
                                  stat="identity",
                                  scenario_labels, # TODO provide default arguments
                                  scenario_colors # TODO provide default arguments
                                  ) {
  
  period_breaks <- c(min(df$time)-1, as.Date(period_breaks), max(df$time)+1)
  len <- length(period_breaks)
  lbls <- sprintf("%s-%s", format(period_breaks[1:(len-1)], "%b %d"),
                  format(period_breaks[2:len], "%b %d"))
  
  if(stat=="identity") {
    sum_func <- function(x){x}
  } else if(stat=="peak") {
    sum_func <- function (x) {
      x %>% 
        group_by(sim_num, scenario_name, period) %>% 
        summarize(N=max(N)) %>% 
        ungroup()
    }
  } else {
    stop("Unknown statistic")
  }
  

  rc <- df %>%
    mutate(period = cut(time, period_breaks, labels=lbls)) %>%
    sum_func %>% 
    ggplot(aes(x=reorder(period, desc(period)), y=N, fill=scenario_name)) +
      geom_boxplot(outlier.alpha = .1, outlier.size=.2) +
      facet_wrap(~scenario_name,ncol=1)+
      scale_fill_manual("Scenario",
                         labels = scenario_labels,
                         values = scenario_colors) +
    coord_flip()
  
  return(rc)
    
}


##'
##' Compare model outputs and data from CSSE
##' 
##' @param state_hosp_totals state hosp data frame
##' @param jhu_obs_dat dataframe with case data NincidConfirmed and NincidDeathsObs
##' @param scenario_labels character vector with scenario labels
##' @param scenario_cols character vector with scenario colors
##' @param pdeath_level IFR level assumption
##' @param obs_data_col character string of observed data color
##' @param ci.L lower bound confidence interval
##' @param ci.U upper bound confidence interval
##' @param date_breaks breaks for dates in figure
##' @param sim_start_date simulation start date
##' @param sim_end_date simulation end date
##' @param week whether to aggregate values to weeks
##' 
##' @export
plot_model_vs_obs <- function(state_hosp_totals,
                              jhu_obs_dat,
                              scenario_labels,
                              scenario_cols,
                              pdeath_level,
                              obs_data_col = "black",
                              ci.L = 0,
                              ci.U = 1,
                              date_breaks = "1 month",
                              sim_start_date,
                              sim_end_date,
                              week=FALSE,
                              hosp=FALSE) {

  state_hosp_totals <-
    state_hosp_totals %>%
    dplyr::filter(pdeath == pdeath_level) %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_labels,
                                         labels = scenario_labels),
                  sim_num = factor(sim_num)) %>%
    dplyr::rename(date = time) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(between(date, lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date)))
  
  jhu_obs_dat <- dplyr::filter(jhu_obs_dat, between(date, lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date)))
  
  state_inf_summary <-
    state_hosp_totals %>%
    {if(week) group_by(.,date=lubridate::floor_date(date, "weeks", 3), scenario_name, sim_num) %>%
        dplyr::summarize(NincidInf=sum(NincidInf),
                         NincidCase=sum(NincidCase))
      else(.)}%>%
    group_by(date, scenario_name) %>%
    dplyr::summarize(ci_lower_incid_inf = quantile(NincidInf, ci.L),
                     ci_upper_incid_inf = quantile(NincidInf, ci.U),
                     mean_incid_inf = mean(NincidInf),
                     median_incid_inf = median(NincidInf),
                     ci_lower_incid_cas = quantile(NincidCase, ci.L),
                     ci_upper_incid_cas = quantile(NincidCase, ci.U),
                     mean_incid_cas = mean(NincidCase),
                     median_incid_cas = median(NincidCase)) 
  
  ### Incidence of infections plot
  incid_infections_plot <-
    ggplot(state_inf_summary, aes(x = date)) +
    geom_line(aes(y = mean_incid_cas, color = scenario_name)) +
    geom_ribbon(aes(ymin=ci_lower_incid_cas, ymax=ci_upper_incid_cas, fill = scenario_name), linetype = 0, alpha=0.2) +
    geom_point(data = jhu_obs_dat, aes(x = date, y = NincidConfirmed), color = obs_data_col) +
    #ylab("Incident Cases") +
    #theme(legend.position = "bottom") +
    scale_x_date(date_breaks = date_breaks,
                 date_labels = "%b %Y",
                 limits = c(lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date))) +
    scale_y_continuous("Incident Cases", labels = scales::comma) +
    scale_color_manual("Scenario",
                       labels = scenario_labels,
                       values = scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          axis.text.x = element_text(angle = 45),
          legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)),
           fill = FALSE) +
    coord_cartesian(ylim = c(0, 2.5*max(jhu_obs_dat$NincidConfirmed)))
  
  state_death_summary <-
    state_hosp_totals %>%
    {if(week) group_by(.,date=lubridate::floor_date(date, "weeks", 3), scenario_name, sim_num) %>%
        dplyr::summarize(NincidDeath=sum(NincidDeath))
      else(.)}%>%
    group_by(date, scenario_name) %>%
    dplyr::summarize(ci_lower_incid_death = quantile(NincidDeath, ci.L),
                     ci_upper_incid_death = quantile(NincidDeath, ci.U),
                     mean_incid_death = mean(NincidDeath),
                     median_incid_death = median(NincidDeath))
  incid_deaths_plot <-
    ggplot(state_death_summary, aes(x = date)) +
    geom_line(aes(y = mean_incid_death, color = scenario_name)) +
    geom_ribbon(aes(ymin=ci_lower_incid_death, ymax=ci_upper_incid_death, fill = scenario_name), linetype = 0, alpha=0.2) +
    geom_point(data = jhu_obs_dat, aes(x = date, y = NincidDeathsObs), color = obs_data_col) +
    #ylab("Incident Cases") +
    #theme(legend.position = "bottom") +
    scale_x_date(date_breaks = date_breaks,
                 date_labels = "%b %Y",
                 limits = c(lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date))) +
    scale_y_continuous("Incident Deaths", labels = scales::comma) +
    scale_color_manual("Scenario",
                       labels = scenario_labels,
                       values = scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          axis.text.x = element_text(angle = 45),
          legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)),
           fill = FALSE) +
    coord_cartesian(ylim = c(0, 2.5*max(jhu_obs_dat$NincidDeathsObs)))
  
  if(hosp){
    state_hosp_summary <-
      state_hosp_totals %>%
      group_by(date, scenario_name) %>%
      dplyr::summarize(ci_lower_incid_hosp = quantile(NhospCurr, ci.L),
                       ci_upper_incid_hosp = quantile(NhospCurr, ci.U),
                       mean_incid_hosp= mean(NhospCurr),
                       median_incid_hosp = median(NhospCurr))
    incid_hosp_plot <-
      ggplot(state_hosp_summary, aes(x = date)) +
      geom_line(aes(y = mean_incid_hosp, color = scenario_name)) +
      geom_ribbon(aes(ymin=ci_lower_incid_hosp, ymax=ci_upper_incid_hosp, fill = scenario_name), linetype = 0, alpha=0.2) +
      geom_point(data = jhu_obs_dat, aes(x = date, y = currhosp), color = obs_data_col) +
      #ylab("Incident Cases") +
      #theme(legend.position = "bottom") +
      scale_x_date(date_breaks = date_breaks,
                   date_labels = "%b %Y",
                   limits = c(lubridate::ymd(sim_start_date), lubridate::ymd(sim_end_date))) +
      scale_y_continuous("Daily occupied hospital beds", labels = scales::comma) +
      scale_color_manual("Scenario",
                         labels = scenario_labels,
                         values = scenario_cols) +
      theme_minimal() +
      theme(axis.title.x =  element_blank(),
            axis.text.x = element_text(angle = 45),
            legend.position = "bottom",
            legend.title = element_blank()) +
      guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)),
             fill = FALSE) +
      coord_cartesian(ylim = c(0, 2.5*max(jhu_obs_dat$currhosp)))
    output<-list(incid_infections_plot, incid_hosp_plot, incid_deaths_plot)
  } else {
    output <- list(incid_infections_plot, incid_deaths_plot)
  } 
  return(output)
}


##'
##' Plot heatmap of excess variables, as absolute number
##' (default), per-capita number needed, or % over threshold
##' 
##' @param hosp_dat timeseries with incident and cumulative hospitalizations, ICU admissions, etc.
##' @param current_scenario text string of scenario label to plot
##' @param type either "absolute", for total number excess; "percent", for percent excess; or "per-capita" for number excess per 10,000 population
##' @param threshold named vector of threshold value for given variable
##' @param shp shapefile with GEOID names
##' @param varname character string of variable name to plot from hosp data
##' @param varlabel character string of variable name for plot legend
##' @param geodata geodata object from load_config; only needed if type="per-capita"
##' @param popnodes name of pop variable in geodata; only needed if type="per-capita"
##' @param trunc_value value at which to truncate values for easy plotting; if null, defaults to max
##' 
##' @export
##' 
make_excess_heatmap <- function(hosp_dat, 
                                current_scenario,
                                type,
                                threshold,
                                shp,
                                varname,
                                varlabel,
                                geodata = NULL,
                                popnodes = config$spatial_setup$popnodes,
                                trunc_value = NULL){
  
  shp$threshold <- threshold[match(shp$geoid, names(threshold))]
  
  hosp_dat <- hosp_dat %>%
              dplyr::filter(scenario_name == current_scenario) %>%
              rename(var = !!varname)
  
  if(type=="per-capita"){
    plt_dat <- as.data.frame(shp) %>% 
               select(geoid, name, threshold) %>% 
               left_join(geodata %>% dplyr::rename(pop=!!popnodes) %>% dplyr::select(geoid, pop), by="geoid") %>%
               arrange(name)
    
    hosp_dat <- hosp_dat %>%
                left_join(plt_dat, by="geoid") %>%
                replace_na(list(threshold=0)) %>%
                mutate(exc_raw = var - threshold,
                       exc_abs = ifelse(exc_raw>0, exc_raw, NA),
                       exc_toplot = exc_abs / pop * 10000)
  }
  if(type=="absolute"){
    plt_dat <- as.data.frame(shp) %>% 
               select(geoid, name, threshold) %>% 
               arrange(name)
  
    hosp_dat <- hosp_dat %>% 
                left_join(plt_dat, by="geoid") %>%
                replace_na(list(threshold=0)) %>%
                mutate(exc_raw = var - threshold,
                       exc_toplot = ifelse(exc_raw>0, exc_raw, NA))
  }
  if(type=="percent"){
    plt_dat <- as.data.frame(shp) %>% 
      select(geoid, name, threshold) %>% 
      arrange(name)
    
    hosp_dat <- hosp_dat %>% 
      left_join(plt_dat, by="geoid") %>%
      replace_na(list(threshold=0)) %>%
      mutate(exc_raw = var - threshold,
             exc_abs = ifelse(exc_raw>0, exc_raw, NA),
             exc_toplot = exc_abs / threshold * 100)
  }
  
  if(!is.null(trunc_value)){
    hosp_dat$exc_toplot <- ifelse(hosp_dat$exc_toplot>trunc_value, trunc_value, hosp_dat$exc_toplot)
  }
  
  rc <- ggplot(hosp_dat, aes(x=time, y=name, fill=exc_toplot)) + 
          geom_tile() + 
          scale_fill_viridis_c(option = "magma", direction = -1, na.value = "white", name = varlabel) +
          theme_minimal() +
          xlab("") +
          ylab("") +
          scale_x_date(date_breaks = "2 weeks",
                       date_labels = "%b %d") +
          theme(axis.text.x = element_text(angle=45, vjust = 0.5))
  
  return(rc)
}


##'
##' Plot heatmap of needs relative to a threshold (e.g. bed needs)
##'
##' @param hosp_geounit_relative hosp_geounit_relative data
##' @param shapefile object with geoid and name
##' @param scenario_labels character vector of scenario labels from config
##' @param scale_colors character vector of colors for low, mid, and high plot values
##' @param legend_title label for legend
##' @param value_name name of secondary axis value
##' @param value_label secondary axis character label
##' @param start_date start date as character string "2020-01-01"
##' @param end_date end date as character string
##'
##' @return plot daily heatmap by geoid
##'
##' @export
##'
plot_needs_relative_to_threshold_heatmap <- function(
    hosp_geounit_relative,
    scenario_labels,
    scale_colors = c("#066f6c", "#f8e6e7", "#ba0a0f"),
    legend_title,
    value_name,
    value_label,
    start_date,
    end_date,
    incl_geoids = NULL){

  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date) 

  if(is.null(incl_geoids)) { incl_geoids <- unique(hosp_geounit_relative$geoid)}
  
  plt_dat <- hosp_geounit_relative %>%
    dplyr::rename(threshold = !!value_name) %>%
    dplyr::filter(time >= start_date & time <= end_date) %>%
    dplyr::filter(scenario_label %in% scenario_labels) %>%
    dplyr::mutate(scenario_label = factor(scenario_label,
                                         levels = scenario_labels,
                                         labels = scenario_labels)) %>%
    dplyr::arrange(name)%>%
    dplyr::group_by(scenario_label, time) %>%
    dplyr::mutate(name_num=seq_along(name))

  if(length(scenario_labels)==1){

    rc <- ggplot(plt_dat, aes(x = time, y = name_num)) +
      geom_tile(aes(fill = log_prop_needed)) +
      scale_fill_gradient2(paste("Log", legend_title), low = scale_colors[1], mid = scale_colors[2], high = scale_colors[3], midpoint = 0, na.value = "grey 30", labels = scales::comma, limits = c(floor(min(plt_dat$log_prop_needed)), ceiling(max(plt_dat$log_prop_needed)))) +
      scale_y_continuous("",
        breaks = plt_dat$name_num,
        labels = plt_dat$name,
        sec.axis = dup_axis(name = value_label,
                            breaks = plt_dat$name_num,
                            labels = plt_dat$threshold)) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
      theme_bw() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  } else{

    rc <- ggplot(plt_dat, aes(x = time, y = name_num)) +
      geom_tile(aes(fill = log_prop_needed)) +
      scale_fill_gradient2(paste("Log", legend_title), low = scale_colors[1], mid = scale_colors[2], high = scale_colors[3], midpoint = 0, na.value = "grey 30", labels = scales::comma, limits = c(floor(min(plt_dat$log_prop_needed)), ceiling(max(plt_dat$log_prop_needed)))) +
      scale_y_continuous("",
        breaks = plt_dat$name_num,
        labels = plt_dat$name,
        sec.axis = dup_axis(name = value_label,
                            breaks = plt_dat$name_num,
                            labels = plt_dat$threshold)) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
      theme_bw() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      facet_wrap(~scenario_label, nrow = 1)
  }
  
  return(rc)
}



##' Plotting R or effectiveness estimates
##' 
##' @param r_dat df with R or reduction estimates per sim and npi to be included
##' @param npi_trim pattern used by str_remove to group NPIs
##' @param periodcolors 
##' @param npi_labels labels for plotted NPIs
##' @param npi_levels levels of NPIs 
##' @param effectiveness whether to reduction estimates instead
##' @param pi_lo lower quantile for summarization
##' @param pi_hi higher quantile for summarization
##' @param geo_dat df with location names
##' 
##' @return plot estimated R or effectiveness of intervention periods by geoid
##' 
##'
##'
##'@export

plot_inference_r <- function(r_dat,
                             npi_trim="[[A-Z]].+\\_",
                             npi_labels, 
                             npi_levels,
                             periodcolors = c("chartreuse3", "brown2", "turquoise4", "black"),
                             effectiveness=FALSE,
                             pi_lo=0.25,
                             pi_hi=0.75, 
                             geo_dat=geodata){
  
  rplot <- r_dat %>%
    mutate(npi_name=str_remove(npi_name, npi_trim)) %>%
    left_join(geo_dat) %>%
    group_by(geoid, npi_name, name) %>%
    summarize(r_lo = quantile(r, pi_lo, na.rm=TRUE), 
              r_hi = quantile(r, pi_hi, na.rm=TRUE),
              r = mean(r, na.rm=TRUE),
              reduction_lo = quantile(reduction, pi_lo, na.rm=TRUE), 
              reduction_hi = quantile(reduction, pi_hi, na.rm=TRUE),
              reduction = mean(reduction, na.rm=TRUE)) 
  
  if(is.na(npi_labels) & is.na(npi_levels)){
    npi_labels <- unique(rplot$npi_name)
    npi_levels <- unique(rplot$npi_name)
  }
  
  if(length(periodcolors)<length(npi_labels)){
    stop("Specify additional colors")
  }
  
  if(effectiveness==TRUE){
    rc<-rplot %>%
      filter(npi_name!="local_variance") %>%
      mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels)) %>%
      ggplot(aes(x=reduction, y=name, col = npi_name)) +
      geom_point(position = position_dodge(1)) + 
      geom_linerange(aes(xmin=reduction_lo, xmax=reduction_hi, col = npi_name), 
                     position = position_dodge(1)) + 
      scale_color_manual(breaks = npi_labels, 
                         values = periodcolors) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()) +
      scale_x_continuous(expand = c(0,0)) +
      coord_cartesian(xlim = c(0, 1.01)) +
      ylab("County") +
      xlab("Estimated intervention effect (mean and IQR)") +
      guides(color=guide_legend(nrow=2,byrow=TRUE))
    
  } else {
    rc <- rplot %>%
      mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels)) %>%
      ggplot(aes(x=r, y=name, col=npi_name)) + 
      geom_point(position=position_dodge(1)) + 
      scale_color_manual(values = periodcolors)+
      geom_linerange(aes(xmin=r_lo, xmax=r_hi, col=npi_name), position=position_dodge(1)) +
      theme_bw() +
      theme(panel.grid.minor=element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()) +
      scale_x_continuous(expand = c(0,0)) +
      coord_cartesian(xlim = c(0, 5)) +
      ylab("County") +
      xlab("Estimated reproductive number (mean and IQR)")+
      guides(color=guide_legend(nrow=2,byrow=TRUE))
    
  }
  
  return(rc)
}

##' Sparkline table with R estimates 
##' 
##' @param r_dat df with R or reduction estimates per sim (only one scenario)
##' @param npi_trim pattern used by str_remove to group NPIs
##' @param npi_labels labels for plotted NPIs
##' @param npi_levels levels of NPIs after str_remove is applied
##' @param effectiveness whether to reduction estimates instead
##' @param pi_lo lower quantile for summarization
##' @param pi_hi upper quantile for summarization
##' @param geo_dat df with location names
##' @param px_qual sparkline pixel size passed on to ggplot_image
##' @param wh_ratio sparkline width:height ratio passed on to ggplot_image
##' @param brewer_palette pallete name passed on to brewer.pal
##' 
##' @return a table with the R per intervention period and a bar graph
##' 
##'
##'
##'@export

make_sparkline_tab_r <- function(r_dat,
                                 outcome_dir=NULL,
                                 susceptible=FALSE,
                                 current_scenario,
                                 npi_labels, 
                                 npi_levels,
                                 pi_lo=0.025, 
                                 pi_hi=0.975, 
                                 geo_dat=geodata,
                                 trim=TRUE,
                                 npi_trim="[[A-Z]].+\\_", 
                                 px_qual=40,
                                 wh_ratio=3.5,
                                 brewer_palette="Spectral"
){
  
  require(gt)
  require(tidyverse)
  if(length(npi_labels)!=length(npi_levels)) {stop("Length of npi levels and labels must be equal")}
  if(susceptible & is.null(outcome_dir)) {stop("You must specify outcome_dir to load cumulative infections")}
  r_dat <- r_dat%>%
    filter(scenario==current_scenario)
  
  # Set new end date for baseline values
  new_local_end <- r_dat %>%
    filter(npi_name!="local_variance") %>%
    group_by(geoid) %>%
    filter(min(start_date)==start_date) %>%
    ungroup() %>%
    mutate(new_end=start_date-1) %>%
    distinct(geoid, new_end)
  
  r_dat<-r_dat%>%
    left_join(new_local_end) %>%
    mutate(end_date=if_else(npi_name=="local_variance", new_end, end_date))
  
  if(susceptible){
    
    timeline<-crossing(geoid=unique(r_dat$geoid), 
                       date = seq(min(r_dat$start_date), max(r_dat$end_date), by=1))
    
    r_dat<-r_dat %>%
      group_by(geoid, sim_num)%>%
      mutate(mid_point=if_else(max(end_date)==end_date, # for summary values mid_point=date
                          Sys.Date(),
                          start_date+floor((end_date-start_date)/2))) %>%
      left_join(timeline)
    
    r_dat<-load_hosp_sims_filtered(outcome_dir,
                                   pre_process=function(x){x%>%
                                       filter(scenario==current_scenario)%>%
                                       select(geoid, scenario, death_rate, location, time, sim_id, incidI)},
                                   post_process=function(x){x%>%
                                       group_by(geoid, pdeath, scenario, sim_num, location)%>%
                                       mutate(cum_inf=cumsum(incidI))}) %>%
      rename(date=time)%>%
      right_join(r_dat) %>%
      mutate(r=if_else(date<start_date|date>end_date, NA_real_, r)) %>%
      drop_na() %>%
      left_join(geo_dat) %>%
      mutate(r=r*(1-cum_inf/pop2010)) 
  }
  
  r_dat <- r_dat %>%
    group_by(geoid, start_date, end_date, npi_name, name, mid_point, date) %>%
    summarize(est_lo=quantile(r, pi_lo, na.rm=TRUE),
              est_hi=quantile(r, pi_hi, na.rm=TRUE),
              estimate=mean(r, na.rm=TRUE)) %>%
    mutate_if(is.numeric, signif, digits=2)
  

  if(trim){
    r_dat<-r_dat%>%
      mutate(npi_name=str_remove(npi_name, npi_trimmer))
  }
  # Create table with summary values
  r_tab<-r_dat%>%
    filter(mid_point==date) %>%
    mutate_if(is.numeric, as.character) %>%
    mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels),
           est_lo = str_replace(est_lo, "^", '\n\\('),
           est_hi = str_replace(est_hi, "$", '\\)')) %>%
    arrange(npi_name) %>%
    unite(col="pi", est_lo:est_hi, sep="-") %>%
    unite(col="estimate", estimate:pi, sep="\n") %>%
    pivot_wider(id_cols="name", values_from=estimate, names_from=npi_name) %>%
    arrange(name) %>%
    rename(Location=name)
  
  r_tab[is.na(r_tab)] <- ""
  
  # Plotting
  fill_values <- RColorBrewer::brewer.pal(length(npi_labels), brewer_palette)
  color_values <- colorspace::darken(fill_values, 0.3)

  
  # solution from https://stackoverflow.com/questions/61741440/is-there-a-way-to-embed-a-ggplot-image-dynamically-by-row-like-a-sparkline-usi
  
  r_plot <- r_dat %>%
    mutate(plot_var=est_hi-est_lo)%>%
    bind_rows(r_dat%>%
                mutate(plot_var=est_lo,
                       npi_name="blank")) %>%
    mutate(npi_name=factor(npi_name, 
                           levels=c(npi_levels, "blank"), 
                           labels=c(npi_labels,"blank"))) %>%
    select(name, date, estimate, plot_var, npi_name) %>%
    group_by(name) %>%
    nest() %>%
    mutate(plot=map(data, ~ggplot(., aes(x=date, y=plot_var))+
                      geom_col(aes(fill=npi_name), position="stack", width=1)+
                      scale_color_manual(values= c(color_values, "white"),
                                         breaks=c(npi_labels, "white"))+
                      scale_fill_manual(values=c(fill_values, "white"),
                                        breaks=c(npi_labels, "white"))+
                      geom_line(aes(y=estimate, col=npi_name), size=4)+
                      geom_hline(yintercept=1, col="black", size=3)+
                      scale_y_continuous(breaks=c(0, 0.5, 1, 2.25, 3.5))+
                      theme(legend.position="none",
                            axis.line = element_blank(),
                            axis.title = element_blank(),
                            axis.ticks = element_blank(),
                            axis.text= element_blank(),
                            panel.background = element_blank(),
                            panel.grid=element_blank()))) %>%
    select(-data) %>%
    mutate(` `=NA)
  
  # Table 
  r_output <- tibble(r_tab,
                     ` ` = NA,
                     .rows = nrow(r_tab)) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars(` `)),
      fn = function(x) {
        map(r_plot$plot, ggplot_image, height = px(px_qual), aspect_ratio=wh_ratio)
      }
    )
  
  r_output %>%
    tab_options(table.font.size = pct(80)) %>%
    tab_options(column_labels.font.weight = "bold",
                table_body.hlines.color = "#000000",
                table_body.border.bottom.color="#000000",
                table_body.border.top.color = "#000000",
                column_labels.border.top.color = "#000000",
                column_labels.border.bottom.color = "#000000") %>%
    tab_style(style=list(cell_text(style="oblique"),
                         cell_borders(sides="right")), 
              locations=cells_body(columns=vars(Location)))
}


##' Sparkline table for intervention period effectiveness estimates 
##' 
##' @param r_dat df with reduction estimates per sim
##' @param trim whether or not to apply npi_trim to npi_name
##' @param npi_trim pattern used by str_remove to group by npi_name
##' @param periodcolors 
##' @param npi_labels labels for plotted NPIs
##' @param npi_levels levels of NPIs 
##' @param pi_lo lower quantile for summarization
##' @param pi_hi higher quantile for summarization
##' @param geo_dat df with location names
##' @param px_qual sparkline pixel size passed on to ggplot_image
##' @param wh_ratio sparkline width:height ratio passed on to ggplot_image
##' @param brewer_palette pallete name passed on to brewer.pal
##' 
##' @return a table with the effectiveness per intervention period and a bar graph
##' 
##'
##'
##'@export
##'
make_sparkline_tab_intervention_effect <- function(r_dat,
                                                   npi_labels, 
                                                   npi_levels,
                                                   pi_lo=0.025, 
                                                   pi_hi=0.975, 
                                                   geo_dat=geodata,
                                                   trim=TRUE,
                                                   npi_trim="[[A-Z]].+\\_", 
                                                   px_qual=40,
                                                   wh_ratio=3.5,
                                                   brewer_palette="Spectral"
){
  
  require(gt)
  require(tidyverse)
  
  r_dat <- r_dat %>%
    left_join(geo_dat) %>%
    group_by(geoid, start_date, end_date, npi_name, name) %>%
    summarize(est_lo=quantile(reduction, pi_lo, na.rm=TRUE),
              est_hi=quantile(reduction, pi_hi, na.rm=TRUE),
              estimate=mean(reduction, na.rm=TRUE)) %>%
    mutate_if(is.numeric, signif, digits=2) %>%
    filter(npi_name!="local_variance")
  
  # Set new end date for baseline values
  
  if(trim){r_dat<-r_dat%>%
    mutate(npi_name=str_remove(npi_name, npi_trim))}
  
  # Create table with summary values
  r_tab<-r_dat%>%
    mutate_if(is.numeric, as.character) %>%
    mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels),
           est_lo = str_replace(est_lo, "^", '\n\\('),
           est_hi = str_replace(est_hi, "$", '\\)')) %>%
    arrange(npi_name) %>%
    unite(col="pi", est_lo:est_hi, sep="-") %>%
    unite(col="estimate", estimate:pi, sep="\n") %>%
    pivot_wider(id_cols="name", values_from=estimate, names_from=npi_name) %>%
    arrange(name) %>%
    rename(Location=name)
  
  r_tab[is.na(r_tab)] <- ""
  
  # Plotting
  fill_values <- RColorBrewer::brewer.pal(length(npi_labels), brewer_palette)
  fill_values<-fill_values[-1]
  color_values <- colorspace::darken(fill_values, 0.3)
  
  
  # solution from https://stackoverflow.com/questions/61741440/is-there-a-way-to-embed-a-ggplot-image-dynamically-by-row-like-a-sparkline-usi
  
  r_plot <- r_dat %>%
    mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels)) %>%
    select(name, start_date, estimate, est_lo, est_hi, npi_name) %>%
    group_by(name) %>%
    mutate(time=1, 
           time=cumsum(time))%>%
    nest() %>%
    mutate(plot=map(data, ~ggplot(., aes(x=time, y=estimate, ymin=est_lo, ymax=est_hi))+
                      geom_col(aes(fill=npi_name), stat="identity")+
                      geom_errorbar(aes(col=npi_name), size=3) +
                      scale_color_manual(values= c(color_values, "white"),
                                         breaks=c(npi_labels, "white"))+
                      scale_fill_manual(values=c(fill_values, "white"),
                                        breaks=c(npi_labels, "white"))+
                      geom_hline(yintercept=1, col="black", size=3)+
                      theme(legend.position="none",
                            axis.line = element_blank(),
                            axis.title = element_blank(),
                            axis.ticks = element_blank(),
                            axis.text= element_blank(),
                            panel.background = element_blank(),
                            panel.grid=element_blank()))) %>%
    select(-data) %>%
    mutate(` `=NA)
  
  #Table 
  r_output <- tibble(r_tab,
                     ` ` = NA,
                     .rows = nrow(r_tab)) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars(` `)),
      fn = function(x) {
        map(r_plot$plot, ggplot_image, height = px(px_qual), aspect_ratio=wh_ratio)
      }
    )

  r_output %>%
    tab_options(table.font.size = pct(80)) %>%
    tab_options(column_labels.font.weight = "bold",
                table_body.hlines.color = "#000000",
                table_body.border.bottom.color="#000000",
                table_body.border.top.color = "#000000",
                column_labels.border.top.color = "#000000",
                column_labels.border.bottom.color = "#000000") %>%
    tab_style(style=list(cell_text(style="oblique"),
                         cell_borders(sides="right")), 
              locations=cells_body(columns=vars(Location)))
}


##' Time series comparing reported and estimated cases and deaths in each
##' geoid by pdeath or scenario; possible to show hospitalization data
##' 
##' @param truth_dat df with date, geoid, incidI, incidDeath; currhosp if adding
##' hospitalization data
##' @param county_dat df with model estimates 
##' @param hosp whether hospitalization data is included in truth_dat with varname currhosp
##' @param filter_by variable name for filtering estimates either: scenario or pdeath 
##' @param filter_val desired value of variable
##' @param geodata df with location names
##' 
##' @return plot comparing observed and modeled estimates by geoid
##' 
##'
##'
##'@export
##'
plot_truth_by_county <- function(truth_dat,
                                 county_dat,
                                 hosp=FALSE, 
                                 filter_by,
                                 filter_val,
                                 start_date,
                                 end_date,
                                 geo_dat=geodata,
                                 fig_labs=c("Incident Cases", "Incident Deaths"),
                                 pi_lo=0.025,
                                 pi_hi=0.975
){
  
  start_dat<-lubridate::ymd(start_date)
  end_date<-lubridate::ymd(end_date)
  if(filter_by!="pdeath" & filter_by!="scenario") stop("You can only filter by 'pdeath' or 'scenario'")
  
  group_var<-if_else(filter_by=="pdeath", "scenario", "pdeath")
  
  county_dat<-county_dat%>%
    filter(!!as.symbol(filter_by)==filter_val)%>%
    group_by(geoid, !!as.symbol(group_var), sim_num, time=lubridate::floor_date(time, unit="week", week_start=3))%>%
    summarize(NincidCase=sum(NincidCase, na.rm=TRUE),
              NincidDeath=sum(NincidDeath, na.rm=TRUE),
              NhospCurr=sum(NhospCurr, na.rm=TRUE)) %>%
    group_by(geoid) %>%
    filter(time<max(time))
  
  if(hosp){
    
    if(length(fig_labs)!=3){
      fig_labs <- c("Incident Cases", "Incident Deaths", "Occupied Hospital Beds")
    }
    truth_dat <- truth_dat %>%
      group_by(geoid, time=lubridate::floor_date(date, unit="week", week_start=3)) %>%
      summarize(incidI=sum(incidI, na.rm=TRUE),
                incidDeath=sum(incidDeath, na.rm=TRUE),
                currhosp=sum(currhosp, na.rm=TRUE)) %>%
      group_by(geoid)%>%
      filter(time<max(time))
    
    rc <- bind_rows(truth_dat%>%
                      mutate(confirmed=incidI,
                             type=fig_labs[1]),
                    truth_dat%>%
                      mutate(confirmed=incidDeath,
                             type=fig_labs[2]),
                    truth_dat%>%
                      mutate(confirmed=currhosp,
                             type=fig_labs[3])) %>%
      select(-starts_with("incid")) %>%
      right_join(
        bind_rows(county_dat %>%
                    group_by(time, geoid, !!as.symbol(group_var))%>%     
                    summarize(low=quantile(NincidCase,pi_lo),
                              high=quantile(NincidCase,pi_hi),
                              est=mean(NincidCase),
                              type=fig_labs[1]),
                  county_dat %>%
                    group_by(time, geoid, !!as.symbol(group_var))%>%  
                    summarize(low=quantile(NincidDeath,pi_lo),
                              high=quantile(NincidDeath,pi_hi),
                              est=mean(NincidDeath),
                              type=fig_labs[2]),
                  county_dat %>%
                    group_by(time, geoid, !!as.symbol(group_var))%>%  
                    summarize(low=quantile(NhospCurr,pi_lo),
                              high=quantile(NhospCurr,pi_hi),
                              est=mean(NhospCurr),
                              type=fig_labs[3]))) %>%
      ungroup() %>%
      mutate(type = factor(type, levels = fig_labs[3:1]),
             confirmed=if_else(confirmed==0, NA_real_, confirmed))
  } else{
    
    truth_dat <- truth_dat %>%
      group_by(geoid, time=lubridate::floor_date(date, unit="week", week_start=3)) %>%
      summarize(incidI=sum(incidI),
                incidDeath=sum(incidDeath)) %>%
      filter(time<max(time))
    
    rc <- bind_rows(truth_dat%>%
                      mutate(confirmed=incidI,
                             type=fig_labs[1]),
                    truth_dat%>%
                      mutate(confirmed=incidDeath,
                             type=fig_labs[2])) %>%
      select(-starts_with("incid")) %>%
      right_join(
        bind_rows(county_dat %>%
                    group_by(time, geoid, !!as.symbol(group_var))%>%     
                    summarize(low=quantile(NincidCase,pi_lo),
                              high=quantile(NincidCase,pi_hi),
                              est=mean(NincidCase),
                              type=fig_labs[1]),
                  county_dat %>%
                    group_by(time, geoid, !!as.symbol(group_var))%>%
                    summarize(low=quantile(NincidDeath,pi_lo),
                              high=quantile(NincidDeath,pi_hi),
                              est=mean(NincidDeath),
                              type=fig_labs[2]))) %>%
      ungroup() %>%
      mutate(type = factor(type, levels = fig_labs),
             confirmed=if_else(confirmed==0, NA_real_, confirmed))
  }
  
  rc %>%
    group_by(type, !!as.symbol(group_var), geoid)%>%
    filter(time<max(time))%>%
    ungroup()%>%
    filter(time>lubridate::ymd(start_date), time<lubridate::ymd(end_date))%>%
    left_join(geo_dat)%>%
    ggplot(aes(x=time)) +
    geom_line(aes(y=est, color=!!as.symbol(group_var))) +
    geom_ribbon(alpha=0.1, aes(fill=!!as.symbol(group_var), ymin=low, ymax=high))+
    geom_point(aes(y=confirmed), color="black") +
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.title=element_blank(),
          legend.position="bottom",
          strip.background.x = element_blank(),
          strip.background.y=element_rect(fill="white"),
          strip.text.y =element_text(face="bold"))+
    ylab("Counts (log scale)")+
    xlab("Time (weeks)")+ 
    facet_grid(name~ type, scales="free") +
    scale_y_log10()
}

##' Time series comparing Rt estimates by scenario over time
##' @param outcome_dir directory with spar/snpi folders
##' @param truth_dat df with date, geoid, incidI, incidDeath
##' @param scenario_colors colors for each scenario
##' @param scenario_levels levels applied to scenarios
##' @param scenario_labels label applied to scenarios
##' @param start_date start of timeline
##' @param end_date end of timeline
##' @param geo_data df with geoid and pop2010 
##' @param pi_lo lower limit to interval
##' @param pi_hi upper limit to interval
##' 
##' @return a table with the effectiveness per intervention period and a bar graph
##' 
##'
##'
##'@export
##'

plot_rt_ts <- function(outcome_dir, 
                       truth_dat,
                       scenario_colors,
                       scenario_levels,
                       scenario_labels,
                       start_date,
                       end_date, 
                       geo_dat=geodata,
                       susceptible=TRUE,
                       pi_lo=0.025,
                       pi_hi=0.975
){
  require(tidyverse)
  start_date<-as.Date(start_date)
  end_date<-as.Date(end_date)
  
  geoiddate<-crossing(geoid=geo_dat$geoid, date=seq(start_date, end_date, by=1))
  
  rc<-list()
  for(i in 1:length(scenario_levels)){
    rc[[i]]<-load_r_sims_filtered(outcome_dir,
                                  pre_process=function(x){filter(x, scenario==scenario_levels[i])}) %>%
      arrange(geoid, sim_num, start_date) %>%
      mutate(end_date=if_else(npi_name=="local_variance",
                              lead(start_date)-1,
                              end_date)) %>%
      left_join(geoiddate)%>%
      mutate(r=if_else(date<start_date | date>end_date, NA_real_, r)) %>% 
      drop_na()
    
    if(susceptible){
    rc[[i]]<-load_hosp_sims_filtered(outcome_dir,
                                     pre_process=function(x){x%>%
                                         filter(scenario==scenario_levels[i])%>%
                                         select(geoid, scenario, death_rate, location, time, sim_id, incidI)},
                                     post_process=function(x){x%>%
                                         group_by(geoid, pdeath, scenario, sim_num, location)%>%
                                         mutate(cum_inf=cumsum(incidI))}) %>%
      rename(date=time)%>%
      right_join(rc[[i]]) %>%
      left_join(geodata) %>%
      mutate(r=r*(1-cum_inf/pop2010))
    }
    
  }
  
  rc<-bind_rows(rc) %>%
    left_join(geodata)%>%
    group_by(scenario, date) %>%
    mutate(weight=pop2010/sum(pop2010)) %>% # count
    summarize(estimate=Hmisc::wtd.mean(r, weights=weight, normwt=TRUE),
              lower=Hmisc::wtd.quantile(r, weights=weight, normwt=TRUE, probs=pi_lo),
              upper=Hmisc::wtd.quantile(r, weights=weight, normwt=TRUE, probs=pi_hi))
  
  truth_dat<-truth_dat%>%
    filter(NcumulConfirmed!=0)%>%
    calcR0(geodata=geodata, by_geoid=FALSE) %>%
    mutate(scenario="USA Facts")
  
  bind_rows(rc, truth_dat) %>%
    mutate(`Based on`=factor(scenario, 
                             levels=c(scenario_levels, "USA Facts"),
                             labels=c(scenario_labels, "USA Facts confirmed cases"))) %>%
    ggplot(aes(x=date, y=estimate, ymin=lower, ymax=upper))+
    geom_line(aes(col=`Based on`), size=0.75)+
    geom_ribbon(aes(fill=`Based on`), alpha=0.12) +
    geom_hline(yintercept = 1, col="black", alpha=0.6) +
    scale_y_continuous(trans="log1p", breaks=c(0, 0.5, 1, 1.5, 2, 4, 8)) +
    scale_x_date(breaks="1 month", date_labels="%b")+
    scale_color_manual("Scenario",
                       values = c(scenario_colors, "red")) +
    scale_fill_manual("Scenario",
                      values = c(scenario_colors, "red")) +
    theme_bw() +
    ylab("Effective reproduction number (Rt)")+
    xlab("Time")+
    theme(legend.position= "bottom",
          panel.grid=element_blank()) +
    guides(col=guide_legend(nrow=2))
}

##' Plot ratio of outcomes 
##' @param hosp_state_totals df with hospitalization outcomes
##' @param start_date start of comparison period
##' @param end_date end of comparison period
##' @param pdeath_filter select pdeath: high, med, low
##' @param scenario_colors config$report$formatting$scenario_colors
##' @param scenario_levels config$report$formatting$scenario_labels_short
##' @param scenario_labels config$report$formatting$scenario_labels
##' @param pi_lo lower limit to interval
##' @param pi_hi upper lim to interval
##' 
##' @return a table with the effectiveness per intervention period and a bar graph
##' 
##'
##'
##'@export
##'


plot_scn_outcomes_ratio<-function(hosp_state_totals,
                                  start_date,
                                  end_date,
                                  pdeath_filter,
                                  scenario_labels, 
                                  scenario_levels,
                                  scenario_colors,
                                  pi_lo,
                                  pi_hi){
  
  start_date<-lubridate::ymd(start_date)
  end_date<-lubridate::ymd(end_date)
  
  dat_long<- state_hosp_totals %>%
    filter(time<=end_date,
           time>=start_date,
           pdeath==pdeath_filter) %>%
    group_by(scenario, pdeath, sim_num) %>%
    summarize(AvghospCurr=mean(NhospCurr),
              AvgICUCurr=mean(NICUCurr), 
              NincidHosp=sum(NincidHosp),
              NincidICU=sum(NincidICU),
              AvgincidDeath=mean(NincidDeath),
              NincidDeath=sum(NincidDeath),
              AvgincidCase=mean(NincidCase),
              NincidCase=sum(NincidInf)) %>%
    mutate(scenario=factor(scenario, levels=scenario_levels,
                           labels=scenario_labels))
  
  scn_names<-scenario_labels[-1]
  dat_wide<-list()
  for(i in 1:length(scn_names)){
    dat_wide[[i]]<-dat_long %>%
      filter(scenario==scn_names[i]|scenario==scenario_labels[1]) %>%
      arrange(scenario) %>%
      group_by(sim_num) %>%
      mutate_if(is.numeric, function(x){x/lag(x)}) %>%
      drop_na() %>%
      group_by(scenario) %>%
      summarize(AvghospCurr_lo=quantile(AvghospCurr, pi_lo),
                AvghospCurr_hi=quantile(AvghospCurr, pi_hi),
                AvghospCurr=mean(AvghospCurr),
                NincidHosp_lo=quantile(NincidHosp, pi_lo),
                NincidHosp_hi=quantile(NincidHosp, pi_hi),
                NincidHosp=mean(NincidHosp),
                AvgICUCurr_lo=quantile(AvgICUCurr, pi_lo),
                AvgICUCurr_hi=quantile(AvgICUCurr, pi_hi),
                AvgICUCurr=mean(AvgICUCurr),
                NincidICU_lo=quantile(NincidICU, pi_lo),
                NincidICU_hi=quantile(NincidICU, pi_hi),
                NincidICU=mean(NincidICU),
                AvgincidDeath_lo=quantile(AvgincidDeath, pi_lo),
                AvgincidDeath_hi=quantile(AvgincidDeath, pi_hi),
                AvgincidDeath=mean(AvgincidDeath),
                NincidDeath_lo=quantile(NincidDeath, pi_lo),
                NincidDeath_hi=quantile(NincidDeath, pi_hi),
                NincidDeath=mean(NincidDeath),
                AvgincidCase_lo=quantile(AvgincidCase, pi_lo),
                AvgincidCase_hi=quantile(AvgincidCase, pi_hi),
                AvgincidCase=mean(AvgincidCase),
                NincidCase_lo=quantile(NincidCase, pi_lo),
                NincidCase_hi=quantile(NincidCase, pi_hi),
                NincidCase=mean(NincidCase))
  }
  
  plt_dat<-dat_wide %>%
    bind_rows() %>%
    pivot_longer(cols=AvghospCurr_lo:NincidCase) %>%
    mutate(var=case_when(str_detect(name, "Avghosp")~"Daily average of occupied hospital beds", 
                         str_detect(name, "incidHosp")~"Total hospital admissions",
                         str_detect(name, "AvgICU") ~ "Daily average of occupied ICU beds",
                         str_detect(name, "incidICU") ~ "Total ICU admissions",
                         str_detect(name, "AvgincidDeath") ~ "Daily average deaths",
                         str_detect(name, "NincidDeath") ~ "Total deaths",
                         str_detect(name, "AvgincidCase") ~ "Daily average cases",
                         str_detect(name, "NincidCase")~ "Total cases"),
           var=factor(var, levels=c("Daily average cases", "Total cases",
                                    "Daily average of occupied hospital beds", "Total hospital admissions",
                                    "Daily average of occupied ICU beds", "Total ICU admissions",
                                    "Daily average deaths", "Total deaths")),
           name=case_when(str_detect(name, "_lo")~"lower",
                          str_detect(name, "_hi")~"upper", 
                          TRUE~"estimate")) %>%
    pivot_wider(names_from=name, values_from=value)
  
  plt_dat %>%
    ggplot()+
    #geom_col(aes(x=estimate, y=var, fill=scenario), position=position_dodge(0.75), width=1) +
    geom_point(aes(x=estimate, y=var, col=scenario), position=position_dodge(0.75))+
    geom_linerange(aes(xmin=lower, xmax=upper, y=var, group=scenario, col=scenario), position=position_dodge(0.75)) +
    theme_bw() +
    xlab(paste0('Relative to "', scenario_labels[1], '" scenario')) +
    ylab(paste0("Summarized outcomes from ", format(start_date, "%B %d"),"-",format(end_date, "%B %d"))) +
    scale_color_manual("Scenario",
                       values = scenario_cols) +
    scale_x_continuous(trans="log1p", breaks = c(0, 0.5, 1, 2, 3, 6, 9))+
    theme(legend.position="bottom") +
    geom_vline(xintercept=1)
}

##'
##' Function makes a summary table for each county
##'
##' @param current_scenario scenario to summarize
##' @param county_dat contains the relevant hospital data
##' @param start_date summarization period start
##' @param end_date summarization period end
##' @param pi_low low side of the prediction interval
##' @param pi_high high side of the prediction interval
##' @param pdeath_filter if summarizing results for one pdeath only; leave NA to show all 
##' @param pdeath_labels to label pdeath
##' @param pdeath_levels to order pdeaths
##' 
##' @export
##'
make_scn_county_table_withVent <- function(current_scenario,
                                           county_dat, 
                                           pi_lo = 0.025, 
                                           pi_hi = 0.975, 
                                           geo_dat=geodata,
                                           start_date,
                                           end_date,
                                           pdeath_filter = "high", #if NA will plot all IFRs
                                           pdeath_labels=c("1% IFR", "0.5% IFR", "0.25% IFR"),
                                           pdeath_levels=c("high", "med", "low")
){
  
  start_date <- lubridate::ymd(start_date)
  end_date<-lubridate::ymd(end_date)
  
  county_dat<-county_dat %>% 
    left_join(geo_dat) %>%
    mutate(name=factor(name, levels=sort(geo_dat$name)))
  
  county_tab <- county_dat %>% 
    filter(!is.na(time) & scenario==current_scenario) %>% 
    filter(time >= start_date, time <= end_date) %>% 
    group_by(pdeath, sim_num, name) %>%
    summarize(TotalIncidCase = sum(NincidCase, na.rm = TRUE),
              TotalIncidHosp = sum(NincidHosp, na.rm = TRUE),
              TotalIncidICU = sum(NincidICU, na.rm = TRUE),
              TotalIncidVent = sum(NincidVent, na.rm=TRUE),
              TotalIncidDeath = sum(NincidDeath, na.rm = TRUE),
              AvgIncidCase = sum(NincidCase, na.rm=TRUE)/n(),
              AvgIncidDeath = sum(NincidDeath, na.rm = TRUE)/n(),
              maxHospAdm = max(NincidHosp, na.rm=TRUE),
              maxICUAdm = max(NincidICU, na.rm=TRUE),
              maxVentAdm = max(NincidVent, na.rm=TRUE),
              maxHospCap = max(NhospCurr, na.rm = TRUE),
              maxICUCap = max(NICUCurr, na.rm=TRUE),
              maxVentCap = max(NVentCurr, na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(pdeath, name) %>% 
    summarize(nIncidCase_final = mean(TotalIncidCase),
              nIncidCase_lo = quantile(TotalIncidCase, pi_lo),
              nIncidCase_hi = quantile(TotalIncidCase, pi_hi),
              aIncidCase_final = mean(AvgIncidCase),
              aIncidCase_lo = quantile(AvgIncidCase, pi_lo),
              aIncidCase_hi = quantile(AvgIncidCase, pi_hi),
              aIncidDeath_final = mean(AvgIncidDeath),
              aIncidDeath_lo = quantile(AvgIncidDeath, pi_lo),
              aIncidDeath_hi = quantile(AvgIncidDeath, pi_hi),
              nIncidHosp_final = mean(TotalIncidHosp),
              nIncidHosp_lo = quantile(TotalIncidHosp, pi_lo),
              nIncidHosp_hi = quantile(TotalIncidHosp, pi_hi),
              pIncidHosp_final = mean(maxHospAdm),
              pIncidHosp_lo = quantile(maxHospAdm, pi_lo),
              pIncidHosp_hi = quantile(maxHospAdm, pi_hi),
              nIncidICU_final = mean(TotalIncidICU),
              nIncidICU_lo = quantile(TotalIncidICU, pi_lo),
              nIncidICU_hi = quantile(TotalIncidICU, pi_hi),
              pIncidICU_final = mean(maxICUAdm),
              pIncidICU_lo = quantile(maxICUAdm, pi_lo),
              pIncidICU_hi = quantile(maxICUAdm, pi_hi),
              nIncidVent_final = mean(TotalIncidVent),
              nIncidVent_lo = quantile(TotalIncidVent, pi_lo),
              nIncidVent_hi = quantile(TotalIncidVent, pi_hi),
              pIncidVent_final = mean(maxVentAdm),pIncidVent_lo = quantile(maxVentAdm, pi_lo),
              pIncidVent_hi = quantile(maxVentAdm, pi_hi),
              nIncidDeath_final = mean(TotalIncidDeath),
              nIncidDeath_lo = quantile(TotalIncidDeath, pi_lo),
              nIncidDeath_hi = quantile(TotalIncidDeath, pi_hi),
              nCurrHosp_final = mean(maxHospCap),
              nCurrHosp_lo = quantile(maxHospCap, pi_lo),
              nCurrHosp_hi = quantile(maxHospCap, pi_hi),
              nCurrICU_final = mean(maxICUCap),
              nCurrICU_lo = quantile(maxICUCap, pi_lo),
              nCurrICU_hi = quantile(maxICUCap, pi_hi),
              nCurrVent_final = mean(maxVentCap),
              nCurrVent_lo = quantile(maxVentCap, pi_lo),
              nCurrVent_hi = quantile(maxVentCap, pi_hi)) %>%
    ungroup() %>%
    mutate(aIncidCase = prettyNum(conv_round(aIncidCase_final), big.mark=",", scientific=FALSE,trim=TRUE),
           aIncidCase_CI = make_CI(aIncidCase_lo, aIncidCase_hi),
           aIncidDeath = prettyNum(conv_round(aIncidDeath_final), big.mark=",", scientific=FALSE,trim=TRUE),
           aIncidDeath_CI = make_CI(aIncidDeath_lo, aIncidDeath_hi),
           nIncidCase = prettyNum(conv_round(nIncidCase_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nIncidCase_CI = prettyNum(make_CI(nIncidCase_lo, nIncidCase_hi), big.mark=",",scientific=FALSE,trim=TRUE),
           nIncidHosp = prettyNum(conv_round(nIncidHosp_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nIncidHosp_CI = make_CI(nIncidHosp_lo, nIncidHosp_hi),
           pIncidHosp = prettyNum(conv_round(pIncidHosp_final), big.mark=",",scientific=FALSE,trim=TRUE),
           pIncidHosp_CI = make_CI(pIncidHosp_lo, pIncidHosp_hi),
           nCurrHosp = prettyNum(conv_round(nCurrHosp_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nCurrHosp_CI = make_CI(nCurrHosp_lo, nCurrHosp_hi),
           nIncidICU = prettyNum(conv_round(nIncidICU_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nIncidICU_CI = make_CI(nIncidICU_lo, nIncidICU_hi),
           pIncidICU = prettyNum(conv_round(pIncidICU_final), big.mark=",",scientific=FALSE,trim=TRUE),
           pIncidICU_CI = make_CI(pIncidICU_lo, pIncidICU_hi),
           nCurrICU = prettyNum(conv_round(nCurrICU_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nCurrICU_CI = make_CI(nCurrICU_lo, nCurrICU_hi),
           nIncidVent = prettyNum(conv_round(nIncidVent_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nIncidVent_CI = make_CI(nIncidVent_lo, nIncidVent_hi),
           pIncidVent = prettyNum(conv_round(pIncidVent_final), big.mark=",",scientific=FALSE,trim=TRUE),
           pIncidVent_CI = make_CI(pIncidVent_lo, pIncidVent_hi),
           nCurrVent = prettyNum(conv_round(nCurrVent_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nCurrVent_CI = make_CI(nCurrVent_lo, nCurrVent_hi),
           nIncidDeath = prettyNum(conv_round(nIncidDeath_final), big.mark=",",scientific=FALSE,trim=TRUE),
           nIncidDeath_CI = make_CI(nIncidDeath_lo, nIncidDeath_hi)) %>%
    select(-ends_with("lo"), -ends_with("hi"), -ends_with("final"))
  
  county_tab <- county_tab[order(colnames(county_tab))]
  
  county_tab <- county_tab %>%
    unite("CaseAvg", aIncidCase:aIncidCase_CI, sep="\n") %>%
    unite("DeathAvg", aIncidDeath:aIncidDeath_CI, sep="\n") %>%
    unite("HospPeakMax", nCurrHosp:nCurrHosp_CI, sep="\n") %>%
    unite("ICUPeakMax", nCurrICU:nCurrICU_CI, sep="\n") %>%
    unite("VentPeakMax", nCurrVent:nCurrVent_CI, sep="\n") %>%
    unite("DeathIncid", nIncidDeath:nIncidDeath_CI, sep="\n") %>%
    unite("HospIncid", nIncidHosp:nIncidHosp_CI, sep="\n") %>%
    unite("ICUIncid", nIncidICU:nIncidICU_CI, sep="\n") %>%
    unite("CaseIncid", nIncidCase:nIncidCase_CI, sep="\n") %>%
    unite("VentIncid", nIncidVent:nIncidVent_CI, sep="\n") %>%
    unite("HospPeakAdmin", pIncidHosp:pIncidHosp_CI, sep="\n") %>%
    unite("ICUPeakAdmin", pIncidICU:pIncidICU_CI, sep="\n") %>%
    unite("VentPeakAdmin", pIncidVent:pIncidVent_CI, sep="\n")
  
  county_tab <- county_tab[order(colnames(county_tab))] %>%
    select(name, pdeath, starts_with("Case"), starts_with("Hosp"), starts_with("ICU"), starts_with("Vent"), starts_with("Death"))
  
  if(!is.na(pdeath_filter)){
    newnames <- c(NA_character_, "Daily average","Total", "Total", "Daily peak admissions", "Daily peak capacity", "Total", "Daily peak admissions", "Daily peak capacity", "Total", "Daily peak admissions", "Daily peak capacity", "Daily average", "Total")
    
    county_tab %>%
      arrange(name) %>%
      filter(pdeath == pdeath_filter) %>%
      select(-pdeath) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(name = "County", CaseAvg = "CONFIRMED CASES", CaseIncid = "CONFIRMED CASES", HospIncid = "HOSPITALIZATIONS", # pdeath= "IFR", 
                                   HospPeakAdmin = "HOSPITALIZATIONS", HospPeakMax = "HOSPITALIZATIONS", ICUIncid = "ICU", 
                                   ICUPeakAdmin = "ICU", ICUPeakMax = "ICU", VentIncid = "VENTILATIONS", VentPeakAdmin = "VENTILATIONS",
                                   VentPeakMax = "VENTILATIONS", DeathAvg = "DEATHS", DeathIncid = "DEATHS") %>%
      flextable::merge_at(i = 1, j = 2:3, part = "header") %>%
      flextable::merge_at(i = 1, j = 4:6, part = "header") %>%
      flextable::merge_at(i = 1, j = 7:9, part = "header") %>%
      flextable::merge_at(i = 1, j = 10:12, part = "header") %>%
      flextable::merge_at(i = 1, j = 13:14, part = "header") %>%
      flextable::add_header_row(values = c(newnames), top = FALSE) %>%
      #flextable::merge_v(j = 1) %>%
      flextable::autofit() %>%
      #flextable::border(i=seq(3, 174, by = 3), border.bottom=officer::fp_border(color="black")) %>%
      flextable::border(j=c(1,3,6,9,12), border.right = officer::fp_border(color="grey", style = "solid", width=0.5)) %>%
      flextable::align(align="center", part = "all") %>%
      flextable::bold(part="header")%>%
      flextable::bold(j=1, part="body")
  } else {
    newnames <- c(NA_character_,NA_character_,"Daily average", "Total", "Total", "Daily peak admissions", "Daily peak capacity", "Total", "Daily
                peak admissions", "Daily peak capacity", "Total", "Daily peak admissions", "Daily peak capacity", "Daily average", "Total")
    
    county_tab %>%
      mutate(pdeath = factor(pdeath, 
                             levels=pdeath_levels,
                             labels=pdeath_labels)) %>%
      arrange(name, desc(pdeath)) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(name = "County", pdeath = "IFR", CaseAvg = "CONFIRMED CASES", 
                                   CaseIncid = "CONFIRMED CASES", HospIncid = "HOSPITALIZATIONS", 
                                   HospPeakAdmin = "HOSPITALIZATIONS", HospPeakMax = "HOSPITALIZATIONS", ICUIncid = "ICU", 
                                   ICUPeakAdmin = "ICU", ICUPeakMax = "ICU", VentIncid = "VENTILATIONS", VentPeakAdmin = "VENTILATIONS",
                                   VentPeakMax = "VENTILATIONS", DeathAvg = "DEATHS", DeathIncid = "DEATHS") %>%
      flextable::merge_at(i = 1, j = 3:4, part = "header") %>%
      flextable::merge_at(i = 1, j = 5:7, part = "header") %>%
      flextable::merge_at(i = 1, j = 8:10, part = "header") %>%
      flextable::merge_at(i = 1, j = 11:13, part = "header") %>%
      flextable::merge_at(i = 1, j = 14:15, part = "header") %>%
      flextable::add_header_row(values = c(newnames), top = FALSE) %>%
      flextable::merge_v(j = 1) %>%
      flextable::autofit() %>%
      flextable::border(i=seq(3, 174, by = 3), border.bottom=officer::fp_border(color="grey", width=0.5)) %>%
      flextable::border(j=c(2,4,7,10,13), border.right = officer::fp_border(color="grey", style = "solid", width=0.5)) %>%
      flextable::align(align="center", part = "all") %>%
      flextable::bold(part="header")%>%
      flextable::bold(j=c(1,2), part="body")
  }
  
}

##'
##' Function calculates R from model outputs using R0 package
##'
##' @param USAfacts df with observed cases col NincidConfirmed
##' @param geodata geodata file
##' @param included_geoids geoids to include
##' @param by_geoid estimate R for each county 
##' @param min.date start date for analysis
##' @param max.date end date for analysis
##' 
##' @author Kyra Grantz
##' 
##' @export
##'
calcR0 <- function(USAfacts, 
                   geodata, 
                   included_geoids, 
                   by_geoid=FALSE, 
                   min.date=NULL, 
                   max.date=NULL){
  require(R0)
  if(is.null(max.date)){
    max.date <- max(USAfacts$date)-7
  }
  if(is.null(min.date)){
    min.date <- min(USAfacts$date)
  }
  covid <- USAfacts %>% 
    dplyr::rename(Date = date, New.Cases = NincidConfirmed)
  covid <- covid[which(covid$Date>=min.date),]
  mGT <- R0::generation.time("gamma",c(6.5,4.5))
  
  if(by_geoid){
    Rt1 <- list()
    for(i in 1:length(included_geoids)){
      pop <- geodata[geodata$geoid == included_geoids[i],config$spatial_setup$popnodes]
      tmp <- covid %>% dplyr::filter(geoid == included_geoids[i])
      incid <- setNames(tmp$New.Cases,1:nrow(tmp))
      estR0 <- R0::estimate.R(incid, mGT, begin=1, end=as.numeric(length(incid)), methods=c("TD"), pop.size=pop, nsim=1000)
      Rt1[[i]] <- cbind(tmp$Date,estR0$estimates$TD$R,estR0$estimates$TD$conf.int, geoid=rep(included_geoids[i], nrow(tmp)))
      colnames(Rt1[[i]]) <- c("date","estimate","lower","upper", "geoid")
    }
    Rt1 <- dplyr::bind_rows(Rt1)
  }else{
    covid <- covid %>%
      select(-geoid, -source) %>%
      group_by(Date) %>%
      summarise_all(sum) %>%
      ungroup()
    pop <- sum(geodata[geodata$geoid == included_geoids[i],config$spatial_setup$popnodes])
    incid <- setNames(covid$New.Cases,1:nrow(covid))
    estR0 <- R0::estimate.R(incid, mGT, begin=1, end=as.numeric(length(incid)), methods=c("TD"), pop.size=pop, nsim=1000)
    Rt1 <- cbind(covid$Date,estR0$estimates$TD$R,estR0$estimates$TD$conf.int)
    colnames(Rt1) <- c("date","estimate","lower","upper")
  }
  Rt1 <- Rt1[which(Rt1$estimate!=0 & Rt1$date<=max.date),]
  Rt1$scenario <- "Statistical"
  return(Rt1)
}

##'
##' Comparison of pop-adjusted outcomes by IFR
##'
##' @param county_dat df with incident cases, hospitalizations, and deaths
##' @param geo_dat geodata file
##' @param pdeath_levels death rate levels
##' @param pdeath_labels death rate labels
##' @param start_date summarization period start
##' @param end_date summarization period end
##' @param dodger for plotting IFR estimates
##' 
##' @export
##'
##'
plot_outcome_rate<- function(county_dat,
                             start_date, 
                             end_date,
                             geo_dat=geodata,
                             pdeath_levels=c("high", "med", "low"),
                             pdeath_labels=c("1% IFR", "0.5% IFR", "0.25% IFR"),
                             dodger=0
){

  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)
  
  sum_tab <- county_dat %>%
    dplyr::filter(!is.na(time)) %>% 
    dplyr::filter(time >= start_date, time <= end_date) %>% 
    dplyr::group_by(pdeath, sim_num, geoid) %>%
    dplyr::summarize(TotalIncidCase = sum(NincidCase, na.rm = TRUE),
                     TotalIncidHosp = sum(NincidHosp, na.rm = TRUE),
                     TotalIncidDeath = sum(NincidDeath, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    left_join(geo_dat) %>%
    dplyr::mutate(Case=TotalIncidCase/pop2010*1000,
                  Hosp=TotalIncidHosp/pop2010*1000,
                  Death=TotalIncidDeath/pop2010*1000)%>%
    dplyr::group_by(pdeath, name) %>% 
    dplyr::summarize(Case = mean(Case),
                     Hosp = mean(Hosp),
                     Death = mean(Death)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(name=factor(name, levels=sort(geo_dat$name, decreasing=TRUE)),
                  pdeath = factor(pdeath, 
                                  labels = pdeath_labels,
                                  levels = pdeath_levels))
  
  
  rc <- dplyr::bind_rows(
    dplyr::mutate(sum_tab,type=1, est=Death),
    dplyr::mutate(sum_tab,type=2, est=Hosp),
    dplyr::mutate(sum_tab,type=3, est=Case)
    ) %>%
    dplyr::select(type, est, pdeath, name) %>%
    dplyr::mutate(type = factor(type, levels = c(3,2,1), labels = c("Cases", "Hospitalizations", "Deaths"))) %>%
    ggplot(aes(x=est, y=name, col=pdeath)) +
    geom_point(position=position_dodge(dodger)) + 
    scale_x_sqrt() + 
    theme_bw() + 
    facet_grid(~type, scales = "free") +
    xlab("per 1,000 population") + 
    ylab("County") +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(face="bold"),
          strip.background = element_blank())
  
  return(rc)
  
}  

##' Comparison of pop-adjusted outcomes to effectiveness of current intervention
##'
##' @param county_dat df with incident cases, hospitalizations, and deaths
##' @param geo_dat geodata file
##' @param r_dat df with effectiveness estimates, from load_r_sims_filtered
##' @param pdeath_levels death rate levels
##' @param pdeath_labels death rate labels
##' @param start_date summarization period start
##' @param end_date summarization period end
##' 
##' @export
##'
##'
plot_hosp_effec <- function(county_dat,
                            start_date, 
                            end_date,
                            geo_dat=geodata,
                            r_dat=inference_r,
                            pdeath_levels=c("high", "med", "low"),
                            pdeath_labels=c("1% IFR", "0.5% IFR", "0.25% IFR")
){
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)
  
  rc <- county_dat %>% 
    dplyr::filter(!is.na(time)) %>% 
    dplyr::filter(time >= start_date, time <= end_date) %>% 
    dplyr::group_by(pdeath, sim_num, geoid) %>%
    dplyr::summarize(TotalIncidHosp = sum(NincidHosp, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(geo_dat) %>%
    dplyr::mutate(est=TotalIncidHosp/pop2010*1000) %>%
    dplyr::group_by(pdeath, name, geoid) %>% 
    dplyr::summarize(est = mean(est)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pdeath=factor(pdeath, 
                         levels=pdeath_levels, 
                         labels=pdeath_labels))
  
  rc <- r_dat%>%
    dplyr::group_by(geoid, scenario) %>%
    dplyr::filter(npi_name!="local_variance" & max(end_date)==end_date)%>%
    dplyr::summarize(reduc=mean(reduction))%>%
    dplyr::right_join(rc) 
  
    rc<-rc%>%
    ggplot(aes(x=est, y=reduc, label = name, col=pdeath)) +
    ggrepel::geom_text_repel(segment.size = 0.2, alpha = 0.75, segment.alpha=0.5) +
    geom_point() +
    scale_x_sqrt()+
    facet_grid(~pdeath)+
    theme_bw() +
    theme(legend.position = "none",
          strip.text = element_text(face="bold"),
          strip.background = element_blank())+
    xlab(paste0("Hospitalization between ", lubridate::month(start_date, label=TRUE), " ", lubridate::mday(start_date), "-", lubridate::month(end_date, label=TRUE), " ", lubridate::mday(end_date)," per 1,000 people")) +
    ylab("Estimated effectiveness of social distancing since the most recent policy change")
  
    return(rc)
}  

##' Time series for cases, hospitalizations, and ICU by county
##'
##' @param county_dat df with incident cases, hospitalizations, and deaths
##' @param geo_dat geodata file
##' @param pdeath_levels death rate levels
##' @param pdeath_labels death rate labels
##' @param start_date x-axis plot limits
##' @param end_date x-axis plot limits
##' @param pi_lo lower limit to interval
##' @param pi_hi upper limit to interval
##' 
##' @export
##'
##'

plot_county_outcomes <- function(county_dat, 
                                 pi_lo=0.025,
                                 pi_hi=0.975,
                                 start_date,
                                 end_date,
                                 geo_dat=geodata,
                                 pdeath_levels=death_rate_levels,
                                 pdeath_labels=death_rate_labels){
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)
  
  county_dat<- county_dat %>%
    group_by(geoid,time,pdeath)%>%
    summarize(hosp=mean(NhospCurr),
              hosp_hi=quantile(NhospCurr, pi_hi),
              hosp_lo=quantile(NhospCurr, pi_lo),
              icu=mean(NICUCurr),
              icu_lo=quantile(NICUCurr,pi_hi),
              icu_hi=quantile(NICUCurr,pi_lo),
              case=mean(NincidCase),
              case_hi=quantile(NincidCase, pi_hi),
              case_lo=quantile(NincidCase,pi_lo)) %>%
    filter(time>=start_date, time<end_date) %>%
    left_join(geo_dat) %>%
    mutate(pdeath = factor(pdeath,
                           levels=pdeath_levels,
                           labels=pdeath_labels))
  
  bind_rows(county_dat%>%mutate(est=hosp, lo=hosp_lo, hi=hosp_hi,type="Occupied Hospital Beds"),
            county_dat%>%mutate(est=icu, lo=icu_lo, hi=icu_hi,type="Occupied ICU Beds"),
            county_dat%>%mutate(est=case, lo=case_lo, hi=case_hi,type="Incident Cases")) %>%
    ggplot(aes(x=time))+
    geom_line(aes(y=est, color=pdeath))+
    geom_ribbon(aes(ymin=lo, ymax=hi, fill=pdeath), alpha=0.1)+
    facet_grid(name~type, scales = "free_y") +
    scale_y_sqrt()+
    theme_bw()+
    theme(legend.position="top",
          legend.title=element_blank(),
          panel.grid=element_blank(),
          strip.background.x=element_blank(),
          strip.text=element_text(face="bold"),
          strip.background.y = element_rect(fill="white"))+
    ylab("Estimate") +
    xlab("Time")+
    scale_x_date(limits = c(start_date, end_date))
}


