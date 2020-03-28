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
print_pretty_date <- function() {

  return(lubridate::stamp("September 12, 1999"))
}


##'
##' Pretty print short date for text and tables
##'
##' @return a function that formats short pretty dates
##'
##' @export
##'
print_pretty_date_short <- function() {

  return(lubridate::stamp("Sep 12"))
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
    paste0("(", conv_round(lo), ", ", conv_round(hi), ")") }else{
      paste0("(", format.Date(lo, format="%d %b"), ", ", format.Date(hi, format="%d %b"), ")") }
}

##'
##' Plot figure showing 15 random sims of hospitalization occupancy
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
##' @return plot with 15 random simulations of hospital occupancy
##'
##' @export
##'
plot_ts_hosp_state_sample <- function (hosp_state_totals,
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
               aes(x=time, colour = scenario_name,
                   group = interaction(sim_num, scenario_name))) +
    geom_line(aes(y = NincidHosp), alpha=0.3, size=.75) +
    scale_y_continuous("Daily hospital occupancy", labels = scales::comma) +
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
##' Plot figure showing histogram of cumulative hospitalizations by a certain date
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param summary_date date at which to present cumulative summary of hospitalizations
##'
##' @return plot of cum hosp for state across simulations
##'
##' @export
##'
plot_hist_incidHosp_state <- function (hosp_state_totals,
                                       pdeath_level = "high",
                                       scenario_labels,
                                       scenario_cols,
                                       sim_start_date,
                                       summary_date) {

  sim_start_date <- lubridate::ymd(sim_start_date)
  summary_date <- lubridate::ymd(summary_date)

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_state_totals %>%
    dplyr::filter(pdeath==pdeath_level) %>%
    dplyr::filter(time >= sim_start_date & time <= summary_date) %>%
    group_by(scenario_name, sim_num) %>%
    dplyr::summarise(cumHosp = sum(NincidHosp)) %>%
    ungroup %>%
    dplyr::mutate(scenario_name = factor(scenario_name,
                                         levels = scenario_labels,
                                         labels = scenario_labels))

  rc <- ggplot(data=to_plt,
               aes(x = cumHosp, fill = scenario_name, color = scenario_name)) +
    geom_histogram() +
    facet_wrap(scenario_name~., ncol = 1) +
    scale_fill_manual(values = scenario_cols,
                      labels = scenario_labels,
                      aesthetics = c("colour", "fill")) +
    scale_x_continuous(paste("Cumulative hospitalizations by",
                             print_pretty_date()(summary_date)),
                       labels = scales::comma) +
    ylab("Number of simulations") +
    theme_bw() +
    guides("none") +
    theme(legend.position = "none")


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
##' Plot figure showing histogram of peak hospital occupancy by a certain date
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data
##' @param start_date date to filter to start search for peak timing (character string)
##' @param end_date date to filter to end search for peak timing (character string)
##'
##' @return plot of distribution of peak timing across simulations by county
##'
##' @export
##'
plot_county_attack_rate_map <- function (inf_cty_totals,
                                         geodata,
                                         shp,
                                         # pdeath_level = c("high", "med", "low"),
                                         scenario,
                                         display_date) {
  # pdeath_level <- match.arg(pdeath_level)
  # scenario <- match.arg(scenario)
  display_date <- as.Date(display_date)

  shp$geoid <- as.character(shp$geoid)
  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- inf_cty_totals %>%
    dplyr::filter(scenario_name == scenario,
                  time == display_date) %>%
    dplyr::mutate(geoid = ifelse(nchar(geoid)==4, paste0("0",geoid),geoid)) %>%
    left_join(geodata) %>%
    # mutate(attack_rate=Nincid/population)
    group_by(geoid) %>%
    dplyr::summarise(attack_rate=mean(Nincid/pop2010)) %>%
    ungroup

  plot_shp <- left_join(shp, to_plt, by="geoid")

    rc <- ggplot(plot_shp) +
      geom_sf(aes(fill=attack_rate)) +
      theme_minimal() +
      scale_fill_viridis_c(option="plasma", limits=c(0,1)) +
      ggtitle(as.Date(display_date, format="%b %d")) +
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
##' Plot figure showing median and IQR time series for the state for one scenario
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param geodata geodata object from load_config
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
                                    pop_name,
                                    pdeath_level = "high", ## doesn't really matter since data should be the same for infections
                                    scenario_label,
                                    scenario_col,
                                    sim_start_date,
                                    sim_end_date,
                                    plot_intervention = FALSE, ## may not want to plot if it is too complicated
                                    interv_start_date = NA,
                                    interv_end_date = NA) {

  geopop <- geodata %>% 
    dplyr::filter(include_in_report) %>%
    dplyr::select(!! pop_name) %>% 
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


##'
##' Plot modeling assumption parameter distributions
##'
##' @param name parameter name
##' @param config config file
##' @return plot of distribution of peak timing across simulations by county
##'
##' @export
##'
plot_model_parameter_distributions <- function(name, config){
  dist_plot_config <- config$report$plot_settings$parameters_to_display
  local_config <- dist_plot_config[[name]]
  value <- config[[local_config$type]][['parameters']][[name]]
  if((length(value) > 1) & ("distribution" %in% names(value))){
    if(value$distribution == 'uniform' ){
      value <- runif(1e5,covidcommon::as_evaled_expression(value$low) , covidcommon::as_evaled_expression(value$high))
    }
  } else {
    value <- covidcommon::as_evaled_expression(value)
  }
  if('transform' %in% names(dist_plot_config[[name]]) ){
    if(dist_plot_config[[name]][['transform']] == 'invert'){
      value = 1 / value
    }
  }
  rval <- NaN
  if(local_config$distribution == "lnormal"){
    rval <- (rlnorm(1e5,meanlog= value[1], sdlog = value[2]))
  }
  if(local_config$distribution == "exp"){
    rval <- rexp(1e5,rate=value)
  }
  if(local_config$distribution == "gamma"){
    all_compartments <<- unique(report.generation::load_scenario_sims_filtered('mid-west-coast-AZ-NV_CaliforniaMild/',pre_process=function(x){return(x[x$time==x$time[1], ])})$comp)
    number_compartments <<- sum(grepl("I[[:digit:]]+",all_compartments))
    if(length(value)==1){
      value[2] <- 1/value[1]
    }
    rval <- rgamma(1e5,shape=value/number_compartments,scale=number_compartments)
  }
  if('xlim' %in% names(local_config)){
    plt <- plot(density(rval),main = local_config$formal_name, xlab = local_config$xlab, bty='n', xlim=as.numeric(local_config$xlim))
  } else {
    plt <- plot(density(rval),main = local_config$formal_name, xlab = local_config$xlab, bty='n')
  }
  return(plt)
}


##'
##' Make figure captions
##'
##' @param scenario scenario name from config
##' @param scenario_nums
##' @param interv_start_date intervention start date
##' @param interv_end_date intervention end date
##' @param sim_start_date sim start date
##' @param sim_end_date sim end date
##' @param location_name report location name
##' @param figure_num figure number
##'
##' @return plot of distribution of peak timing across simulations by county
##'
##' @export
##'
make_fig_captions <- function(scenario,
                           scenario_nums = length(scenario_labels),
                           interv_start_date = interv_date_start,
                           interv_end_date = interv_date_end,
                           sim_start_date = sim_date_start,
                           sim_end_date = sim_date_end,
                           location_name = report_location_name,
                           figure_num = 1){
  
  
  start.Int = format(as.Date(interv_start_date), "%b %d")  # intervention start date
  end.Int = format(as.Date(interv_end_date), "%b %d")      # intervention end date
  start.Obs = format(as.Date(sim_start_date), "%b %d")  # simulation observation start date
  end.Obs = format(as.Date(sim_end_date), "%b %d")      # simulation observatoin end date
  
  ## Caption for daily hospital occupancy
  if (scenario == "ts_hosp_state_sample"){
    
    return(paste("Figure", figure_num, "**Daily hospital occupancy** for 15 random example simulations (light lines) for", scenario_nums,
                 " planning scenarios of SARS-CoV-2 spread in", location_name," with 1% IFR assumptions. Interventions were applied from", 
                 start.Int," to", end.Int," (grey box).*"))
    
  }
  
  ## Caption for daily incident infections
  if (scenario == "ts_incid_inf_state_sample"){
    caption = paste("Figure", figure_num, "**Daily incident infections** for 15 random example simulations (light lines) for", scenario_nums,
                    " planning scenarios of SARS-CoV-2 spread in", location_name," with 1% IFR assumptions. Interventions were applied from", 
                    start.Int," to", end.Int," (grey box).*")
    return(caption)
    
  }
  
  ## Caption for daily incident deaths
  if (scenario == "ts_incid_death_state_sample_allPdeath"){
    caption = paste("Figure", figure_num, "**Daily incident deaths** for 15 random example simulations (light lines) for", scenario_nums,
                    " planning scenarios of SARS-CoV-2 spread in", location_name," with 1% IFR assumptions. Interventions were applied from", 
                    start.Int," to", end.Int," (grey box).*")
    return(caption)
    
  }
  
  ## Caption for distribution of simulation results for the cumulative number of COVID-19 hospitalizations
  if (scenario == "hist_incidHosp_state"){
    caption = paste("Figure", figure_num, "Distribution of simulation results for the cumulative number of COVID-19 hospitalizations from", 
                    start.Obs, "through", end.Obs, "for", scenario_nums," scenarios with 1% IFR assumptions.*")
    
    return(caption)
  }

  ## Caption for distribution of simulation results for the cumulative number of COVID-19 hospitalizations
  if (scenario == "line_hospPeak_time_county"){
    caption = paste("Figure", figure_num, "Distribution of peak hospital occupancy from", 
                    start.Obs, "through", end.Obs, "for", scenario_nums," scenarios with 1% IFR assumptions.*")
    
    return(caption)
  }

   ## Caption for county-level cumulative attack rates
  if (scenario == "county_attack_rate_map"){
    caption = paste("Figure", figure_num, "County-level cumulative attack rates for", scenario_nums," scenarios.*")
    
    return(caption)
  }


} 



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

tmp <- data.frame(name=c("Infections",
                         "Hospitalizations\n  total", "", "",
                         "  daily peak admissions", "", "",
                         "  daily peak capacity", "", "",
                         "ICU Admissions\n  total", "", "",
                         "  daily peak admissions", "", "",
                         "  daily peak capacity", "", "",
                         "Deaths\n  total", "", ""))
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
      nIncidInf_lo = quantile(TotalIncidInf, 0.25),
      nIncidInf_hi = quantile(TotalIncidInf, 0.75),
      nIncidHosp_final = mean(TotalIncidHosp),
      nIncidHosp_lo = quantile(TotalIncidHosp, 0.25),
      nIncidHosp_hi = quantile(TotalIncidHosp, 0.75),
      pIncidHosp_final = mean(maxHospAdm),
      pIncidHosp_lo = quantile(maxHospAdm, 0.25),
      pIncidHosp_hi = quantile(maxHospAdm, 0.75),
      nIncidICU_final = mean(TotalIncidICU),
      nIncidICU_lo = quantile(TotalIncidICU, 0.25),
      nIncidICU_hi = quantile(TotalIncidICU, 0.75),
      pIncidICU_final = mean(maxICUAdm),
      pIncidICU_lo = quantile(maxICUAdm, 0.25),
      pIncidICU_hi = quantile(maxICUAdm, 0.75),
      nIncidDeath_final = mean(TotalIncidDeath),
      nIncidDeath_lo = quantile(TotalIncidDeath, 0.25),
      nIncidDeath_hi = quantile(TotalIncidDeath, 0.75),
      nCurrHosp_final = mean(maxHospCap),
      nCurrHosp_lo = quantile(maxHospCap, 0.25),
      nCurrHosp_hi = quantile(maxHospCap, 0.75),
      nCurrICU_final = mean(maxICUCap),
      nCurrICU_lo = quantile(maxICUCap, 0.25),
      nCurrICU_hi = quantile(maxICUCap, 0.75)) %>%
    ungroup() %>%
    mutate(pdeath = pdeath_labels[match(pdeath, pdeath_filecode)])


  tmp <- bind_cols(tmp,
                   xx %>% filter(pdeath==pdeath_labels[1]) %>%
                     mutate(ci = make_CI(nIncidInf_lo, nIncidInf_hi),
                            est = conv_round(nIncidInf_final),
                            lvl = paste0("total inc infections")) %>%
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
               paste0(print_pretty_date_short()(table_dates[i]), "\nmean"),
               "\nIQR")
  if(i>1){nlabels <- c(nlabels, paste0("est", i-1), paste0("ci", i-1))}
}
names(tlabels) <- nlabels

flextable(tmp[,nlabels]) %>%
  set_header_labels(values=tlabels) %>%
  valign(valign="bottom") %>%
  colformat_num(digits=0) %>%
  autofit()

}


##'
##' Make caption for statewide table of infections, hosp, ICU, deaths for given scenario
##'
##' @param current_scenario text string of scenario label for which to build table
##' @param table_dates display dates for table
##' @param table_num table number
##'
##' @return plot of distribution of peak timing across simulations by county
##'
##' @export
##'
make_scn_state_table_cap <- function(current_scenario,
                                     table_dates,
                                     table_num = ""){

  to_print <- paste0("Table ", table_num,
                     "Number of infections, hospitalizations, and deaths due to COVID-19 estimated to occur cumulatively by",
                     print_pretty_date_short()(table_dates),
                     "under multiple estimates of the infection fatality rate (IFR) and moderate to high transmission of SARS-CoV-2 under",
                     current_scenario,
                     "scenario of transmission")
  print(to_print)
}


