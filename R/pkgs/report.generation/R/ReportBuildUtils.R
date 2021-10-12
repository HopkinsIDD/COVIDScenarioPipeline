
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
##'Function to add one row per intervention period for interventions using the MultiTimeReduce template
##'
##'@param rt_dat df with estimates of snpi/spar outputs, with columns for geoid, scenario, pdeath, unique intervention names (must start with "npi"), start_date, end_date, 
##'@param n_periods maximum number of non-contiguous dates 
##'
##'@export

mtr_estimates <- function(rt_dat, 
                          n_periods=10){
  
  mtr_start <- rt_dat %>%
    dplyr::select(geoid, scenario, pdeath, starts_with("npi"), start_date) %>%
    dplyr::distinct() %>%
    tidyr::separate(start_date, into = as.character(c(1:n_periods)), sep=",")
  
  mtr_end <- rt_dat %>%
    dplyr::select(geoid, scenario, pdeath, starts_with("npi"), end_date) %>%
    dplyr::distinct() %>%
    tidyr::separate(end_date, into = as.character(c(1:n_periods)), sep=",")
  
  xx <- tibble()
  
  for(i in 1:n_periods){
    
    xx<-mtr_start %>%
      dplyr::select(geoid, scenario, pdeath, tidyselect::starts_with("npi"), start_date=as.symbol(i)) %>%
      dplyr::left_join(mtr_end %>%
                  dplyr::select(geoid, scenario, pdeath, tidyselect::starts_with("npi"), end_date=as.symbol(i))) %>%
      tidyr::drop_na() %>%
      dplyr::right_join(rt_dat%>%
                   dplyr::select(-start_date, -end_date)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(dplyr::across(tidyselect::ends_with("date"), ~lubridate::ymd(.x)))%>%
      dplyr::bind_rows(xx)
  }
  
  return(xx)
  
}

##'
##'Function to generate summary estimates of daily rt  
##' 
##' @param r_dat df with daily r estimates by geoid/sim
##' @param seir_dat df with geoid-specific outputs from SEIR
##' @param geodat
##' @param hi
##' @param lo
##' @param pdeath_filter
##' @param location_only
##' 
##' 
##' @export

rt_estimates <- function(r_dat, 
                         seir_dat, 
                         geodat,
                         lo=0.025,
                         hi=0.975, 
                         pdeath_filter = "med",
                         location_only = TRUE){
  
  geodat<-geodat %>%
    dplyr::rename(pop=starts_with("pop"))
  
  
  r_dat<-r_dat %>%
    dplyr::filter(pdeath==pdeath_filter) %>%
    dplyr::filter(geoid %in% geodat$geoid) 
  
  susceptible <- dat %>%
    dplyr::filter(comp=="S") %>%
    dplyr::left_join(geodat) %>% 
    dplyr::mutate(vacc = value/pop, 
                  vacc = dplyr::case_when(p_comp==0 ~ vacc, 
                                          p_comp==1 ~ vacc*(1-0.5), 
                                          p_comp==2 ~ vacc*(1-0.9)))
  
  susceptible <- susceptible %>% 
    dplyr::group_by(time, scenario, sim_num, pdeath, geoid, pop) %>%
    dplyr::summarize(vacc = sum(vacc))
  
  rc <- r_dat %>%
    dplyr::left_join(susceptible) %>% 
    dplyr::mutate(rt = rt*vacc) %>%  
    dplyr::group_by(scenario, time, location) %>%
    dplyr::mutate(weight=pop/sum(pop))
    
  
  if(location_only){
    rc<-rc%>%
      dplyr::group_by(scenario, date=as.Date(time), pdeath, location) %>%
      dplyr::arrange(rt) %>% 
      dplyr::summarize(estimate=Hmisc::wtd.mean(rt, weights = weight, normwt=TRUE), 
                       lower=Hmisc::wtd.quantile(rt, weights = weight, normwt=TRUE, probs=lo),
                       upper=Hmisc::wtd.quantile(rt, weights = weight, normwt=TRUE, probs=hi)) %>%
      dplyr::rename(geoid=location)
  } else{
    rc_state<-rc%>%
      dplyr::group_by(scenario, date=as.Date(time), pdeath, location) %>%
      dplyr::arrange(rt) %>% 
      dplyr::summarize(estimate=Hmisc::wtd.mean(rt, weights = weight, normwt=TRUE), 
                       lower=Hmisc::wtd.quantile(rt, weights = weight, normwt=TRUE, probs=lo),
                       upper=Hmisc::wtd.quantile(rt, weights = weight, normwt=TRUE, probs=hi)) %>%
      dplyr::rename(geoid=location)
    
    rc<-rc %>%
      dplyr::group_by(scenario, date=as.Date(time), pdeath, location, geoid) %>%
      dplyr::summarize(estimate=Hmisc::wtd.mean(rt, weights = weight, normwt=TRUE), 
                       lower=Hmisc::wtd.quantile(rt, weights = weight, normwt=TRUE, probs=lo),
                       upper=Hmisc::wtd.quantile(rt, weights = weight, normwt=TRUE, probs=hi)) %>%
      dplyr::bind_rows(rc_state)
  }
  
  return(rc)
  
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
##' @param pdeath_filter level of IFR for filtering hospitalization data - TODO: Move our of functions
##' @param scenario_colors colors to plot each scenario in  - TODO: give a default
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
                                       pdeath_filter = pdeath_default,
                                       scenario_colors = config$report$formatting$scenario_colors,
                                       sim_start_date,
                                       sim_end_date,
                                       plot_intervention = FALSE, ## may not want to plot if it is too complicated
                                       interv_start_date = NA,
                                       interv_end_date = NA,
                                       sampled_sims=NULL) {

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  if(is.null(sampled_sims)){
    sampled_sims <- sample(unique(hosp_state_totals$sim_num), 
                         min(num_sims, length(unique(hosp_state_totals$sim_num))),
                         replace=FALSE) 
  }
  
  to_plt <- hosp_state_totals %>%
    dplyr::filter(pdeath==pdeath_filter,
                  sim_num %in% sampled_sims) %>%
    dplyr::mutate(sim_num = factor(sim_num)) %>%
    dplyr::rename(pltvar = !!varname)

  rc <- ggplot(data=to_plt,
               aes(x=time, colour = scenario_name,
                   group = interaction(sim_num, scenario_name))) +
    geom_line(aes(y = pltvar), alpha=0.3, size=.75) +
    scale_y_continuous(varlabel, labels = scales::comma) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 limits = c(as.Date(sim_start_date), as.Date(sim_end_date))) +
    scale_color_manual("Scenario",
                      values = scenario_colors) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
         legend.position = "bottom",
         legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)))


  if(plot_intervention){
    interv_dates <- data.frame(xmin = as.Date(interv_start_date),
                               xmax = as.Date(interv_end_date),
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
##' Plot figure showing histogram of cumulative hospitalizations (or other incident outcome) by a certain date
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param var_name variable name for final size distribution
##' @param pdeath_filter level of IFR (string: high/med/low) for filtering hospitalization data
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param summary_date date at which to present cumulative summary of hospitalizations
##' @param scenario_colors to add scenario colors
##'
##' @return plot of cum hosp for state across simulations
##'
##' @export
##'
plot_hist_incidHosp_state <- function(hosp_state_totals,
                                      var_name,
                                      pdeath_filter = pdeath_default,
                                      scenario_colors = config$report$formatting$scenario_colors,
                                      sim_start_date,
                                      summary_date) {

  sim_start_date <- as.Date(sim_start_date)
  summary_date <- as.Date(summary_date)

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_state_totals %>%
    dplyr::rename(pltVar = !!var_name) %>%
    dplyr::filter(pdeath==pdeath_filter) %>%
    dplyr::filter(time >= sim_start_date & time <= summary_date) %>%
    dplyr::group_by(scenario_name, sim_num) %>%
    dplyr::summarise(pltVar = sum(pltVar)) %>%
    ungroup 

  rc <- ggplot(data=to_plt,
               aes(x = pltVar, fill = scenario_name, color = scenario_name)) +
    geom_histogram(binwidth = 2000) +
    facet_wrap(scenario_name~., ncol = 1) +
    scale_fill_manual(values = scenario_colors,
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
##' Plot map showing infections per 10K on a specific date for one scenario
##'
##' @param cum_inf_geounit_dates dataframe with cumulative infections up through a specific date, produced by load_cum_inf_geounit_dates, perhaps
##' @param varlabel whether showing 'Infections' or 'Cases'
##' @param geodat as loaded by skeleton
##' @param shp shapefile with geounits
##' @param scenariolabel scenario label character string
##' @param popnodes name of pop variable in geodat
##' @param display_datecharacter string of display date for map
##' @param viridis_palette character string of viridis palette
##'
##' @return plot of cumulative infections per 10K by a specific date by geounit for a single scenario
##'
##' @export
##'
plot_geounit_attack_rate_map <- function (cum_inf_geounit_dates,
                                          geodat,
                                          shp,
                                          varlabel = "Infections",
                                          scenariolabel = config$report$formatting$scenario_labels[1],
                                          popnodes = "pop",
                                          display_date,
                                          viridis_palette = "plasma") {

  display_date <- as.Date(display_date)
  shp$geoid <- as.character(shp$geoid)

  to_plt <- cum_inf_geounit_dates %>%
    dplyr::filter(scenario_name == scenariolabel,
                  time == display_date) %>%
    left_join(geodat) %>%
    dplyr::rename(pop = !!popnodes) %>%
    dplyr::group_by(geoid) %>%
    dplyr::summarise(attack_rate=mean(N/pop)*10000) %>%
    dplyr::ungroup()

  plot_shp <- left_join(shp, to_plt, by="geoid")

  rc <- ggplot(plot_shp) +
    geom_sf(aes(fill=attack_rate)) +
    theme_minimal() +
    scale_fill_viridis_c(paste0(varlabel, "\nper 10K"), option=viridis_palette, labels = scales::comma) +
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
##' @param geodat as loaded by skeleton
##' @param shp shapefile with geounits
##' @param scenariolabel scenario label character string
##' @param popnodes name of pop variable in geodat
##' @param display_date string of display date for map
##' @param viridis_palette character string of viridis palette
##'
##' @return map by geounit, filled by variable per 10K at a specific date for a single scenario
##'
##' @export
##'
plot_geounit_map <- function(cum_inf_geounit_dates, 
                             plot_var,
                             geodat, 
                             shp, 
                             legend_name = "Value per 10K",
                             scenariolabel = config$report$formatting$scenario_labels[1], 
                             popnodes = "pop", 
                             display_date,
                             clims = NULL){
  
  display_date <- as.Date(display_date)
  
  shp$geoid <- as.character(shp$geoid)
  to_plt <- cum_inf_geounit_dates %>% 
    dplyr::filter(scenario_name == scenariolabel, 
                  time == display_date) %>% 
    dplyr::left_join(geodat) %>% 
    dplyr::rename(pop = !!popnodes, plot_var = !!plot_var) %>% group_by(geoid) %>% 
    dplyr::summarise(geoid_rate = mean(plot_var/pop) * 10000) %>% 
    dplyr::ungroup
  
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
##' Make statewide table of infections, hosp, ICU, deaths, vents for given scenario
##'
##' @param current_scenario text string of scenario label for which to build table
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param table_dates formatted table_dates object
##' @param pdeath_labels pdeath formatted labels
##' @param pdeath_levels pdeath file column codes
##'
##' @return state scenario table
##'
##' @export
##'
make_scn_state_table_withVent <- function(current_scenario,
                                          hosp_state_totals,
                                          table_dates,
                                          pdeath_labels = c("1%", "0.5%", "0.25%"),
                                          pdeath_levels = c("high", "med", "low")){
  
  ci_lo = 0.025
  ci_hi = 0.975
  
  table_dates<-c(min(hosp_state_totals$time), as.Date(table_dates))
  
  table_names<-""
  for(i in 2:length(table_dates)){
    table_names[i-1]<-paste0(format(table_dates[i-1], "%B %d"), "-", format(table_dates[i], "%B %d"),"\n Mean (95% PI)")
  }
  
  for(i in 2:length(table_dates)){
    xx <- hosp_state_totals %>%
      dplyr::filter(!is.na(time) & scenario==current_scenario) %>%
      dplyr::filter(time <= table_dates[i],
                    time >= table_dates[i-1]) %>%
      dplyr::group_by(pdeath, sim_num) %>%
      dplyr::summarize(TotalIncidInf = sum(NincidInf, na.rm = TRUE),
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
      dplyr::ungroup() %>%
      dplyr::group_by(pdeath) %>%
      dplyr::summarize(
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
      dplyr::ungroup() %>%
      dplyr::mutate(pdeath = pdeath_labels[match(pdeath, pdeath_levels)])
    
    
    xx<-pivot_longer(xx, cols = nIncidInf_final:nCurrVent_hi) %>% 
      mutate(Outcome = case_when(stringr::str_detect(name, "nIncidInf")~"Total infections",
                                 stringr::str_detect(name, "nIncidHosp")~"Total hospital admissions",
                                 stringr::str_detect(name, "pIncidHosp")~"Peak daily hospital admissions",
                                 stringr::str_detect(name, "nCurrHosp")~"Maximum daily hospital occupancy",
                                 stringr::str_detect(name, "nIncidICU")~"Total ICU admissions",
                                 stringr::str_detect(name, "pIncidICU")~"Peak daily ICU admissions",
                                 stringr::str_detect(name, "nCurrICU")~"Maximum daily ICU occupancy",
                                 stringr::str_detect(name, "nIncidVent")~"Total Ventilators",
                                 stringr::str_detect(name, "pIncidVent")~"Peak daily ventilator usage",
                                 stringr::str_detect(name, "nCurrVent")~"Maximum daily ventilator usage",
                                 stringr::str_detect(name, "nIncidDeath")~"Total deaths"), 
             name = case_when(stringr::str_detect(name, "final")~"est", 
                              stringr::str_detect(name, "lo")~"lo",
                              stringr::str_detect(name, "hi")~"hi")) %>%
      pivot_wider(names_from="name", values_from="value") %>%
      mutate(ci = make_CI(lo, hi),
             est = prettyNum(conv_round(est), big.mark=",")) %>%
      dplyr::select(Outcome, pdeath, est, ci) %>%
      unite("est", est:ci, sep = "\n")
    
    xx <- xx %>%
      add_row(.before = 1) %>%
      mutate(est = if_else(is.na(est), table_names[i-1], est))
    
    if(i==2){
      tmp<-xx
    } else{
      tmp <- xx %>%
        dplyr::select(est) %>%
        bind_cols(tmp, .)
    }
  }
  
  tmp[1,1] <- "Outcome"
  tmp[1,2] <- "IFR"
  
  if(unique(hosp_state_totals$pdeath)==1){ # TODO fix this so IFR is dropped when only one estimate is shown
    
    tmp<-tmp%>%
      dplyr::select(-pdeath)
    
    flextable::flextable(tmp[-1,]) %>%
      flextable::set_header_labels(values=tmp[1,]) %>%
      flextable::colformat_num(digits=0) %>%
      flextable::autofit() %>%
      flextable::bold(part="header") %>%
      flextable::bold(j=1, part="body") %>%
      flextable::align(align="center", part="header") %>%
      flextable::align(align="center", part="body") %>%
      flextable::align(align="right", part="body", j=1)
  } else{
    flextable::flextable(tmp[-1,]) %>%
      flextable::set_header_labels(values=tmp[1,]) %>%
      flextable::colformat_num(digits=0) %>%
      flextable::autofit() %>%
      flextable::bold(part="header") %>%
      flextable::bold(j=1, part="body") %>%
      flextable::align(align="center", part="header") %>%
      flextable::align(align="center", part="body") %>%
      flextable::align(align="right", part="body", j=1) %>%
      flextable::merge_v(j=1, part="body")
      
  }
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
    dplyr::mutate(period = cut(time, period_breaks, labels=lbls)) %>%
    dplyr::group_by(period, scenario_name, sim_num) %>% #summarize totals in periods by scenario
    dplyr::summarize(PeriodInf = sum(NincidInf),
              PeriodDeath = sum(NincidDeath),
              PeriodHosp = sum(NincidHosp),
              PeriodPkHosp = max(NhospCurr),
              PeriodICU = sum(NincidICU),
              PeriodPkICU = max(NICUCurr),
              PeriodVent = sum(NincidVent),
              PeriodPkVent = max(NVentCurr)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(period, scenario_name) %>%  #now get means and prediction intervals
    dplyr::summarize(PeriodInfPILow = round(quantile(PeriodInf, probs = c(pi_low)),digits = round_digit),
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
    dplyr::ungroup() %>% ##make hi/low into CIs
    dplyr::mutate(PeriodInfPI = paste(format(PeriodInfPILow,big.mark=","), format(PeriodInfPIHigh,big.mark=","), sep="-"),
           PeriodDeathPI = paste(format(PeriodDeathPILow,big.mark=","), format(PeriodDeathPIHigh,big.mark=","), sep="-"),
           PeriodHospPI = paste(format(PeriodHospPILow,big.mark=","), format(PeriodHospPIHigh,big.mark=","), sep="-"),
           PeriodPkHospPI = paste(format(PeriodPkHospPILow,big.mark=","), format(PeriodPkHospPIHigh,big.mark=","), sep="-"),
           PeriodICUPI = paste(format(PeriodICUPILow,big.mark=","), format(PeriodICUPIHigh,big.mark=","), sep="-"),
           PeriodPkICUPI = paste(format(PeriodPkICUPILow,big.mark=","), format(PeriodPkICUPIHigh,big.mark=","), sep="-"),
           PeriodVentPI = paste(format(PeriodVentPILow,big.mark=","), format(PeriodVentPIHigh,big.mark=","), sep="-"),
           PeriodPkVentPI = paste(format(PeriodPkVentPILow,big.mark=","), format(PeriodPkVentPIHigh,big.mark=","), sep="-")) %>%
    dplyr::select(-PeriodInfPILow, -PeriodInfPIHigh,
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
    dplyr::bind_rows(tbl_df%>%
                       dplyr::select(period,scenario_name, PeriodInf, PeriodInfPI)%>%
                       dplyr::mutate(outcome="Infections in Period")%>%
                       dplyr::rename(mean=PeriodInf,`95% PI`=PeriodInfPI),
                      tbl_df%>%
                        dplyr::select(period,scenario_name, PeriodDeath, PeriodDeathPI)%>%
                        dplyr::mutate(outcome="Deaths in Period")%>%
                        dplyr::rename(mean=PeriodDeath,`95% PI`=PeriodDeathPI),
                      tbl_df%>%
                        dplyr::select(period,scenario_name, PeriodHosp, PeriodHospPI)%>%
                        dplyr::mutate(outcome="Hospital Admissions in Period")%>%
                        rename(mean=PeriodHosp,`95% PI`=PeriodHospPI),
                      tbl_df%>%
                        dplyr::select(period,scenario_name, PeriodPkHosp, PeriodPkHospPI)%>%
                        dplyr::mutate(outcome="Peak Hospital Occupancy in Period")%>%
                        dplyr::rename(mean=PeriodPkHosp,`95% PI`=PeriodPkHospPI),
                      tbl_df%>%
                        dplyr::select(period,scenario_name, PeriodICU, PeriodICUPI)%>%
                        dplyr::mutate(outcome="ICU Admissions in Period")%>%
                        dplyr::rename(mean=PeriodICU,`95% PI`=PeriodICUPI),
                      tbl_df%>%
                        dplyr::select(period,scenario_name, PeriodPkICU, PeriodPkICUPI)%>%
                        dplyr::mutate(outcome="Peak ICU Occupancy in Period")%>%
                        dplyr::rename(mean=PeriodPkICU,`95% PI`=PeriodPkICUPI),
                      tbl_df%>%
                        dplyr::select(period,scenario_name, PeriodVent, PeriodVentPI)%>%
                        dplyr::mutate(outcome="Incident Ventilations in Period")%>%
                        dplyr::rename(mean=PeriodVent,`95% PI`=PeriodVentPI),
                      tbl_df%>%
                        dplyr::select(period,scenario_name, PeriodPkVent, PeriodPkVentPI)%>%
                        dplyr::mutate(outcome="Peak Ventilators in Use in Period")%>%
                        dplyr::rename(mean=PeriodPkVent,`95% PI`=PeriodPkVentPI)) %>%
    dplyr::mutate(period=as.character(period)) %>%
    tidyr::pivot_wider(names_from=period, values_from = c(mean,`95% PI`), names_sep=".")%>%
    setNames(nm = sub("(.*)\\.(.*)", "\\2_\\1", names(.)))%>%
    dplyr::select(outcome,scenario_name,all_of(tmp))
  
  #tells how to group columns
  tbl_df <- flextable::as_grouped_data(tbl_df,groups="outcome")
  tmp <- is.na(tbl_df$scenario_name)
  tbl_df$scenario_name[tmp] <-tbl_df$outcome[tmp]
  tbl_df <- tbl_df%>%dplyr::select(-outcome)
  typology<-tibble(col_keys=colnames(tbl_df),
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
                                     scenariolabel, 
                                     shapefile,
                                     scenario_colors, # TODO provide default arguments
                                     time_caption,
                                     geoid_caption,
                                     value_name,
                                     value_label,
                                     start_date,      # TODO provide default arguments
                                     end_date) {
 
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if(is.null(value_name) & (length(unique(hosp_county_peaks$scenario_name)) == 1)){stop("Value name must be provided if only one scenario is plotted.")}

  value_name <- rlang::sym(value_name)

  hosp_county_peaks$time[is.na(hosp_county_peaks$time)] <- end_date+1
  hosp_county_peaks$time[hosp_county_peaks$time > end_date] <- end_date+1
  hosp_county_peaks$time[hosp_county_peaks$time < start_date] <- start_date-1

  to_plt <- hosp_county_peaks %>%
    dplyr::group_by(geoid, scenario_name) %>%
    dplyr::summarise(mean_time = mean(time),
                     median_time = median(time),
                     low_time = quantile(time, probs=.25, type=1),
                     hi_time = quantile(time, probs=.75, type=1),
                     value = round(mean(!!value_name,na.rm=T),0)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(shapefile, by = c("geoid")) %>%
    dplyr::mutate(name = reorder(name, -as.numeric(median_time)))

  if(length(unique(hosp_county_peaks$scenario_name))==1){
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
                     color = scenario_name)) +
      geom_pointrange() +
      scale_y_date(time_caption,
                   date_breaks = "1 week",
                   date_labels = "%b %d"
                   ) +
      xlab(geoid_caption) +
      scale_color_manual("Scenario",
                         values = scenario_colors) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")  +
      coord_flip()
  }


  return(rc)

}


##'
##' Compare model outputs and data from CSSE
##' 
##' @param state_hosp_totals state hosp data frame
##' @param jhu_obs_dat df with case data by geoid and cols incidI, incidDeath, and currhosp (if plotting hospitalizations)
##' @param scenario_colors character vector with scenario colors
##' @param pdeath_filter IFR level assumption
##' @param obs_data_col character string of observed data color
##' @param ci.L lower bound confidence interval
##' @param ci.U upper bound confidence interval
##' @param date_breaks breaks for dates in figure
##' @param sim_start_date simulation start date
##' @param sim_end_date simulation end date
##' @param week whether to aggregate incident cases/deaths to weeks
##' @param hosp whether to show hosp values 
##' 
##' @export
plot_model_vs_obs <- function(state_hosp_totals,
                              jhu_obs_dat,
                              scenario_colors,
                              pdeath_filter = pdeath_default,
                              obs_data_col = "black",
                              ci.L = 0,
                              ci.U = 1,
                              date_breaks = "1 month",
                              sim_start_date,
                              sim_end_date,
                              week=FALSE,
                              hosp=FALSE,
                              tendency="mean"
) {
  
  state_hosp_totals <-
    state_hosp_totals %>%
    dplyr::filter(pdeath == pdeath_filter) %>%
    dplyr::mutate(sim_num = factor(sim_num)) %>%
    dplyr::rename(date = time) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(between(date, as.Date(sim_start_date), as.Date(sim_end_date)))
  
  jhu_obs_dat <- jhu_obs_dat %>%
    dplyr::select(source, date, incidI, incidDeath) %>%
    dplyr::filter(between(date, as.Date(sim_start_date), as.Date(sim_end_date))) %>%
    dplyr::group_by(source, date) %>%
    dplyr::summarise_all(sum, na.rm=TRUE) %>% 
    ungroup()%>%
    rename(NincidConfirmed=incidI,
           NincidDeathsObs=incidDeath) 
  
  if(hosp){
    state_hosp_summary <-
      state_hosp_totals %>%
      dplyr::group_by(date, scenario_name) %>%
      dplyr::summarize(lo = quantile(NhospCurr, ci.L),
                       hi = quantile(NhospCurr, ci.U),
                       mean= mean(NhospCurr),
                       median = median(NhospCurr))%>%
      dplyr::rename(est=!!as.symbol(tendency))
    
    incid_hosp_plot <-
      ggplot(state_hosp_summary, aes(x = date)) +
      geom_line(aes(y = est, color = scenario_name)) +
      geom_ribbon(aes(ymin=lo, ymax=hi, fill = scenario_name), linetype = 0, alpha=0.2) +
      geom_point(data = jhu_obs_dat %>% filter(date < max(date)), aes(x = date, y = currhosp), color = obs_data_col) +
      #ylab("Incident Cases") +
      #theme(legend.position = "bottom") +
      scale_x_date(date_breaks = date_breaks,
                   date_labels = "%b %Y",
                   limits = c(as.Date(sim_start_date), as.Date(sim_end_date))) +
      scale_y_continuous("Daily occupied hospital beds", labels = scales::comma) +
      scale_color_manual("Scenario",
                         values = scenario_colors) +
      theme_minimal() +
      theme(axis.title.x =  element_blank(),
            axis.text.x = element_text(angle = 45),
            legend.position = "bottom",
            legend.title = element_blank()) +
      guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)),
             fill = FALSE) +
      coord_cartesian(ylim = c(0, 1.5*max(jhu_obs_dat$currhosp)))
  }
  
  if(week){
    jhu_obs_dat <- jhu_obs_dat %>%
      dplyr::group_by(date=lubridate::ceiling_date(date, "weeks")) %>%
      dplyr::summarize(NincidConfirmed=sum(NincidConfirmed),
                       NincidDeathsObs=sum(NincidDeathsObs))%>%
      ungroup() %>%
      dplyr::filter(date!=max(date))
  }
  
  state_inf_summary <-
    state_hosp_totals %>%
    {if(week) dplyr::group_by(.,date=lubridate::ceiling_date(date, "weeks"), scenario_name, sim_num) %>%
        dplyr::summarize(NincidInf=sum(NincidInf),
                         NincidCase=sum(NincidCase))
      else(.)}%>%
    dplyr::group_by(date, scenario_name) %>%
    dplyr::summarize(
      # ci_lower_incid_inf = quantile(NincidInf, ci.L),
      #                ci_upper_incid_inf = quantile(NincidInf, ci.U),
      #                mean_incid_inf = mean(NincidInf),
      #                median_incid_inf = median(NincidInf),
      lo = quantile(NincidCase, ci.L),
      hi = quantile(NincidCase, ci.U),
      mean = mean(NincidCase),
      median = median(NincidCase)) %>%
    dplyr::group_by(scenario_name) %>%
    dplyr::filter(date!=max(date))%>%
    dplyr::rename(est=!!as.symbol(tendency))
  
  ### Incidence of infections plot
  incid_infections_plot <-
    ggplot(state_inf_summary, aes(x = date)) +
    geom_line(aes(y = est, color = scenario_name)) +
    geom_ribbon(aes(ymin=lo, ymax=hi, fill = scenario_name), linetype = 0, alpha=0.2) +
    geom_point(data = jhu_obs_dat %>% filter(date < max(date)), aes(x = date, y = NincidConfirmed), color = obs_data_col) +
    #ylab("Incident Cases") +
    #theme(legend.position = "bottom") +
    scale_x_date(date_breaks = date_breaks,
                 date_labels = "%b %Y",
                 limits = c(as.Date(sim_start_date), as.Date(sim_end_date))) +
    scale_y_continuous("Incident Cases", labels = scales::comma) +
    scale_color_manual("Scenario",
                       values = scenario_colors) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          axis.text.x = element_text(angle = 45),
          legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)),
           fill = FALSE) +
    coord_cartesian(ylim = c(0, 1.5*max(jhu_obs_dat$NincidConfirmed)))
  
  state_death_summary <-
    state_hosp_totals %>%
    {if(week) dplyr::group_by(.,date=lubridate::ceiling_date(date, "weeks"), scenario_name, sim_num) %>%
        dplyr::summarize(NincidDeath=sum(NincidDeath))
      else(.)}%>%
    dplyr::group_by(date, scenario_name) %>%
    dplyr::summarize(lo = quantile(NincidDeath, ci.L),
                     hi = quantile(NincidDeath, ci.U),
                     mean = mean(NincidDeath),
                     median = median(NincidDeath)) %>%
    dplyr::group_by(scenario_name) %>%
    dplyr::filter(date!=max(date)) %>%
    dplyr::rename(est=!!as.symbol(tendency))
  
  incid_deaths_plot <-
    ggplot(state_death_summary, aes(x = date)) +
    geom_line(aes(y = est, color = scenario_name)) +
    geom_ribbon(aes(ymin=lo, ymax=hi, fill = scenario_name), linetype = 0, alpha=0.2) +
    geom_point(data = jhu_obs_dat %>% filter(date<max(date)), aes(x = date, y = NincidDeathsObs), color = obs_data_col) +
    #ylab("Incident Cases") +
    #theme(legend.position = "bottom") +
    scale_x_date(date_breaks = date_breaks,
                 date_labels = "%b %Y",
                 limits = c(as.Date(sim_start_date), as.Date(sim_end_date))) +
    scale_y_continuous("Incident Deaths", labels = scales::comma) +
    scale_color_manual("Scenario",
                       values = scenario_colors) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          axis.text.x = element_text(angle = 45),
          legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)),
           fill = FALSE) +
    coord_cartesian(ylim = c(0, 1.5*max(jhu_obs_dat$NincidDeathsObs)))
  
  if(hosp){
    output <- list(incid_infections_plot, incid_deaths_plot, incid_hosp_plot)
  } else {
    output <- list(incid_infections_plot, incid_deaths_plot)
  } 
  return(output)
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
plot_needs_relative_to_threshold_heatmap <- function(hosp_geounit_relative,
                                                      scenario_labels,
                                                      scale_colors = c("#066f6c", "#f8e6e7", "#ba0a0f"),
                                                      legend_title,
                                                      value_name,
                                                      value_label,
                                                      start_date,
                                                      end_date,
                                                      incl_geoids = NULL){

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date) 

  if(is.null(incl_geoids)) { incl_geoids <- unique(hosp_geounit_relative$geoid)}
  
  plt_dat <- hosp_geounit_relative %>%
    dplyr::rename(threshold = !!value_name) %>%
    dplyr::filter(time >= start_date & time <= end_date) %>%
    dplyr::filter(scenario_name %in% scenario_labels) %>%
    dplyr::arrange(name)%>%
    dplyr::group_by(scenario_name, time) %>%
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
      facet_wrap(~scenario_name, nrow = 1)
  }
  
  return(rc)
}



##' Plotting R or effectiveness estimates
##' 
##' @param r_dat df with R or reduction estimates per sim and npi to be included
##' @param current_scenario name of scenario inputs to use
##' @param periodcolors 
##' @param npi_labels labels for plotted NPIs optional
##' @param npi_levels levels of NPIs optional
##' @param effectiveness whether to reduction estimates instead
##' @param pi_lo lower quantile for summarization
##' @param pi_hi higher quantile for summarization
##' @param geodat df with location names
##' 
##' @return plot estimated R or effectiveness of intervention periods by geoid
##' 
##'
##'
##'@export

plot_inference_r <- function(r_dat,
                             current_scenario,
                             npi_labels=NA, 
                             npi_levels=NA,
                             periodcolors = c("chartreuse3", "brown2", "turquoise4", "black"),
                             effectiveness=FALSE,
                             pi_lo=0.25,
                             pi_hi=0.75, 
                             geodat=geodata){
  
  rplot <- r_dat %>%
    dplyr::filter(current_scenario==scenario)%>%
    dplyr::left_join(geodat) %>%
    dplyr::group_by(geoid, npi_name, name) %>%
    dplyr::summarize(r_lo = quantile(r, pi_lo, na.rm=TRUE), 
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
      dplyr::filter(npi_name!="local_variance") %>%
      dplyr::mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels)) %>%
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
      dplyr::mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels)) %>%
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
##' @param county_dat df with SEIR outputs 
##' @param current_scenario name of scenario inputs to use
##' @param susceptible whether to show estimates adjusted for cumulative infections, 
##' in which case the estimate reflects the median Rt for past interventions and the Rt for
##' Sys.Date() for ongoing interventions
##' @param npi_labels labels for plotted NPIs
##' @param npi_levels levels of NPIs 
##' @param pdeath_filter which pdeath value to select, does not support multiple pdeath
##' @param pi_lo lower quantile for summarization
##' @param pi_hi upper quantile for summarization
##' @param geodat df with location names
##' @param px_qual sparkline pixel size passed on to ggplot_image
##' @param wh_ratio sparkline width:height ratio passed on to ggplot_image
##' @param brewer_palette pallete name passed on to brewer.pal
##' 
##' @return a table with the R per intervention period and a bar graph - note 
##' 
##'
##'
##'@export

make_sparkline_tab_r <- function(r_dat,
                                 county_dat=NULL,
                                 susceptible=TRUE,
                                 current_scenario,
                                 npi_labels, 
                                 npi_levels,
                                 pdeath_filter = pdeath_default, 
                                 pi_lo=0.025, 
                                 pi_hi=0.975, 
                                 geodat=geodata,
                                 px_qual=40,
                                 wh_ratio=3.5,
                                 brewer_palette="Spectral"
){
  
  require(gt)
  require(tidyverse)
  if(length(npi_labels)!=length(npi_levels)) {stop("Length of npi levels and labels must be equal")}
  if(susceptible & is.null(county_dat)) {stop("You must specify county_dat")}
 
  timeline<-crossing(geoid=unique(r_dat$geoid), 
                     date = seq(min(r_dat$start_date), max(r_dat$end_date), by=1))
  
  intervention_names <- r_dat %>%
    dplyr::filter(scenario==current_scenario,
                  pdeath==pdeath_filter & npi_name %in% npi_levels) %>%
    dplyr::distinct(npi_name, start_date, end_date, geoid) %>%
    dplyr::group_by(geoid) %>%
    dplyr::mutate(end_date=if_else(npi_name=="local_variance",
                                   lead(start_date)-1,
                                   end_date)) 
  
  r_dat2 <- list()
  for(i in 1:length(unique(r_dat$geoid))){
    r_dat2[[i]] <- r_dat%>%
      dplyr::filter(scenario==current_scenario,
                    pdeath==pdeath_filter,
                    geoid==unique(r_dat$geoid)[i]) %>%
      dplyr::select(geoid, sim_num, npi_name, reduction, local_r, scenario, start_date, end_date) %>%
      dplyr::left_join(timeline) %>%
      dplyr::mutate(reduction=if_else(date<start_date | date>end_date, NA_real_, reduction)) %>% 
      drop_na() %>%
      dplyr::mutate(reduction=ifelse(npi_name=="local_variance", 1, 1-reduction)) %>%
      dplyr::group_by(geoid, sim_num, date, scenario) %>%
      dplyr::summarize(reduction=prod(reduction),
                       local_r=unique(local_r)) %>%
      dplyr::mutate(r=reduction*local_r) %>%
      dplyr::select(-local_r, -reduction)
    
    if(susceptible){
      susceptible_dat <- county_dat %>%
        dplyr::filter(comp=="S" & geoid == unique(r_dat$geoid)[i] & pdeath==pdeath_filter & scenario==current_scenario) %>%
        dplyr::left_join(geodat) %>% 
        dplyr::mutate(vacc = value/pop, 
                      vacc = dplyr::case_when(p_comp==0 ~ vacc, 
                                              p_comp==1 ~ vacc*(1-0.5), 
                                              p_comp==2 ~ vacc*(1-0.9)))
      
      susceptible_dat <- susceptible_dat %>% 
        dplyr::group_by(time, scenario, sim_num, pdeath, geoid, location, pop, name) %>%
        dplyr::summarize(vacc = sum(vacc))
      
      r_dat2[[i]]<- r_dat2[[i]] %>%
        dplyr::left_join(susceptible_dat) %>% 
        dplyr::mutate(rt = r*vacc) %>%  
        dplyr::group_by(scenario, time, location) %>%
        dplyr::mutate(weight=pop/sum(pop))
    }
    
    r_dat2[[i]] <- r_dat2[[i]] %>%
      dplyr::group_by(geoid, name, date) %>%
      dplyr::summarize(est_lo=quantile(r, pi_lo, na.rm=TRUE),
                       est_hi=quantile(r, pi_hi, na.rm=TRUE),
                       estimate=mean(r, na.rm=TRUE))
  }
  
  r_dat <- bind_rows(r_dat2)
  
  r_dat <- r_dat %>%
    left_join(intervention_names) %>%
    dplyr::mutate(estimate = if_else(date<start_date|date>end_date, NA_real_, estimate)) %>%
    drop_na() %>%
    dplyr::group_by(geoid) %>%
    dplyr::mutate(mid_point=if_else(max(end_date)==end_date, # for summary values mid_point=date
                                    Sys.Date(),
                                    start_date+floor((end_date-start_date)/2)))
  
  # Create table with summary values
  r_tab<-r_dat%>%
    dplyr::filter(mid_point==date) %>%
    dplyr::mutate_if(is.numeric, signif, digits=2) %>%
    dplyr::mutate_if(is.numeric, as.character) %>%
    dplyr::mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels),
           est_lo = stringr::str_replace(est_lo, "^", '\n\\('),
           est_hi = stringr::str_replace(est_hi, "$", '\\)')) %>%
    dplyr::arrange(npi_name) %>%
    unite(col="pi", est_lo:est_hi, sep="-") %>%
    unite(col="estimate", estimate:pi, sep="\n") %>%
    tidyr::pivot_wider(id_cols="name", values_from=estimate, names_from=npi_name) %>%
    dplyr::arrange(name) %>%
    dplyr::rename(Location=name)
  
  r_tab[is.na(r_tab)] <- ""
  
  
  # Plotting
  npi_num = length(npi_labels)
  expand_palette = colorRampPalette(RColorBrewer::brewer.pal(npi_num, brewer_palette))
  fill_values <- expand_palette(npi_num)
  
  color_values <- colorspace::darken(fill_values, 0.3)

  
  # solution from https://stackoverflow.com/questions/61741440/is-there-a-way-to-embed-a-ggplot-image-dynamically-by-row-like-a-sparkline-usi
  
  r_plot <- r_dat %>%
    dplyr::mutate(plot_var=est_hi-est_lo)%>%
    dplyr::bind_rows(r_dat%>%
                       dplyr::mutate(plot_var=est_lo,
                                     npi_name="blank")) %>%
    dplyr::mutate(npi_name=factor(npi_name, 
                           levels=c(npi_levels, "blank"), 
                           labels=c(npi_labels,"blank"))) %>%
    dplyr::select(name, date, estimate, plot_var, npi_name) %>%
    dplyr::arrange(name)%>%
    dplyr::group_by(name) %>%
    tidyr::nest() %>%
    dplyr::mutate(plot=purrr::map(data, ~ggplot(., aes(x=date, y=plot_var))+
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
    dplyr::select(-data) %>%
    dplyr::mutate(` `=NA)
  
  # Table 
  r_output <- tibble(r_tab,
                     ` ` = NA,
                     .rows = nrow(r_tab)) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars(` `)),
      fn = function(x) {
        purrr::map(r_plot$plot, ggplot_image, height = px(px_qual), aspect_ratio=wh_ratio)
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
##' @param r_dat df with reduction estimates per sim (from load_r_sims_filtered)
##' @param periodcolors 
##' @param npi_labels labels for plotted NPIs
##' @param npi_levels levels of NPIs 
##' @param current_scenario name of current scenario to use
##' @param pi_lo lower quantile for summarization
##' @param pi_hi higher quantile for summarization
##' @param geodat df with location names
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
                                                   current_scenario, 
                                                   pdeath_filter=pdeath_default,
                                                   pi_lo=0.025, 
                                                   pi_hi=0.975, 
                                                   geodat=geodata,
                                                   px_qual=40,
                                                   wh_ratio=3.5,
                                                   brewer_palette="Spectral"
){
  
  require(gt)
  require(tidyverse)
  
  r_dat <- r_dat %>%
    dplyr::filter(scenario==current_scenario,
                  pdeath==pdeath_filter) %>%
    dplyr::left_join(geodat) %>%
    dplyr::group_by(geoid, start_date, end_date, npi_name, name) %>%
    dplyr::summarize(est_lo=quantile(reduction, pi_lo, na.rm=TRUE),
                      est_hi=quantile(reduction, pi_hi, na.rm=TRUE),
                      estimate=mean(reduction, na.rm=TRUE)) %>%
    dplyr::mutate_if(is.numeric, signif, digits=2) %>%
    dplyr::filter(npi_name!="local_variance")
  
  # Create table with summary values
  r_tab<-r_dat%>%
    dplyr::mutate_if(is.numeric, as.character) %>%
    dplyr::mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels),
                   est_lo = stringr::str_replace(est_lo, "^", '\n\\('),
                   est_hi = stringr::str_replace(est_hi, "$", '\\)')) %>%
    dplyr::arrange(npi_name) %>%
    unite(col="pi", est_lo:est_hi, sep="-") %>%
    unite(col="estimate", estimate:pi, sep="\n") %>%
    tidyr::pivot_wider(id_cols="name", values_from=estimate, names_from=npi_name) %>%
    dplyr::arrange(name) %>%
    dplyr::rename(Location=name)
  
  r_tab[is.na(r_tab)] <- ""
  
  # Plotting
  npi_num = length(npi_labels)
  expand_palette = colorRampPalette(RColorBrewer::brewer.pal(npi_num, brewer_palette))
  fill_values <- expand_palette(npi_num)
  color_values <- colorspace::darken(fill_values, 0.3)
  
  
  # solution from https://stackoverflow.com/questions/61741440/is-there-a-way-to-embed-a-ggplot-image-dynamically-by-row-like-a-sparkline-usi
  
  r_plot <- r_dat %>%
    dplyr::mutate(npi_name=factor(npi_name, levels=npi_levels, labels=npi_labels)) %>%
    dplyr::select(name, start_date, estimate, est_lo, est_hi, npi_name) %>%
    dplyr::arrange(name)%>%
    dplyr::group_by(name) %>%
    dplyr::mutate(time=1, 
                  time=cumsum(time))%>%
    tidyr::nest() %>%
    dplyr::mutate(plot=purrr::map(data, ~ggplot(., aes(x=time, y=estimate, ymin=est_lo, ymax=est_hi))+
                                    geom_point(aes(fill=npi_name), stat="identity", size=15)+
                                    geom_errorbar(aes(col=npi_name), size=5) +
                                    scale_color_manual(values= c(color_values),
                                                       breaks=c(npi_labels))+
                                    scale_fill_manual(values=c(fill_values),
                                                      breaks=c(npi_labels))+
                                    geom_hline(yintercept=1, col="black", size=3)+
                                    geom_hline(yintercept=0, col="black", size=3,  linetype="dashed")+
                                    theme(legend.position="none",
                                          axis.line = element_blank(),
                                          axis.title = element_blank(),
                                          axis.ticks = element_blank(),
                                          axis.text= element_blank(),
                                          panel.background = element_blank(),
                                          panel.grid=element_blank()))) %>%
    dplyr::select(-data) %>%
    dplyr::mutate(` `=NA)
  
  #Table 
  r_output <- tibble(r_tab,
                     ` ` = NA,
                     .rows = nrow(r_tab)) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars(` `)),
      fn = function(x) {
        purrr::map(r_plot$plot, ggplot_image, height = px(px_qual), aspect_ratio=wh_ratio)
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
##' @param geodat df with location names
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
                                 geodat=geodata,
                                 fig_labs=c("Incident Cases", "Incident Deaths"),
                                 pi_lo=0.025,
                                 pi_hi=0.975
){
  
  start_dat<-as.Date(start_date)
  end_date<-as.Date(end_date)
  if(filter_by!="pdeath" & filter_by!="scenario") stop("You can only filter by 'pdeath' or 'scenario'")
  
  group_var<-if_else(filter_by=="pdeath", "scenario_name", "pdeath")
  
  county_dat<-county_dat%>%
    dplyr::filter(!!as.symbol(filter_by)==filter_val)%>%
    dplyr::group_by(geoid, !!as.symbol(group_var), sim_num, time=lubridate::ceiling_date(time, unit="week"))%>%
    dplyr::summarize(NincidCase=sum(NincidCase, na.rm=TRUE),
                      NincidDeath=sum(NincidDeath, na.rm=TRUE),
                      NhospCurr=mean(NhospCurr, na.rm=TRUE)) %>%
    dplyr::group_by(geoid) %>%
    dplyr::filter(time<max(time))
  
  if(hosp){
    
    if(length(fig_labs)!=3){
      fig_labs <- c("Incident Cases", "Incident Deaths", "Occupied Hospital Beds")
    }
    truth_dat <- truth_dat %>%
      dplyr::group_by(geoid, time=lubridate::ceiling_date(date, unit="week")) %>%
      dplyr::summarize(incidI=sum(incidI, na.rm=TRUE),
                        incidDeath=sum(incidDeath, na.rm=TRUE),
                        currhosp=mean(currhosp, na.rm=TRUE)) %>%
      dplyr::group_by(geoid)%>%
      dplyr::filter(time<max(time))
    
    rc <- bind_rows(truth_dat%>%
                       dplyr::mutate(confirmed=incidI,
                              type=fig_labs[1]),
                      truth_dat%>%
                        dplyr::mutate(confirmed=incidDeath,
                               type=fig_labs[2]),
                      truth_dat%>%
                        dplyr::mutate(confirmed=currhosp,
                               type=fig_labs[3])) %>%
      dplyr::select(-starts_with("incid")) %>%
      dplyr::right_join(
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
      dplyr::mutate(type = factor(type, levels = fig_labs[3:1]),
                     confirmed=if_else(confirmed==0, NA_real_, confirmed))
  } else{
    
    truth_dat <- truth_dat %>%
      dplyr::group_by(geoid, time=lubridate::ceiling_date(date, unit="week")) %>%
      dplyr::summarize(incidI=sum(incidI),
                       incidDeath=sum(incidDeath)) %>%
      dplyr::filter(time<max(time))
    
    rc <- dplyr::bind_rows(truth_dat%>%
                            mutate(confirmed=incidI,
                                   type=fig_labs[1]),
                          truth_dat%>%
                            mutate(confirmed=incidDeath,
                                   type=fig_labs[2])) %>%
      dplyr::select(-starts_with("incid")) %>%
      dplyr::right_join(
        dplyr::bind_rows(county_dat %>%
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
      dplyr::mutate(type = factor(type, levels = fig_labs),
                     confirmed=if_else(confirmed==0, NA_real_, confirmed))
  }
  
  plot_rc<-list()
  
  for(i in 1:length(unique(as.character(rc$type)))){
    plot_rc[[i]]<-rc %>%
      filter(type==unique(as.character(rc$type))[i]) %>%
      dplyr::group_by(!!as.symbol(group_var), geoid)%>%
      dplyr::filter(time<max(time))%>%
      dplyr::ungroup()%>%
      dplyr::filter(time>as.Date(start_date), time<as.Date(end_date))%>%
      dplyr::left_join(geodat)%>%
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
      facet_grid(rows=vars(name), scales="free") +
      scale_y_sqrt()+
      labs(subtitle = unique(as.character(rc$type))[i])
  }
  
  return(plot_rc)
}

##' Time series comparing Rt estimates by scenario over time
##' @param rc df with daily rt estimates for one geoid/location and columns "estimate", "lower", and "upper"
##' @param geodat df with geoid and population columns
##' @param pdeath_filter which pdeath to select
##' @param scenario_colors colors for each scenario
##' @param scenario_levels levels applied to scenarios
##' @param scenario_labels label applied to scenarios
##' @param truth_source
##' 
##' @return a table with the effectiveness per intervention period and a bar graph
##' 
##'
##'
##'@export
##'

plot_rt_ts <- function(rc, 
                       geodat,
                       truth_dat, 
                       scenario_levels = scn_levels, 
                       scenario_labels = scn_labels,
                       scenario_colors = scn_colors, 
                       truth_source = "JHU CSSE", 
                       pdeath_filter = "med"){
  
  geodat<-geodat %>%
    dplyr::rename(pop=starts_with("pop"))
  
  truth_dat<-truth_dat%>%
    dplyr::filter(geoid %in% geodat$geoid) %>%
    dplyr::group_by(date)%>% 
    dplyr::summarize(NincidConfirmed=sum(incidI), 
                     NcumulConfirmed=sum(Confirmed))
  
  truth_dat<-truth_dat%>%
    dplyr::filter(NcumulConfirmed!=0)%>%
    calcR0(geodat=geodat, by_geoid=FALSE, incl_geoids = geodat$geoid, pop_col="pop") %>%
    dplyr::mutate(scenario=truth_source)
  
  rc<-dplyr::bind_rows(rc, truth_dat) %>%
    dplyr::mutate(`Based on`=factor(scenario, 
                                    levels=c(scenario_levels, truth_source),
                                    labels=c(scenario_labels, paste0(truth_source, " confirmed cases")))) %>%
    ggplot(aes(x=date, y=estimate, ymin=lower, ymax=upper))+
    geom_line(aes(col=`Based on`), size=0.75)+
    geom_ribbon(aes(fill=`Based on`), alpha=0.12) +
    geom_hline(yintercept = 1, col="black", alpha=0.6) +
    scale_y_continuous(trans="log1p", breaks=c(0, 0.5, 1, 1.5, 2, 4, 8)) +
    scale_x_date(breaks="1 month", date_labels="%b")+
    scale_color_manual("Based on",
                       values = c(scenario_colors, "red")) +
    scale_fill_manual("Based on",
                      values = c(scenario_colors, "red")) +
    theme_bw() +
    ylab("Effective reproduction number (Rt)")+
    xlab("Time")+
    theme(legend.position= "bottom",
          panel.grid=element_blank()) +
    guides(col=guide_legend(nrow=2))
  
  return(rc)
  
}



##' Plot ratio of outcomes 
##' @param hosp_state_totals df with hospitalization outcomes
##' @param start_date start of comparison period
##' @param end_date end of comparison period
##' @param pdeath_filter select pdeath: high, med, low
##' @param scenario_colors config$report$formatting$scenario_colors
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
                                  scenario_colors,
                                  pi_lo,
                                  pi_hi
                                  ){
  
  start_date<-as.Date(start_date)
  end_date<-as.Date(end_date)
  
  dat_long<- hosp_state_totals %>%
    dplyr::filter(time<=end_date,
                  time>=start_date,
                  pdeath==pdeath_filter) %>%
    dplyr::group_by(scenario_name, pdeath, sim_num) %>%
    dplyr::summarize(AvghospCurr=mean(NhospCurr),
                      AvgICUCurr=mean(NICUCurr), 
                      NincidHosp=sum(NincidHosp),
                      NincidICU=sum(NincidICU),
                      AvgincidDeath=mean(NincidDeath),
                      NincidDeath=sum(NincidDeath),
                      AvgincidCase=mean(NincidCase),
                      NincidCase=sum(NincidInf)) 
  
  scn_names<-levels(dat_long$scenario_name)[-1]
  dat_wide<-list()
  for(i in 1:length(scn_names)){
    dat_wide[[i]]<-dat_long %>%
      dplyr::filter(scenario_name==scn_names[i]|!scenario_name %in% scn_names) %>%
      dplyr::arrange(scenario_name) %>%
      dplyr::group_by(sim_num) %>%
      dplyr::mutate_if(is.numeric, function(x){x/lag(x)}) %>%
      drop_na() %>%
      dplyr::group_by(scenario_name) %>%
      dplyr::summarize(AvghospCurr_lo=quantile(AvghospCurr, pi_lo),
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
    dplyr::bind_rows() %>%
    tidyr::pivot_longer(cols=AvghospCurr_lo:NincidCase) %>%
    dplyr::mutate(var=case_when(stringr::str_detect(name, "Avghosp")~"Daily average of occupied hospital beds", 
                                stringr::str_detect(name, "incidHosp")~"Total hospital admissions",
                                stringr::str_detect(name, "AvgICU") ~ "Daily average of occupied ICU beds",
                                stringr::str_detect(name, "incidICU") ~ "Total ICU admissions",
                                stringr::str_detect(name, "AvgincidDeath") ~ "Daily average deaths",
                                stringr::str_detect(name, "NincidDeath") ~ "Total deaths",
                                stringr::str_detect(name, "AvgincidCase") ~ "Daily average cases",
                                stringr::str_detect(name, "NincidCase")~ "Total cases"),
           var=factor(var, levels=c("Daily average cases", "Total cases",
                                    "Daily average of occupied hospital beds", "Total hospital admissions",
                                    "Daily average of occupied ICU beds", "Total ICU admissions",
                                    "Daily average deaths", "Total deaths")),
           name=case_when(stringr::str_detect(name, "_lo")~"lower",
                          stringr::str_detect(name, "_hi")~"upper", 
                          TRUE~"estimate")) %>%
    tidyr::pivot_wider(names_from=name, values_from=value)
  
  plt_dat %>%
    ggplot()+
    #geom_col(aes(x=estimate, y=var, fill=scenario), position=position_dodge(0.75), width=1) +
    geom_point(aes(x=estimate, y=var, col=scenario_name), position=position_dodge(0.75))+
    geom_linerange(aes(xmin=lower, xmax=upper, y=var, group=scenario_name, col=scenario_name), position=position_dodge(0.75)) +
    theme_bw() +
    xlab(paste0('Relative to "', levels(dat_long$scenario_name)[1], '" scenario')) +
    ylab(paste0("Summarized outcomes from ", format(start_date, "%B %d"),"-",format(end_date, "%B %d"))) +
    scale_color_manual("Scenario",
                       values = scenario_colors) +
    scale_x_continuous(trans="log1p", breaks = c(0, 0.5, 1, 2, 3, 6, 9))+
    theme(legend.position="bottom") +
    geom_vline(xintercept=1)
}

##'
##' Function makes a summary table for each county
##' 
##' @param county_dat contains the relevant hospital data
##' @param start_date summarization period start
##' @param end_date summarization period end
##' @param pi_low low side of the prediction interval
##' @param pi_high high side of the prediction interval
##' @param pdeath_filter if summarizing results for one pdeath only; leave NA to show all 
##' 
##' @export
##'
make_scn_county_table_withVent <- function(county_dat, 
                                           pi_lo = 0.025, 
                                           pi_hi = 0.975, 
                                           geodat=geodata,
                                           start_date,
                                           end_date,
                                           pdeath_filter = pdeath_default #if NA will plot all IFRs
                                           
){
  pdeath_labels=c("1% IFR", "0.5% IFR", "0.25% IFR")
  pdeath_levels=c("high", "med", "low")
  
  start_date <- as.Date(start_date)
  end_date<-as.Date(end_date)
  
  county_dat<-county_dat %>% 
    dplyr::left_join(geodat) %>%
    dplyr::mutate(name=factor(name, levels=sort(geodat$name)))
  
  county_tab <- county_dat %>% 
    dplyr::filter(!is.na(time)) %>% 
    dplyr::filter(time >= start_date, time <= end_date) %>% 
    dplyr::group_by(pdeath, sim_num, name, scenario_name) %>%
    dplyr::summarize(TotalIncidCase = sum(NincidCase, na.rm = TRUE),
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
    dplyr::ungroup() %>%
    dplyr::group_by(pdeath, name, scenario_name) %>% 
    dplyr::summarize(nIncidCase_final = mean(TotalIncidCase),
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
    dplyr::ungroup() %>%
    dplyr::mutate(aIncidCase = prettyNum(conv_round(aIncidCase_final), big.mark=",", scientific=FALSE,trim=TRUE),
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
    dplyr::select(-ends_with("lo"), -ends_with("hi"), -ends_with("final"))
  
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
    dplyr::select(name, scenario_name, pdeath, starts_with("Case"), starts_with("Hosp"), starts_with("ICU"), starts_with("Vent"), starts_with("Death"))
  
  if(!is.na(pdeath_filter)){
    newnames <- c(NA_character_, NA_character_, "Daily average","Total", "Total", "Daily peak admissions", "Daily peak capacity", "Total", "Daily peak admissions", "Daily peak capacity", "Total", "Daily peak admissions", "Daily peak capacity", "Daily average", "Total")
    
    county_tab %>%
      dplyr::arrange(name) %>%
      dplyr::filter(pdeath == pdeath_filter) %>%
      dplyr::select(-pdeath) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(name = "County", scenario_name = "Scenario", CaseAvg = "CONFIRMED CASES", CaseIncid = "CONFIRMED CASES", HospIncid = "HOSPITALIZATIONS", # pdeath= "IFR", 
                                   HospPeakAdmin = "HOSPITALIZATIONS", HospPeakMax = "HOSPITALIZATIONS", ICUIncid = "ICU", 
                                   ICUPeakAdmin = "ICU", ICUPeakMax = "ICU", VentIncid = "VENTILATIONS", VentPeakAdmin = "VENTILATIONS",
                                   VentPeakMax = "VENTILATIONS", DeathAvg = "DEATHS", DeathIncid = "DEATHS") %>%
      flextable::merge_at(i = 1, j = 3:4, part = "header") %>%
      flextable::merge_at(i = 1, j = 5:7, part = "header") %>%
      flextable::merge_at(i = 1, j = 8:10, part = "header") %>%
      flextable::merge_at(i = 1, j = 11:13, part = "header") %>%
      flextable::merge_at(i = 1, j = 14:15, part = "header") %>%
      flextable::add_header_row(values = c(newnames), top = FALSE) %>%
      #flextable::merge_v(j = 1) %>%
      flextable::autofit() %>%
      #flextable::border(i=seq(3, 174, by = 3), border.bottom=officer::fp_border(color="black")) %>%
      flextable::border(j=c(1,2,4,7,10,13,15), border.right = officer::fp_border(color="grey", style = "solid", width=0.5)) %>%
      flextable::align(align="center", part = "all") %>%
      flextable::bold(part="header")%>%
      flextable::bold(j=1, part="body")
  } else {
    newnames <- c(NA_character_,NA_character_, NA_character_,"Daily average", "Total", "Total", "Daily peak admissions", "Daily peak capacity", "Total", "Daily
                peak admissions", "Daily peak capacity", "Total", "Daily peak admissions", "Daily peak capacity", "Daily average", "Total")
    
    county_tab %>%
      dplyr::mutate(pdeath = factor(pdeath, 
                             levels=pdeath_levels,
                             labels=pdeath_labels)) %>%
      dplyr::arrange(name, desc(pdeath)) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(name = "County", scenario_name="Scenario", pdeath = "IFR", CaseAvg = "CONFIRMED CASES", 
                                   CaseIncid = "CONFIRMED CASES", HospIncid = "HOSPITALIZATIONS", 
                                   HospPeakAdmin = "HOSPITALIZATIONS", HospPeakMax = "HOSPITALIZATIONS", ICUIncid = "ICU", 
                                   ICUPeakAdmin = "ICU", ICUPeakMax = "ICU", VentIncid = "VENTILATIONS", VentPeakAdmin = "VENTILATIONS",
                                   VentPeakMax = "VENTILATIONS", DeathAvg = "DEATHS", DeathIncid = "DEATHS") %>%
      flextable::merge_at(i = 1, j = 4:5, part = "header") %>%
      flextable::merge_at(i = 1, j = 6:8, part = "header") %>%
      flextable::merge_at(i = 1, j = 9:11, part = "header") %>%
      flextable::merge_at(i = 1, j = 12:14, part = "header") %>%
      flextable::merge_at(i = 1, j = 15:16, part = "header") %>%
      flextable::add_header_row(values = c(newnames), top = FALSE) %>%
      #flextable::merge_v(j = 1) %>%
      flextable::autofit() %>%
      #flextable::border(i=seq(3, 174, by = 3), border.bottom=officer::fp_border(color="grey", width=0.5)) %>%
      flextable::border(j=c(1,2,3,5,8,11,14,16), border.right = officer::fp_border(color="grey", style = "solid", width=0.5)) %>%
      flextable::align(align="center", part = "all") %>%
      flextable::bold(part="header")%>%
      flextable::bold(j=c(1,2,3), part="body")
  }
  
}

##'
##' Function calculates R from model outputs using R0 package
##'
##' @param USAfacts df with observed cases col NincidConfirmed
##' @param geodat geodata file
##' @param incl_geoids geoids to include
##' @param by_geoid estimate R for each county 
##' @param min.date start date for analysis
##' @param max.date end date for analysis
##' @param pop_col name of geodat column with population data
##' 
##' @author Kyra Grantz
##' 
##' @export
##'
calcR0 <- function(USAfacts, 
                   geodat, 
                   incl_geoids, 
                   by_geoid=FALSE, 
                   min.date=NULL, 
                   max.date=NULL,
                   pop_col = "pop"){
  
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
    for(i in 1:length(incl_geoids)){
      pop <- geodat[geodat$geoid == incl_geoids[i], pop_col]
      tmp <- covid %>% dplyr::filter(geoid == incl_geoids[i])
      incid <- setNames(tmp$New.Cases,1:nrow(tmp))
      estR0 <- R0::estimate.R(incid, mGT, begin=1, end=as.numeric(length(incid)), methods=c("TD"), pop.size=pop, nsim=1000)
      Rt1[[i]] <- cbind(tmp$Date,estR0$estimates$TD$R,estR0$estimates$TD$conf.int, geoid=rep(incl_geoids[i], nrow(tmp)))
      colnames(Rt1[[i]]) <- c("date","estimate","lower","upper", "geoid")
    }
    Rt1 <- dplyr::bind_rows(Rt1)
  }else{
    covid <- covid %>%
      group_by(Date) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup()
    pop <- sum(geodat[geodat$geoid %in% incl_geoids, pop_col])
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
##' @param current_scenario  current scenario
##' @param county_dat df with incident cases, hospitalizations, and deaths
##' @param geodat geodat file
##' @param pdeath_levels death rate levels
##' @param pdeath_labels death rate labels
##' @param start_date summarization period start
##' @param end_date summarization period end
##' @param dodger for plotting IFR estimates
##' 
##' @export
##'
##'
plot_outcome_rate<- function(current_scenario, 
                             county_dat,
                             start_date, 
                             end_date,
                             geodat=geodata,
                             pdeath_levels=c("high", "med", "low"),
                             pdeath_labels=c("1% IFR", "0.5% IFR", "0.25% IFR"),
                             dodger=0
){

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  sum_tab <- county_dat %>%
    dplyr::filter(scenario==current_scenario) %>%
    dplyr::filter(!is.na(time)) %>% 
    dplyr::filter(time >= start_date, time <= end_date) %>% 
    dplyr::group_by(pdeath, sim_num, geoid) %>%
    dplyr::summarize(TotalIncidCase = sum(NincidCase, na.rm = TRUE),
                     TotalIncidHosp = sum(NincidHosp, na.rm = TRUE),
                     TotalIncidDeath = sum(NincidDeath, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(geodat) %>%
    dplyr::mutate(Case=TotalIncidCase/pop*1000,
                  Hosp=TotalIncidHosp/pop*1000,
                  Death=TotalIncidDeath/pop*1000)%>%
    dplyr::group_by(pdeath, name) %>% 
    dplyr::summarize(Case = mean(Case),
                     Hosp = mean(Hosp),
                     Death = mean(Death)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(name=factor(name, levels=sort(geodat$name, decreasing=TRUE)),
                  pdeath = factor(pdeath, 
                                  labels = pdeath_labels,
                                  levels = pdeath_levels))
  
  
  rc <- dplyr::bind_rows(dplyr::mutate(sum_tab,type=1, est=Death),
                         dplyr::mutate(sum_tab,type=2, est=Hosp),
                         dplyr::mutate(sum_tab,type=3, est=Case)) %>%
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
##' @param current_scenario current scenario to plot
##' @param county_dat df with incident cases, hospitalizations, and deaths
##' @param geodat geodata file
##' @param r_dat df with effectiveness estimates, from load_r_sims_filtered
##' @param pdeath_levels death rate levels
##' @param pdeath_labels death rate labels
##' @param start_date summarization period start
##' @param end_date summarization period end
##' 
##' @export
##'
##'
plot_hosp_effec <- function(current_scenario, 
                            county_dat,
                            start_date, 
                            end_date,
                            geodat=geodata,
                            r_dat=inference_r,
                            pdeath_levels=c("high", "med", "low"),
                            pdeath_labels=c("1% IFR", "0.5% IFR", "0.25% IFR")
){
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  rc <- county_dat %>% 
    dplyr::filter(scenario==current_scenario) %>%
    dplyr::filter(!is.na(time)) %>% 
    dplyr::filter(time >= start_date, time <= end_date) %>% 
    dplyr::group_by(pdeath, sim_num, geoid) %>%
    dplyr::summarize(TotalIncidHosp = sum(NincidHosp, na.rm = TRUE)) %>% # TODO user-defined vars
    dplyr::ungroup() %>%
    dplyr::left_join(geodat) %>%
    dplyr::mutate(est=TotalIncidHosp/pop*1000) %>%
    dplyr::group_by(pdeath, name, geoid) %>% 
    dplyr::summarize(est = mean(est)) %>%
    dplyr::ungroup() 
  
  rc <- r_dat%>%
    dplyr::group_by(geoid, scenario, pdeath) %>%
    dplyr::filter(scenario==current_scenario) %>%
    dplyr::filter(npi_name!="local_variance" & max(end_date)==end_date)%>%
    dplyr::summarize(reduc=mean(reduction))%>%
    dplyr::right_join(rc) 
  
    rc<-rc%>%
      dplyr::mutate(pdeath=factor(pdeath, 
                                  levels=pdeath_levels, 
                                  labels=pdeath_labels)) %>%
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

##' Time series for cases, hospitalizations, and ICU by county and either pdeath or scenario
##'
##' @param county_dat df with incident cases, hospitalizations, and deaths
##' @param geodat geodat file with name column
##' @param filter_by specify either one pdeath or scenario 
##' @param filter_val value for filtering 
##' @param var_levels levels of comparison var 
##' @param var_labels labels of comparison var
##' @param start_date x-axis plot limits
##' @param end_date x-axis plot limits
##' @param pi_lo lower limit to interval
##' @param pi_hi upper limit to interval
##' 
##' @export
##'
##'

plot_county_outcomes <- function(county_dat, 
                                 filter_by = "pdeath", 
                                 filter_val = pdeath_default,
                                 pi_lo=0.025,
                                 pi_hi=0.975,
                                 start_date,
                                 end_date,
                                 geodat=geodata,
                                 var_levels,
                                 var_labels){
  
  if(filter_by!="pdeath" & filter_by!="scenario") stop("You can only filter by 'pdeath' or 'scenario'")
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  group_var <- if_else(filter_by=="pdeath", "scenario", "pdeath")
  
  county_dat<- county_dat %>%
    dplyr::filter(!!as.symbol(filter_by)==filter_val)%>%
    dplyr::group_by(geoid,time,!!as.symbol(group_var))%>%
    dplyr::summarize(hosp=mean(NhospCurr),
                     hosp_hi=quantile(NhospCurr, pi_hi),
                     hosp_lo=quantile(NhospCurr, pi_lo),
                     icu=mean(NICUCurr),
                     icu_lo=quantile(NICUCurr,pi_hi),
                     icu_hi=quantile(NICUCurr,pi_lo),
                     case=mean(NincidCase),
                     case_hi=quantile(NincidCase, pi_hi),
                     case_lo=quantile(NincidCase,pi_lo)) %>%
    dplyr::filter(time>=start_date, time<end_date) %>%
    dplyr::left_join(geodat) %>%
    dplyr::mutate(var = factor(!!as.symbol(group_var),
                               levels=var_levels,
                               labels=var_labels))
  
  rc<-dplyr::bind_rows(county_dat%>%
                         dplyr::select(name, var, time, est=hosp, lo=hosp_lo, hi=hosp_hi) %>%
                         dplyr::mutate(type=2),
                       county_dat%>%
                         dplyr::select(name, var, time, est=icu, lo=icu_lo, hi=icu_hi) %>%
                         dplyr::mutate(type=3),
                       county_dat%>%
                         dplyr::select(name, var, time, est=case, lo=case_lo, hi=case_hi) %>%
                         dplyr::mutate(type=1)) %>%
    mutate(type=factor(type, levels=c(1, 2, 3), labels=c("Incident Cases", "Occupied Hospital Beds", "Occupied ICU Beds")))
  
  plot_rc<-list()
  rc_type<-levels(rc$type)
  
  for(i in 1:length(rc_type)){
    plot_rc[[i]]<-rc %>%
      filter(type==rc_type[i]) %>%
      ggplot(aes(x=time)) +
      geom_line(aes(y=est, color=var)) +
      geom_ribbon(aes(ymin=lo, ymax=hi, fill=var), alpha=0.1)+
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
      scale_x_date(limits = c(start_date, end_date)) +
      labs(subtitle = rc_type[i])
  }
  
  return(plot_rc)
}

##' Forecast estimates compared to groundtruth and reichlab
##'
##' @param usa_facts df with groundtruth columns "incidI", "incidDeath" or "hosps"
##' @param county_dat df with incident cases, hospitalizations, and deaths
##' @param forecast_start last date of groundtruth
##' @param scenarios vector of scenarios to include
##' @param geodat geodata file with geoid and name columns
##' @param reichlab df with reichlab estimates from covidHubUtils::load_latest_forecast()
##' @param var name of variable in country_dat to plot
##' @param truth_source label for groundtruth
##' @param color_vals colors for scenarios and reichlab estimate
##' @param xmin_date start date for plot
##' @param tendency mean or median
##' @param pi_lo lower limit to interval
##' @param pi_hi upper limit to interval
##'
##' @export
##'
##'

forecast_plot<-function(usa_facts=NULL,
                        county_dat=res_state,
                        forecast_start=projection_start,
                        scenarios=forecast,
                        reichlab=NULL,
                        var="NincidCase",
                        pi_lo=0.025,
                        pi_hi=0.975,
                        truth_source="JHU CSSE",
                        color_vals=scn_colors,
                        xmin_date=min_date,
                        tendency="mean"){
  
  if(!any(scenarios %in% unique(county_dat$scenario))){stop("None of the specified scenarios exist in county_dat.")}
  forecast_start<-as.Date(forecast_start)
  truth_var<-if_else(str_detect(var, "ase"), "incidI",
                     if_else(str_detect(var, "eath"), "incidDeath",
                             if_else(str_detect(var, "osp"), "hosps", NULL)))
  
  county_dat<-county_dat%>%
      filter(scenario %in% scenarios)
  
  if(is.null(reichlab)){
    color_vals <- c("black", color_vals[1:length(forecast)])
    scen_levels<-c(truth_source, unique(county_dat$scenario_name))
  } else {
    color_vals <- c("black", color_vals[1:length(forecast)], max(color_vals))
    scen_levels<-c(truth_source, unique(county_dat$scenario_name), "COVID-19 Forecast Hub")
  }
  
  if(is.null(usa_facts)){
    county_dat<-county_dat%>%
      mutate(scenario=scenario_name) %>%
      group_by(time=lubridate::ceiling_date(time, "weeks"), sim_num, scenario) %>%
      summarize(est=sum(!!as.symbol(var)))%>%
      group_by(time, scenario) %>%
      summarize(lo=quantile(est, pi_lo),
                hi=quantile(est, pi_hi),
                mean=mean(est),
                median=median(est)) %>%
      mutate(truth_var=NA_real_) %>%
      group_by(scenario) %>%
      filter(time!=max(time))%>%
      rename(est=!!as.symbol(tendency))
   
    } else{

        usa_facts<-usa_facts %>%
          dplyr::filter(date <= forecast_start) %>%
          group_by(time=lubridate::ceiling_date(date, "weeks")) %>%
          summarize(truth_var=sum(!!as.symbol(truth_var))) %>%
          filter(time!=max(time)|
                   (as.numeric(as.Date(forecast_start)-time)==6)) %>%
          mutate(scenario=truth_source)
        
         county_dat<-county_dat %>%
           mutate(scenario=scenario_name) %>%
           group_by(time=lubridate::ceiling_date(time, "weeks"), sim_num, scenario) %>%
           summarize(est=sum(!!as.symbol(var))) %>%
           group_by(time, scenario) %>%
           summarize(lo=quantile(est, pi_lo),
                     hi=quantile(est, pi_hi),
                     mean=mean(est),
                     median=median(est)) %>%
           filter(time>=max(usa_facts$time)) %>%
           group_by(scenario) %>%
           filter(time!=max(time)) %>%
           rename(est=!!as.symbol(tendency))%>%
           bind_rows(usa_facts, .)
        
    }
  
  if(!is.null(reichlab)){
    reichlab_var<-if_else(str_detect(var, "ase"), "case", "death")
    
    county_dat<-reichlab %>%
      filter(quantile %in% c(pi_lo, 0.5, pi_hi) &
               str_detect(target_variable, paste("inc", reichlab_var))) %>%
    mutate(N=as.numeric(horizon),
           quantile=case_when(quantile==0.5~"est",
                              quantile==pi_lo~"lo",
                              quantile==pi_hi~"hi"),
           time=lubridate::ceiling_date(forecast_date, "weeks")+7*(N-1),
           scenario="COVID-19 Forecast Hub") %>%
      dplyr::select(N, time, scenario, quantile, value) %>%
      pivot_wider(names_from=quantile, values_from=value) %>%
      dplyr::select(time, scenario, lo, hi, est) %>%
      bind_rows(county_dat)
  }

  if(is.null(usa_facts)){
    ymax <- max(
      county_dat %>% filter(time > xmin_date) %>% filter(est==max(est)) %>% pull(est) %>% unique()
    )
  } else {
    ymax <- max(
      usa_facts %>% filter(time > xmin_date) %>% filter(truth_var==max(truth_var)) %>% pull(truth_var) %>% unique(),
      county_dat %>% filter(time > xmin_date) %>% filter(est==max(est)) %>% pull(est) %>% unique()
    )
  }
  
  
  plot_dat<-county_dat %>%
    mutate(scenario=factor(scenario, levels=scen_levels)) %>%
    arrange(scenario, time) %>%
    ggplot(aes(x=time))+
    geom_line(aes(y=truth_var, col=scenario))+
    geom_line(aes(y=est, col=scenario), linetype="dashed")+
    geom_point(aes(y=est, col=scenario), linetype="dashed")+
    geom_ribbon(aes(ymin=lo, ymax=hi, fill=scenario), alpha=0.1)+
    theme_bw()+
    xlab("")+
    scale_color_manual(values=color_vals)+
    scale_fill_manual(values=color_vals)+
    coord_cartesian(ylim = c(0, 1.2*ymax))
  
  return(plot_dat)
  
}
