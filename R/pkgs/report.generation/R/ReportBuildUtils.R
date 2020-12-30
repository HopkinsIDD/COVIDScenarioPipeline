
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
  to_plt <- hosp_state_totals %>%
    dplyr::filter(pdeath==pdeath_level,
                  sim_num %in% sample(unique(sim_num),
                                      min(num_sims, length(unique(sim_num))),
                                      replace=FALSE)) %>%
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
##' Plot figure showing histogram of cumulative hospitalizations (or other incident outcome) by a certain date
##'
##' @param hosp_state_totals totals for hospitalization related data for state for all pdeath
##' @param var_name variable name for final size distribution
##' @param pdeath_level level of IFR (string: high/med/low) for filtering hospitalization data
##' @param sim_start_date simulation start date as character string "2020-01-01"
##' @param summary_date date at which to present cumulative summary of hospitalizations
##'
##' @return plot of cum hosp for state across simulations
##'
##' @export
##'
plot_hist_incidHosp_state <- function (hosp_state_totals,
                                      var_name,
                                       pdeath_level = "high",
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
                                         levels = scenario_labels,
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
##' Compare model outputs and data from CSSE
##' 
##' @param state_hosp_totals state hosp data frame
##' @param jhu_obs_dat dataframe with CSSE data
##' @param scenario_labels character vector with scenario labels
##' @param scenario_cols character vector with scenario colors
##' @param pdeath_level IFR level assumption
##' @param obs_data_col character string of observed data color
##' @param ci.L lower bound confidence interval
##' @param ci.U upper bound confidence interval
##' @param date_breaks breaks for dates in figure
##' @param sim_start_date simulation start date
##' @param sim_end_date simulation end date
##' @param assumed_reporting_rate assumed reporting rate (0.2 means 20% of infections are reported cases)
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
                              assumed_reporting_rate) {

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
    group_by(date, scenario_name) %>%
    dplyr::summarize(ci_lower_incid_inf = quantile(NincidInf, ci.L),
                     ci_upper_incid_inf = quantile(NincidInf, ci.U),
                     mean_incid_inf = mean(NincidInf),
                     median_incid_inf = median(NincidInf),
                     ci_lower_incid_cas = quantile(assumed_reporting_rate*NincidInf, ci.L),
                     ci_upper_incid_cas = quantile(assumed_reporting_rate*NincidInf, ci.U),
                     mean_incid_cas = mean(assumed_reporting_rate*NincidInf),
                     median_incid_cas = median(assumed_reporting_rate*NincidInf)) 
  
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
  
  output <- list(incid_infections_plot, incid_deaths_plot)
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
plot_needs_relative_to_threshold_heatmap <- function(
    hosp_geounit_relative,
    shapefile,
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
  
  shp <- shapefile %>%
    sf::st_drop_geometry() %>%
    dplyr::select(geoid, name) %>%
    dplyr::filter(geoid %in% incl_geoids) %>%
    dplyr::arrange(name) %>%
    dplyr::mutate(name_num = seq_along(name)) ## secondary axes only work with continuous values
  
  plt_dat <- left_join(hosp_geounit_relative, shp, by = c("geoid")) %>%
    dplyr::rename(threshold = !!value_name) %>%
    dplyr::filter(time >= start_date & time <= end_date) %>%
    dplyr::filter(scenario_label %in% scenario_labels) %>%
    dplyr::mutate(scenario_label = factor(scenario_label,
                                         levels = scenario_labels,
                                         labels = scenario_labels))

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


