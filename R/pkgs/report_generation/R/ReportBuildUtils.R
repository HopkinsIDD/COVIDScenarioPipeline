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
                                         levels = params$scenario_labels,
                                         labels = params$scenario_labels),
                  sim_num = factor(sim_num))

  rc <- ggplot(data=to_plt,
               aes(x=time, colour = scenario_name,
                   group = interaction(sim_num, scenario_name))) +
    geom_line(aes(y = Nhosp), alpha=0.3, size=.75) +
    scale_y_continuous("Daily hospital occupancy", labels = scales::comma) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 limits = c(as.Date(sim_start_date), as.Date(sim_end_date))) +
    scale_color_manual("Scenario",
                      labels = params$scenario_labels,
                      values = params$scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
         legend.position = "bottom",
         legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)))


  if(plot_intervention){
    interv_dates <- data.frame(xmin = as.Date(interv_start_date),
                               xmax = as.Date(interv_end_date),
                               ymin = 0,
                               ymax = 1.05*max(to_plt$Nhosp))

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
                                         levels = params$scenario_labels,
                                         labels = params$scenario_labels),
                  sim_num = factor(sim_num))

  rc <- ggplot(data=to_plt,
               aes(x = time, color = scenario_name,
                   group = interaction(sim_num, scenario_name))) +
    geom_line(aes(y = NincidInf), alpha=0.2, size=.75) +
    scale_y_continuous("Daily incident infections", labels = scales::comma) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 limits = c(as.Date(sim_start_date), as.Date(sim_end_date))) +
    scale_color_manual("Scenario",
                       labels = params$scenario_labels,
                       values = params$scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)))


  if(plot_intervention){
    interv_dates <- data.frame(xmin = as.Date(interv_start_date),
                               xmax = as.Date(interv_end_date),
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
                                         levels = params$scenario_labels),
                  pdeath = factor(pdeath,
                                  levels = params$pdeath_filecode,
                                  labels = params$pdeath_labels),
                  sim_num = factor(sim_num))

  rc <- ggplot(data=to_plt,
               aes(x=time, color=scenario_name,
                   group=interaction(sim_num, scenario_name))) +
    geom_line(aes(y=Ndeath), alpha=0.2, size=.75) +
    scale_y_continuous("Daily incident deaths", labels = scales::comma) +
    scale_x_date(date_breaks = "2 months",
                 date_labels = "%b",
                 limits = c(as.Date(sim_start_date), as.Date(sim_end_date))) +
    scale_color_manual("Scenario",
                       labels = params$scenario_labels,
                       values = params$scenario_cols) +
    theme_minimal() +
    theme(axis.title.x =  element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    facet_wrap(~pdeath, nrow = 1) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha=1)))


  if(plot_intervention){
    interv_dates <- data.frame(xmin = as.Date(interv_start_date),
                               xmax = as.Date(interv_end_date),
                               ymin = 0,
                               ymax = 1.05*max(to_plt$Ndeath))

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
                                            sim_start_date,
                                            summary_date) {

  sim_start_date <- as.Date(sim_start_date)
  summary_date <- as.Date(summary_date)

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_state_totals %>%
    dplyr::filter(pdeath==pdeath_level) %>%
    dplyr::filter(time >= sim_start_date & time <= summary_date) %>%
    group_by(scenario_name, sim_num) %>%
    dplyr::summarise(cumHosp = sum(NincidHosp)) %>%
    ungroup %>%
    dplyr::mutate(scenario_name = factor(scenario_name, levels = params$scenario_labels, labels = params$scenario_labels))

  rc <- ggplot(data=to_plt, aes(x = cumHosp, fill = scenario_name, color = scenario_name)) +
    geom_histogram() +
    facet_wrap(scenario_name~., ncol = 1) +
    scale_fill_manual(values = params$scenario_cols, labels = params$scenario_labels, aesthetics = c("colour", "fill")) +
    scale_x_continuous(paste("Cumulative hospitalizations by", print_pretty_date()(summary_date)), labels = scales::comma) +
    ylab("Number of simulations") +
    theme_bw() +
    guides("none") +
    theme(legend.position = "none")


  return(rc)

}



##' THIS FUNCTION HAS NOT YET BEEN TESTED 3/25/2020 ECL
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
plot_line_hospPeak_time_county <- function (hosp_cty_peaks,
                                            cty_names,
                                            pdeath_level = "high",
                                            start_date,
                                            end_date) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  ##TODO: Make this so each scenario does not use the same sims...though should not matter.
  to_plt <- hosp_cty_peaks %>%
    dplyr::filter(pdeath==pdeath_level) %>%
    group_by(geoid, scenario_name) %>%
    dplyr::summarise(mean_pkTime = mean(time),
                      median_pkTime = median(time),
                      low_pkTime = as.Date(quantile(unclass(time), probs=.25), origin = "1970-01-01"),
                      hi_pkTime = as.Date(quantile(unclass(time), .75), origin = "1970-01-01")) %>%
    ungroup %>%
    dplyr::mutate(scenario_name = factor(scenario_name, levels = params$scenario_labels, labels = params$scenario_labels)) %>%
    dplyr::left_join(cty_names, by = c("geoid"))


  rc <- ggplot(data=to_plt, aes(x = reorder(county, -as.numeric(mean_pkTime)), y = mean_pkTime, ymin = low_pkTime, ymax = hi_pkTime)) +
    geom_pointrange() +
    scale_y_date("Date of peak hospital occupancy", date_breaks = "2 months", date_labels = "%b", limits = c(as.Date(start_date), as.Date(end_date))) +
    xlab("County") +
    theme_bw() +
    guides("none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
    coord_flip()


  return(rc)

}
