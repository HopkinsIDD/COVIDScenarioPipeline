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
##' Plot figure showing random curves of hospitalizatoins 
##' 
##' @param hosp_state_totals totals for hospitalization related data for state
##' @param num_sims the number of simulations to show
##' 
plot_hosp_occupancy_curves <- function (hosp_state_totals, num_sims,
                                        pdeath) {
  
  
  ##TODO: MAke thsi so each scenario does not use the same sims...though syoudl nto matter.
  to_plt <- hosp_state_totals %>% 
    filter(pdeath=="high") %>% 
    filter(sim_num %in% sample(unique(sim_num), min(num_sims, length(unique(sim_num))), replace=FALSE)) 
  
  rc <- ggplot(data=to_plt, aes(x=time, color=as.factor(scenario_name), group=as.factor(sim_num))) +
    geom_line(aes(y=Nhosp), alpha=0.2, size=.75) +
    scale_y_continuous("Daily hospital utilization")# +
    #scale_x_date(date_breaks = "1 month", date_labels = "%B", limits = c(as.Date("2020-1-31"), as.Date("2020-9-30"))) +
    #scale_color_manual("Scenario",
    #                   labels = params$scenario_labels,
    #                   values = params$scenario_cols,
    #                   breaks = names(params$scenario_labels)) +
    #theme_minimal() +
    #theme(axis.title.x =  element_blank(),
    #      legend.position = "bottom",
    #      legend.title = element_blank()) +
    #guides(color = guide_legend(nrow = 2))
  
  return(rc)
  
}
