##' Plot the estimated and reported number of importations over time
##' 
##' Author: Shaun Truelove
##' email: shauntruelove@jhu.edu
##' Updated: 31 Jan 2020


# SETUP -------------------------------------------------------------------

if (!exists("version")){       version <- "1"                    }
if (!exists("batch")){         batch <- "1"                    }
if (!exists("project_name")){  project_name <- "california_import" }
import_sim_file <- file.path("output",project_name, sprintf("covid_importation_sim_%s_batch_detect_v%s.RData", batch, version))

options(scipen=999)

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('RColorBrewer')) install.packages('RColorBrewer'); library(RColorBrewer)
if(!require('grid')) install.packages('grid'); library(grid)
if(!require('gridExtra')) install.packages('gridExtra'); library(gridExtra)


# LOAD SUMMARIZED SIMULATION RESULTS ---------------------------------------------------------------

# Destination by time
import_results_desttime_detect <- read_csv(file.path("results",project_name, sprintf("import_results_desttime_detect_v%s.csv", version)))
import_results_desttime_detect$t <- as.Date(import_results_desttime_detect$t)
t_values <- sort(unique(import_results_desttime_detect$t))
import_results_desttime_detect <- import_results_desttime_detect %>% 
    mutate(t = factor(as.character(t))) %>% 
    mutate(t_num = as.integer(t))

# Source by time, single destination
import_results_sourcetime_detect <- read_csv(file.path("results",project_name, sprintf("import_results_sourcetime_detect_v%s.csv", version)))
import_results_sourcetime_detect$t <- as.Date(import_results_sourcetime_detect$t)
t_values <- sort(unique(import_results_sourcetime_detect$t))
import_results_sourcetime_detect <- import_results_sourcetime_detect %>%
    mutate(t = factor(as.character(t))) %>%
    mutate(t_num = as.integer(t))
# 
# # Destination by time, Cumulative
# import_results_desttime_cum <- read_csv(file.path("results",project_name, sprintf("import_results_desttime_cumulative_detect_v%s.csv", version)))
# import_results_desttime_cum$t <- as.Date(import_results_desttime_cum$t)
# t_values <- sort(unique(import_results_desttime_cum$t))
# import_results_desttime_cum <- import_results_desttime_cum %>% 
#     mutate(t = factor(as.character(t))) %>% 
#     mutate(t_num = as.integer(t))


# t_limits <- as.Date(c("2020-01-01","2020-01-27"))
if (!exists("t_limits")){ t_limits <- range(t_values) }


# 
# # FUNCTIONS ---------------------------------------------------------------
# 
# plot_imports <- function(data=import_results_desttime, region_ = "All", t_limits=as.Date(c("2020-01-01","2020-01-27")),
#                                     y.size = 9, x.size = 9, 
#                                     leg.text.size = 12, leg.title.size = 14,
#                                     rep_col = "indianred2", est_col = "blue3", est_ribbon_col = "blue3", est_ribbon_alpha=0.25,
#                          time_int = 5, # time interval for x-axis
#                          plot_legend=TRUE, x_axis=TRUE){
#     # Subset by region if wanted
# if (!is.na(t_limits[1])){
#     data <- data %>% mutate(t = as.Date(t)) %>% 
#         filter(t >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])) %>%
#         mutate(t_num = as.integer(as.factor(t)))
# }
#     # Limit the time of the plots
#     if (!is.na(t_limits[1])){
#         data <- data %>% rowwise %>% filter(as.Date(t) >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2]))
#     }
#     t_values <- as.character(sort(data$t))
#     
#     p <- ggplot(data=data, aes(x=t_num, y=import_mean)) + 
#             #geom_hline(yintercept=0)+ 
#             geom_line(aes(color='Detected', linetype="Detected"), size=1.2) +
#             geom_ribbon(aes(ymin=import_ll, ymax=import_ul), alpha=est_ribbon_alpha, fill=est_ribbon_col) +
#             scale_x_continuous(breaks=seq(1,length(t_values),time_int), 
#                            labels=t_values[seq(1,length(t_values),time_int)]) +
#             ylab("Detected imported nCoV cases (n)") +
#             #scale_colour_manual(values=c(Reported=rep_col, Estimated=est_col), guide="none") +
#             #scale_linetype_manual(values=c("solid","11"), guide=guide_legend(title=element_blank())) +
#             scale_colour_manual(values=c(Detected=est_col), guide="none") +
#             scale_linetype_manual(values=c("solid"), guide=guide_legend(title=element_blank())) +
#             #scale_y_continuous(expand = c(0,0)) +
#             theme(axis.line = element_line(colour = "black"),
#                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                   panel.border = element_blank(), panel.background=element_blank(),
#                   axis.text.y = element_text(size=y.size), 
#                   axis.text.x = element_text(size=x.size, angle = 90, hjust = 1, vjust=.5, margin=margin(t=5)),
#                   axis.title.x = element_blank(),
#                   legend.title=element_text(size=leg.title.size), 
#                   legend.text=element_text(size=leg.text.size), # legend text size
#                   legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-20),
#                   legend.key.size=unit(15, "pt"), # Change key size in the legend 
#                   legend.key = element_blank(),
#                   legend.position=c(.075, .80), legend.background = element_blank(), legend.box.background = element_blank(),
#                   plot.title = element_text(size=leg.title.size, face="bold", vjust=-10, hjust = 0.025),
#                   plot.margin = unit(c(-.75,.25,0,0.25), "cm")) 
#     
#     if (region_ != "All"){
#         p <- p + ggtitle(region_)
#     }
#     if (plot_legend==FALSE){
#         p <- p + theme(legend.position = "none")
#     }
#     if (x_axis==FALSE){
#         p <- p + theme(axis.text.x=element_blank(), #axis.line.x=element_blank(), 
#                        axis.ticks.x=element_blank(), axis.title.x=element_blank())
#     }
#     return(p)
# } 
# 
# 
# plot_imports_barchart <- function(data=import_results_desttime, region_ = "All", t_limits=as.Date(c("2020-01-01","2020-01-27")),
#                          y.size = 9, x.size = 9,
#                          leg.text.size = 12, leg.title.size = 14,
#                          rep_col = "indianred2", est_col = "blue3",
#                          time_int = 5, # time interval for x-axis
#                          plot_legend=TRUE, x_axis=TRUE){
#     # Subset by region if wanted
# if (!is.na(t_limits[1])){
#     data <- data %>% mutate(t = as.Date(t)) %>% 
#         filter(t >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])) %>%
#         mutate(t_num = as.integer(as.factor(t)))
# }
#     # Limit the time of the plots
#     if (!is.na(t_limits[1])){
#         data <- data %>% rowwise %>% filter(as.Date(t) >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2]))
#     }
#     t_values <- as.character(sort(data$t))
#     
#     p <- ggplot(data=data, aes(x=t_num, y=import_mean, fill='Detected')) + 
#         geom_bar(stat="identity", color="black") +
#         geom_errorbar(aes(ymin=import_ll, ymax=import_ul), width=.4) +
#         ylab("Detected imported nCoV cases (n)") +
#         scale_x_continuous(breaks=seq(1,length(t_values),time_int), 
#                            labels=t_values[seq(1,length(t_values),time_int)]) +
#         scale_colour_manual(values=c(Detected=est_col), guide="none") +
#         scale_linetype_manual(values=c("solid"), guide=guide_legend(title=element_blank())) +
#         #scale_y_continuous(expand = c(0,0)) +
#         theme(axis.line = element_line(colour = "black"),
#               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#               panel.border = element_blank(), panel.background=element_blank(),
#               axis.text.y = element_text(size=y.size), 
#               axis.text.x = element_text(size=x.size, angle = 90, hjust = 1, vjust=.5, margin=margin(t=5)),
#               axis.title.x = element_blank(),
#               legend.title=element_text(size=leg.title.size), 
#               legend.text=element_text(size=leg.text.size), # legend text size
#               legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-20),
#               legend.key.size=unit(15, "pt"), # Change key size in the legend 
#               legend.key = element_blank(),
#               legend.position=c(.075, .80), legend.background = element_blank(), legend.box.background = element_blank(),
#               plot.title = element_text(size=leg.title.size, face="bold", vjust=-10, hjust = 0.025),
#               plot.margin = unit(c(-.75,.25,0.25,0.25), "cm")) +
#         guides(fill=guide_legend(title = element_blank()))
#     
#     if (region_ != "All"){
#         p <- p + ggtitle(region_)
#     }
#     if (plot_legend==FALSE){
#         p <- p + theme(legend.position = "none")
#     }
#     if (x_axis==FALSE){
#         p <- p + theme(axis.text.x=element_blank(), #axis.line.x=element_blank(), 
#                        axis.ticks.x=element_blank(), axis.title.x=element_blank())
#     }
#     return(p)
# } 
#     
# 
# # Barchart plot of importations stacked by source
# plot_imports_stackedsource <- function(data=import_results_sourcetime,  t_limits=as.Date(c("2020-01-01","2020-01-27")),
#                                   y.size = 9, x.size = 9, 
#                                   leg.text.size = 8, leg.title.size = 10,
#                                   rep_col = "indianred2", est_col = "blue3",
#                                   time_int = 5){ # time interval for x-axis
# 
#         # Limit the time of the plots
# if (!is.na(t_limits[1])){
#     data <- data %>% mutate(t = as.Date(t)) %>% 
#         filter(t >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])) %>%
#         mutate(t_num = as.integer(as.factor(t)))
# }
#     t_values <- as.character(unique(sort(data$t)))
#     
#     p <- ggplot(data=data, aes(x=t_num, y=import_mean, fill=source)) + 
#         geom_bar(position="stack", stat="identity", color="black") +
#         ylab("Detected imported nCoV cases (n)") +
#         scale_x_continuous(breaks=seq(1,length(t_values),time_int), 
#                            labels=t_values[seq(1,length(t_values),time_int)]) +
#         scale_colour_manual(values=c(Detected=est_col), guide="none") +
#         scale_linetype_manual(values=c("solid"), guide=guide_legend(title=element_blank())) +
#         #scale_y_continuous(expand = c(0,0)) +
#         theme(axis.line = element_line(colour = "black"),
#               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#               panel.border = element_blank(), panel.background=element_blank(),
#               axis.text.y = element_text(size=y.size), 
#               axis.text.x = element_text(size=x.size, angle = 90, hjust = 1, vjust=.5, margin=margin(t=5)),
#               axis.title.x = element_blank(),
#               legend.title=element_text(size=leg.title.size), 
#               legend.text=element_text(size=leg.text.size), # legend text size
#               legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,10,-20,-20),
#               legend.key.size=unit(8, "pt"),
#               legend.key = element_blank(),
#               legend.background = element_blank(), legend.box.background = element_blank(),
#               plot.title = element_text(size=leg.title.size, face="bold", hjust = 0.025),
#               plot.margin = unit(c(0.5,.25,0.25,0.25), "cm")) +
#         guides(fill=guide_legend(title = element_blank(), ncol=1))
#     return(p)
# } 
# 
# 
# 
# 
# 
# 
# # For plotting importions from multiple regions
# plot_imports_byregion <- function(data=import_results_desttime_byregion, t_limits=as.Date(c("2020-01-01","2020-01-27")),
#                          y.size = 9, x.size = 9, leg.text.size = 12, leg.title.size = 14,
#                          rep_col = "indianred2", est_col = "blue3", est_ribbon_col = "blue3", est_ribbon_alpha=0.25,
#                          plot_legend=TRUE){
#         
#     # Limit the time of the plots
# if (!is.na(t_limits[1])){
#     data <- data %>% mutate(t = as.Date(t)) %>% 
#         filter(t >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])) %>%
#         mutate(t_num = as.integer(as.factor(t)))
# }
#     
#     p <- ggplot(data=data, aes(x=t_num, color=region, fill=region)) + 
#         geom_line(data=data, aes(y=import_mean), size=1.2) +
#         geom_ribbon(data=data, aes(ymin=import_ll, ymax=import_ul, fill=region), alpha=est_ribbon_alpha) +
#         #geom_line(data=data, aes(y=rep_import, color="Reported"), size=1.2) +
#         ylab("Detected imported nCoV cases (n)") +
#         #scale_colour_manual(name="Imported cases", values=c(Reported=rep_col, Estimated=est_col)) +
#         scale_color_manual(values=c("grey50", brewer.pal(3,"Dark2"), "red"), guide=guide_legend(title="Detected imported cases")) +
#         
#         theme(axis.line = element_line(colour = "black"),
#               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#               panel.border = element_blank(), panel.background=element_blank(),
#               axis.text.y = element_text(size=y.size), axis.text.x = element_text(size=x.size, angle = 90, hjust = 1),
#               axis.title.x = element_blank(),
#               legend.title=element_text(size=leg.title.size), 
#               legend.text=element_text(size=leg.text.size), # legend text size
#               legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-20),
#               legend.key.size=unit(15, "pt"), # Change key size in the legend 
#               legend.key = element_blank(),
#               legend.position=c(.1, .90), legend.background = element_blank(), legend.box.background = element_blank(),
#               plot.margin = unit(c(.5,1,.25,.25), "cm"))
#     
#     p + facet_grid(region ~ .)
# 
# 
#     return(p)
# } 
# 
# 
# 
# 
# plot_cum_imports <- function(data=import_results_desttime_cum, region_ = "All", t_limits=as.Date(c("2020-01-01","2020-01-27")),
#                          y.size = 9, x.size = 9, 
#                          leg.text.size = 12, leg.title.size = 14,
#                          rep_col = "indianred2", est_col = "blue3", est_ribbon_col = "blue3", est_ribbon_alpha=0.25,
#                          time_int = 5, # time interval for x-axis
#                          plot_legend=TRUE, x_axis=TRUE){
#     # Subset by region if wanted
#     if (region_!="All"){
#         data <- data %>% filter(region==region_)
#     }
#     # Limit the time of the plots
# if (!is.na(t_limits[1])){
#     data <- data %>% mutate(t = as.Date(t)) %>% 
#         filter(t >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])) %>%
#         mutate(t_num = as.integer(as.factor(t)))
# }
#     t_values <- as.character(sort(data$t))
#     
#     p <- ggplot(data=data, aes(x=t_num, y=cum_import_mean)) + 
#         geom_line(data=data, aes(color='Detected', linetype="Detected"), size=1.2) +
#         geom_ribbon(data=data, aes(ymin=cum_import_ll, ymax=cum_import_ul), alpha=est_ribbon_alpha, fill=est_ribbon_col) +
#         scale_x_continuous(breaks=seq(1,length(t_values),time_int), 
#                            labels=t_values[seq(1,length(t_values),time_int)]) +
#         ylab("Detected imported nCoV cases (n)") +
#         #scale_colour_manual(values=c(Reported=rep_col, Estimated=est_col), guide="none") +
#         #scale_linetype_manual(values=c("solid","11"), guide=guide_legend(title=element_blank())) +
#         scale_colour_manual(values=c(Detected=est_col), guide="none") +
#         scale_linetype_manual(values=c("solid"), guide=guide_legend(title=element_blank())) +
#         #scale_y_continuous(expand = c(0,0)) +
#         theme(axis.line = element_line(colour = "black"),
#               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#               panel.border = element_blank(), panel.background=element_blank(),
#               axis.text.y = element_text(size=y.size), 
#               axis.text.x = element_text(size=x.size, angle = 90, hjust = 1, vjust=.5, margin=margin(t=5)),
#               axis.title.x = element_blank(),
#               legend.title=element_text(size=leg.title.size), 
#               legend.text=element_text(size=leg.text.size), # legend text size
#               legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-20),
#               legend.key.size=unit(15, "pt"), # Change key size in the legend 
#               legend.key = element_blank(),
#               legend.position=c(.075, .80), legend.background = element_blank(), legend.box.background = element_blank(),
#               plot.title = element_text(size=leg.title.size, face="bold", vjust=-10, hjust = 0.025),
#               plot.margin = unit(c(-.75,.25,0,0.25), "cm")) 
#     
#     if (region_ != "All"){
#         p <- p + ggtitle(region_)
#     }
#     if (plot_legend==FALSE){
#         p <- p + theme(legend.position = "none")
#     }
#     if (x_axis==FALSE){
#         p <- p + theme(axis.text.x=element_blank(), #axis.line.x=element_blank(), 
#                        axis.ticks.x=element_blank(), axis.title.x=element_blank())
#     }
#     return(p)
# } 
# 
# 






# # PLOT SPECIFICS
# 
# y.size = x.size = 9
# leg.text.size = 12
# leg.title.size = 14
# 
# rep_col <- "indianred2"
# est_col <- "blue3"
# est_ribbon_col <- "blue3"
# est_ribbon_alpha <- 0.2

# 
# # PLOT TOTAL IMPORTATIONS -------------------------------------------------
# 
# p_total <- plot_imports(data=import_results_desttime, region_ = "All", t_limits=t_limits, 
#                        y.size = 9, x.size = 9, leg.text.size = 12, leg.title.size = 14,
#                        rep_col = "indianred2", 
#                        est_col = "blue3", 
#                        est_ribbon_col = "blue3", 
#                        est_ribbon_alpha=0.25,
#                        plot_legend=TRUE)
# #plot(p_total)
# 
# 
# p_total_barchart <- plot_imports_barchart(data=import_results_desttime, region_ = "All", t_limits=t_limits, 
#                         y.size = 9, x.size = 9, leg.text.size = 12, leg.title.size = 14,
#                         rep_col = "indianred2", 
#                         est_col = "blue3", 
#                         plot_legend=TRUE)
# #plot(p_total_barchart)
# 
# 
# 
# p_cum <- plot_cum_imports(data=import_results_desttime_cum, region_ = "All", t_limits=t_limits, 
#                         y.size = 9, x.size = 9, leg.text.size = 12, leg.title.size = 14,
#                         rep_col = "indianred2", 
#                         est_col = "blue3", 
#                         est_ribbon_col = "blue3", 
#                         est_ribbon_alpha=0.25,
#                         plot_legend=TRUE)
# 
# # Plot 2 plots together
# #gridExtra::grid.arrange(p_total, p_cum, nrow=2)
# 
# 
# 
# p_stacked_barchart <- plot_imports_stackedsource(data=import_results_sourcetime, t_limits=t_limits, 
#                                           y.size = 8, x.size = 8, leg.text.size = 6, leg.title.size = 8,
#                                           rep_col = "indianred2", 
#                                           est_col = "blue3")
# # plot(p_stacked_barchart)
# 
# p_stackeddest_barchart <- plot_imports_stackeddest(data=import_results_desttime, t_limits=t_limits, 
#                                                    y.size = 8, x.size = 8, leg.text.size = 6, leg.title.size = 8,
#                                                    rep_col = "indianred2", 
#                                                    est_col = "blue3")
# 
# 
# 




# 
# 
# # PLOT ESTIMATED VS REPORTED ----------------------------------------------
# 
# # Reported from CDC
# shen_cases <- read_csv("data/shenzhen_data/shenzhen_case_counts.csv")
# shen_cases <- shen_cases %>% mutate(cum_cases = cumsum(count))
# # From Linelists
# ll_data <- read_csv("data/linelist_current.csv")
# shen_rows <- apply(ll_data, 1, FUN=function(x) sum(grepl("shenzhen", x, ignore.case = TRUE)))>0
# ll_data_shenzhen <- ll_data[shen_rows, ]
# shen_data_aggr <- ll_data_shenzhen %>% count(date_confirmation)
# rm(ll_data, ll_data_shenzhen, shen_rows)
# 
# shen_counts <- full_join(shen_cases %>% rename(count_CDC = count),
#                          shen_data_aggr %>% rename(count_ll = n, date=date_confirmation),
#                          by=c("date"="date"))
# shen_counts[is.na(shen_counts)] <- 0
# 
# # Merge with estimates  
# shen_counts_all <- full_join(shen_counts, 
#                              import_results_desttime %>% select(date=t, import_mean, import_ll, import_ul) %>% mutate(date = as.Date(date)), by=c("date"))
# shen_counts_all <- full_join(shen_counts_all, 
#                              import_results_desttime_cum %>% select(date=t, cum_import_mean, cum_import_ll, cum_import_ul) %>% mutate(date = as.Date(date)), by=c("date"))
# 
# 
# # Limit the time of the plots
# if (!is.na(t_limits[1])){
#     shen_counts_all <- shen_counts_all %>% rowwise %>% filter(as.Date(date) >= as.Date(t_limits[1]) & as.Date(date) <= as.Date(t_limits[2])) %>% as.data.frame()
# }
# 
# 
# p_incid_shen_inclest <- ggplot(shen_counts_all %>% mutate(label1 = "CDC counts", label2 = "Linelist counts", label3="Est. detected imports"), 
#            aes(x=date, y=count_CDC)) +
#         geom_bar(stat="identity", aes(fill=label1)) +
#         geom_point(aes(x=date,y=count_ll, shape=label2, color=label2), size=2) +
#         geom_point(aes(x=date,y=import_mean, shape=label3, color=label3), size=2) +
#         scale_color_manual(values=c("maroon","green")) +
#         scale_fill_manual(values=c("navy")) +
#         ylab("Reported Cases") + ggtitle("Shenzhen Reported nCoV Cases") + 
#         theme_classic() +
#         theme(#legend.background = element_rect(color="grey"),
#             legend.position = c(0.05, 1),
#             legend.justification = c(0, 1),
#             legend.title = element_blank())
# #plot(p_incid_shen_inclest)
# 
# 
# 
# # Plot cumulative cases in Shenzhen
# # Get cumulative case counts in each
# shen_counts_cum <- shen_counts_all %>% arrange(as.character(date)) %>% mutate(cum_CDC = cumsum(count_CDC), cum_ll = cumsum(count_ll))
# 
# 
# p_cum_shen_inclest <- ggplot(shen_counts_cum %>% mutate(label1 = "CDC cumulative", label2 = "Linelist cumulative", label3="Est. detected imports"), 
#                aes(x=date, y=cum_CDC)) +
#             geom_bar(stat="identity", aes(fill=label1)) +
#             geom_point(aes(x=date,y=cum_ll, shape=label2, color=label2), size=2) +
#             geom_point(aes(x=date,y=cum_import_mean, shape=label3, color=label3), size=2) +
#             scale_color_manual(values=c("maroon","green")) +
#             scale_fill_manual(values=c("navy")) +
#             ylab("Cumulative Cases") + ggtitle("Shenzhen Cumulative nCoV Cases") + 
#             coord_cartesian() +
#             theme_classic() +
#             theme(#legend.background = element_rect(color="grey"),
#                 legend.position = c(0.05, 1),
#                 legend.justification = c(0, 1),
#                 legend.title = element_blank())
# #plot(p_cum_shen_inclest)      
# 
# 
