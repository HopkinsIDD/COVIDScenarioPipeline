##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##'  nCoV Importation Analyses --  Risk Analyses    ####
##'                 
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if (!exists("version") | is.list(version)){     version <- "1"                    }
if (!exists("batch") | is.list(batch)){         batch <- "1st"                    }
if (!exists("project_name")){                   project_name <- "california_import" }
if (!exists("start_date")){                   start_date <- "2020-01-01" }
if (!exists("na_to_zero")){                   na_to_zero <- TRUE }

print(project_name)
print(version)
print(batch)

dir.create(file.path("results", project_name), recursive = TRUE)
import_sim_file <- file.path("output",project_name, sprintf("covid_importation_sim_%s_batch_v%s.RData", batch, version))



# SETUP -------------------------------------------------------------------

options(scipen=999)
if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('reshape2')) install.packages('reshape2'); library(reshape2)
if(!require('abind')) install.packages('abind'); library(abind)



# IMPORT DATA -------------------------------------------------------------
    
load(import_sim_file) # load importation_sim
Sys.sleep(5) # pause to let the computer catch up
#format(object.size(importation_sim),"Mb")

sources_ <- dimnames(importation_sim)[[1]]
dests_ <- dimnames(importation_sim)[[2]]
t_ <- dimnames(importation_sim)[[3]]
sims_ <- dimnames(importation_sim)[[4]]


# Check range of sims
range(sims_)
tmp <- apply(importation_sim, c(3,4), sum, na.rm=TRUE)
rowMeans(tmp)



# ~ Convert to Long Form --------------------------------------------------

# Convert raw data array into long format
import_data_long <- arrayhelpers::array2df(importation_sim)
Sys.sleep(5) # pause to let the computer catch up

colnames(import_data_long) <- c("imports","source","destination","t","sim")
import_data_long <- import_data_long %>% mutate(t = lubridate::ymd(t)) %>% as.data.frame()

# Remove time prior to start_date
min((import_data_long %>% filter(!is.na(imports)))$t)
import_data_long <- import_data_long %>% filter(t>=start_date)


#write_csv(import_data_long, paste0("data/est_imports_long_ver",version,".csv"))
import_data_long$imports <- as.integer(as.character(import_data_long$imports))
dims_import_data <- dim(importation_sim)
n_sim <- dims_import_data[4]
format(object.size(import_data_long), "Mb")

# # Add reported
# import_data_long <- left_join(import_data_long, imports_reported_bydestination)

# Make NAs into 0 if desired
if (na_to_zero){
    import_data_long <- import_data_long %>% mutate(imports = ifelse(is.na(imports), 0, imports))
}

# ~ Summarize imporations in long data ------------------------------------

# Get mean and medians for ea/ch sim
import_data_long <- import_data_long %>% group_by(sim) %>% 
    mutate(mean_sim=mean(imports, na.rm=T),
           median_sim=median(imports, na.rm=T)) %>% ungroup()

# Get RR compared to the mean and median
import_data_long$RR.vs.sim.mean <- import_data_long$imports / import_data_long$mean_sim
import_data_long$RR.vs.sim.median <- import_data_long$imports / import_data_long$median_sim

# Get RR compared to the destination mean
import_data_long <- import_data_long %>% group_by(destination, sim) %>%
    mutate(destination_sim_mean = mean(imports, na.rm=TRUE)) %>% ungroup()
import_data_long$RR.vs.destination.mean <- import_data_long$imports / import_data_long$destination_sim_mean
import_data_long$RR.vs.destination.mean[import_data_long$destination_sim_mean==0] <- 1


# Save it
dir.create(file.path("results", project_name), recursive = TRUE, showWarnings = FALSE)
write_csv(import_data_long, file.path("results", project_name, sprintf("import_data_long_v%s.csv", version)))


# # Load it
import_data_long <- read_csv(file.path("results", project_name, sprintf("import_data_long_v%s.csv", version))) %>% as.data.frame()

cols.numeric <- c("mean_sim", "RR.vs.sim.mean", "RR.vs.sim.median", "destination_sim_mean", "RR.vs.destination.mean")
cols.integer <- c("imports", "sim", "median_sim")
#"rep_imports", "rep_imports_orig", "missing_data_count", "available_data_count","mean_travel", "mean_population", 
import_data_long[cols.numeric] <- sapply(import_data_long[cols.numeric], as.numeric)
import_data_long[cols.integer] <- sapply(import_data_long[cols.integer], as.integer)
sapply(import_data_long, class)

n_sim <- max(import_data_long$sim)







# SUMMARIZE RESULTS -------------------------------------------------------


# ~ import_results_dest --------------------------------------------------------

# Get RR mean and CIs
import_results_dest <- import_data_long %>% group_by(destination, sim) %>% 
    mutate(imports = sum(imports, na.rm = TRUE)) %>% ungroup() %>% group_by(destination) %>%
    summarize(
        import_mean=mean(imports, na.rm=T),
        import_sd=sd(imports, na.rm=T),
        import_ll=quantile(imports, probs=.025, na.rm=T),
        import_ul=quantile(imports, probs=.975, na.rm=T),
        import_median=median(imports, na.rm=T),
        import_min=min(imports, na.rm=T),
        import_max=max(imports, na.rm=T),
        import_25pt=quantile(imports, probs=.25, na.rm=T),
        import_75pt=quantile(imports, probs=.75, na.rm=T),
        
        RR_mean=mean(RR.vs.sim.mean, na.rm=T),
        RR_ll=quantile(RR.vs.sim.mean, probs=.025, na.rm=T),
        RR_ul=quantile(RR.vs.sim.mean, probs=.975, na.rm=T),
        RR_sd=sd(RR.vs.sim.mean, na.rm=T),
        RR_median=median(RR.vs.sim.mean, na.rm=T),
        
        RR_destination_mean=mean(RR.vs.destination.mean, na.rm=T),
        RR_destination_ll=quantile(RR.vs.destination.mean, probs=.025, na.rm=T),
        RR_destination_ul=quantile(RR.vs.destination.mean, probs=.975, na.rm=T),
        RR_destination_sd=sd(RR.vs.destination.mean, na.rm=T),
        RR_destination_median=median(RR.vs.destination.mean, na.rm=T)#,
        #rep_imports = mean(rep_imports, na.rm = T)
    )

write_csv(import_results_dest, file.path("results",project_name,sprintf("import_results_dest_v%s.csv", version)))



# ~ import_results_desttime --------------------------------------------------------



# Get RR mean and CIs
import_results_desttime <- import_data_long %>% group_by(destination, t, sim) %>% 
    mutate(imports = sum(imports, na.rm = TRUE)) %>% ungroup() %>% group_by(destination, t) %>%
    summarize(
        import_mean=mean(imports, na.rm=T),
        import_sd=sd(imports, na.rm=T),
        import_ll=quantile(imports, probs=.025, na.rm=T),
        import_ul=quantile(imports, probs=.975, na.rm=T),
        import_median=median(imports, na.rm=T),
        import_min=min(imports, na.rm=T),
        import_max=max(imports, na.rm=T),
        import_25pt=quantile(imports, probs=.25, na.rm=T),
        import_75pt=quantile(imports, probs=.75, na.rm=T),
        
        RR_mean=mean(RR.vs.sim.mean, na.rm=T),
        RR_ll=quantile(RR.vs.sim.mean, probs=.025, na.rm=T),
        RR_ul=quantile(RR.vs.sim.mean, probs=.975, na.rm=T),
        RR_sd=sd(RR.vs.sim.mean, na.rm=T),
        RR_median=median(RR.vs.sim.mean, na.rm=T),
        
        RR_destination_mean=mean(RR.vs.destination.mean, na.rm=T),
        RR_destination_ll=quantile(RR.vs.destination.mean, probs=.025, na.rm=T),
        RR_destination_ul=quantile(RR.vs.destination.mean, probs=.975, na.rm=T),
        RR_destination_sd=sd(RR.vs.destination.mean, na.rm=T),
        RR_destination_median=median(RR.vs.destination.mean, na.rm=T)#,
        #rep_imports = mean(rep_imports, na.rm = T)
        )

# Get the probability of >0 imports
prob_any_import <- import_data_long %>% group_by(destination, t, sim) %>% 
    summarise(imports = sum(imports)) %>% group_by(destination, t) %>%
    summarize(n_import = sum(imports>0)) %>% mutate(prob_import=n_import/(n_sim))

# Add Probability
import_results_desttime$prob_any_import <- prob_any_import$prob_import
import_results_desttime <- import_results_desttime %>% dplyr::select(1,2,prob_any_import, everything())
rm(prob_any_import)

write_csv(import_results_desttime, file.path("results",project_name,sprintf("import_results_desttime_v%s.csv", version)))


# # ADD REPORTED TO ESTIMATED
# #source("Source/clean_reported_importations.R")
# reported_file_path <- paste0("Results/",results_folder,"/import_reported_long_matched.csv")
# 
# if (file.exists(reported_file_path)){
#     # load reported imports summed by destination
#     imports_reported_long <- read_csv(reported_file_path) %>% as.data.frame() %>%
#         rename(rep_imports=imports)
# 
#     # merge with import_results
#     import_results <- left_join(import_results, imports_reported_long)
#     import_data_long <- left_join(import_data_long, imports_reported_long)
#     
# } else {
#     import_results$rep_imports <- NA
#     import_data_long$rep_imports <- NA
# }







# ~ import_results_desttime_cum --------------------------------------------------------
# Cumulate importations into destination

# Add up importations by location by simulation
import_data_long <- import_data_long %>% group_by(sim, source, destination) %>%
    mutate(imports_cum = cumsum(imports)) %>% ungroup()

# Get RR mean and CIs
import_results_desttime_cum <- import_data_long %>% group_by(destination, t, sim) %>% 
    mutate(imports_cum = sum(imports_cum, na.rm = TRUE)) %>% ungroup() %>% group_by(destination, t) %>%
    summarize(
        cum_import_mean=mean(imports_cum, na.rm=T),
        cum_import_sd=sd(imports_cum, na.rm=T),
        cum_import_ll=quantile(imports_cum, probs=.025, na.rm=T),
        cum_import_ul=quantile(imports_cum, probs=.975, na.rm=T),
        cum_import_median=median(imports_cum, na.rm=T),
        cum_import_min=min(imports_cum, na.rm=T),
        cum_import_max=max(imports_cum, na.rm=T),
        cum_import_25pt=quantile(imports_cum, probs=.25, na.rm=T),
        cum_import_75pt=quantile(imports_cum, probs=.75, na.rm=T),
    )

# Get the probability of >0 imports
prob_any_import <- import_data_long %>% group_by(destination, t, sim) %>% 
    summarise(imports_cum = sum(imports_cum)) %>% group_by(destination, t) %>%
    summarize(n_import = sum(imports_cum>0)) %>% mutate(prob_import=n_import/(n_sim))

# Add Probability
import_results_desttime_cum$cum_prob_any_import <- prob_any_import$prob_import
import_results_desttime_cum <- import_results_desttime_cum %>% dplyr::select(1,2,cum_prob_any_import, everything())
rm(prob_any_import)

write_csv(import_results_desttime_cum, file.path("results",project_name,sprintf("import_results_desttime_cumulative_v%s.csv", version)))





# ~ import_results_desttime_overall --------------------------------------------------------


# Get RR mean and CIs
import_results_desttime_overall <- import_data_long %>% group_by(t, sim) %>% 
    mutate(imports = sum(imports, na.rm = TRUE)) %>% ungroup() %>% group_by(t) %>%
    summarize(
        import_mean=mean(imports, na.rm=T),
        import_sd=sd(imports, na.rm=T),
        import_ll=quantile(imports, probs=.025, na.rm=T),
        import_ul=quantile(imports, probs=.975, na.rm=T),
        import_median=median(imports, na.rm=T),
        import_min=min(imports, na.rm=T),
        import_max=max(imports, na.rm=T),
        import_25pt=quantile(imports, probs=.25, na.rm=T),
        import_75pt=quantile(imports, probs=.75, na.rm=T),
        
        RR_mean=mean(RR.vs.sim.mean, na.rm=T),
        RR_ll=quantile(RR.vs.sim.mean, probs=.025, na.rm=T),
        RR_ul=quantile(RR.vs.sim.mean, probs=.975, na.rm=T),
        RR_sd=sd(RR.vs.sim.mean, na.rm=T),
        RR_median=median(RR.vs.sim.mean, na.rm=T),
        
        RR_destination_mean=mean(RR.vs.destination.mean, na.rm=T),
        RR_destination_ll=quantile(RR.vs.destination.mean, probs=.025, na.rm=T),
        RR_destination_ul=quantile(RR.vs.destination.mean, probs=.975, na.rm=T),
        RR_destination_sd=sd(RR.vs.destination.mean, na.rm=T),
        RR_destination_median=median(RR.vs.destination.mean, na.rm=T)#,
        #rep_imports = mean(rep_imports, na.rm = T)
    )

# Get the probability of >0 imports
prob_any_import <- import_data_long %>% group_by(t, sim) %>% 
    summarise(imports = sum(imports)) %>% group_by(t) %>%
    summarize(n_import = sum(imports>0)) %>% mutate(prob_import=n_import/(n_sim))

# Add Probability
import_results_desttime_overall$prob_any_import <- prob_any_import$prob_import
import_results_desttime_overall <- import_results_desttime_overall %>% dplyr::select(1,2,prob_any_import, everything())
rm(prob_any_import)

write_csv(import_results_desttime_overall, file.path("results",project_name,sprintf("import_results_desttime_overall_v%s.csv", version)))


# # ADD REPORTED TO ESTIMATED
# #source("Source/clean_reported_importations.R")
# reported_file_path <- paste0("Results/",results_folder,"/import_reported_long_matched.csv")
# 
# if (file.exists(reported_file_path)){
#     # load reported imports summed by destination
#     imports_reported_long <- read_csv(reported_file_path) %>% as.data.frame() %>%
#         rename(rep_imports=imports)
# 
#     # merge with import_results
#     import_results <- left_join(import_results, imports_reported_long)
#     import_data_long <- left_join(import_data_long, imports_reported_long)
#     
# } else {
#     import_results$rep_imports <- NA
#     import_data_long$rep_imports <- NA
# }







# ~ import_results_desttime_overall_cum --------------------------------------------------------
# Cumulate importations into overall destination (i.e., a state)

# Get RR mean and CIs
import_results_desttime_overall_cum <- import_data_long %>% 
    group_by(t, sim) %>% summarise(imports = sum(imports, na.rm = TRUE)) %>% as.data.frame() %>%
    group_by(sim) %>% mutate(imports_cum = cumsum(imports)) %>% ungroup(sim) %>% 
    group_by(t) %>%
    summarize(
        cum_import_mean=mean(imports_cum, na.rm=T),
        cum_import_sd=sd(imports_cum, na.rm=T),
        cum_import_ll=quantile(imports_cum, probs=.025, na.rm=T),
        cum_import_ul=quantile(imports_cum, probs=.975, na.rm=T),
        cum_import_median=median(imports_cum, na.rm=T),
        cum_import_min=min(imports_cum, na.rm=T),
        cum_import_max=max(imports_cum, na.rm=T),
        cum_import_25pt=quantile(imports_cum, probs=.25, na.rm=T),
        cum_import_75pt=quantile(imports_cum, probs=.75, na.rm=T),
    )

# Get the probability of >0 imports
prob_any_import <- import_data_long %>% group_by(t, sim) %>% 
    summarise(imports = sum(imports, na.rm = TRUE)) %>% as.data.frame() %>%
    group_by(sim) %>% mutate(imports_cum = cumsum(imports)) %>% ungroup(sim) %>% 
    group_by(t) %>%
    summarize(n_import = sum(imports_cum>0)) %>% 
    mutate(prob_import=n_import/(n_sim))

# Add Probability
import_results_desttime_overall_cum$cum_prob_any_import <- prob_any_import$prob_import
import_results_desttime_overall_cum <- import_results_desttime_overall_cum %>% dplyr::select(1,2,cum_prob_any_import, everything())
rm(prob_any_import)

write_csv(import_results_desttime_overall_cum, file.path("results",project_name,sprintf("import_results_desttime_overall_cumulative_v%s.csv", version)))





# ~ import_results_sources ---------------------------------------------------
#  -- SUMMARIZE IMPORTATION FROM PROVINCES - FROM LONG DATA

head(import_data_long)

# long source year
import_data_sourcesday_long <- import_data_long %>% 
    group_by(source, t, sim) %>% 
    summarize(imports=sum(imports, na.rm=T))
head(import_data_sourcesday_long)

# long source
import_data_sources_long <- import_data_long %>% group_by(source, sim) %>% 
    summarize(imports=sum(imports, na.rm=T))
head(import_data_sources_long)

# Get mean and medians for each sim
import_data_sources_long <- import_data_sources_long %>% group_by(sim) %>% 
    mutate(mean_sim=mean(imports, na.rm=T), median_sim=median(imports, na.rm=T)) %>% ungroup()

# Get the probability of >0 imports
prob_any_import <- import_data_sources_long %>% group_by(source) %>% 
    summarize(n_anyimport=sum(imports>0)) %>% mutate(prob_import=n_anyimport/n_sim)

# Get RR compared to the mean
import_data_sources_long <- import_data_sources_long %>% mutate(RR.vs.sim.mean = imports / mean_sim)

# Get RR mean and CIs
import_results_sources <- import_data_sources_long %>% group_by(source) %>% 
    summarize(
        import_mean=mean(imports, na.rm=T),
        import_sd=sd(imports, na.rm=T),
        import_ll=quantile(imports, probs=.025, na.rm=T),
        import_ul=quantile(imports, probs=.975, na.rm=T),
        import_median=median(imports, na.rm=T),
        import_min=min(imports, na.rm=T),
        import_max=max(imports, na.rm=T),
        
        RR_mean=mean(RR.vs.sim.mean, na.rm=T),
        RR_ll=quantile(RR.vs.sim.mean, probs=.025, na.rm=T),
        RR_ul=quantile(RR.vs.sim.mean, probs=.975, na.rm=T),
        RR_sd=sd(RR.vs.sim.mean, na.rm=T),
        RR_median=median(RR.vs.sim.mean, na.rm=T)) %>% as.data.frame()

import_results_sources <- left_join(import_results_sources, prob_any_import %>% select(-n_anyimport), by=c("source"="source"))
head(import_results_sources)
rm(prob_any_import)

write_csv(import_results_sources, file.path("results",project_name,sprintf("import_results_sources_v%s.csv", version)))





# ~ import_results_sourcetime ---------------------------------------

# SUMMARIZE IMPORTATIONS BY STATE AND YEAR

# Sum up by t
import_sourcetime_long <- import_data_long %>% as.data.frame() %>% 
    group_by(source, t, sim) %>% 
    summarise(imports=sum(imports))
import_sourcetime_long <- import_sourcetime_long %>%
    group_by(sim) %>% mutate(mean_sim=mean(imports, na.rm=T)) %>% ungroup() %>%
    mutate(RR.vs.sim.mean=imports/mean_sim)

# Get RR mean and CIs
import_results_sourcetime <- import_sourcetime_long %>% group_by(source, t) %>% 
    summarize(
        import_mean=mean(imports, na.rm=T),
        import_sd=sd(imports, na.rm=T),
        import_ll=quantile(imports, probs=.025, na.rm=T),
        import_ul=quantile(imports, probs=.975, na.rm=T),
        import_ll_pois = NA,
        import_ul_pois = NA,
        import_median=median(imports, na.rm=T),
        import_min=min(imports, na.rm=T),
        import_max=max(imports, na.rm=T),
        
        #rep_import=mean(rep_imports, na.rm=T),

        RR_mean=mean(RR.vs.sim.mean, na.rm=T),
        RR_ll=quantile(RR.vs.sim.mean, probs=.025, na.rm=T),
        RR_ul=quantile(RR.vs.sim.mean, probs=.975, na.rm=T),
        RR_sd=sd(RR.vs.sim.mean, na.rm=T),
        RR_median=median(RR.vs.sim.mean, na.rm=T)
    )

import_results_sourcetime <- import_results_sourcetime %>% 
    mutate(import_ll_pois = import_mean - 1.96 * sqrt(import_mean/10000),
           import_ul_pois = import_mean + 1.96 * sqrt(import_mean/10000))


# Get the probability of >0 imports
prob_any_import <- import_data_long %>% group_by(source, t, sim) %>% 
    summarise(imports = sum(imports)) %>% group_by(source, t) %>%
    summarize(n_import = sum(imports>0)) %>% mutate(prob_import=n_import/(n_sim))


prob_for_cis <- prob_any_import %>% filter(!is.na(n_import)) %>%
    mutate(n = n_sim)
cis <- Hmisc::binconf(prob_for_cis$n_import, prob_for_cis$n, 
                      alpha=0.05,
                      method=c("wilson"),
                      include.x=FALSE, include.n=FALSE, return.df=TRUE)
prob_for_cis <- data.frame(prob_for_cis, cis)          
prob_for_cis <- prob_for_cis %>% mutate(Lower=round(Lower, 3), Upper=round(Upper, 3))       
prob_any_import <- left_join(prob_any_import, prob_for_cis %>% 
                                 select(source, t, prob_ll=Lower, prob_ul=Upper))
rm(prob_for_cis)

t_meanprob <- prob_any_import %>% group_by(source) %>%
    summarise(mean = round(mean(prob_import, na.rm=TRUE), 3),
              mean_ll = round(mean(prob_ll, na.rm=TRUE), 3),
              mean_ul = round(mean(prob_ul, na.rm=TRUE), 3),
              probyr_sd = sd(prob_import, na.rm=TRUE),
              n_samp = sum(!is.na(prob_import))) %>%
    mutate(probyr_ll = round(mean - 1.96*probyr_sd/sqrt(n_samp), 3),
           probyr_ul = round(mean + 1.96*probyr_sd/sqrt(n_samp), 3))


import_results_sourcetime$prob_any_import <- prob_any_import$prob_import
import_results_sourcetime <- import_results_sourcetime %>% dplyr::select(1,2,prob_any_import, everything())
rm(prob_any_import)

import_results_sourcetime <- import_results_sourcetime %>% 
    dplyr::select(source, t, everything())
# 
# # Add travel and population adjustment
# Travelers.source.t <- read_csv("Data/travel_t_sources.csv")
# import_results_sourcetime <- add_t_pop_and_travel(import_results_sourcetime)
# import_results_sourcetime <- import_results_sourcetime %>%
#     mutate(exp_per_100000trav = import_mean /  (travelers/100000), 
#            exp_per_100000trav_ll = import_ll /  (travelers/100000),
#            exp_per_100000trav_ul = import_ul /  (travelers/100000))

write_csv(import_results_sourcetime, file.path("results",project_name,sprintf("import_results_sourcetime_v%s.csv", version)))
#import_results_sourcetime <- read_csv(paste0("Results/",results_folder,"/import_results_sourcetime_summary.csv")) %>% as.data.frame()





# ~ import_results_sourcetime_cum --------------------------------------------------------
# Cumulate importations into source


# Get RR mean and CIs
import_results_sourcetime_cum <- import_data_long %>% group_by(source, t, sim) %>% 
    mutate(imports_cum = sum(imports_cum, na.rm = TRUE)) %>% ungroup() %>% group_by(source, t) %>%
    summarize(
        cum_import_mean=mean(imports_cum, na.rm=T),
        cum_import_sd=sd(imports_cum, na.rm=T),
        cum_import_ll=quantile(imports_cum, probs=.025, na.rm=T),
        cum_import_ul=quantile(imports_cum, probs=.975, na.rm=T),
        cum_import_median=median(imports_cum, na.rm=T),
        cum_import_min=min(imports_cum, na.rm=T),
        cum_import_max=max(imports_cum, na.rm=T),
        cum_import_25pt=quantile(imports_cum, probs=.25, na.rm=T),
        cum_import_75pt=quantile(imports_cum, probs=.75, na.rm=T),
    )

# Get the probability of >0 imports
prob_any_import <- import_data_long %>% group_by(source, t, sim) %>% 
    summarise(imports_cum = sum(imports_cum)) %>% group_by(source, t) %>%
    summarize(n_import = sum(imports_cum>0)) %>% mutate(prob_import=n_import/(n_sim))

# Add Probability
import_results_sourcetime_cum$cum_prob_any_import <- prob_any_import$prob_import
import_results_sourcetime_cum <- import_results_sourcetime_cum %>% dplyr::select(1,2,cum_prob_any_import, everything())
rm(prob_any_import)

write_csv(import_results_sourcetime_cum, file.path("results",project_name,sprintf("import_results_sourcetime_cumulative_v%s.csv", version)))

















# REMOVE EVERYTHING FROM THE CURRENT WORKSPACE
#rm(list = ls()); gc()


