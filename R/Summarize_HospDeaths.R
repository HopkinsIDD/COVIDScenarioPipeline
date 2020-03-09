


# SETUP -------------------------------------------------------------------

##'
library(tidyverse)
source("R/DataUtils.R")
source("R/CalcHospDeaths.R")
#source("R/Run_CalcHospDeaths.R") # dont need this unless re-running the model

# Probs
p_death <- c(.001, .0025, .01)
p_hosp <- p_death*10


##'  define metropolitan areas
##'  
get_metro_labels <- function(data = sanfran_hosp_low){
    LA <- c('06037', '06059', '06065', '06071', '06111')
    SF <- c('06001', '06013', '06075', '06081', '06041', '06085', '06069', 
            '06077', '06099', '06095', '06097', '06087', '06047', '06055')
    SD <- c('06073')
    FN <- c('06019','06031','06039')
    SC <- c('06067', '06061', '06113', '06017', '06101', '06115', '06057')
    RD <- c('06089', '06103')
    
    data$county <- paste0("0",data$county)
    data$new_metrop <- 0
    data$new_metrop[data$county %in% LA] <- "LA"
    data$new_metrop[data$county %in% SF] <- "SF"
    data$new_metrop[data$county %in% SD] <- "SD"
    data$new_metrop[data$county %in% FN] <- "FN"
    data$new_metrop[data$county %in% SC] <- "SC"
    data$new_metrop[data$county %in% RD] <- "RD"
    
    ##Update the labels
    data$metrop_labels <- NA
    data$metrop_labels[data$new_metrop=="LA"] <- "Los Angeles"
    data$metrop_labels[data$new_metrop=="SF"] <- "San Francisco"
    data$metrop_labels[data$new_metrop=="SD"] <- "San Diego"
    data$metrop_labels[data$new_metrop=="FN"] <- "Fresno"
    data$metrop_labels[data$new_metrop=="SC"] <- "Sacremento"
    data$metrop_labels[data$new_metrop=="RD"] <- "Redding"
    data$metrop_labels <- as.factor(data$metrop_labels)

    return(data)
}




summ_hosp_death_table <- function(df = sanfran_hosp_low, end_date="2020-04-01"){
    # Hospitalization - by location
    df_summ_H <- df %>% filter(time <= as.Date(end_date)) %>%
        group_by(sim_num, p_death, metrop_labels) %>% 
        summarize(hosp = sum(incidH)) %>% 
        group_by(p_death, metrop_labels) %>% 
        summarize(mean=mean(hosp), 
                  pi_low=quantile(hosp, probs=0.2), 
                  pi_high=quantile(hosp, probs=0.8))
    
    dat <- df_summ_H %>% mutate(estH = paste0(round(mean,1), " (", round(pi_low,1),"-",round(pi_high,1),")")) %>% 
        mutate(metrop_labels = factor(metrop_labels, levels=c( "San Francisco",
                                                               "Sacremento",
                                                               "Fresno",
                                                               "Los Angeles",
                                                               "San Diego",
                                                               "Redding"), ordered = TRUE)) %>% arrange(p_death, metrop_labels)
    locs_ <- dat %>% select(-(mean:pi_high)) %>% spread(key=p_death, value=estH)
    
    
    # Hospitalization - All locations
    df_summ_H <- df %>% filter(time <= as.Date(end_date)) %>%
        group_by(sim_num, p_death) %>% 
        summarize(hosp = sum(incidH)) %>% 
        group_by(p_death) %>% 
        summarize(mean=mean(hosp), pi_low=quantile(hosp, probs=0.2), pi_high=quantile(hosp, probs=0.8))
    all_ <- df_summ_H %>% mutate(estH = paste0(round(mean,1), " (", round(pi_low,1),"-",round(pi_high,1),")")) %>% 
        mutate(metrop_labels = "All Locations") %>% select(-(mean:pi_high)) %>% spread(key=p_death, value=estH)
    
    # Final table
    tab_H <- bind_rows(all_, locs_)
    
    labs <- c(" ",rep("Hospitalizations", 3))
    names(labs) <- c("metrop_labels", "0.001", "0.0025", "0.01")
    tab_H <- bind_rows(labs, tab_H)
    
    
    # ~ Deaths - Low ----------------------------------------------------------
    
    # Deaths - by location
    df_summ_D <- df %>% filter(time <= as.Date(end_date)) %>%
        group_by(sim_num, p_death, metrop_labels) %>% 
        summarize(death = sum(incidD)) %>% 
        group_by(p_death, metrop_labels) %>% 
        summarize(mean=mean(death), pi_low=quantile(death, probs=0.2), pi_high=quantile(death, probs=0.8))
    
    dat <- df_summ_D %>% mutate(estD = paste0(round(mean,1), " (", round(pi_low,1),"-",round(pi_high,1),")")) %>% 
        mutate(metrop_labels = factor(metrop_labels, levels=c( "San Francisco",
                                                               "Sacremento",
                                                               "Fresno",
                                                               "Los Angeles",
                                                               "San Diego",
                                                               "Redding"), ordered = TRUE)) %>% arrange(p_death, metrop_labels)
    locs_ <- dat %>% select(-(mean:pi_high)) %>% spread(key=p_death, value=estD)
    
    
    # Deaths - by location
    df_summ_D <- df %>% filter(time <= as.Date(end_date)) %>%
        group_by(sim_num, p_death) %>% 
        summarize(death = sum(incidD)) %>% 
        group_by(p_death) %>% 
        summarize(mean=mean(death), pi_low=quantile(death, probs=0.2), pi_high=quantile(death, probs=0.8))
    all_ <- df_summ_D %>% mutate(estD = paste0(round(mean,1), " (", round(pi_low,1),"-",round(pi_high,1),")")) %>% 
        mutate(metrop_labels = "All Locations") %>% select(-(mean:pi_high)) %>% spread(key=p_death, value=estD)
    
    # Final table
    tab_D <- bind_rows(all_, locs_)
    
    labs <- c(" ",rep("Deaths", 3))
    names(labs) <- c("metrop_labels", "0.001", "0.0025", "0.01")
    tab_D <- bind_rows(labs, tab_D)
    
    # combine for final table
    tab_res <- bind_cols(tab_H, tab_D[,-1])
    tab_res <- tab_res[, c(1,2,5,3,6,4,7)]
    
    return(tab_res)
}









# DATA --------------------------------------------------------------------

# ~ MID scenario
sim_hospdeath_midhigh <- read_csv(file.path("model_output/SanFrancisco/hosp_death","scenario_A_high.csv"))
sim_hospdeath_midmid <- read_csv(file.path("model_output/SanFrancisco/hosp_death","scenario_A_mid.csv"))
sim_hospdeath_midlow <- read_csv(file.path("model_output/SanFrancisco/hosp_death","scenario_A_low.csv"))

sanfran_hosp_mid <- bind_rows(sim_hospdeath_midlow %>% mutate(p_death=p_death[1], scenario="A"), 
                              sim_hospdeath_midmid %>% mutate(p_death=p_death[2], scenario="A"),
                              sim_hospdeath_midhigh %>% mutate(p_death=p_death[3], scenario="A"))

# ~ LOW scenario
sim_hospdeath_lowhigh <- read_csv(file.path("model_output/SanFrancisco/hosp_death","scenario_B_high.csv"))
sim_hospdeath_lowmid <- read_csv(file.path("model_output/SanFrancisco/hosp_death","scenario_B_mid.csv"))
sim_hospdeath_lowlow <- read_csv(file.path("model_output/SanFrancisco/hosp_death","scenario_B_low.csv"))

sanfran_hosp_low <- bind_rows(sim_hospdeath_lowlow %>% mutate(p_death=p_death[1], scenario="B"), 
                              sim_hospdeath_lowmid %>% mutate(p_death=p_death[2], scenario="B"),
                              sim_hospdeath_lowhigh %>% mutate(p_death=p_death[3], scenario="B"))

# ~ Assign metro areas
sanfran_hosp_low <- get_metro_labels(data = sanfran_hosp_low)
sanfran_hosp_mid <- get_metro_labels(data = sanfran_hosp_mid)






# SUMMARIZE SCENARIOS --------------------------------------------------


# ~ Hospitalization/Death - LOW -------------------------------------------------

summ_hosp_death_table(df = sanfran_hosp_low, end_date="2020-04-01")


# ~ Hospitalization/Death - MID -------------------------------------------------

summ_hosp_death_table(df = sanfran_hosp_mid, end_date="2020-04-01")




