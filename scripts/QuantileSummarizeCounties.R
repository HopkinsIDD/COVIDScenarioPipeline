## First parse the options to the scripts
#suppressMessages({
library(optparse, quietly=TRUE)
library(tidyverse, quietly=TRUE)
library(parallel)
library(tictoc)
#})

if(packageVersion("dplyr")[[1,1]] < 1) {
    stop("requires dplyr 1.0 or higher.")
}

##List of specified options
option_list <- list(
    make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
    make_option(c("-j", "--jobs"), action="store", default=detectCores()-1, type='numeric', help="number of cores used if parallel"),
    make_option(c("-m", "--multiplyr"), action="store_true", default=FALSE, help="should multiplyr be used. Must be true to process in parallel"),
     make_option(c("--geodata"), type="character", default=NULL, help="location of geodata. Only used if not parallel processing"),
    make_option(c("-n", "--num_simulations"),
                action="store", default=-1, type='numeric',
                help="number of simulations to run, overrides config file value"),
    make_option(c("-d", "--death_filter"), type="character", default="", help="filename filter, usually deaths"),
    
    make_option(c("-o","--outfile"), type="character", default=NULL, help="file to save output"),
    make_option("--start_date", type="character", default=NULL, help="earliest date to include"),
    make_option("--end_date",  type="character", default=NULL, help="latest date to include"),
    make_option(c("--week","-w"), action="store_true", default=FALSE, help="Aggregate to epi week")
)


opt_parser <- OptionParser(option_list = option_list,
                           usage="%prog [options] [directory to be used]")
arguments <- parse_args(opt_parser, positional_arguments=c(1,Inf))
opt <- arguments$options

if(is.null(opt$outfile)) {
    stop("outfile must be specified")
}

directory <- arguments$args

if(opt$death_filter == "") {
  warning("death_filter is empty. Did you mean to filter by death rate?")
}


if(opt$week) {
    stop("Weekly aggregation not yet implemented")
}

##Loading the results is the same no matter what
tic("Loaded data")
##Load the data
res<- arrow::open_dataset(sprintf("%s/hosp",directory), 
                          partitioning =c("location", 
                                          "scenario", 
                                          "death_rate", 
                                          "date", "
                                                 lik_type", 
                                          "is_final", 
                                          "sim_id"))%>%
  select(time, geoid, incidD, incidH, incidC, hosp_curr, incidI, death_rate, sim_id)%>%
  filter(time>=opt$start_date& time<=opt$end_date)%>%
  collect()%>%
  filter(stringr::str_detect(death_rate,opt$death_filter))%>%
  mutate(time=as.Date(time))%>%
  mutate(sim_num = sim_id)%>%
  rename(death=incidD,
         hosp=incidH,
         infections=incidI,
         confirmed=incidC)

toc()



if (opt$multiplyr) {
    print("Running in parallel")

    library(multidplyr)

    tic("Registered Cluster")
    cl <- new_cluster(opt$jobs)
    toc()

    tic("Calculated cumsums")
    res <- res %>%
        group_by(geoid)%>%
        partition(cl)%>%
        group_by(geoid, sim_id, death_rate) %>%
        mutate(cum_infections=cumsum(infections),
               cum_death=cumsum(death),
               cum_confirmed=cumsum(confirmed)) %>%
        collect()%>%
        ungroup()
    toc()

    tic("Calculated quantiles")
    res <- res %>%
        group_by(geoid,time)%>%
        partition(cl)%>%
        summarize(hosp_curr=quantile(hosp_curr, 
                                     probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                  cum_death=quantile(cum_death, 
                                     probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                  death=quantile(death, 
                                 probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                  confirmed=quantile(confirmed, 
                                     probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                  cum_confirmed=quantile(cum_confirmed, 
                                         probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))) %>%
        mutate(quantile=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))%>%
        collect()%>%
        ungroup()  
    toc()

    print(res)


    
    
} else {
    print("Running in serial.")

    tic("joined to geodata")
    geodata <- read_csv(opt$geodata)
    res<-inner_join(res, geodata)
    toc()

    state_res <- list()
    states <- unique(res$USPS)


    for(i in 1:length(states)) {
 
        state<- states[i]
        cat(i,": processing state ",state,"\n")
        tic("processed state")

        state_res[[i]] <- res%>%
            filter(USPS==state)%>%
            group_by(geoid,sim_id, death_rate) %>%
            mutate(cum_infections=cumsum(infections)) %>%
            mutate(cum_death=cumsum(death)) %>%
            mutate(cum_confirmed=cumsum(confirmed)) %>%
            ungroup() %>% 
            group_by(geoid, time)%>% 
            summarize(hosp_curr=quantile(hosp_curr, 
                                         probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                      cum_death=quantile(cum_death, 
                                         probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                      death=quantile(death, 
                                     probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                      confirmed=quantile(confirmed, 
                                         probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                      cum_confirmed=quantile(cum_confirmed, 
                                             probs=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)))%>%
            mutate(quantile=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))%>%
            ungroup()

        toc()
    }

    tic("final bind")
    res <- bind_rows(state_res)
    rm(state_res)
    toc()

    
    
}

print("Saving results.")
tic("Saved data")
write_csv(res, path=opt$outfile)
toc()
