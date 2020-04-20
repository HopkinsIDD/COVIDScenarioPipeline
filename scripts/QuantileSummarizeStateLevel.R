## First parse the options to the scripts
suppressMessages({
    library(optparse, quietly=TRUE)
    library(tidyverse, quietly=TRUE)
})

##List of specified options
option_list <- list(
    make_option("--name_filter", type="character", default="", help="filename filter, usually deaths"),
    make_option("--nfiles", type="numeric", default=NA, help="number of files to load, default is all"),
    make_option("--ncores", type="numeric", default=6, help="number of cores to use in data load, default =6"),
    make_option(c("--outfile","-o"), type="character", default=NULL, help="file to saver output")
)

opt_parser <- OptionParser(option_list = option_list, usage="%prog [options] [one or more scenarios]")

## Paerse the
arguments <- parse_args(opt_parser, positional_arguments=c(1,Inf))
opt <- arguments$options
scenarios <- arguments$args

if(is.null(opt$outfile)) {
    stop("outfile must be specified")
}

##Load the geodata file
suppressMessages(geodata <- readr::read_csv("data/geodata.csv"))


## Register the parallel backend
doParallel::registerDoParallel(opt$ncores)


##Per file filtering code
post_proc <- function(x,geodata) {


  x%>%
    group_by(geoid) %>%
    mutate(cum_infections=cumsum(incidI)) %>%
    mutate(cum_death=cumsum(incidD)) %>%
    ungroup()%>%
    filter(time>="2020-03-15" & time<="2020-05-16") %>%
    inner_join(geodata%>%select(geoid, USPS)) %>%
    group_by(USPS, time) %>%
    summarize(hosp_curr=sum(hosp_curr),
              cum_death=sum(cum_death),
              death=sum(incidD),
              infections=sum(incidI),
              cum_infections=sum(cum_infections)) %>%
    ungroup
}

##Run over scenarios and death rates as appropriate. Note that
##Final results will average accross whatever is included
res_state <-list()
for (i in 1:length(scenarios)) {
    res_state[[i]] <- report.generation::load_hosp_sims_filtered(scenarios[i],
                                                                 name_filter = opt$name_filter,
                                                                 num_files = opt$nfiles,
                                                                 post_process = post_proc,
                                                                 geodata=geodata)%>%
        mutate(scenario=scenarios[i])


}

##Put in one data frame
res_state<-dplyr::bind_rows(res_state)

##deregister backend
doParallel::stopImplicitCluster()


## Extract quantiles
tmp_col <- function(x, col) {
    x%>%
        group_by(time,USPS) %>%
        summarize(x=list(enframe(quantile(!!sym(col), probs=c(0.01, 0.025,
                                                              seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                                 "quantile",col))) %>%
        unnest(x)

}


to_save_st <- inner_join(tmp_col(res_state,"hosp_curr"),
                         tmp_col(res_state,"cum_death"))%>%
    inner_join(tmp_col(res_state,"death"))%>%
    inner_join(tmp_col(res_state,"infections"))%>%
    inner_join(tmp_col(res_state,"cum_infections"))


write_csv(to_save_st, path=opt$outfile)





