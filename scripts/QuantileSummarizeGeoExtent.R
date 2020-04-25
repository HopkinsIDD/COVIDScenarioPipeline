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
    make_option(c("--outfile","-o"), type="character", default=NULL, help="file to saver output"),
    make_option("--start_date", type="character", default="2020-01-01", help="earliest date to include"),
    make_option("--end_date",  type="character", default="2022-01-01", help="latest date to include")
)

opt_parser <- OptionParser(option_list = option_list, usage="%prog [options] [one or more scenarios]")

## Paerse the
arguments <- parse_args(opt_parser, positional_arguments=c(1,Inf))
opt <- arguments$options
scenarios <- arguments$args

if(is.null(opt$outfile)) {
    stop("outfile must be specified")
}


## Register the parallel backend
doParallel::registerDoParallel(opt$ncores)


##Convert times to date objects
opt$start_date <- as.Date(opt$start_date)
opt$end_date <- as.Date(opt$end_date)


##Per file filtering code
post_proc <- function(x,opt) {


  x%>%
    group_by(geoid) %>%
      mutate(cum_infections=cumsum(incidI)) %>%
      mutate(cum_death=cumsum(incidD)) %>%
      ungroup()%>%
      filter(time>=opt$start_date& time<=opt$end_date) %>%
      group_by(time) %>%
      summarize(hosp_curr=sum(hosp_curr),
                cum_death=sum(cum_death),
                hosp=sum(incidH),
                death=sum(incidD),
                infections=sum(incidI),
                cum_infections=sum(cum_infections)) %>%
      ungroup()
}

##Run over scenarios and death rates as appropriate. Note that
##Final results will average accross whatever is included
res_all <-list()
for (i in 1:length(scenarios)) {
    res_all[[i]] <- report.generation::load_hosp_sims_filtered(scenarios[i],
                                                                 name_filter = opt$name_filter,
                                                                 num_files = opt$nfiles,
                                                                 post_process = post_proc,
                                                                 opt=opt)%>%
        mutate(scenario=scenarios[i])


}

##Put in one data frame
res_all<-dplyr::bind_rows(res_all)

##deregister backend
doParallel::stopImplicitCluster()


## Extract quantiles
tmp_col <- function(x, col) {
    x%>%
        group_by(time) %>%
        summarize(x=list(enframe(quantile(!!sym(col), probs=c(0.01, 0.025,
                                                              seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                                 "quantile",col))) %>%
        unnest(x)

}


to_save <- inner_join(tmp_col(res_all,"hosp_curr"),
                         tmp_col(res_all,"cum_death"))%>%
    inner_join(tmp_col(res_all,"death"))%>%
    inner_join(tmp_col(res_all,"infections"))%>%
    inner_join(tmp_col(res_all,"cum_infections"))%>%
    inner_join(tmp_col(res_all,"hosp"))


write_csv(to_save, path=opt$outfile)





