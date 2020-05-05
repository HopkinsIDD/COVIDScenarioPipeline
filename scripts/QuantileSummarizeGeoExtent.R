## First parse the options to the scripts
suppressMessages({
    library(optparse, quietly=TRUE)
    library(tidyverse, quietly=TRUE)
    library(parallel, quietly=TRUE)
})

##List of specified options
option_list <- list(
    make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
    make_option(c("-j", "--jobs"), action="store", default=detectCores(), type='numeric', help="number of cores used"),
    make_option(c("-n", "--num_simulations"), action="store", default=-1, type='numeric', help="number of simulations to run, overrides config file value"),

    make_option(c("-d", "--name_filter"), type="character", default="", help="filename filter, usually deaths"),
    make_option(c("-o","--outfile"), type="character", default=NULL, help="file to save output"),
    make_option("--start_date", type="character", default=NULL, help="earliest date to include"),
    make_option("--end_date",  type="character", default=NULL, help="latest date to include")
)

opt_parser <- OptionParser(option_list = option_list, usage="%prog [options] [one or more scenarios]")
arguments <- parse_args(opt_parser, positional_arguments=TRUE)
opts <- arguments$options

scenarios <- arguments$args

if(is.null(opts$outfile)) {
    stop("outfile must be specified")
}

config <- covidcommon::load_config(opts$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}


## Register the parallel backend
doParallel::registerDoParallel(opts$j)


##Convert times to date objects
if (is.null(opts$start_date)) {
  opts$start_date <- as.Date(config$start_date)
}
opts$start_date <- as.Date(opts$start_date)

if (is.null(opts$end_date)) {
  opts$end_date <- config$end_date
}
opts$end_date <- as.Date(opts$end_date)

##Per file filtering code
post_proc <- function(x, opts) {


  x%>%
    group_by(geoid) %>%
      mutate(cum_infections=cumsum(incidI)) %>%
      mutate(cum_death=cumsum(incidD)) %>%
      ungroup()%>%
      filter(time>=opts$start_date & time<=opts$end_date) %>%
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
setup_name <- config$spatial_setup$setup_name
for (i in 1:length(scenarios)) {
    scenario_dir = paste0(setup_name,"_",scenarios[i])
    res_all[[i]] <- report.generation::load_hosp_sims_filtered(scenario_dir,
                                                                 name_filter = opts$name_filter,
                                                                 num_files = ifelse(opts$num_simulations > 0, opts$num_simulations, config$nsimulations),
                                                                 post_process = post_proc,
                                                                 opt=opts)%>%
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


write_csv(to_save, path=opts$outfile)





