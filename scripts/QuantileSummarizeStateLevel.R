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

## Paerse the
arguments <- parse_args(opt_parser, positional_arguments=c(1,Inf))
opt <- arguments$options
scenarios <- arguments$args

if(is.null(opt$outfile)) {
    stop("outfile must be specified")
}

config <- covidcommon::load_config(opt$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

##Load the geodata file
suppressMessages(geodata <- readr::read_csv(paste0(config$spatial_setup$base_path,"/",config$spatial_setup$geodata)))
geodata <- geodata %>% mutate(geoid=as.character(geoid))

## Register the parallel backend
doParallel::registerDoParallel(opt$jobs)


##Convert times to date objects
if (is.null(opt$start_date)) {
  opt$start_date <- as.Date(config$start_date)
}
opt$start_date <- as.Date(opt$start_date)

if (is.null(opt$end_date)) {
  opt$end_date <- config$end_date
}
opt$end_date <- as.Date(opt$end_date)


##Per file filtering code
post_proc <- function(x,geodata,opt) {


  x%>%
    group_by(geoid) %>%
      mutate(cum_infections=cumsum(incidI)) %>%
      mutate(cum_death=cumsum(incidD)) %>%
      ungroup()%>%
      filter(time>=opt$start_date& time<=opt$end_date) %>%
      inner_join(geodata%>%select(geoid, USPS)) %>%
      group_by(USPS, time) %>%
      summarize(hosp_curr=sum(hosp_curr),
                cum_death=sum(cum_death),
                death=sum(incidD),
                hosp=sum(incidH),
                infections=sum(incidI),
                cum_infections=sum(cum_infections)) %>%
      ungroup()
}

##Run over scenarios and death rates as appropriate. Note that
##Final results will average accross whatever is included
res_state <-list()
setup_name <- config$spatial_setup$setup_name
for (i in 1:length(scenarios)) {
    scenario_dir = paste0(setup_name,"_",scenarios[i])
    res_state[[i]] <- report.generation::load_hosp_sims_filtered(scenario_dir,
                                                                 name_filter = opt$name_filter,
                                                                 ifelse(opt$num_simulations > 0, opt$num_simulations, config$nsimulations),
                                                                 post_process = post_proc,
                                                                 geodata=geodata,
                                                                 opt=opt)%>%
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
    inner_join(tmp_col(res_state,"cum_infections"))%>%
    inner_join(tmp_col(res_state,"hosp"))


write_csv(to_save_st, path=opt$outfile)





