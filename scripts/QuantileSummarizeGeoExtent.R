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
    make_option("--end_date",  type="character", default=NULL, help="latest date to include"),
    make_option(c("--week","-w"), action="store_true", default=FALSE, help="Aggregate to epi week")
)

opt_parser <- OptionParser(option_list = option_list, usage="%prog [options] [one or more scenarios]")
arguments <- parse_args(opt_parser, positional_arguments=TRUE)
opt <- arguments$options

scenarios <- arguments$args

if(is.null(opt$outfile)) {
    stop("outfile must be specified")
}

config <- covidcommon::load_config(opt$c)

if(length(scenarios) == 0) {
  config_name <- config$name
  scenarios <- config$interventions$scenarios %>% lapply(function(x) { paste0(config_name,"_",x)}) %>% unlist()
}

if(opt$num_simulations == -1) {
  opt$num_simulations <- config$nsimulations
}

if(opt$name_filter == "") {
  warning("name_filter is empty. Did you mean to filter by death rate?")
}

## Register the parallel backend
cl <- parallel::makeCluster(opt$jobs)
doParallel::registerDoParallel(cl)


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
if (!opt$week) {
    post_proc <- function(x, opt) {

        if (!("incidC" %in% names(x))) {
          x$incidC <- as.numeric(NA)
        }
        x <- x%>%
            group_by(geoid) %>%
            mutate(cum_infections=cumsum(incidI)) %>%
            mutate(cum_death=cumsum(incidD)) %>%
            mutate(cum_confirmed=cumsum(incidC)) %>%
            ungroup()%>%
            filter(time>=opt$start_date & time<=opt$end_date) %>%
            group_by(time) %>%
            summarize(hosp_curr=sum(hosp_curr),
                      cum_death=sum(cum_death),
                      hosp=sum(incidH),
                      death=sum(incidD),
                      infections=sum(incidI),
                      confirmed=sum(incidC),
                      cum_confirmed=sum(cum_confirmed),
                      cum_infections=sum(cum_infections)) %>%
            ungroup()
    }
} else {
     post_proc <- function(x, opt) {

        if (!("incidC" %in% names(x))) {
          x$incidC <- as.numeric(NA)
        }
        x%>%
            group_by(geoid) %>%
            mutate(cum_infections=cumsum(incidI)) %>%
            mutate(cum_death=cumsum(incidD)) %>%
            mutate(cum_confirmed=cumsum(incidC)) %>%
            ungroup()%>%
            filter(time>=opt$start_date & time<=opt$end_date) %>%
            group_by(time) %>%
            summarize(hosp_curr=sum(hosp_curr),
                      cum_death=sum(cum_death),
                      hosp=sum(incidH),
                      death=sum(incidD),
                      infections=sum(incidI),
                      confirmed=sum(incidC),
                      cum_confirmed=sum(cum_confirmed),
                      cum_infections=sum(cum_infections)) %>%
            ungroup()%>%
            mutate(week=lubridate::epiweek(time))%>%
            group_by(week)%>%
            summarize(hosp_curr=mean(hosp_curr),
                      cum_death=max(cum_death),
                      hosp=sum(hosp),
                      death=sum(death),
                      infections=sum(infections),
                      cum_infections=max(cum_infections),
                      confirmed=sum(confirmed),
                      cum_confirmed=max(cum_confirmed),
                      time = max(time))%>%
            ungroup()
                      
                      
    }
    
}

##Run over scenarios and death rates as appropriate. Note that
##Final results will average accross whatever is included
res_all <-list()
for (i in 1:length(scenarios)) {
    scenario_dir = scenarios[i]
    res_all[[i]] <- report.generation::load_hosp_sims_filtered(scenario_dir,
                                                                 name_filter = opt$name_filter,
                                                                 num_files = opt$num_simulations,
                                                                 post_process = post_proc,
                                                                 opt=opt)%>%
        mutate(scenario=scenarios[i])


}

##Put in one data frame
res_all<-dplyr::bind_rows(res_all)

##deregister backend
parallel::stopCluster(cl)


## Extract quantiles
tmp_col <- function(x, col) {
    x%>%
        group_by(time) %>%
        summarize(x=list(enframe(c(quantile(!!sym(col), probs=c(0.01, 0.025,
                                                                seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                                   mean=mean(!!sym(col))),
                                 "quantile",col))) %>%
        unnest(x)

}


to_save <- inner_join(tmp_col(res_all,"hosp_curr"),
                         tmp_col(res_all,"cum_death"))%>%
    inner_join(tmp_col(res_all,"death"))%>%
    inner_join(tmp_col(res_all,"infections"))%>%
    inner_join(tmp_col(res_all,"cum_infections"))%>%
    inner_join(tmp_col(res_all,"hosp"))

if(!(any(is.na(res_all$confirmed)))) {
    to_save <- to_save %>%
    inner_join(tmp_col(res_all,"confirmed"))%>%
    inner_join(tmp_col(res_all,"cum_confirmed"))
}

suppressWarnings(dir.create(dirname(opt$outfile), recursive=TRUE))
write_csv(to_save, path=opt$outfile)





