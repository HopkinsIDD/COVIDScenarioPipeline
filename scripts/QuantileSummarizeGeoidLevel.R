
# XXX: this branch was in a transitory state before packrat, so these packages need to be installed to run this script:
# install.packages("tdigest")
#
# TODO: update dependencies

PROBS = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

suppressMessages({
    library(optparse, quietly=TRUE)
    library(tidyverse, quietly=TRUE)
    library(tdigest)
    library(scales)
    library(parallel)
    library(doParallel)
    library(data.table)
})

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
arguments <- parse_args(opt_parser, positional_arguments=c(1,Inf))
opt <- arguments$options

scenarios <- arguments$args

if(is.null(opt$outfile)) {
    stop("outfile must be specified")
}

config <- covidcommon::load_config(opt$c)

if(length(scenarios) == 0) {
  setup_name <- config$spatial_setup$setup_name
  scenarios <- config$interventions$scenarios %>% lapply(function(x) { paste0(setup_name,"_",x)}) %>% unlist()
}

if(opt$num_simulations == -1) {
  opt$num_simulations <- config$nsimulations
}

if(is.null(opt$geodata)) {
  if(length(config) > 0) {
    opt$geodata <- file.path(config$spatial_setup$base_path, config$spatial_setup$geodata)
  } else {
    opt$geodata <- "data/geodata.csv"
  }
}

cl = makeCluster(opt$jobs, outfile="")
doParallel::registerDoParallel(cl)

##Load the geodata file
suppressMessages(geodata <- readr::read_csv(opt$geodata))

##Convert times to date objects
if (is.null(opt$start_date)) {
  opt$start_date <- as.Date(config$start_date)
}
opt$start_date <- as.Date(opt$start_date)

if (is.null(opt$end_date)) {
  opt$end_date <- config$end_date
}
opt$end_date <- as.Date(opt$end_date)

post_proc <- function(x, geodata, opt) {
    if (!("incidC" %in% names(x))) {
        x$incidC <- as.numeric(NA)
    }
    x %>%
        group_by(geoid) %>%
        mutate(cum_infections=cumsum(incidI)) %>%
        mutate(cum_death=cumsum(incidD)) %>%
        mutate(cum_confirmed=cumsum(incidC)) %>%
        ungroup() %>%
        filter(time >= opt$start_date & time <= opt$end_date) %>%
        rename(infections=incidI, death=incidD, hosp=incidH, confirmed=incidC)
}

res_geoid <- data.table::rbindlist(purrr::pmap(data.frame(scenario=scenarios), function(scenario) { 
    report.generation::load_hosp_sims_filtered(scenario,
                                               name_filter=opt$name_filter,
                                               num_files=opt$num_simulations,
                                               post_process=post_proc,
                                               geodata=geodata,
                                               opt=opt) 
}))

q <- function(col) {
  if(all(is.na(col))) {
    return(NA)
  }
  # if col is empty, tquantile fails; in that case, return what quantile() would (all 0's)
  tryCatch(tquantile(tdigest(col), PROBS), error = function(e) { quantile(col, PROBS) })
}

res_split <- split(res_geoid, res_geoid$time)
to_save_geo <- foreach(r_split=res_split, .combine=rbind, .packages="data.table", .inorder=FALSE, .multicombine=TRUE, .verbose=TRUE) %dopar% {
  stopifnot(is.data.table(r_split))
  r_split[, .(quantile=scales::percent(PROBS),
              hosp_curr=q(hosp_curr),
              cum_death=q(cum_death),
              death=q(death),
              infections=q(infections),
              cum_infections=q(cum_infections),
              cum_confirmed=q(cum_confirmed),
              confirmed=q(confirmed),
              hosp=q(hosp)), by=list(time, geoid)]
}

if(all(is.na(to_save_geo$confirmed))) {
  to_save_geo$confirmed <- NULL
  to_save_geo$cum_confirmed <- NULL
}

data.table::fwrite(to_save_geo, file=opt$outfile)

stopCluster(cl)
