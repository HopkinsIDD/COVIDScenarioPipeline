
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

## Paerse the
arguments <- parse_args(opt_parser, positional_arguments=TRUE)
opt <- arguments$options
scenarios <- arguments$args

if (is.null(opt$outfile)) {
    stop("outfile must be specified")
}

cl = makeCluster(opt$jobs, outfile="")
doParallel::registerDoParallel(cl)

config <- covidcommon::load_config(opt$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

##Load the geodata file
suppressMessages(geodata <- readr::read_csv(paste0(config$spatial_setup$base_path,"/",config$spatial_setup$geodata)))


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
    x %>%
        group_by(geoid) %>%
        mutate(cum_infections=cumsum(incidI)) %>%
        mutate(cum_death=cumsum(incidD)) %>%
        ungroup() %>%
        filter(time >= opt$start_date & time <= opt$end_date) %>%
        rename(infections=incidI, death=incidD, hosp=incidH)
}

setup_name <- config$spatial_setup$setup_name
scenarios <- scenarios %>% lapply(function(x) { paste0(setup_name,"_",x)}) %>% unlist()
res_geoid <- data.table::rbindlist(purrr::pmap(data.frame(scenario=scenarios), function(scenario) { 
    report.generation::load_hosp_sims_filtered(scenario,
                                               name_filter=opt$name_filter,
                                               num_files=opt$num_simulations,
                                               post_process=post_proc,
                                               geodata=geodata,
                                               opt=opt) 
}))

q <- function(col) {
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
              hosp=q(hosp)), by=list(time, geoid)]
}
data.table::fwrite(to_save_geo, file=opt$outfile)

stopCluster(cl)
