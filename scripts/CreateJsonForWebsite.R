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
    
    make_option(c("-o","--outdir"), type="character", default=NULL, help="file to save output"),
    make_option("--start_date", type="character", default=NULL, help="earliest date to include"),
    make_option("--end_date",  type="character", default=NULL, help="latest date to include"),
    make_option(c("--week","-w"), action="store_true", default=FALSE, help="Aggregate to epi week")
)
opt_parser <- OptionParser(option_list = option_list, usage="%prog [options] [one or more scenarios]")

opt_parser <- OptionParser(option_list = option_list,
                           usage="%prog [options] [directory to be used]")
arguments <- parse_args(opt_parser, positional_arguments=c(0,Inf))
opt <- arguments$options

if(is.null(opt$outdir)) {
  opt$outdir <- tempdir()
}
if(!dir.exists(opt$outdir)){
  dir.create(opt$outdir,recursive=TRUE)
}

directory <- arguments$args
if(length(directory) == 0){
  directory <- "model_output"
}

if(opt$death_filter == "") {
  warning("death_filter is empty. Did you mean to filter by death rate?")
}

config <- covidcommon::load_config(opt$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

##Convert times to date objects
if (is.null(opt$start_date)) {
  opt$start_date <- config$start_date
}
opt$start_date <- lubridate::ymd(opt$start_date)

if (is.null(opt$end_date)) {
  opt$end_date <- config$end_date
}
opt$end_date <- lubridate::ymd(opt$end_date)

if(opt$week) {
    stop("Weekly aggregation not yet implemented")
}

##Loading the results is the same no matter what
tic("Loaded data")
##Load the data
all_values <- rlang::syms(c("incidC","incidH","incidD"))
res<- arrow::open_dataset(sprintf("%s/hosp",directory), 
                          partitioning =c("location", 
                                          "scenario", 
                                          "death_rate", 
                                          "date",
                                          "lik_type", 
                                          "is_final", 
                                          "sim_id"))%>%
  filter(lik_type == 'global', is_final == 'final') %>%
  select(time, geoid, death_rate, !!!all_values, sim_id)%>%
  filter(time>=opt$start_date& time<=opt$end_date) %>%
  collect() %>%
  mutate(time=as.Date(time,tz='UTC'),slot = sapply(strsplit(sim_id,split='[.]'),function(x){as.integer(x[[1]])})) %>%
  select(-sim_id)

tic("Processing geoids")
tmp <- res %>%
  tidyr::pivot_longer(as.character(all_values), names_to = 'outcome') %>%
  group_by(geoid) %>%
  group_map(function(.x1,.y1){ 
    rc1 <- group_map(group_by(.x1,death_rate),function(.x2,.y2){
      rc2 <- group_map(group_by(.x2,outcome),function(.x3,.y3){
        rc3 <- group_map(group_by(.x3, slot),function(.x4,.y4){
          rc4 <- list(name = .y4$slot, max = max(.x4$value), over = FALSE,  r0 = 1, vals = .x4$value)
          return(rc4)
        })
        rc3 <- setNames(list(rc3),.y3$outcome)
        return(rc3)
      })
      rc2 <- unlist(rc2,recursive=FALSE)
      rc2 <- setNames(list(rc2),.y2$death_rate)
      return(rc2)
    })
   rc1 <- unlist(rc1,recursive=FALSE)
   filename <- paste0(opt$outdir,'/',.y1$geoid,".json")
   ofp <- file(filename)
   writeLines(rjson::toJSON(rc1),ofp)
   close(ofp)
   NULL
 })
toc()
