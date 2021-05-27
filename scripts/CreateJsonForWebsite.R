options(error=quit)
## First parse the options to the scripts
#suppressMessages({
library(optparse, quietly=TRUE)
library(tidyverse, quietly=TRUE)
library(parallel)
library(tictoc)
#})


tic()
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
  opt$outdir <- "json_output"
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
tic()
##Load the data
all_values <- rlang::syms(c("incidC","incidH","incidD"))
res <- arrow::open_dataset(sprintf("%s/hosp",directory), 
                           partitioning =c("location", 
                                           "scenario", 
                                           "death_rate", 
                                           "run_id",
                                           "lik_type", 
                                           "is_final", 
                                           "sim_id"))%>%
  filter(lik_type == 'global', is_final == 'final') %>%
  select(time, geoid, scenario, death_rate, run_id, !!!all_values, sim_id)%>%
  filter(time>=opt$start_date& time<=opt$end_date) %>%
  collect() %>%
  mutate(
    scenario = gsub("--*","-",gsub("[:.?]","-",paste(run_id,scenario,sep='-'))),
    time=as.Date(time,tz='UTC'),
    slot = sapply(strsplit(sim_id,split='[.]'),function(x){as.integer(x[[1]])})
  ) %>%
  select(-sim_id,-run_id) %>%
  tidyr::pivot_longer(as.character(all_values), names_to = 'outcome') %>%
  mutate(state = gsub('...$','',geoid)) %>%
  split(.$state)

for(state in names(res)){
  print(state)
  tic()
  res[[state]] <- res[[state]] %>%
    group_by(time,scenario,slot,state,death_rate,outcome) %>%
    summarize(value = sum(value), geoid = state[[1]]) %>%
    bind_rows(res[[state]])
  toc()
}
toc()

tic()
parameters <- arrow::open_dataset(sprintf("%s/spar",directory), 
                           partitioning =c("location", 
                                           "scenario", 
                                           "death_rate", 
                                           "run_id",
                                           "lik_type", 
                                           "is_final", 
                                           "sim_id"))%>%
  filter(lik_type == 'global', is_final == 'final',parameter == 'R0') %>%
  select(value, scenario, death_rate, run_id, sim_id)%>%
  collect() %>%
  mutate(
    scenario = gsub("--*","-",gsub("[:.?]","-",paste(run_id,scenario,sep='-'))),
    slot = sapply(strsplit(sim_id,split='[.]'),function(x){as.integer(x[[1]])})
  ) %>%
  select(-sim_id,-run_id)

parameter_offsets <- arrow::open_dataset(sprintf("%s/snpi",directory), 
                           partitioning =c("location", 
                                           "scenario", 
                                           "death_rate", 
                                           "run_id",
                                           "lik_type", 
                                           "is_final", 
                                           "sim_id"))%>%
  filter(lik_type == 'global', is_final == 'final',parameter == 'r0', npi_name == 'local_variance') %>%
  select(geoid, reduction, scenario, death_rate, run_id, sim_id)%>%
  collect() %>%
  mutate(
    scenario = gsub("--*","-",gsub("[:.?]","-",paste(run_id,scenario,sep='-'))),
    slot = sapply(strsplit(sim_id,split='[.]'),function(x){as.integer(x[[1]])})
  ) %>%
  select(-sim_id,-run_id)

parameter_offsets <- parameter_offsets %>%
  mutate(geoid = gsub('...$','',geoid)) %>%
  group_by(geoid,scenario,death_rate,slot) %>%
  summarize(reduction = 0) %>%
  bind_rows(parameter_offsets)

parameters <- parameters %>%
  left_join(parameter_offsets) %>%
  mutate(value = ifelse(is.na(reduction),value,value * (1 - reduction)) ) %>%
  select(-reduction)
toc()

hash <- function(...){paste(...,collapse = '-',sep = '::')}

json_parameters <- rlang::env()
tic()
parameters %>%
  group_by(geoid) %>%
  group_map(function(.x,.y,.env = json_parameters){
    .env[[.y$geoid]] <- rlang::env()
    rc <- group_map(group_by(.x,scenario),function(.x1,.y1,.env1 = .env[[.y$geoid]]){
      .env1[[.y1$scenario]] <- rlang::env()
      rc1 <- group_map(group_by(.x1,death_rate),function(.x2,.y2, .env2 = .env1[[.y1$scenario]]){
        .env2[[.y2$death_rate]] <- rlang::env()
        rc2 <- group_map(group_by(.x2,slot),function(.x3,.y3,.env3 = .env2[[.y2$death_rate]]){
          .env3[[as.character(.y3$slot)]] <- .x3$value
          json_parameters[[hash(.y$geoid,.y1$scenario,.y2$death_rate,.y3$slot)]] <- .x3$value
          return()
        })
        return()
      })
      return()
    })
    return()
  })
toc()

tic()
i <- 0
for(state in names(res)){
  tic()
  purrr::map(split(res[[state]],res[[state]]$geoid),function(.x){
    rc <- purrr::map(split(.x,.x$scenario),function(.x1){
      rc1 <- purrr::map(split(.x1,.x1$death_rate),function(.x2){
        rc2 <- purrr::map(split(.x2,.x2$outcome),function(.x3){
          rc3 <- purrr::map(split(.x3,.x3$slot),function(.x4){
            i <<- i + 1
            rc4 <- list(
              name = i,
              max = max(.x4$value),
              vals = .x4$value,
              over = FALSE,
              r0 = round(100 * json_parameters[[.x$geoid[[1]] ]][[.x1$scenario[[1]] ]][[.x2$death_rate[[1]] ]][[as.character(.x4$slot[[1]]) ]]) / 100
            )
            return(rc4)
          })
          rc3 <- unname(rc3)
          return(rc3)
        })
        return(rc2)
      })
      rc1$dates <- format(sort(unique(.x1$time)),"%Y-%m-%d")
      return(rc1)
    })
    filename <- paste0(opt$outdir,'/', .x$geoid[[1]], ".json")
    ofp <- file(filename)
    writeLines(rjson::toJSON(rc),ofp)
    close(ofp)
  })
  toc()

}
RProf(NULL)
a <- summaryRprof()
toc()

tic()
tmp <- lapply(
  res,
  function(.x){
    tic()
    rc <- purrr::map(split(.x,.x$geoid),function(.x1){
        rc1 <- purrr::map(split(.x1,.x1$scenario),function(.x2){
          rc2 <- purrr::map(split(.x2,.x2$outcome),function(.x3){
            rc3 <- summarize(group_by(arrange(.x3,time),time),value = median(value),.groups="drop")[['value']]
            return(rc3)
          })
          return(rc2)
        })
        return(rc1)
      })
    toc()
    return(rc)
  }
)
filename <- paste0(opt$outdir,'/',"statsForMap.json")
ofp <- file(filename)
writeLines(rjson::toJSON(tmp),ofp)
close(ofp)
toc()

toc()
NULL

all_geoids <- unlist(lapply(res,function(x){unique(x$geoid)}))
tic()
actuals <- inference::get_ground_truth(tempfile(),all_geoids,fips_column_name="FIPS",start_date = lubridate::ymd('2020-01-01'), end_date = lubridate::now())
actuals <- actuals %>%
  mutate(FIPS = gsub("...$","",FIPS)) %>%
  group_by(FIPS,date) %>%
  summarize(
    confirmed_incid = sum(confirmed_incid),
    death_incid = sum(death_incid)
  ) %>%
  bind_rows(actuals) %>%
  select(-cumConfirmed, -cumDeaths, -source) %>%
  ungroup()
tmp <- actuals %>%
  rename(incidC = confirmed_incid, incidD = death_incid) %>%
  tidyr::pivot_longer(c(incidC,incidD),"outcome","value") %>%
  split(.$FIPS) %>%
  lapply(function(x){
    rc <- lapply(
      split(x,x$outcome),
      function(.x2){
        .x2$date <- format(.x2$date,format="%Y-%m-%d")
        unname(split(.x2[,c("date","value")],.x2$date))
      }
    )
    filename <- paste0(opt$outdir,'/',x$FIPS[[1]],"_actuals.json")
    ofp <- file(filename)
    writeLines(rjson::toJSON(rc),ofp)
    close(ofp)
  })
toc()

