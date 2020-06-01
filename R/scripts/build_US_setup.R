library(dplyr)
library(tidyr)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-p", "--path"), action="store", default="COVIDScenarioPipeline", type='character', help="path to the COVIDScenarioPipeline directory"),
  optparse::make_option(c("-w", "--wide_form"), action="store",default=FALSE,type='logical',help="Whether to generate the old wide format mobility or the new long format")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opt$c)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

outdir <- config$spatial_setup$base_path
filterUSPS <- config$spatial_setup$modeled_states

commute_data <- readr::read_csv(paste(opt$p,"sample_data","united-states-commutes","commute_data.csv",sep='/'))
census_data <- readr::read_csv(paste(opt$p,"sample_data","united-states-commutes","census_tracts_2010.csv", sep = '/'))


census_data <- census_data %>%
  filter(USPS %in% filterUSPS) %>%
  select(USPS,GEOID,POP10) %>%
  mutate(GEOID = substr(GEOID,1,5))

name_changer <- setNames(
  unique(census_data$GEOID),
  unique(census_data$GEOID)
)
name_changer[grepl("^60",name_changer)] <- "60000"
name_changer[grepl("^66",name_changer)] <- "66000"
name_changer[grepl("^69",name_changer)] <- "69000"
name_changer[grepl("^72",name_changer)] <- "72000"
name_changer[grepl("^78",name_changer)] <- "78000"

census_data <- census_data %>%
  mutate(GEOID = name_changer[GEOID]) %>%
  group_by(GEOID) %>%
  summarize(USPS = unique(USPS),POP10 = sum(POP10)) %>%
  arrange(POP10)

commute_data <- commute_data %>%
  mutate(OFIPS = substr(OFIPS,1,5), DFIPS = substr(DFIPS,1,5)) %>%
  mutate(OFIPS = name_changer[OFIPS], DFIPS = name_changer[DFIPS]) %>%
  filter(OFIPS %in% census_data$GEOID, DFIPS %in% census_data$GEOID) %>%
  group_by(OFIPS,DFIPS) %>%
  summarize(FLOW = sum(FLOW)) %>%
  filter(OFIPS != DFIPS)

padding_table <- tibble(
  OFIPS = census_data$GEOID,
  DFIPS = census_data$GEOID,
  FLOW = 0
)

t_commute_table <- tibble(
  OFIPS = commute_data$DFIPS,
  DFIPS = commute_data$OFIPS,
  FLOW = commute_data$FLOW
)

rc <- padding_table %>% bind_rows(commute_data) %>% bind_rows(t_commute_table)
if(opt$w){
  rc <- rc %>%pivot_wider(OFIPS,names_from=DFIPS,values_from=FLOW, values_fill=c("FLOW"=0),values_fn = list(FLOW=sum))
}

print(outdir)
if(opt$w){
  if(!isTRUE(all(rc$OFIPS == census_data$GEOID))){
    stop("There was a problem generating the mobility matrix")
  }
  write.table(file = file.path(outdir,'mobility.txt'), as.matrix(rc[,-1]), row.names=FALSE, col.names = FALSE, sep = " ")
} else {
  names(rc) <- c("ori","dest","amount")
  rc <- rc[rc$ori != rc$dest,]
  write.csv(file = file.path(outdir,'mobility.csv'), rc, row.names=FALSE)
}
names(census_data) <- c("geoid","USPS","pop2010")
write.csv(file = file.path(outdir,'geodata.csv'), census_data,row.names=FALSE)
