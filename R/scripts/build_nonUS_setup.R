library(dplyr)
library(tidyr)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default="config.yml", type='character', help="path to the config file"),
  optparse::make_option(c("-p", "--path"), action="store", default=".", type='character', help="path to the spatial repository directory"),
  optparse::make_option(c("-w", "--wide_form"), action="store",default=FALSE,type='logical',help="Whether to generate the old wide format mobility or the new long format"),
  optparse::make_option(c("-n", "--population"), action="store",default="population_data.csv",type='character',help="Name of the popultion data file"),
  optparse::make_option(c("-m", "--mobility"), action="store",default="mobility_data.csv",type='character',help="Name of the mobility data file")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opt$config)
if (is.na(config)) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

outdir <- config$spatial_setup$base_path
filterADMIN1 <- config$spatial_setup$modeled_states

commute_data <- readr::read_csv(file.path(opt$path,"data","geodata", opt$mobility))
census_data <- readr::read_csv(file.path(opt$path,"data","geodata", opt$population))


census_data <- census_data %>%
  dplyr::filter(ADMIN1 %in% filterADMIN1) %>%
  dplyr::select(ADMIN1,GEOID,POP) #%>%  mutate(GEOID = substr(GEOID,1,5))

# name_changer <- setNames(
#   unique(census_data$GEOID),
#   unique(census_data$GEOID)
# )
# name_changer[grepl("^60",name_changer)] <- "60000"
# name_changer[grepl("^66",name_changer)] <- "66000"
# name_changer[grepl("^69",name_changer)] <- "69000"
# name_changer[grepl("^72",name_changer)] <- "72000"
# name_changer[grepl("^78",name_changer)] <- "78000"

census_data <- census_data %>%
  #mutate(GEOID = name_changer[GEOID]) %>%
  group_by(GEOID) %>%
  summarize(ADMIN1 = unique(ADMIN1), POP = sum(POP)) %>%
  arrange(POP)

commute_data <- commute_data %>%
  #mutate(OGEOID = substr(OGEOID,1,5), DGEOID = substr(DGEOID,1,5)) %>%
  #mutate(OGEOID = name_changer[OGEOID], DGEOID = name_changer[DGEOID]) %>%
  filter(OGEOID %in% census_data$GEOID, DGEOID %in% census_data$GEOID) %>%
  group_by(OGEOID, DGEOID) %>%
  summarize(FLOW = sum(FLOW)) %>%
  filter(OGEOID != DGEOID)

padding_table <- tibble(
  OGEOID = census_data$GEOID,
  DGEOID = census_data$GEOID,
  FLOW = 0
)

t_commute_table <- tibble(
  OGEOID = commute_data$DGEOID,
  DGEOID = commute_data$OGEOID,
  FLOW = commute_data$FLOW
)

rc <- padding_table %>% bind_rows(commute_data) %>% bind_rows(t_commute_table)
if(opt$w){
  rc <- rc %>% pivot_wider(OGEOID, names_from=DGEOID, values_from=FLOW, values_fill=c("FLOW"=0), values_fn = list(FLOW=sum))
}

print(outdir)
if(opt$w){
  if(!isTRUE(all(rc$OGEOID == census_data$GEOID))){
    stop("There was a problem generating the mobility matrix")
  }
  write.table(file = file.path(outdir,'mobility.txt'), as.matrix(rc[,-1]), row.names=FALSE, col.names = FALSE, sep = " ")
} else {
  names(rc) <- c("ori","dest","amount")
  rc <- rc[rc$ori != rc$dest,]
  write.csv(file = file.path(outdir,'mobility.csv'), rc, row.names=FALSE)
}

# Save population geodata
names(census_data) <- c("geoid","admin1","pop")
write.csv(file = file.path(outdir,'geodata.csv'), census_data,row.names=FALSE)

