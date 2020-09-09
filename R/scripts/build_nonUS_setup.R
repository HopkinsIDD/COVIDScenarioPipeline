##
# @file
# @brief Creates mobility and geodata for non-US location
#
# @details
#
# ## Configuration Items
#
# ```yaml
# spatial_setup:
#   base_path: <path to directory>
#   modeled_states: <list of country ISO3 codes> e.g. ZMB, BGD, CAN
#   mobility: <path to file relative to base_path> optional; default is 'mobility.csv'
#   geodata: <path to file relative to base_path> optional; default is 'geodata.csv'
#   popnodes: <string> optional; default is 'pop'
# 
#
# ## Input Data
#
# None
#
# ## Output Data
#
# * {spatial_setup::base_path}/{spatial_setup::mobility}
# * {spatial_setup::base_path}/{spatial_setup::geodata}
#

## @cond

library(dplyr)
library(tidyr)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default="config.yml", type='character', help="path to the config file"),
  optparse::make_option(c("-w", "--wide_form"), action="store",default=FALSE,type='logical',help="Whether to generate the old wide format mobility or the new long format"),
  optparse::make_option(c("-n", "--population"), action="store",default="population_data.csv",type='character',help="Name of the popultion data file"),
  optparse::make_option(c("-m", "--mobility"), action="store",default="mobility_data.csv",type='character',help="Name of the mobility data file")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opt$config)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

outdir <- config$spatial_setup$base_path
filterADMIN0 <- config$spatial_setup$modeled_states

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Read in needed data
commute_data <- readr::read_csv(file.path(config$spatial_setup$base_path, "geodata", opt$mobility)) %>% 
  mutate(OGEOID = as.character(OGEOID),
         DGEOID = as.character(DGEOID))
census_data <- readr::read_csv(file.path(config$spatial_setup$base_path, "geodata", opt$population)) %>%
  mutate(GEOID = as.character(GEOID))

# Filter if needed
if (!(is.null(filterADMIN0) || is.na(filterADMIN0))){
  census_data <- census_data %>%
    dplyr::filter(ADMIN0 %in% filterADMIN0)
}

census_data <- census_data %>%
  dplyr::select(ADMIN0,GEOID,ADMIN2,POP) %>%
  dplyr::group_by(GEOID,ADMIN2) %>%
  dplyr::summarize(ADMIN0 = unique(ADMIN0), POP = sum(POP)) %>%
  dplyr::arrange(POP)

commute_data <- commute_data %>%
  dplyr::filter(OGEOID %in% census_data$GEOID, DGEOID %in% census_data$GEOID) %>%
  dplyr::group_by(OGEOID, DGEOID) %>%
  dplyr::summarize(FLOW = sum(FLOW)) %>%
  dplyr::filter(OGEOID != DGEOID)

padding_table <- tibble::tibble(
  OGEOID = census_data$GEOID,
  DGEOID = census_data$GEOID,
  FLOW = 0
)

t_commute_table <- tibble::tibble(
  OGEOID = commute_data$DGEOID,
  DGEOID = commute_data$OGEOID,
  FLOW = commute_data$FLOW
)

rc <- padding_table %>% 
  dplyr::bind_rows(commute_data) %>% 
  dplyr::bind_rows(t_commute_table)

# Make wide if specified
if(opt$w){
  rc <- rc %>% tidyr::pivot_wider(OGEOID, names_from=DGEOID, values_from=FLOW, values_fill=c("FLOW"=0), values_fn = list(FLOW=sum))
}


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
names(census_data) <- c("geoid","admin2","admin0","pop")
write.csv(file = file.path(outdir,'geodata.csv'), census_data,row.names=FALSE)

print("Census Data Check (up to 6 rows)")
print(head(census_data))
print("Commute Data Check (up to 6 rows)")
print(head(commute_data))

print(paste0("mobility.csv/.txt and geodata.csv saved to: ", outdir))


