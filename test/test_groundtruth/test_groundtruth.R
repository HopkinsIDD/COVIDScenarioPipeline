
# TEST GROUNDTRUTH ----------------------------------------------



# -- a lot of code comes from `filter_MC.R`
library(inference)
suppressMessages(library(readr))
suppressWarnings(suppressMessages(library(covidcommon)))
suppressMessages(library(magrittr))
options(warn = 1)
options(readr.num_columns = 0)


# load test config
opt$config <- "test/test_groundtruth/config.yml"
config = covidcommon::load_config(opt$config)
config$seeding$variant_filename <- "test/test_groundtruth/data/variant_props_long.csv"

##Load infromationon geographic locations from geodata file.
suppressMessages(geodata <- report.generation::load_geodata_file(
  paste(
    "test/test_groundtruth/", config$spatial_setup$base_path,
    config$spatial_setup$geodata, sep = "/"
  ),
  geoid_len=5 #Is this hardcode a good idea.
))
obs_nodename <- config$spatial_setup$nodenames





# TEST rawcovidata package (by Luke Mullany)  ----------------------------------------------


#data_path <- file.path("test/test_groundtruth/test_data", config$filtering$data_path)
data_path <- file.path("test/test_groundtruth/test_data/us_data_csselm.csv")
dir.create(file.path("test/test_groundtruth/data/test_data"), recursive = TRUE, showWarnings = FALSE)

config$filtering$gt_source <- "csse_lm"
opt$ground_truth_start <- ""
opt$ground_truth_end <- ""

# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)
    
## backwards compatibility with configs that don't have filtering$gt_source parameter will use the previous default data source (USA Facts)
if(is.null(config$filtering$gt_source)){
  gt_source <- "usafacts"
} else{
  gt_source <- config$filtering$gt_source
}

gt_scale <- ifelse(state_level, "US state", "US county")
fips_codes_ <- geodata[[obs_nodename]]

gt_start_date <- lubridate::ymd(config$start_date)
if (opt$ground_truth_start != "") {
  gt_start_date <- lubridate::ymd(opt$ground_truth_start)
} else if (!is.null(config$start_date_groundtruth)) {
  gt_start_date <- lubridate::ymd(config$start_date_groundtruth)
}
if (gt_start_date < lubridate::ymd(config$start_date)) {
  gt_start_date <- lubridate::ymd(config$start_date)
}

gt_end_date <- lubridate::ymd(config$end_date)
if (opt$ground_truth_end != "") {
  gt_end_date <- lubridate::ymd(opt$ground_truth_end)
} else if (!is.null(config$end_date_groundtruth)) {
  gt_end_date <- lubridate::ymd(config$end_date_groundtruth)
}
if (gt_end_date > lubridate::ymd(config$end_date)) {
  gt_end_date <- lubridate::ymd(config$end_date)
}


gt_prelim <- get_ground_truth_file(data_path = data_path, 
                                   cache = FALSE, 
                                   fix_negatives = TRUE,
                                   gt_source = gt_source, 
                                   gt_scale = gt_scale, 
                                   variant_filename = variant_filename)


obs <- inference::get_ground_truth(
          data_path = data_path,
          fips_codes = fips_codes_,
          fips_column_name = obs_nodename,
          start_date = gt_start_date,
          end_date = gt_end_date,
          cache = FALSE,
          gt_source = gt_source,
          gt_scale = gt_scale,
          fix_negatives = TRUE,
          variant_filename = config$seeding$variant_filename)













# TEST CSSE  ----------------------------------------------

#data_path <- file.path("test/test_groundtruth/test_data", config$filtering$data_path)
data_path <- file.path("test/test_groundtruth/test_data/us_data_csse.csv")
dir.create(file.path("test/test_groundtruth/data/test_data"), recursive = TRUE, showWarnings = FALSE)

config$filtering$gt_source <- "csse"
opt$ground_truth_start <- ""
opt$ground_truth_end <- "" 
    
# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)

## backwards compatibility with configs that don't have filtering$gt_source parameter will use the previous default data source (USA Facts)
if(is.null(config$filtering$gt_source)){
  gt_source <- "usafacts"
} else{
  gt_source <- config$filtering$gt_source
}

gt_scale <- ifelse(state_level, "US state", "US county")
fips_codes_ <- geodata[[obs_nodename]]

gt_start_date <- lubridate::ymd(config$start_date)
if (opt$ground_truth_start != "") {
  gt_start_date <- lubridate::ymd(opt$ground_truth_start)
} else if (!is.null(config$start_date_groundtruth)) {
  gt_start_date <- lubridate::ymd(config$start_date_groundtruth)
}
if (gt_start_date < lubridate::ymd(config$start_date)) {
  gt_start_date <- lubridate::ymd(config$start_date)
}

gt_end_date <- lubridate::ymd(config$end_date)
if (opt$ground_truth_end != "") {
  gt_end_date <- lubridate::ymd(opt$ground_truth_end)
} else if (!is.null(config$end_date_groundtruth)) {
  gt_end_date <- lubridate::ymd(config$end_date_groundtruth)
}
if (gt_end_date > lubridate::ymd(config$end_date)) {
  gt_end_date <- lubridate::ymd(config$end_date)
}


gt_prelim <- inference::get_ground_truth_file(data_path = data_path, 
                                   cache = TRUE, 
                                   gt_source = gt_source, 
                                   gt_scale = gt_scale, 
                                   variant_filename = config$seeding$variant_filename)

obs <- inference::get_ground_truth(
          data_path = data_path,
          fips_codes = fips_codes_,
          fips_column_name = obs_nodename,
          start_date = gt_start_date,
          end_date = gt_end_date,
          gt_source = gt_source,
          gt_scale = gt_scale,
          variant_filename = config$seeding$variant_filename)

geonames <- unique(obs[[obs_nodename]])
obs_csse <- obs









# TEST COVIDCAST  ----------------------------------------------


#data_path <- file.path("test/test_groundtruth/test_data", config$filtering$data_path)
data_path <- file.path("test/test_groundtruth/test_data/us_data_covidcast.csv")
dir.create(file.path("test/test_groundtruth/data/test_data"), recursive = TRUE, showWarnings = FALSE)

config$filtering$gt_source <- "covidcast"
opt$ground_truth_start <- ""
opt$ground_truth_end <- ""

# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)
    
## backwards compatibility with configs that don't have filtering$gt_source parameter will use the previous default data source (USA Facts)
if(is.null(config$filtering$gt_source)){
  gt_source <- "usafacts"
} else{
  gt_source <- config$filtering$gt_source
}

gt_scale <- ifelse(state_level, "US state", "US county")
fips_codes_ <- geodata[[obs_nodename]]

gt_start_date <- lubridate::ymd(config$start_date)
if (opt$ground_truth_start != "") {
  gt_start_date <- lubridate::ymd(opt$ground_truth_start)
} else if (!is.null(config$start_date_groundtruth)) {
  gt_start_date <- lubridate::ymd(config$start_date_groundtruth)
}
if (gt_start_date < lubridate::ymd(config$start_date)) {
  gt_start_date <- lubridate::ymd(config$start_date)
}

gt_end_date <- lubridate::ymd(config$end_date)
if (opt$ground_truth_end != "") {
  gt_end_date <- lubridate::ymd(opt$ground_truth_end)
} else if (!is.null(config$end_date_groundtruth)) {
  gt_end_date <- lubridate::ymd(config$end_date_groundtruth)
}
if (gt_end_date > lubridate::ymd(config$end_date)) {
  gt_end_date <- lubridate::ymd(config$end_date)
}


gt_prelim <- get_ground_truth_file(data_path = data_path, 
                                   cache = TRUE, 
                                   gt_source = gt_source, 
                                   gt_scale = gt_scale, 
                                   variant_filename = variant_filename)


obs <- inference::get_ground_truth(
          data_path = data_path,
          fips_codes = fips_codes_,
          fips_column_name = obs_nodename,
          start_date = gt_start_date,
          end_date = gt_end_date,
          gt_source = gt_source,
          gt_scale = gt_scale,
          variant_filename = config$seeding$variant_filename
)










# OTHER CHECKING CODE -----------------------------------------------------

# 
# state_dat_pre <- state_dat
# 
# 
# all.equal(state_dat_pre, state_dat)
# 
# check1 <- full_join(
#     group_by(state_dat_pre, source) %>% summarise(case = max(Confirmed, na.rm=TRUE)),
#     group_by(state_dat, source) %>% summarise(case2 = max(Confirmed, na.rm=TRUE))) %>%
#     mutate(diff = case2 - case)
# check2 <- full_join(
#     group_by(state_dat_pre, source) %>% summarise(death = max(Deaths, na.rm=TRUE)),
#     group_by(state_dat, source) %>% summarise(death2 = max(Deaths, na.rm=TRUE))) %>%
#     mutate(diff = death2 - death)
# 
# 
# check1 <- full_join(
#     group_by(state_dat_pre, source) %>% summarise(case = max(incidI, na.rm=TRUE)),
#     group_by(state_dat, source) %>% summarise(case2 = max(incidI, na.rm=TRUE))) %>%
#     mutate(diff = case2 - case)
# check2 <- full_join(
#     group_by(state_dat_pre, source) %>% summarise(death = max(incidDeath, na.rm=TRUE)),
#     group_by(state_dat, source) %>% summarise(death2 = max(incidDeath, na.rm=TRUE))) %>%
#     mutate(diff = death2 - death)



