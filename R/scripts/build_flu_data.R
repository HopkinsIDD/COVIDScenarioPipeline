

# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(readr)
library(lubridate)


option_list = list(
    optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
    optparse::make_option(c("-p", "--path"), action="store", default=Sys.getenv("COVID_PATH", "COVIDScenarioPipeline"), type='character', help="path to the COVIDScenarioPipeline directory"),
    optparse::make_option(c("-w", "--wide_form"), action="store",default=FALSE,type='logical',help="Whether to generate the old wide format mobility or the new long format")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config <- covidcommon::load_config(opt$c)
if (length(config) == 0) {
    stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

outdir <- config$spatial_setup$base_path
filterUSPS <- config$spatial_setup$modeled_states
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)





# Pull Resources and Source from FluSight Github -------------------

# create needed directories
dir.create("data-locations")
dir.create("data-truth")

# get locations file
download.file("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv",
              "data-locations/locations.csv")

# source function and pull weekly hospitalizations from HHS
source("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-truth/get_truth.R")

# Pull daily hospitalizations for model run
us_data <- load_flu_hosp_data(temporal_resolution = 'daily', na.rm = TRUE)
locs <- read_csv(file.path(config$spatial_setup$base_path, config$spatial_setup$geodata))

us_data <- us_data %>% 
    filter(location != "US") %>%
    mutate(location = stringr::str_pad(location, width=5, side="right", pad="0")) %>%
    left_join(locs, by = c("location"="geoid")) %>%
    rename(FIPS = location, 
           incidH = value,
           source = USPS) %>%
    select(-location_name, -pop2019est)

# Filter to dates we care about for speed and space
end_date_ <- config$end_date_groundtruth
if (is.null(end_date_)){
    end_date_ <- config$end_date
}
us_data <- us_data %>%
    filter(date >= lubridate::as_date(config$start_date) & date <= lubridate::as_date(end_date_)) %>%
    filter(!is.na(source))

write_csv(us_data, config$filtering$data_path)








# PULL VARIANT DATA -------------------------------------------------------

variant_props_file <- config$seeding$variant_filename
adjust_for_variant <- !is.null(variant_props_file)

if (adjust_for_variant){
    
    # Variant Data (need to automate this data pull still)
    variant_data <- read_csv(file.path(config$spatial_setup$base_path, "variant/WHO_NREVSS_Clinical_Labs.csv"), skip = 1)
    
    # location data
    loc_data <- read_csv("data-locations/locations.csv")
    
    
    # CLEAN DATA
    
    variant_data <- variant_data %>%
        select(state = REGION, 
               week = WEEK,
               year = YEAR,
               FluA = `TOTAL A`,
               FluB = `TOTAL B`) %>%
        pivot_longer(cols = starts_with("Flu"),
                     names_to = "variant",
                     values_to = "n") %>%
        mutate(n = ifelse(n == "X", 0, n)) %>%
        mutate(n = as.integer(n)) %>%
        group_by(state, week, year) %>%
        mutate(prop = n / sum(n, na.rm=TRUE)) %>%
        mutate(prop_tot = sum(prop, na.rm=TRUE)) %>%
        ungroup() %>%
        mutate(prop = ifelse(prop_tot==0 & variant=="FluA", 1, prop)) %>%
        group_by(state, week, year) %>%
        mutate(prop_tot = sum(prop, na.rm=TRUE)) %>%
        mutate(prop = prop / sum(prop, na.rm = TRUE)) %>%
        ungroup() %>%
        select(-prop_tot, -n) %>%
        mutate(prop = ifelse(is.na(prop), 0, prop)) %>%
        filter(!is.na(week)) %>%
        mutate(week_end = as_date(MMWRweek::MMWRweek2Date(year, week))+1)
    
    match_data <- loc_data %>% 
        select(state = location_name,
               source = abbreviation) %>%
        expand_grid(week_end = seq.Date(min(variant_data$week_end), max(variant_data$week_end), 7),
                    variant = c("FluA","FluB")) %>%
        filter(state != "US")
    
    variant_data <- variant_data %>%
        full_join(match_data) %>%
        mutate(week = epiweek(week_end), year = epiyear(week_end)) %>%
        mutate(prop = ifelse(is.na(prop) & variant=="FluA", 1, prop)) %>%
        mutate(prop = ifelse(is.na(prop) & variant!="FluA", 0, prop))
    
    variant_data <- variant_data %>%
        expand_grid(day = 1:7) %>%
        mutate(date = as_date(MMWRweek::MMWRweek2Date(year, week, day))) %>%
        select(-c(week, year, day, week_end, state))
    
    write_csv(variant_data, variant_props_file)
}


# APPLY VARIANTS ----------------------------------------------------------


if (adjust_for_variant) {
    
    us_data <- read_csv(config$filtering$data_path)
    
    tryCatch({
        us_data <- covidcommon::do_variant_adjustment(us_data, variant_props_file)
        write_csv(us_data, config$filtering$data_path)
    }, error = function(e) {
        stop(paste0("Could not use variant file |", variant_props_file, 
                    "|, with error message", e$message))
    })
}



cat(paste0("Ground truth data saved\n", 
           "  -- file:      ", config$filtering$data_path,".\n",
           "  -- outcomes:  ", paste(grep("incid", colnames(us_data), value = TRUE), collapse = ", ")))


# END
