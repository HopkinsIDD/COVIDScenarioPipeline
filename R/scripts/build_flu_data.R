

# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(readr)
library(lubridate)

#check for cdc fluview package
cdcflueview_installed <- require(cdcfluview)
if (!cdcflueview_installed){
    remotes::install_github("hrbrmstr/cdcfluview")
}


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
    # variant_data <- read_csv(file.path(config$spatial_setup$base_path, "variant/WHO_NREVSS_Clinical_Labs.csv"), skip = 1)
    variant_data <- cdcfluview::who_nrevss(region="state", years = 2022)$clinical_labs
    
    # location data
    loc_data <- read_csv("data-locations/locations.csv")
    
    
    # CLEAN DATA
    
    variant_data <- variant_data %>%
        select(state = region,
               week = week,
               year = year,
               FluA = total_a,
               FluB = total_b) %>%
      # select(state = REGION, 
      #        week = WEEK,
      #        year = YEAR,
      #        FluA = `TOTAL A`,
      #        FluB = `TOTAL B`) %>%
        pivot_longer(cols = starts_with("Flu"),
                     names_to = "variant",
                     values_to = "n") %>%
        # mutate(n = ifelse(n == "X", 0, n)) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
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
        mutate(week_end = as_date(MMWRweek::MMWRweek2Date(year, week, 7)))
    
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
    
    # Extend to dates of groundtruth
    var_max_dates <- variant_data %>% 
        group_by(source, state) %>%
        filter(week_end == max(week_end)) %>%
        ungroup() %>%
        mutate(max_date = as_date(end_date_)) %>%
        mutate(weeks_missing = as.integer(max_date - week_end)/7) %>%
        rowwise() %>%
        mutate(weeks_missing = paste((seq(from = 1, to=weeks_missing, 1)*7 + week_end), collapse = ",")) %>%
        # mutate(weeks_missing = list(as_date(seq(from = 1, to=weeks_missing, 1)*7 + week_end))) #%>%
        ungroup()
    var_max_dates <- var_max_dates %>%
        rename(max_current = week_end) %>%
        mutate(week_end = strsplit(as.character(weeks_missing), ",")) %>% 
        unnest(week_end) %>%
        select(state, week, year, variant, prop, week_end, source) %>%
        mutate(week_end = as_date(week_end))
        
    variant_data <- variant_data %>%
        bind_rows(var_max_dates)
    
    variant_data <- variant_data %>%
        mutate(week = epiweek(week_end), year = epiyear(week_end))
    
    variant_data <- variant_data %>%
        expand_grid(day = 1:7) %>%
        mutate(date = as_date(MMWRweek::MMWRweek2Date(year, week, day))) %>%
        select(-c(week, year, day, state))
    
    variant_data <- variant_data %>%
        select(-week_end)
    
    variant_data <- variant_data %>% 
            filter(date >= as_date(config$start_date) & date <= as_date(config$end_date_groundtruth))
    
    write_csv(variant_data, variant_props_file)
}


# APPLY VARIANTS ----------------------------------------------------------


if (adjust_for_variant) {
    
    us_data <- read_csv(config$filtering$data_path)
    
    tryCatch({
        us_data <- covidcommon::do_variant_adjustment(us_data, variant_props_file)
        us_data <- us_data %>% 
            filter(date >= as_date(config$start_date) & date <= as_date(config$end_date_groundtruth))
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
