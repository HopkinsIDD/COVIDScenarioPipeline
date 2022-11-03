



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




# PULL DATA ---------------------------------------------------------------

end_date_ <- config$end_date_groundtruth
if (is.null(end_date_)) end_date_ <- config$end_date

# ~ Pull Data from Covidcast -------------------

gt_source <- "covidcast"
gt_scale <- "US state"
us_data <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale, 
                                                    incl_unass = TRUE,
                                                    variables = c("Confirmed", "Deaths", "incidI", "incidDeath"),
                                                    adjust_for_variant = TRUE, 
                                                    variant_props_file = config$seeding$variant_filename)
us_data <- us_data %>%
    mutate(FIPS = stringr::str_pad(FIPS, width=5, side="right", pad="0")) %>% 
    filter(Update >= as_date(config$start_date) & Update <= as_date(end_date_))

# ~ Pull HHS hospitalization  -------------------

us_hosp <- covidcommon::get_hhsCMU_incidH_st_data()
us_hosp <- us_hosp %>%
    dplyr::select(-incidH_all) %>%
    rename(incidH = incidH_confirmed) %>%
    mutate(FIPS = stringr::str_pad(FIPS, width=5, side="right", pad="0")) %>% 
    filter(Update >= as_date(config$start_date) & Update <= as_date(end_date_))

# Apply variants
variant_props_file <- config$seeding$variant_filename
adjust_for_variant <- !is.null(variant_props_file)

head(read_csv(variant_props_file))

if (adjust_for_variant) {
    
    tryCatch({
        us_hosp <- covidcommon::do_variant_adjustment(us_hosp, variant_props_file)
    }, error = function(e) {
        stop(paste0("Could not use variant file |", variant_props_file, 
                    "|, with error message", e$message))
    })
}



# ~ Combine ---------------------------------------------------------------

us_data <- us_data %>% 
    full_join(us_hosp) %>%
    filter(source != "US", source != "USA") %>%
    mutate(FIPS = stringr::str_pad(FIPS, width=5, side="right", pad="0"))



# ~ Filter ----------------------------------------------------------------

# Filter to dates we care about for speed and space
us_data <- us_data %>%
    filter(Update >= lubridate::as_date(config$start_date) & Update <= lubridate::as_date(end_date_)) 

# Filter to states we care about 
locs <- config$spatial_setup$modeled_states
us_data <- us_data %>%
    filter(source %in% locs) %>%
    filter(!is.na(source)) %>%
    rename(date = Update)



# Save pre-variants
write_csv(us_data, config$filtering$data_path)



cat(paste0("Ground truth data saved\n", 
           "  -- file:      ", config$filtering$data_path,".\n",
           "  -- outcomes:  ", paste(grep("incid", colnames(us_data), value = TRUE), collapse = ", ")))


# END
