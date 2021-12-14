##
# @file
# @brief Creates a seeding file
#
# @details
#
#
# ## Configuration Items
#
# ```yaml
# start_date: <date>
# end_date: <date>
#
# spatial_setup:
#   setup_name: <string>
#   base_path: <path to directory>
#   geodata: <path to file>
#   nodenames: <string>
#
# seeding:
#   lambda_file: <path to file>

# ```
#
# ## Input Data
#
# * <b>{spatial_setup::base_path}/{spatial_setup::geodata}</b> is a csv with column {spatial_setup::nodenames} that denotes the geoids
#
# ## Output Data
#
# * <b>data/case_data/USAFacts_case_data.csv</b> is the case csv downloaded from USAFacts
# * <b>data/case_data/USAFacts_death_data.csv</b> is the death csv downloaded from USAFacts
# * <b>{seeding::lambda_file}</b>: filter file
#

## @cond

library(covidcommon)
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)

option_list <- list(
  optparse::make_option(
    c("-c", "--config"),
    action = "store",
    default = Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")),
    type = "character",
    help = "path to the config file"
  )
)

opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

print(paste0("Using config file: ", opt$config))
config <- covidcommon::load_config(opt$config)
if (length(config) == 0) {
  stop("no configuration found -- please set CONFIG_PATH environment variable or use the -c command flag")
}

if (is.null(config$spatial_setup$us_model)) {
  config$spatial_setup$us_model <- FALSE
  if ("modeled_states" %in% names(config$spatial_setup)) {
    config$spatial_setup$us_model <- TRUE
  }
}

is_US_run <- config$spatial_setup$us_model
seed_variants <- "variant_filename" %in% names(config$seeding)


## backwards compatibility with configs that don't have filtering$gt_source
## parameter will use the previous default data source (USA Facts)
if (is.null(config$filtering$gt_source)) {
  if (is_US_run) {
    gt_source <- "usafacts"
  } else {
    gt_source <- NULL
  }
} else{
  gt_source <- config$filtering$gt_source
}
if (is.null(config$seeding$delay_incidC)) {
  config$seeding$delay_incidC <- 5
}
if (is.null(config$seeding$ratio_incidC)) {
  config$seeding$ratio_incidC <- 10
}

if (!is.null(gt_source)) {
  cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = "US county")
  print(paste("Successfully pulled", gt_source, "data for seeding."))
} else {
  data_path <- config$filtering$data_path
  if (is.null(data_path)) {
    data_path <- config$seeding$casedata_file
    if (is.null(data_path)) {
      stop(paste(
        "Please provide a ground truth file for non-us runs",
        "with no data source as filtering::data_path or seeding::casedata_file"
      ))
    }
  }
  cases_deaths <- read.csv(data_path)
  print(paste("Successfully loaded data from ", data_path, "for seeding."))
}

# Aggregation to state level if in config
if (is_US_run) {
  state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)
  if (state_level) {
    gt_scale <- "US state"
    cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale, incl_unass = TRUE)
  } else{
    gt_scale <- "US county"
    cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale)
  }
  cases_deaths <- cases_deaths %>%
    mutate(FIPS = stringr::str_pad(FIPS, width = 5, side = "right", pad = "0"))
}

if (seed_variants) {
  variant_data <- readr::read_csv(config$seeding$variant_filename)
  cases_deaths <- cases_deaths %>%
    left_join(variant_data) %>%
    mutate(incidI = incidI * prop) %>%
    select(-prop) %>%
    pivot_wider(names_from = variant, values_from = incidI)
}

## Check some data attributes:
## This is a hack:
if ("geoid" %in% names(cases_deaths)) {
  cases_deaths$FIPS <- cases_deaths$geoid
  warning("Changing FIPS name in seeding. This is a hack")
}
if ("date" %in% names(cases_deaths)) {
  cases_deaths$Update <- cases_deaths$date
  warning("Changing Update name in seeding. This is a hack")
}
obs_nodename <- config$spatial_setup$nodenames
required_column_names <- NULL

check_required_names <- function(df, cols, msg) {
  if (!all(cols %in% names(df))) {
    stop(msg)
  }
}

if ("compartments" %in% names(config[["seir"]])) {
  if (all(names(config$seeding$seeding_compartments) %in% names(cases_deaths))) {
    required_column_names <- c("FIPS", "Update", names(config$seeding$seeding_compartments))
    check_required_names(
      cases_deaths,
      required_column_names,
      paste(
        "To create the seeding, we require the following columns to exist in the case data",
        paste(required_column_names, collapse = ", ")
      )
    )
    incident_cases <- cases_deaths[, required_column_names] %>%
      tidyr::pivot_longer(!!names(config$seeding$seeding_compartments), names_to = "seeding_group") %>%
      dplyr::mutate(
        source_column = sapply(
          config$seeding$seeding_compartments[seeding_group],
          function(x){
            paste(x$source_compartment, collapse = "_")
          }
        ),
        destination_column = sapply(
          config$seeding$seeding_compartments[seeding_group],
          function(x){
            paste(x$destination_compartment, collapse = "_")
          }
        )
      ) %>%
      tidyr::separate(source_column, paste("source", names(config$seir$compartments), sep = "_")) %>%
      tidyr::separate(destination_column, paste("destination", names(config$seir$compartments), sep = "_"))
    required_column_names <- c("FIPS", "Update", "value", paste("source", names(config$seir$compartments), sep = "_"), paste("destination", names(config$seir$compartments), sep = "_"))
    incident_cases <- incident_cases[, required_column_names]
  } else if ("seeding_compartments" %in% names(config$seeding) ) {
    stop(paste(
      "Could not find all compartments.  Looking for",
      paste(names(config$seeding$seeding_compartments), collapse = ", "),
      "from selection",
      paste(names(cases_deaths), collapse = ", ")
    ))
  } else {
    stop("Please add a seeding_compartments section to the config")
  }
} else {
  required_column_names <- c("FIPS", "Update", "incidI")
  check_required_names(
    cases_deaths,
    required_column_names,
    paste(
      "To create the seeding, we require the following columns to exist in the case data",
      paste(required_column_names, collapse = ", ")
    )
  )
  incident_cases <- cases_deaths[, required_column_names] %>%
    tidyr::pivot_longer(cols = "incidI", names_to = "source_infection_stage", values_to = "value")
  incident_cases$destination_infection_stage <- "E"
  incident_cases$source_infection_stage <- "S"
  required_column_names <- c("FIPS", "Update", "value", "source_infection_stage", "destination_infection_stage")

  if ("parallel_structure" %in% names(config[["seir"]][["parameters"]])) {
    parallel_compartments <- config[["seir"]][["parameters"]][["parallel_structure"]][["compartments"]]
  } else {
    parallel_compartments <- setNames(NA, "unvaccinated")
  }
  incident_cases[["source_vaccination_stage"]] <- names(parallel_compartments)[[1]]
  incident_cases[["destination_vaccination_stage"]] <- names(parallel_compartments)[[1]]
  required_column_names <- c(required_column_names, "source_vaccination_stage", "destination_vaccination_stage")
}
print(required_column_names)
incident_cases <- incident_cases[, required_column_names]


all_times <- lubridate::ymd(config$start_date) +
  seq_len(lubridate::ymd(config$end_date) - lubridate::ymd(config$start_date))

geodata <- report.generation:::load_geodata_file(
  file.path(config$spatial_setup$base_path, config$spatial_setup$geodata),
  5,
  "0",
  TRUE
)

all_geoids <- geodata[[config$spatial_setup$nodenames]]


incident_cases <- incident_cases %>%
  dplyr::filter(FIPS %in% all_geoids) %>%
  dplyr::select(!!!required_column_names)

incident_cases[["Update"]] <- as.Date(incident_cases$Update)

if (is.null(config[["seeding"]][["seeding_inflation_ratio"]])) {
  config[["seeding"]][["seeding_inflation_ratio"]] <- 10
}
if (is.null(config[["seeding"]][["seeding_delay"]])) {
  config[["seeding"]][["seeding_delay"]] <- 5
}

grouping_columns <- required_column_names[!required_column_names %in% c("Update", "value")]
incident_cases <- incident_cases %>%
  dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
  dplyr::group_modify(function(.x, .y) {
    .x %>%
      dplyr::arrange(Update) %>%
      dplyr::filter(value > 0) %>%
      .[seq_len(min(nrow(.x), 5)), ] %>%
      dplyr::mutate(
        Update = Update - lubridate::days(config[["seeding"]][["seeding_delay"]]),
        value = config[["seeding"]][["seeding_inflation_ratio"]] * value + .05
      ) %>%
      return
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(!!!rlang::syms(required_column_names))

names(incident_cases)[1:3] <- c("place", "date", "amount")

incident_cases <- incident_cases %>%
  dplyr::filter(!is.na(amount) | !is.na(date))

lambda_dir <- dirname(config$seeding$lambda_file)
if (!dir.exists(lambda_dir)) {
  suppressWarnings(dir.create(lambda_dir, recursive = TRUE))
}



# Combine with population seeding for compartments (current hack to get population in)

if ("compartments" %in% names(config[["seir"]]) & "pop_seed_file" %in% names(config[["seeding"]])) {
  seeding_pop <- readr::read_csv(config$seeding$pop_seed_file)
  seeding_pop <- seeding_pop %>%
    dplyr::filter(place %in% all_geoids) %>%
    dplyr::select(!!!colnames(incident_cases))
    
  incident_cases <- incident_cases %>%
    dplyr::bind_rows(seeding_pop) %>% 
    dplyr::arrange(place, date)
}
  

# Limit seeding to on or after the config start date and before the config end date
incident_cases <- incident_cases %>% 
    dplyr::filter(date >= lubridate::as_date(config$start_date) & date <= lubridate::as_date(config$end_date))





# Save it

write.csv(
  incident_cases,
  file = file.path(config$seeding$lambda_file),
  row.names = FALSE
)

print(paste("Saved seeding to", config$seeding$lambda_file))

## @endcond
