
# Test `filter_MC.R` ------------------------------------------------------



# SETUP -------------------------------------------------------------------

library(tidyverse)

Sys.setenv(CONFIG_PATH="test/test_groundtruth/config.yml")
Sys.getenv("CONFIG_PATH")

# from full_filter.R ------------------------------------------------------


suppressMessages(library(parallel))
suppressMessages(library(foreach))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
options(readr.num_columns = 0)

option_list = list(
    optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
    optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
    optparse::make_option(c("-s", "--scenarios"), action="store", default=Sys.getenv("COVID_SCENARIOS", 'all'), type='character', help="name of the intervention to run, or 'all' to run all of them"),
    optparse::make_option(c("-d", "--deathrates"), action="store", default=Sys.getenv("COVID_DEATHRATES", 'all'), type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
    optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", parallel::detectCores()), type='integer', help="Number of jobs to run in parallel"),
    optparse::make_option(c("-k", "--sims_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT", NA), type='integer', help = "Number of simulations to run per slot"),
    optparse::make_option(c("-n", "--slots"), action="store", default=Sys.getenv("COVID_NSIMULATIONS", as.numeric(NA)), type='integer', help = "Number of slots to run."),
    optparse::make_option(c("-b", "--this_block"), action="store", default=Sys.getenv("COVID_BLOCK_INDEX",1), type='integer', help = "id of this block"),
    optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
    optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
    optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)


# not full_filter.R

# load test config
config = covidcommon::load_config(opt$config)
config$seeding$variant_filename <- "test/test_groundtruth/data/variant_props_long.csv"

##Load infromationon geographic locations from geodata file.
suppressMessages(geodata <- report.generation::load_geodata_file(
    paste(
        config$spatial_setup$base_path,
        config$spatial_setup$geodata, sep = "/"
    ),
    geoid_len=5 #Is this hardcode a good idea.
))
obs_nodename <- config$spatial_setup$nodenames

#############






print("Starting")
if(opt$config == ""){
    optparse::print_help(parser)
    stop(paste(
        "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
    ))
}

#config <- covidcommon::load_config(opt$config)

deathrates <- opt$deathrates
if(all(deathrates == "all")) {
    deathrates<- config$outcomes$scenarios
} else if (!(deathrates %in% config$outcomes$scenarios)){
    message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
    quit("yes", status=1)
}

scenarios <- opt$scenarios
if (all(scenarios == "all")){
    scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
    message(paste("Invalid scenario arguments: [",paste(setdiff(scenarios, config$interventions$scenarios)), "] did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
    quit("yes", status=1)
}

if(is.na(opt$sims_per_slot)) {
    opt$sims_per_slot <- config$filtering$simulations_per_slot
}

if(is.na(opt$slots)) {
    opt$slots <- config$nsimulations
}

if(is.null(opt$fix_negatives)) {
    opt$fix_negatives <- TRUE
}

covidcommon::prettyprint_optlist(list(scenarios=scenarios,deathrates=deathrates,slots=seq_len(opt$slots)))





# FROM filter_MC.R --------------------------------------------------------


## Preamble ---------------------------------------------------------------------
suppressMessages(library(readr))
suppressWarnings(suppressMessages(library(covidcommon)))
suppressMessages(library(report.generation))
suppressMessages(library(stringr))
suppressMessages(library(foreach))
suppressMessages(library(magrittr))
suppressMessages(library(xts))
suppressMessages(library(reticulate))
suppressMessages(library(truncnorm))
suppressMessages(library(parallel))
options(warn = 1)
options(readr.num_columns = 0)

option_list = list(
    optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
    optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
    optparse::make_option(c("-s", "--scenarios"), action="store", default=Sys.getenv("COVID_SCENARIOS", 'all'), type='character', help="name of the intervention to run, or 'all' to run all of them"),
    optparse::make_option(c("-d", "--deathrates"), action="store", default=Sys.getenv("COVID_DEATHRATES", 'all'), type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
    optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", parallel::detectCores()), type='integer', help="Number of jobs to run in parallel"),
    optparse::make_option(c("-k", "--simulations_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT", NA), type='integer', help = "number of simulations to run for this slot"),
    optparse::make_option(c("-i", "--this_slot"), action="store", default=Sys.getenv("COVID_SLOT_INDEX", 1), type='integer', help = "id of this slot"),
    optparse::make_option(c("-b", "--this_block"), action="store", default=Sys.getenv("COVID_BLOCK_INDEX",1), type='integer', help = "id of this block"),
    optparse::make_option(c("-t", "--stoch_traj_flag"), action="store", default=Sys.getenv("COVID_STOCHASTIC",TRUE), type='logical', help = "Stochastic SEIR and outcomes trajectories if true"),
    optparse::make_option(c("--ground_truth_start"), action = "store", default = Sys.getenv("COVID_GT_START", ""), type = "character", help = "First date to include groundtruth for"),
    optparse::make_option(c("--ground_truth_end"), action = "store", default = Sys.getenv("COVID_GT_END", ""), type = "character", help = "Last date to include groundtruth for"),
    optparse::make_option(c("--cache_gt"), action = "store", default = Sys.getenv("CACHE_GT", TRUE), type = "logical", help = "Whether to use cached grount truth data"),
    optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
    optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
    optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable"),
    optparse::make_option(c("-R", "--is-resume"), action="store", default=Sys.getenv("COVID_IS_RESUME",FALSE), type = 'logical', help = "Is this run a resume"),
    optparse::make_option(c("-I", "--is-interactive"), action="store", default=Sys.getenv("COVID_INTERACTIVE",Sys.getenv("INTERACTIVE_RUN", FALSE)), type = 'logical', help = "Is this run an interactive run")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)
opt$cache_gt <- FALSE


if(opt[["is-interactive"]]) {
    options(error=recover)
} else {
    options(
        error = function(...) {
            quit(..., status = 2)
        }
    )
}
covidcommon::prettyprint_optlist(opt)

#reticulate::use_python(Sys.which(opt$python),require=TRUE)
## Block loads the config file and geodata
if(opt$config == ""){
    optparse::print_help(parser)
    stop(paste(
        "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
    ))
}
config = covidcommon::load_config(opt$config)

if(('perturbation_sd' %in% names(config$seeding))) {
    if(('date_sd' %in% names(config$seeding))) {
        stop("Both the key seeding::perturbation_sd and the key seeding::date_sd are present in the config file, but only one allowed.")
    }
    config$seeding$date_sd <- config$seeding$perturbation_sd
}
if (!('date_sd' %in% names(config$seeding))) {
    stop("Neither the key seeding::perturbation_sd nor the key seeding::date_sd are present in the config file, but one is required.")
}
if (!('amount_sd' %in% names(config$seeding))) {
    config$seeding$amount_sd <- 1
}

if(!(config$seeding$method %in% c('FolderDraw','InitialConditionsFolderDraw'))){
    stop("This filtration method requires the seeding method 'FolderDraw'")
}

if(!(config$seeding$method %in% c('FolderDraw','InitialConditionsFolderDraw'))){
    stop("This filtration method requires the seeding method 'FolderDraw'")
}
#if(!('lambda_file' %in% names(config$seeding))) {
#  stop("Despite being a folder draw method, filtration method requires the seeding to provide a lambda_file argument.")
#}


# Aggregation to state level if in config
state_level <- ifelse(!is.null(config$spatial_setup$state_level) && config$spatial_setup$state_level, TRUE, FALSE)

if(is.null(opt$fix_negatives)) {
    opt$fix_negatives <- TRUE
}



##Load infromationon geographic locations from geodata file.
suppressMessages(geodata <- report.generation::load_geodata_file(
    paste(
        config$spatial_setup$base_path,
        config$spatial_setup$geodata, sep = "/"
    ),
    geoid_len=5 #Is this hardcode a good idea.
))
obs_nodename <- config$spatial_setup$nodenames

##Load simulations per slot from config if not defined on command line
##command options take precendence
if (is.na(opt$simulations_per_slot)) {
    opt$simulations_per_slot <- config$filtering$simulations_per_slot
}
print(paste("Running",opt$simulations_per_slot,"simulations"))

##Define data directory and create if it does not exist
data_path <- config$filtering$data_path
data_dir <- dirname(data_path)
if(!dir.exists(data_dir)){
    suppressWarnings(dir.create(data_dir,recursive=TRUE))
}

# Parse scenarios arguments
##If death rates are specified check their existence
deathrates <- opt$deathrates
if(all(deathrates == "all")) {
    deathrates<- config$outcomes$scenarios
} else if (!(deathrates %in% config$outcomes$scenarios)) {
    message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
    quit("yes", status=1)
}

##If scenarios are specified check their existence
scenarios <- opt$scenarios
if (all(scenarios == "all")){
    scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
    message(paste("Invalid scenario argument:",scenario, "did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
    quit("yes", status=1)
}


##Creat heirarchical stats object if specified
hierarchical_stats <- list()
if("hierarchical_stats_geo"%in%names(config$filtering)) {
    hierarchical_stats <- config$filtering$hierarchical_stats_geo
}


##Create priors if specified
defined_priors <- list()
if("priors"%in%names(config$filtering)) {
    defined_priors <- config$filtering$priors
}






## Runner Script---------------------------------------------------------------------



# ~ Ground-Truth Data -----------------------------------------------------

## backwards compatibility with configs that don't have filtering$gt_source parameter will use the previous default data source (USA Facts)
if(is.null(config$filtering$gt_source)){
    gt_source <- "usafacts"
} else{
    gt_source <- config$filtering$gt_source
}

gt_scale <- ifelse(state_level, "US state", "US county")
fips_codes_ <- geodata[[obs_nodename]]

gt_start_date_ <- lubridate::ymd(config$start_date)
if (opt$ground_truth_start != "") {
    gt_start_date_ <- lubridate::ymd(opt$ground_truth_start)
} else if (!is.null(config$start_date_groundtruth)) {
    gt_start_date_ <- lubridate::ymd(config$start_date_groundtruth)
}
if (gt_start_date_ < lubridate::ymd(config$start_date)) {
    gt_start_date_ <- lubridate::ymd(config$start_date)
}

gt_end_date_ <- lubridate::ymd(config$end_date)
if (opt$ground_truth_end != "") {
    gt_end_date_ <- lubridate::ymd(opt$ground_truth_end)
} else if (!is.null(config$end_date_groundtruth)) {
    gt_end_date_ <- lubridate::ymd(config$end_date_groundtruth)
}
if (gt_end_date_ > lubridate::ymd(config$end_date)) {
    gt_end_date_ <- lubridate::ymd(config$end_date)
}



# ~ Target-Specific Ground Truth ------------------------------------------

gt_info <- as.list(config$filtering$statistics) %>%
    data.table::rbindlist(fill=TRUE) %>% as.data.frame() %>%
    dplyr::select(-likelihood) %>% tibble::as_tibble() 

if (!("gt_start_date" %in% colnames(gt_info))){
    gt_info$gt_start_date <- lubridate::as_date(gt_start_date_)
} else {
    gt_info <- gt_info %>% 
        dplyr::mutate(gt_start_date = lubridate::as_date(gt_start_date)) %>%
        dplyr::mutate(gt_start_date = replace(gt_start_date, is.na(gt_info$gt_start_date), gt_start_date_))
}
if (!("gt_end_date" %in% colnames(gt_info))){
    gt_info$gt_end_date <- lubridate::as_date(gt_end_date_)
} else {
    gt_info <- gt_info %>% 
        dplyr::mutate(gt_end_date = lubridate::as_date(gt_end_date)) %>%
        dplyr::mutate(gt_end_date = replace(gt_end_date, is.na(gt_info$gt_end_date), gt_end_date_))
}


gt_sources <- unique(gt_info$gt_source)
gt_targets_all <- unique(gsub("_(.*)", "", gt_info$data_var))
variant_props_file <- config$seeding$variant_filename

print(gt_info)



obs <- tibble::tibble(geoid = fips_codes_)
# if (length(gt_sources)>1 | length(unique(gt_info$gt_start_date))>1 | length(unique(gt_info$gt_end_date))>1){

if(!(file.exists(data_path) & opt$cache_gt)){
    for (g in 1:length(gt_sources)){
        
        # ground truth targets to pull
        gt_tmp <- gt_info %>% dplyr::filter(gt_source == gt_sources[g])
        print(gt_tmp)
        gt_targets <- unique(gsub("_(.*)", "", gt_tmp$data_var))
        if (("incidDeath" %in% gt_targets) & !("incidI" %in% gt_targets_all)) gt_targets <- c(gt_targets, "incidI")
        if (("incidI" %in% gt_targets) & !("incidI" %in% gt_targets_all)) gt_targets <- c(gt_targets, "incidDeath")
        if ("incidDeath" %in% gt_targets) gt_targets <- c(gt_targets, "Deaths")
        if ("incidI" %in% gt_targets) gt_targets <- c(gt_targets, "Confirmed")
        # if ("incidH" %in% gt_targets) gt_targets <- c(gt_targets, "Hospitalizations")
        
        obs_ <- inference::get_ground_truth(
            data_path = data_path,
            fips_codes = fips_codes_,
            fips_column_name = obs_nodename,
            start_date = gt_start_date_,
            end_date = gt_end_date_,
            cache = FALSE, # cache later
            gt_source = gt_sources[g],
            gt_scale = gt_scale,
            targets = gt_targets, 
            fix_negatives = opt$fix_negatives,
            variant_filename = NULL
        )    
        obs <- obs %>% dplyr::full_join(obs_)
    }
    # save merged
    readr::write_csv(obs, data_path)
    
} else {
    message("*** USING CACHED Data\n")
    obs <- suppressMessages(readr::read_csv(
        data_path,
        col_types = list(geoid = readr::col_character()),
    ))
}

# do variant adjustment
if (!is.null(variant_props_file) & any(c("incidI", "Confirmed") %in% gt_targets_all)) {
    tryCatch({
        obs <- do_variant_adjustment2(obs, variant_props_file, var_targets = c("incidI","Confirmed"))
    }, error = function(e) {
        stop(paste0("Could not use variant file |", variant_props_file, "|, with error message", e$message()))
    })
}

# limit dates
gt_infofull <- gt_info %>%
    dplyr::bind_rows(gt_info %>%
                         dplyr::mutate(data_var = gsub("incidI", "Confirmed", data_var), 
                                       data_var = gsub("incidH", "Hospitalizations", data_var), 
                                       data_var = gsub("incidDeath", "Deaths", data_var)))
target_ <- gt_infofull$data_var

for (s in 1:nrow(gt_infofull)){
    na_inds <- !(obs$date >= gt_infofull$gt_start_date[s]) & (obs$date <= gt_infofull$gt_end_date[s])
    obs[na_inds, target_[s]] <- NA
}

# } else {
#     
#     obs <- inference::get_ground_truth(
#         data_path = data_path,
#         fips_codes = fips_codes_,
#         fips_column_name = obs_nodename,
#         start_date = gt_start_date_,
#         end_date = gt_end_date_,
#         cache = opt$cache_gt,
#         gt_source = gt_sources,
#         gt_scale = gt_scale,
#         targets = gt_targets_all, 
#         fix_negatives = opt$fix_negatives,
#         variant_filename = config$seeding$variant_filename
#     )    
# }

geonames <- unique(obs[[obs_nodename]])

print("Successfully pulled ground truth.")








# Compute Statistics ------------------------------------------------------

data_stats <- lapply(
    geonames,
    function(x) {
        df <- obs[obs[[obs_nodename]] == x, ]
        inference::getStats(
            df,
            "date",
            "data_var",
            stat_list = config$filtering$statistics)
    }) %>%
    set_names(geonames)

required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")









# CREATE_SEEDING.R  ----------------------------------------------



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
    # cases_deaths <- covidcommon::get_groundtruth_from_source(source = gt_source, scale = "US county")
    
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
    cases_deaths <- readr::read_csv(data_path)
    print(paste("Successfully loaded data from ", data_path, "for seeding."))
}



if (seed_variants) {
    variant_data <- readr::read_csv(config$seeding$variant_filename)
    cases_deaths <- cases_deaths %>%
        dplyr::left_join(variant_data) %>%
        dplyr::mutate(incidI = incidI * prop) %>%
        dplyr::select(-prop) %>%
        tidyr::pivot_wider(names_from = variant, values_from = incidI)
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
        
        if (!is.null(config$smh_roun)) {
            if (config$smh_round=="R11"){
                incident_cases_om <- incident_cases %>%
                    dplyr::filter(Update==lubridate::as_date("2021-12-01")) %>%
                    dplyr::group_by(FIPS, Update, source_infection_stage, source_vaccination_stage, source_age_strata,
                                    destination_vaccination_stage, destination_age_strata, destination_infection_stage) %>%
                    dplyr::summarise(value = sum(value, na.rm=TRUE)) %>%
                    dplyr::mutate(source_variant_type = "WILD", destination_variant_type = "OMICRON") %>%
                    dplyr::mutate(value = round(ifelse(FIPS %in% c("06000","36000"), 10, 
                                                       ifelse(FIPS %in% c("53000","12000"), 5, 1)))) %>% 
                    tibble::as_tibble()
            }
        }
        
        
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
if (max(incident_cases$date) < lubridate::as_date(config$start_date)){
    
    incident_cases <- incident_cases %>% 
        group_by(place) %>%
        filter(date == min(date)) %>%
        distinct() %>%
        ungroup() %>%
        mutate(date = lubridate::as_date(config$start_date),
               amount = 0)
}




# Save it

write.csv(
    incident_cases,
    file = file.path(config$seeding$lambda_file),
    row.names = FALSE
)

print(paste("Saved seeding to", config$seeding$lambda_file))

## @endcond




# Look at ground truth
View(obs)

# Look at seeding
View(incident_cases)



















# ## python configuration for minimal_interface.py
# reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
# reticulate::py_run_string(paste0("run_id = '", opt$run_id, "'"))
# #reticulate::import_from_path("SEIR", path=opt$pipepath)
# #reticulate::import_from_path("Outcomes", path=opt$pipepath)
# reticulate::py_run_string(paste0("index = ", 1))
# if(opt$stoch_traj_flag) {
#     reticulate::py_run_string(paste0("stoch_traj_flag = True"))
# } else {
#     reticulate::py_run_string(paste0("stoch_traj_flag = False"))
# }
# 
# for(scenario in scenarios) {
#     
#     reticulate::py_run_string(paste0("scenario = '", scenario, "'"))
#     
#     for(deathrate in deathrates) {
#         # Data -------------------------------------------------------------------------
#         # Load
#         
#         slot_prefix <- covidcommon::create_prefix(config$name,scenario,deathrate,opt$run_id,sep='/',trailing_separator='/')
#         
#         gf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','final',sep='/',trailing_separator='/')
#         ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/')
#         gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/')
#         
#         
#         chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
#         chimeric_local_prefix <- covidcommon::create_prefix(prefix=chimeric_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')
#         
#         global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
#         global_local_prefix <- covidcommon::create_prefix(prefix=global_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')
#         
#         
#         ## pass prefix to python and use
#         reticulate::py_run_string(paste0("deathrate = '", deathrate, "'"))
#         reticulate::py_run_string(paste0("prefix = '", global_block_prefix, "'"))
#         reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
#         
#         
#         
#         first_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block - 1)
#         first_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block - 1)
#         
#         inference::initialize_mcmc_first_block(
#             opt$run_id,
#             opt$this_block,
#             global_block_prefix,
#             chimeric_block_prefix,
#             py,
#             function(sim_hosp){
#                 sim_hosp <- dplyr::filter(sim_hosp,sim_hosp$time >= min(obs$date),sim_hosp$time <= max(obs$date))
#                 lhs <- unique(sim_hosp[[obs_nodename]])
#                 rhs <- unique(names(data_stats))
#                 all_locations <- rhs[rhs %in% lhs]
#                 
#                 inference::aggregate_and_calc_loc_likelihoods(
#                     all_locations = all_locations, # technically different
#                     modeled_outcome = sim_hosp,
#                     obs_nodename = obs_nodename,
#                     config = config,
#                     obs = obs,
#                     ground_truth_data = data_stats,
#                     hosp_file = first_global_files[['llik_filename']],
#                     hierarchical_stats = hierarchical_stats,
#                     defined_priors = defined_priors,
#                     geodata = geodata,
#                     snpi = arrow::read_parquet(first_global_files[['snpi_filename']]),
#                     hnpi = arrow::read_parquet(first_global_files[['hnpi_filename']]),
#                     hpar = dplyr::mutate(arrow::read_parquet(first_global_files[['hpar_filename']]),parameter=paste(quantity,!!rlang::sym(obs_nodename),outcome,sep='_'))
#                 )
#             },
#             is_resume = opt[['is-resume']]
#         )
#         
#         
#         ## So far no acceptances have occurred
#         current_index <- 0
#         ### Load initial files
#         seeding_col_types <- NULL
#         suppressMessages(initial_seeding <- readr::read_csv(first_chimeric_files[['seed_filename']], col_types=seeding_col_types))
#         if (opt$stoch_traj_flag) {
#             initial_seeding$amount <- as.integer(round(initial_seeding$amount))
#         }
#         initial_snpi <- arrow::read_parquet(first_chimeric_files[['snpi_filename']])
#         initial_hnpi <- arrow::read_parquet(first_chimeric_files[['hnpi_filename']])
#         initial_spar <- arrow::read_parquet(first_chimeric_files[['spar_filename']])
#         initial_hpar <- arrow::read_parquet(first_chimeric_files[['hpar_filename']])
#         chimeric_likelihood_data <- arrow::read_parquet(first_chimeric_files[['llik_filename']])
#         global_likelihood_data <- arrow::read_parquet(first_global_files[['llik_filename']])
#         
#         #####Get the full likelihood (WHY IS THIS A DATA FRAME)
#         # Compute total loglik for each sim
#         global_likelihood <- sum(global_likelihood_data$ll)
#         
#         #####LOOP NOTES
#         ### current means proposed
#         ### initial means accepted/current
#         
#         #####Loop over simulations in this block
#         for( this_index in seq_len(opt$simulations_per_slot)) {
#             print(paste("Running simulation", this_index))
#             
#             ## Create filenames
#             this_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, this_index)
#             this_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_local_prefix, this_index)
#             
#             ## Setup python
#             reticulate::py_run_string(paste0("prefix = '", global_local_prefix, "'"))
#             reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
#             
#             ## Do perturbations from accepted
#             proposed_seeding <- inference::perturb_seeding(
#                 seeding = initial_seeding,
#                 date_sd = config$seeding$date_sd,
#                 date_bounds = c(gt_start_date, gt_end_date),
#                 amount_sd = config$seeding$amount_sd,
#                 continuous = !(opt$stoch_traj_flag)
#             )
#             proposed_snpi <- inference::perturb_snpi(initial_snpi, config$interventions$settings)
#             proposed_hnpi <- inference::perturb_hnpi(initial_hnpi, config$interventions$settings)
#             proposed_spar <- initial_spar
#             if(!deathrate %in% names(config$outcomes$settings)){
#                 stop(paste("Deathrate",deathrate,"does not appear in outcomes::settings in the config"))
#             }
#             proposed_hpar <- inference::perturb_hpar(initial_hpar, config$outcomes$settings[[deathrate]])
#             
#             ## Write files that need to be written for other code to read
#             write.csv(proposed_seeding,this_global_files[['seed_filename']])
#             arrow::write_parquet(proposed_snpi,this_global_files[['snpi_filename']])
#             arrow::write_parquet(proposed_hnpi,this_global_files[['hnpi_filename']])
#             arrow::write_parquet(proposed_spar,this_global_files[['spar_filename']])
#             arrow::write_parquet(proposed_hpar,this_global_files[['hpar_filename']])
#             
#             ## Run SEIR
#             err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
#             err <- ifelse(err == 1,0,1)
#             if(err != 0){
#                 stop("SEIR failed to run")
#             }
#             
#             err <- py$onerun_OUTCOMES_loadID(this_index)
#             print(err)
#             err <- ifelse(err == 1,0,1)
#             if(err != 0){
#                 stop("HOSP failed to run")
#             }
#             
#             sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",this_global_files[['hosp_filename']]))(this_global_files[['hosp_filename']]) %>%
#                 dplyr::filter(time >= min(obs$date),time <= max(obs$date))
#             
#             
#             
#             lhs <- unique(sim_hosp[[obs_nodename]])
#             rhs <- unique(names(data_stats))
#             all_locations <- rhs[rhs %in% lhs]
#             
#             proposed_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(
#                 all_locations = all_locations,
#                 modeled_outcome = sim_hosp,
#                 obs_nodename = obs_nodename,
#                 config = config,
#                 obs = obs,
#                 ground_truth_data = data_stats,
#                 hosp_file = this_global_files[["llik_filename"]],
#                 hierarchical_stats = hierarchical_stats,
#                 defined_priors = defined_priors,
#                 geodata = geodata,
#                 snpi = proposed_snpi,
#                 hnpi = proposed_hnpi,
#                 hpar = dplyr::mutate(
#                     proposed_hpar,
#                     parameter = paste(quantity, !!rlang::sym(obs_nodename), outcome, sep = "_")
#                 )
#             )
#             
#             
#             rm(sim_hosp)
#             
#             ## UNCOMMENT TO DEBUG
#             ## print(global_likelihood_data)
#             ## print(chimeric_likelihood_data)
#             ## print(proposed_likelihood_data)
#             
#             ## Compute total loglik for each sim
#             proposed_likelihood <- sum(proposed_likelihood_data$ll)
#             
#             ## For logging
#             print(paste("Current likelihood",global_likelihood,"Proposed likelihood",proposed_likelihood))
#             
#             if(inference::iterateAccept(global_likelihood, proposed_likelihood) || ((current_index == 0) && (opt$this_block == 1))){
#                 print("****ACCEPT****")
#                 if ((opt$this_block == 1) && (current_index == 0)) {
#                     print("by default because it's the first iteration of a block 1")
#                 }
#                 current_index <- this_index
#                 global_likelihood <- proposed_likelihood
#                 global_likelihood_data <- proposed_likelihood_data
#             } else {
#                 print("****REJECT****")
#             }
#             arrow::write_parquet(proposed_likelihood_data, this_global_files[['llik_filename']])
#             
#             seeding_npis_list <- inference::accept_reject_new_seeding_npis(
#                 seeding_orig = initial_seeding,
#                 seeding_prop = proposed_seeding,
#                 snpi_orig = initial_snpi,
#                 snpi_prop = proposed_snpi,
#                 hnpi_orig = initial_hnpi,
#                 hnpi_prop = proposed_hnpi,
#                 hpar_orig = initial_hpar,
#                 hpar_prop = proposed_hpar,
#                 orig_lls = chimeric_likelihood_data,
#                 prop_lls = proposed_likelihood_data
#             )
#             initial_seeding <- seeding_npis_list$seeding
#             initial_snpi <- seeding_npis_list$snpi
#             initial_hnpi <- seeding_npis_list$hnpi
#             initial_hpar <- seeding_npis_list$hpar
#             chimeric_likelihood_data <- seeding_npis_list$ll
#             arrow::write_parquet(chimeric_likelihood_data, this_chimeric_files[['llik_filename']])
#             
#             print(paste("Current index is ",current_index))
#             # print(proposed_likelihood_data)
#             # print(chimeric_likelihood_data)
#             
#             ###Memory managment
#             rm(proposed_snpi)
#             rm(proposed_hnpi)
#             rm(proposed_hpar)
#             rm(proposed_seeding)
#         }
#         
#         #####Do MCMC end copy. Fail if unsucessfull
#         cpy_res <- inference::perform_MCMC_step_copies(current_index,
#                                                        opt$this_slot,
#                                                        opt$this_block,
#                                                        opt$run_id,
#                                                        global_local_prefix,
#                                                        gf_prefix,
#                                                        global_block_prefix)
#         
#         if(!prod(unlist(cpy_res))) {stop("File copy failed:", paste(unlist(cpy_res),paste(names(cpy_res),"|")))}
#         
#         
#         #####Write currently accepted files to disk
#         last_index_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, opt$simulations_per_slot)
#         output_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block)
#         output_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block)
#         readr::write_csv(initial_seeding,output_chimeric_files[['seed_filename']])
#         arrow::write_parquet(initial_snpi,output_chimeric_files[['snpi_filename']])
#         arrow::write_parquet(initial_hnpi,output_chimeric_files[['hnpi_filename']])
#         arrow::write_parquet(initial_spar,output_chimeric_files[['spar_filename']])
#         arrow::write_parquet(initial_hpar,output_chimeric_files[['hpar_filename']])
#         arrow::write_parquet(chimeric_likelihood_data,output_chimeric_files[['llik_filename']])
#         warning("Chimeric hosp and seir files not yet supported, just using the most recently generated file of each type")
#         file.copy(last_index_global_files[['hosp_filename']],output_chimeric_files[['hosp_filename']])
#         file.copy(last_index_global_files[['seir_filename']],output_chimeric_files[['seir_filename']])
#     }
# }




















# required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")
# 
# ## python configuration for minimal_interface.py
# reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
# reticulate::py_run_string(paste0("run_id = '", opt$run_id, "'"))
# #reticulate::import_from_path("SEIR", path=opt$pipepath)
# #reticulate::import_from_path("Outcomes", path=opt$pipepath)
# reticulate::py_run_string(paste0("index = ", 1))
# if(opt$stoch_traj_flag) {
#     reticulate::py_run_string(paste0("stoch_traj_flag = True"))
# } else {
#     reticulate::py_run_string(paste0("stoch_traj_flag = False"))
# }
# 
# for(scenario in scenarios) {
#     
#     reticulate::py_run_string(paste0("scenario = '", scenario, "'"))
#     
#     for(deathrate in deathrates) {
#         # Data -------------------------------------------------------------------------
#         # Load
#         
#         slot_prefix <- covidcommon::create_prefix(config$name,scenario,deathrate,opt$run_id,sep='/',trailing_separator='/')
#         
#         gf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','final',sep='/',trailing_separator='/')
#         ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/')
#         gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/')
#         
#         
#         chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
#         chimeric_local_prefix <- covidcommon::create_prefix(prefix=chimeric_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')
#         
#         global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
#         global_local_prefix <- covidcommon::create_prefix(prefix=global_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')
#         
#         
#         ## pass prefix to python and use
#         reticulate::py_run_string(paste0("deathrate = '", deathrate, "'"))
#         reticulate::py_run_string(paste0("prefix = '", global_block_prefix, "'"))
#         reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
#         
#         
#         
#         first_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block - 1)
#         first_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block - 1)
#         
#         inference::initialize_mcmc_first_block(
#             opt$run_id,
#             opt$this_block,
#             global_block_prefix,
#             chimeric_block_prefix,
#             py,
#             function(sim_hosp){
#                 sim_hosp <- dplyr::filter(sim_hosp,sim_hosp$time >= min(obs$date),sim_hosp$time <= max(obs$date))
#                 lhs <- unique(sim_hosp[[obs_nodename]])
#                 rhs <- unique(names(data_stats))
#                 all_locations <- rhs[rhs %in% lhs]
#                 
#                 inference::aggregate_and_calc_loc_likelihoods(
#                     all_locations = all_locations, # technically different
#                     modeled_outcome = sim_hosp,
#                     obs_nodename = obs_nodename,
#                     config = config,
#                     obs = obs,
#                     ground_truth_data = data_stats,
#                     hosp_file = first_global_files[['llik_filename']],
#                     hierarchical_stats = hierarchical_stats,
#                     defined_priors = defined_priors,
#                     geodata = geodata,
#                     snpi = arrow::read_parquet(first_global_files[['snpi_filename']]),
#                     hnpi = arrow::read_parquet(first_global_files[['hnpi_filename']]),
#                     hpar = dplyr::mutate(arrow::read_parquet(first_global_files[['hpar_filename']]),parameter=paste(quantity,!!rlang::sym(obs_nodename),outcome,sep='_'))
#                 )
#             },
#             is_resume = opt[['is-resume']]
#         )
#         
#         
#         ## So far no acceptances have occurred
#         current_index <- 0
#         ### Load initial files
#         seeding_col_types <- NULL
#         suppressMessages(initial_seeding <- readr::read_csv(first_chimeric_files[['seed_filename']], col_types=seeding_col_types))
#         if (opt$stoch_traj_flag) {
#             initial_seeding$amount <- as.integer(round(initial_seeding$amount))
#         }
#         initial_snpi <- arrow::read_parquet(first_chimeric_files[['snpi_filename']])
#         initial_hnpi <- arrow::read_parquet(first_chimeric_files[['hnpi_filename']])
#         initial_spar <- arrow::read_parquet(first_chimeric_files[['spar_filename']])
#         initial_hpar <- arrow::read_parquet(first_chimeric_files[['hpar_filename']])
#         chimeric_likelihood_data <- arrow::read_parquet(first_chimeric_files[['llik_filename']])
#         global_likelihood_data <- arrow::read_parquet(first_global_files[['llik_filename']])
#         
#         #####Get the full likelihood (WHY IS THIS A DATA FRAME)
#         # Compute total loglik for each sim
#         global_likelihood <- sum(global_likelihood_data$ll)
#         
#         #####LOOP NOTES
#         ### current means proposed
#         ### initial means accepted/current
#         
#         #####Loop over simulations in this block
#         for( this_index in seq_len(opt$simulations_per_slot)) {
#             print(paste("Running simulation", this_index))
#             
#             ## Create filenames
#             this_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, this_index)
#             this_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_local_prefix, this_index)
#             
#             ## Setup python
#             reticulate::py_run_string(paste0("prefix = '", global_local_prefix, "'"))
#             reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))
#             
#             ## Do perturbations from accepted
#             proposed_seeding <- inference::perturb_seeding(
#                 seeding = initial_seeding,
#                 date_sd = config$seeding$date_sd,
#                 date_bounds = c(gt_start_date, gt_end_date),
#                 amount_sd = config$seeding$amount_sd,
#                 continuous = !(opt$stoch_traj_flag)
#             )
#             proposed_snpi <- inference::perturb_snpi(initial_snpi, config$interventions$settings)
#             proposed_hnpi <- inference::perturb_hnpi(initial_hnpi, config$interventions$settings)
#             proposed_spar <- initial_spar
#             if(!deathrate %in% names(config$outcomes$settings)){
#                 stop(paste("Deathrate",deathrate,"does not appear in outcomes::settings in the config"))
#             }
#             proposed_hpar <- inference::perturb_hpar(initial_hpar, config$outcomes$settings[[deathrate]])
#             
#             ## Write files that need to be written for other code to read
#             write.csv(proposed_seeding,this_global_files[['seed_filename']])
#             arrow::write_parquet(proposed_snpi,this_global_files[['snpi_filename']])
#             arrow::write_parquet(proposed_hnpi,this_global_files[['hnpi_filename']])
#             arrow::write_parquet(proposed_spar,this_global_files[['spar_filename']])
#             arrow::write_parquet(proposed_hpar,this_global_files[['hpar_filename']])
#             
#             ## Run SEIR
#             err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
#             err <- ifelse(err == 1,0,1)
#             if(err != 0){
#                 stop("SEIR failed to run")
#             }
#             
#             err <- py$onerun_OUTCOMES_loadID(this_index)
#             print(err)
#             err <- ifelse(err == 1,0,1)
#             if(err != 0){
#                 stop("HOSP failed to run")
#             }
#             
#             sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",this_global_files[['hosp_filename']]))(this_global_files[['hosp_filename']]) %>%
#                 dplyr::filter(time >= min(obs$date),time <= max(obs$date))
#             
#             
#             
#             lhs <- unique(sim_hosp[[obs_nodename]])
#             rhs <- unique(names(data_stats))
#             all_locations <- rhs[rhs %in% lhs]
#             
#             proposed_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(
#                 all_locations = all_locations,
#                 modeled_outcome = sim_hosp,
#                 obs_nodename = obs_nodename,
#                 config = config,
#                 obs = obs,
#                 ground_truth_data = data_stats,
#                 hosp_file = this_global_files[["llik_filename"]],
#                 hierarchical_stats = hierarchical_stats,
#                 defined_priors = defined_priors,
#                 geodata = geodata,
#                 snpi = proposed_snpi,
#                 hnpi = proposed_hnpi,
#                 hpar = dplyr::mutate(
#                     proposed_hpar,
#                     parameter = paste(quantity, !!rlang::sym(obs_nodename), outcome, sep = "_")
#                 )
#             )
#             
#             
#             rm(sim_hosp)
#             
#             ## UNCOMMENT TO DEBUG
#             ## print(global_likelihood_data)
#             ## print(chimeric_likelihood_data)
#             ## print(proposed_likelihood_data)
#             
#             ## Compute total loglik for each sim
#             proposed_likelihood <- sum(proposed_likelihood_data$ll)
#             
#             ## For logging
#             print(paste("Current likelihood",global_likelihood,"Proposed likelihood",proposed_likelihood))
#             
#             if(inference::iterateAccept(global_likelihood, proposed_likelihood) || ((current_index == 0) && (opt$this_block == 1))){
#                 print("****ACCEPT****")
#                 if ((opt$this_block == 1) && (current_index == 0)) {
#                     print("by default because it's the first iteration of a block 1")
#                 }
#                 current_index <- this_index
#                 global_likelihood <- proposed_likelihood
#                 global_likelihood_data <- proposed_likelihood_data
#             } else {
#                 print("****REJECT****")
#             }
#             arrow::write_parquet(proposed_likelihood_data, this_global_files[['llik_filename']])
#             
#             seeding_npis_list <- inference::accept_reject_new_seeding_npis(
#                 seeding_orig = initial_seeding,
#                 seeding_prop = proposed_seeding,
#                 snpi_orig = initial_snpi,
#                 snpi_prop = proposed_snpi,
#                 hnpi_orig = initial_hnpi,
#                 hnpi_prop = proposed_hnpi,
#                 hpar_orig = initial_hpar,
#                 hpar_prop = proposed_hpar,
#                 orig_lls = chimeric_likelihood_data,
#                 prop_lls = proposed_likelihood_data
#             )
#             initial_seeding <- seeding_npis_list$seeding
#             initial_snpi <- seeding_npis_list$snpi
#             initial_hnpi <- seeding_npis_list$hnpi
#             initial_hpar <- seeding_npis_list$hpar
#             chimeric_likelihood_data <- seeding_npis_list$ll
#             arrow::write_parquet(chimeric_likelihood_data, this_chimeric_files[['llik_filename']])
#             
#             print(paste("Current index is ",current_index))
#             # print(proposed_likelihood_data)
#             # print(chimeric_likelihood_data)
#             
#             ###Memory managment
#             rm(proposed_snpi)
#             rm(proposed_hnpi)
#             rm(proposed_hpar)
#             rm(proposed_seeding)
#         }
#         
#         #####Do MCMC end copy. Fail if unsucessfull
#         cpy_res <- inference::perform_MCMC_step_copies(current_index,
#                                                        opt$this_slot,
#                                                        opt$this_block,
#                                                        opt$run_id,
#                                                        global_local_prefix,
#                                                        gf_prefix,
#                                                        global_block_prefix)
#         
#         if(!prod(unlist(cpy_res))) {stop("File copy failed:", paste(unlist(cpy_res),paste(names(cpy_res),"|")))}
#         
#         
#         #####Write currently accepted files to disk
#         last_index_global_files <- inference::create_filename_list(opt$run_id, global_local_prefix, opt$simulations_per_slot)
#         output_chimeric_files <- inference::create_filename_list(opt$run_id, chimeric_block_prefix, opt$this_block)
#         output_global_files <- inference::create_filename_list(opt$run_id, global_block_prefix, opt$this_block)
#         readr::write_csv(initial_seeding,output_chimeric_files[['seed_filename']])
#         arrow::write_parquet(initial_snpi,output_chimeric_files[['snpi_filename']])
#         arrow::write_parquet(initial_hnpi,output_chimeric_files[['hnpi_filename']])
#         arrow::write_parquet(initial_spar,output_chimeric_files[['spar_filename']])
#         arrow::write_parquet(initial_hpar,output_chimeric_files[['hpar_filename']])
#         arrow::write_parquet(chimeric_likelihood_data,output_chimeric_files[['llik_filename']])
#         warning("Chimeric hosp and seir files not yet supported, just using the most recently generated file of each type")
#         file.copy(last_index_global_files[['hosp_filename']],output_chimeric_files[['hosp_filename']])
#         file.copy(last_index_global_files[['seir_filename']],output_chimeric_files[['seir_filename']])
#     }
# }
# 
# 
