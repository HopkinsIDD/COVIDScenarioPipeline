library(config.writer)

# Default Params ----
## Save Names
run_type <- "fchub"  # name to save processed intervention data, pasted with formatted config date. For example, if base_path is "data" then this would be saved as "data/fchub_20210712.csv". 
config_name <- "config.yml" # filename to save in current directory
run_compartment = FALSE # set to false for old config settings

## Data Files
outcomes_parquet_file <- "usa-geoid-params-output_statelevel.parquet"

base_path <- "data" # path to directory with the geodata, intervention, vaccination, outcomes, and variant files
geodata_file <- "geodata_territories_2019_statelevel.csv"
mobility_file <- "mobility_territories_2011-2015_statelevel.csv"
intervention_file <- "intervention_tracking/Shelter-in-place-as-of-08122021.csv"

vaccination_file <- "vaccination/Round7/vacc_rates_ROUND7.csv"
outcomes_file <- "vaccination/Round7/outcome_adj_allscenarios_ROUND7.csv"
hosp_file <- "hosp_adjust/hospitalization_ratios_2021-07-31.csv"
vacc_scenario <- 2 # Scenario number from the vacc_path and outcomes_path files 

variant_file_1 <- "variant/b117_fits_r7.csv"
variant_file_2 <- "variant/b1617_fits_r7.csv"
variant_seeding_file <- "variant/variant_props_long.csv"
b117_only <- FALSE # false if accounting for delta variant as well
variant_transmission_increase <- 0.6 # increase in transmission in delta vs 117

incidC_shift_file <- "US_CFR.csv"

## Broad config settings 
## Header
sim_name <- "USA"
sim_start <- "2020-01-01"
sim_end <- "2021-09-25"
setup_name <- "usa_inference_territories_statelevel"
state_level <- TRUE
n_simulations <- 300 # overwritten by environmental var COVID_NSIMULATIONS

## Seeding
seeding_method = "FolderDraw"
lambda_file <- "minimal/seeding_territories.csv"
perturbation_sd <- 3

#SEIR
gamma_dist <- "uniform"
incl_vacc = TRUE
variant_compartments = c("wild", "alpha", "delta")
vaccine_compartments = c("unvaccinated", "1dose", "2dose") 

## Interventions
all_fixed <- FALSE # fixes all intervention values across chains
npi_scenario_name <- "inference"
exclude_apr_seasonality <- TRUE
add_redux = FALSE # whether to add NPI reduction interventions
redux_geoids = {if(add_redux) "all" else(NULL)}
state_incl_geoid = {if(state_level) NULL else("06000")} # used to get the vaccination rates and vacc-adjusted outcomes, NULL if running state-level for ALL states. Otherwise, specify desired state geoids
VE_shift = TRUE 
VE = c(0.5, 0.9)
VE_delta = c(0.35, 0.6)

## Outcomes
hosp_adjustment = TRUE

add_incidC = FALSE # whether to add incidC shift interventions
incidC_shift_periods = c("2020-01-01", "2020-06-16", "2020-11-27")
incidC_shift_epochs = c("MarJun", "NovJan")
incidC_shift_value_mean = 0.25 # state-specific initial value for those without an ifr estimate; possible to supply vectors to match the number of periods (e.g. c(0.25, 0.5))
incidC_shift_pert_sd = 0.01

ifr = "med"
incidD_prob_value = rep(0.005, length(variant_compartments))
incidC_prob_value = rep(0.4, length(variant_compartments))
incidC_prob_dist = rep("truncnorm", length(variant_compartments))
incidC_prob_dist_pert = rep("truncnorm", length(variant_compartments))
incidC_perturbation = TRUE

## Filtering
do_filtering = TRUE # inference?
sims_per_slot = 1000 # overwritten by environmental var COVID_SIMULATIONS_PER_SLOT
data_file = "us_data.csv"
filtering_data_vars <- {if(run_compartment) c("incidDeath", "incidI") else(c("death_incid", "confirmed_incid"))}
priors_name <- c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar", "Seas_apr",
                 "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
                 "Seas_oct", "Seas_nov", "Seas_dec")
priors_name <- {if(exclude_apr_seasonality) priors_name[priors_name!="Seas_apr"] else(priors_name)}

# Terminal Call ----
## Ignore if not submitting from terminal


option_list = list(
    optparse::make_option(c("-f", "--fixed"), action="store", type='logical', help="whether to set all intervention distributions to 'fixed'", default=all_fixed),
    optparse::make_option(c("-d", "--do-filter"), action="store", type='logical', help="whether inference will be performed", default=do_filtering),
    optparse::make_option(c("-b", "--base-path"), action="store", type='character', help = "path to data files", default = base_path),
    optparse::make_option(c("-c", "--config-name"), action="store", type='character', help = "name of config", default= config_name),
    optparse::make_option(c("-g", "--geodata-file"), action="store", type='character', help="name of geodata file within path", default = geodata_file),
    optparse::make_option(c("-v", "--vaccination-file"), action="store", type='character', help = "name of vaccination rates file within path", default = vaccination_file),
    optparse::make_option(c("-o", "--outcomes-file"), action="store", type='character', help = "name of outcome adjustments within path", default = outcomes_file),
    optparse::make_option(c("-i", "--intervention-file"), action="store", type='character', help = "name of npi intervention file within path", default = intervention_file),
    optparse::make_option(c("-s", "--state-level"), action="store", type='logical', help = "whether state-level run", default = state_level), 
    optparse::make_option(c("-i", "--incidC-file"), action="store", type='logical', help = "name of CFR file within path", default = incidC_shift_file),
    optparse::make_option(c("--var-1"), action="store", type='character', help = "path to fit for variant 1", default = variant_file_1),
    optparse::make_option(c("--var-2"), action="store", type='character', help = "path to fit for variant 2", default = variant_file_2), 
    optparse::make_option(c("--params-output-data"), action="store", type='character', help = "path to params outcomes parquet file", default = outcomes_parquet_file),
    optparse::make_option(c("-k", "--sims_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT", as.numeric(sims_per_slot)), type='integer', help = "Number of simulations to run per slot"),
    optparse::make_option(c("-n", "--slots"), action="store", default=Sys.getenv("COVID_NSIMULATIONS", as.numeric(n_simulations)), type='integer', help = "Number of slots to run.")
    
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

all_fixed = opt$`fixed`
do_filtering = opt$`do-filter`
base_path = opt$`base-path`
config_name = opt$`config-name`
geodata_file = opt$`geodata-file`
vaccination_file = opt$`vaccination-file`
outcomes_file = opt$`outcomes-file`
intervention_file = opt$`intervention-file`
state_level = opt$`state-level`
variant_file_1 = opt$`var-1`
variant_file_2 = opt$`var-2`
outcomes_parquet_file = opt$`params-output-data`

incidC_perturbation = do_filtering

# Set Paths

geodata_path <- file.path(base_path, geodata_file)
lambda_path <- file.path(base_path, lambda_file)

vaccination_path <- file.path(base_path, vaccination_file) 
outcomes_path <- file.path(base_path, outcomes_file)

intervention_path <- file.path(base_path, intervention_file)

variant_seeding_path <- file.path(base_path, variant_seeding_file)
variant_path_1 <- file.path(base_path, variant_file_1)
variant_path_2 <- file.path(base_path, variant_file_2)
hosp_path <- file.path(base_path, hosp_file)

incidC_shift_path <- file.path(base_path, incidC_shift_file)
data_path = file.path(base_path, data_file)

save_config_data <- file.path(base_path, paste0(run_type, "_", format(Sys.Date(), "%Y%m%d"), ".csv"))


# Process Data ----

# Load geodata
geodata <- load_geodata_file(filename = geodata_path)
# Process intervention data
npi_dat <- process_npi_shub(intervention_path = intervention_path,
                            geodata)

# Generate Intervention Df ----

## NPI
npi_dat <- set_npi_params(intervention_file = npi_dat,
                          sim_start_date = sim_start,
                          sim_end_date = sim_end,
                          redux_geoids = redux_geoids,
                          npi_cutoff_date=Sys.Date()-7,
                          inference = do_filtering,
                          v_dist = "truncnorm", v_mean=0.6, v_sd=0.05, v_a=0.0, v_b=0.9,
                          p_dist = "truncnorm", p_mean=0, p_sd=0.05, p_a=-1, p_b=1, 
                          compartment = run_compartment)
## Seasonality
seasonality_dat <- set_seasonality_params(sim_start_date = sim_start,
                                          sim_end_date = sim_end,
                                          inference = do_filtering,
                                          template = "MultiTimeReduce", # TODO: MTR for some, but not all... not critical
                                          v_dist="truncnorm",
                                          v_mean = c(-0.2, -0.133, -0.067, 0, 0.067, 0.133, 0.2, 0.133, 0.067, 0, -0.067, -0.133), # TODO function?
                                          v_sd = 0.05, v_a = -1, v_b = 1,
                                          p_dist="truncnorm",
                                          p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1, 
                                          compartment = run_compartment) %>%
    {if(exclude_apr_seasonality) dplyr::filter(., stringr::str_detect(name, "_apr", negate=TRUE)) else(.)}

## Local Variance
localvar_dat <- set_localvar_params(sim_start_date = sim_start,
                                    sim_end_date = sim_end,
                                    inference = do_filtering,
                                    v_dist="truncnorm",
                                    v_mean =  0, v_sd = 0.025, v_a = -1, v_b = 1, # TODO: add check on limits
                                    p_dist="truncnorm",
                                    p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1)

## Vaccination rates
vacc_dat <- set_vacc_rates_params(vacc_path = vaccination_path,
                                  sim_end_date = sim_end,
                                  vacc_start_date="2021-01-01",
                                  incl_geoid = state_incl_geoid, # TODO: add scenario filter similar to set_vacc_outcome_params
                                  scenario = vacc_scenario, 
                                  compartment = run_compartment
) %>%
    {if(state_level) . else(dplyr::mutate(., geoid = paste0(sort(geodata$geoid), collapse = '", "')))}

## Variants
if(!run_compartment){
    variant_dat <- set_variant_params(variant_path = variant_path_1,
                                      variant_path_2 = variant_path_2,
                                      sim_start_date = sim_start,
                                      sim_end_date = sim_end,
                                      b117_only = b117_only,
                                      geodata = geodata,
                                      state_level = state_level, 
                                      v_sd = 0.01,
                                      transmission_increase = variant_transmission_increase,
                                      v_a = -1.5,
                                      v_b = 0, 
                                      inference = do_filtering)
}


## Redux interventions
if(add_redux){
    redux_dat <- set_redux_params(npi_file = npi_dat,
                                  projection_start_date = Sys.Date(), 
                                  redux_end_date=NULL,
                                  redux_level = 0.5,
                                  v_dist = "truncnorm", 
                                  v_mean=0.8,
                                  v_sd=0.01,
                                  v_a=0,
                                  v_b=1, 
                                  compartment = run_compartment)
}

# Outcome interventions

## Vaccination impact on outcomes
outcome_dat <- set_vacc_outcome_params(outcome_path = outcomes_path,
                                       sim_start_date = sim_start,
                                       sim_end_date = sim_end,
                                       inference = FALSE,
                                       incl_geoid = state_incl_geoid,
                                       scenario = vacc_scenario,
                                       v_dist="truncnorm",
                                       v_sd = 0.01, v_a = 0, v_b = 1,
                                       p_dist="truncnorm",
                                       p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1) %>%
    dplyr::filter(., stringr::str_detect(name, "incidH", negate=TRUE))  %>%
    {if(state_level) . else(dplyr::mutate(., geoid = paste0(sort(geodata$geoid), collapse = '", "')))}

## Hospitalization adjustments
if(hosp_adjustment){
    hosp_dat <- set_incidH_adj_params(outcome_path = hosp_path,
                                      sim_start_date=sim_start,
                                      sim_end_date=sim_end,
                                      geodata=geodata,
                                      inference = FALSE,
                                      v_dist = "fixed", v_sd = 0.01, v_a = -10, v_b = 2,
                                      p_dist = "truncnorm", p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1)
}

## IncidC Shift

if(add_incidC){
    incidC_dat <- set_incidC_shift(periods = incidC_shift_periods, 
                                   geodata = geodata, 
                                   baseline_ifr = 0.005,
                                   cfr_data = incidC_shift_path,
                                   epochs = incidC_shift_epochs,
                                   outcomes_parquet_file = outcomes_parquet_file,
                                   inference = do_filtering,
                                   v_dist="truncnorm",
                                   v_mean=incidC_shift_value_mean, v_sd = 0.05, v_a = 0, v_b = 1,
                                   p_dist="truncnorm",
                                   p_mean = 0, p_sd = incidC_shift_pert_sd, p_a = -1, p_b = 1)
}

## VE Shift

if(VE_shift){
    ve_shift_dat <- set_ve_shift_params(variant_path = variant_path_2,
                                        VE =VE,
                                        VE_delta = VE_delta,
                                        sim_start_date=sim_start,
                                        sim_end_date=sim_end,
                                        geodata,
                                        inference = FALSE,
                                        v_dist = "fixed", v_sd = 0.01, v_a = -1, v_b = 2,
                                        p_dist = "truncnorm", p_mean = 0, p_sd = 0.01, p_a = -1, p_b = 1,
                                        compartment = run_compartment)
}

# Bind and save df

interventions <- mget(objects(pattern = "_dat$")) %>%
    bind_interventions(.,
                       sim_start_date = sim_start,
                       sim_end_date = sim_end,
                       save_name = save_config_data)
# 
# daily_mean_reduction(interventions,
#                      plot=TRUE)

if(all_fixed){
    interventions <- interventions %>%
        dplyr::mutate(value_dist = "fixed")
    
    gamma_dist <- "fixed"
    incidC_prob_dist <- "fixed"
    incidC_prob_dist_pert <- "fixed"
    seeding_method = "PoissonDistributed"
}
# Print Config ----

sink(config_name)

print_header(sim_name = sim_name,
             sim_start_date = sim_start,
             sim_end_date = sim_end,
             n_simulations = n_simulations,
             dt = 0.025,
             census_year = 2019,
             base_path = base_path,
             sim_states = unique(interventions$USPS[!interventions$USPS %in% c("", "all") & !is.na(interventions$USPS)]),
             setup_name,
             geodata = geodata_file,
             mobility = mobility_file,
             popnodes = "pop2019est",
             nodenames = "geoid",
             include_in_report = "include_in_report",
             state_level = state_level
)

print_seeding(method = seeding_method,
              variant_filename = variant_seeding_path,
              seeding_file_type = "seed",
              folder_path = "importation/minimal/",
              lambda_file = lambda_path,
              perturbation_sd = perturbation_sd, 
              compartment = run_compartment, 
              variant_compartments = variant_compartments)

print_seir(alpha = 0.99,
           sigma = 1/5.2,
           gamma_dist = gamma_dist,
           gamma_val = 1/3.83,
           gamma_a = 1/4.5, 
           gamma_b = 1/3,
           R0s_dist = "fixed",
           R0s_val = 2.3,
           incl_vacc = incl_vacc, 
           compartment = run_compartment)

print_interventions(interventions,
                    scenario = npi_scenario_name, 
                    compartment = run_compartment)

print_outcomes(dat = interventions,
               ifr = ifr,
               outcomes_parquet_file = outcomes_parquet_file,
               incidH_prob_dist=c("fixed", "fixed", "fixed"),
               incidH_prob_value=c(0.0175, 0.0175, 0.0175),
               incidH_delay_dist= c("fixed", "fixed", "fixed"),
               incidH_delay_value= c(7, 7, 7),
               incidH_duration_dist=c("fixed", "fixed", "fixed"),
               incidH_duration_value=c(7, 7, 7),
               incidD_prob_dist=c("fixed", "fixed", "fixed"),
               incidD_prob_value=incidD_prob_value,
               incidD_delay_dist=c("fixed", "fixed", "fixed"),
               incidD_delay_value=c(20, 20, 20),
               incidICU_prob_dist="fixed",
               incidICU_prob_value=0.167,
               incidICU_delay_dist="fixed",
               incidICU_delay_value=3,
               incidICU_duration_dist="fixed",
               incidICU_duration_value=8,
               incidVent_prob_dist="fixed",
               incidVent_prob_value=0.463,
               incidVent_delay_dist="fixed",
               incidVent_delay_value= 1,
               incidVent_duration_dist="fixed",
               incidVent_duration_value=7,
               incidC_prob_dist=incidC_prob_dist,
               incidC_prob_value=incidC_prob_value,
               incidC_prob_sd=c(.1, .1, .1),
               incidC_prob_a=c(0, 0, 0),
               incidC_prob_b=c(1, 1, 1),
               incidC_perturbation = incidC_perturbation, 
               incidC_prob_dist_pert=incidC_prob_dist_pert,
               incidC_prob_value_pert=c(0, 0, 0),
               incidC_prob_sd_pert=c(0.05, 0.05, 0.05),
               incidC_prob_a_pert=c(-1, -1, -1), 
               incidC_prob_b_pert=c(1, 1, 1),
               incidC_delay_value=c(7, 7, 7),
               incidC_delay_dist=c("fixed", "fixed", "fixed"), 
               compartment = run_compartment, 
               variant_compartments = variant_compartments, 
               vaccine_compartments = vaccine_compartments, 
               outcomes_included = c("incidH", "incidD", "incidC", "incidICU", "incidVent"))

print_filtering_statistics(sims_per_slot = sims_per_slot,
                           do_filtering = do_filtering,
                           data_path = data_path,
                           gt_source = "csse",
                           stat_names = c("sum_deaths", "sum_confirmed"),
                           variant_compartments = variant_compartments, 
                           aggregator = "sum",
                           period = "1 weeks",
                           sim_var = c("incidD", "incidC"),
                           data_var = filtering_data_vars,
                           remove_na = FALSE,
                           add_one = c(FALSE, TRUE),
                           ll_dist = c("sqrtnorm", "pois"),
                           ll_param = .4, 
                           compartment = run_compartment)

print_filtering_hierarchical(npi_name = c("local_variance", "probability_incidI_incidC"),
                             module = c("seir", "hospitalization"),
                             geo_group_col = "USPS",
                             transform = c("none", "logit"))

print_filtering_prior(dat = interventions,
                      npi_name = priors_name,
                      module = "seir",
                      dist = "normal",
                      param_mean = NULL,
                      param_sd = 1)   

sink()

