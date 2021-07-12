library(config.writer)

# Default Params ----

option_list = list(
    optparse::make_option(c("-f", "--fixed"), action="store", type='logical', help="whether to set all intervention distributions to 'fixed'", default=FALSE),
    optparse::make_option(c("-d", "--do-filter"), action="store", type='logical', help="whether inference will be performed", default=TRUE),
    optparse::make_option(c("-p", "--path"), action="store", type='character', help = "path to data files", default = "data"),
    optparse::make_option(c("-c", "--config-data"), action="store", type='character', help = "name of config", default= "config.yml"),
    optparse::make_option(c("-g", "--geodata-file"), action="store", type='character', help="name geodata file", default = "geodata_territories_2019_statelevel.csv"),
    optparse::make_option(c("-v", "--vaccination-file"), action="store", type='character', help = "path to vaccination data", default = "vaccination/Round6/vacc_rates_ROUND6.csv"),
    optparse::make_option(c("-o", "--outcomes-file"), action="store", type='character', help = "path to outcome intervention data", default = "vaccination/Round6/outcome_adj_allscenarios_ROUND6.csv"),
    optparse::make_option(c("-i", "--intervention-file"), action="store", type='character', help = "path to npi intervention data", default = "intervention_tracking/Shelter-in-place-as-of-06252021.csv"),
    optparse::make_option(c("-s", "--state-level"), action="store", type='logical', help = "whether state-level run", default = TRUE), 
    optparse::make_option(c("--var-1"), action="store", type='character', help = "path to fit for variant 1", default = "variant/B117-fits.csv"),
    optparse::make_option(c("--var-2"), action="store", type='character', help = "path to fit for variant 2", default = "variant/B617-fits.csv"), 
    optparse::make_option(c("--params-output-data"), action="store", type='character', help = "path to params outcomes parquet file", default = "usa-geoid-params-output_statelevel.parquet")
    
)

# option_list = list(
#     optparse::make_option(c("-c", "--config-path"), action="store", type='character', help = "name of and path to config", default= config_path),
#     optparse::make_option(c("-g", "--geodata-path"), action="store", type='character', help="path to geodata file", default = geodata_path),
#     optparse::make_option(c("-v", "--vaccination-path"), action="store", type='character', help = "path to vaccination data", default = vaccination_path),
#     optparse::make_option(c("-o", "--outcomes-path"), action="store", type='character', help = "path to outcome intervention data", default = outcomes_path),
#     optparse::make_option(c("-i", "--intervention-path"), action="store", type='character', help = "path to npi intervention data", default = intervention_path),
#     optparse::make_option(c("-s", "--state-level"), action="store", type='logical', help = "whether state-level run", default = state_level), 
#     optparse::make_option(c("--var-1"), action="store", type='character', help = "path to fit for variant 1", default = variant_path_1),
#     optparse::make_option(c("--var-2"), action="store", type='character', help = "path to fit for variant 2", default = variant_path_2), 
#     optparse::make_option(c("--params-output-data"), action="store", type='character', help = "path to params outcomes parquet file", default = outcomes_parquet_file)
#     
# )

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

## Data Files/Processing
run_type <- "fchub" # used to save processed intervention data (e.g. data/config_data/fchub_20210702.csv)
config_name <- opt$`config-data`
geodata_path <- file.path(opt$path, opt$`geodata-file`)
vaccination_path <- file.path(opt$path, opt$`vaccination-file`) 
outcomes_path <- file.path(opt$path, opt$`outcomes-file`)
vacc_scenario <- 2 # Scenario number from the vacc_path and outcomes_path files 

intervention_path <- file.path(opt$path, opt$`intervention-file`)

variant_path_1 <- file.path(opt$path, opt$`var-1`)
variant_path_2 <- file.path(opt$path, opt$`var-2`)
b117_only <- FALSE 
variant_transmission_increase <- 0.6 # increase in transmission in delta vs 117

## Handy Config Settings 
    ## Header
    sim_name <- "USA"
    sim_start <- "2020-01-01"
    sim_end <- "2021-08-30"
    geodata_file <- opt$`geodata-file`
    mobility_file <- "mobility_territories_2011-2015_statelevel.csv"
    setup_name <- "test"
    state_level <- opt$`state-level`
    n_simulations <- 1
    ## Seeding
    seeding_method = "FolderDraw"
    lambda_file <- "data/seeding.csv"
    perturbation_sd <- 3
    #SEIR
    gamma_dist <- "uniform"
    ## Interventions
    npi_scenario_name <- "inference"
    exclude_apr_seasonality <- TRUE
    ## Outcomes
    ifr = "med"
    outcomes_parquet_file=opt$`params-output-data`
    incidD_prob_value = 0.005
    incidC_prob_value = 0.4
    incidC_prob_dist = "truncnorm"
    incidC_prob_dist_pert = "truncnorm"
    incidC_perturbation = opt$`do-filter`
    ## Filtering
    do_filtering = opt$`do-filter`
    sims_per_slot = 1
    data_path = file.path(opt$path, "us_data.csv")

# Other
save_config_data <- file.path(opt$path, "processed_interventions.csv")
priors_name <- c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar", "Seas_apr",
                 "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
                 "Seas_oct", "Seas_nov", "Seas_dec")
priors_name <- {if(exclude_apr_seasonality) priors_name[priors_name!="Seas_apr"] else(priors_name)}

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
                              redux_geoids = "all",
                              npi_cutoff_date=Sys.Date()-7,
                              inference = TRUE,
                              v_dist = "truncnorm", v_mean=0.6, v_sd=0.05, v_a=0.0, v_b=0.9,
                              p_dist = "truncnorm", p_mean=0, p_sd=0.05, p_a=-1, p_b=1)
    ## Seasonality
    seasonality_dat <- set_seasonality_params(sim_start_date = sim_start,
                                              sim_end_date = sim_end,
                                              inference = TRUE,
                                              template = "MultiTimeReduce", # TODO: MTR for some, but not all... not critical
                                              v_dist="truncnorm",
                                              v_mean = c(-0.2, -0.133, -0.067, 0, 0.067, 0.133, 0.2, 0.133, 0.067, 0, -0.067, -0.133), # TODO function?
                                              v_sd = 0.05, v_a = -1, v_b = 1,
                                              p_dist="truncnorm",
                                              p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1) %>%
        {if(exclude_apr_seasonality) dplyr::filter(., stringr::str_detect(name, "_apr", negate=TRUE)) else(.)}

    ## Local Variance
    localvar_dat <- set_localvar_params(sim_start_date = sim_start,
                                        sim_end_date = sim_end,
                                        inference = TRUE,
                                        v_dist="truncnorm",
                                        v_mean =  0, v_sd = 0.025, v_a = -1, v_b = 1, # TODO: add check on limits
                                        p_dist="truncnorm",
                                        p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1)

    ## Vaccination rates
    vacc_dat <- set_vacc_rates_params(vacc_path = vaccination_path,
                                      sim_end_date = sim_end,
                                      vacc_start_date="2021-01-01",
                                      incl_geoid = NULL, # TODO: add scenario filter similar to set_vacc_outcome_params
                                      scenario = vacc_scenario
                                      )

    ## Variants
    variant_dat <- set_variant_params(variant_path = variant_path_1,
                                      variant_path_2 = variant_path_2,
                                      sim_start_date = sim_start,
                                      sim_end_date = sim_end,
                                      b117_only,
                                      v_sd = 0.01,
                                      transmission_increase = variant_transmission_increase,
                                      v_a = -1.5,
                                      v_b = 0)
    
    ## Redux interventions
    redux_dat <- set_redux_params(npi_file = npi_dat,
                                  projection_start_date = Sys.Date(), 
                                  redux_end_date=NULL,
                                  redux_level = 0.5,
                                  v_dist = "truncnorm", 
                                  v_mean=0.8,
                                  v_sd=0.01,
                                  v_a=0,
                                  v_b=1)

    # Outcome interventions

    ## Vaccination impact on outcomes
    outcome_dat <- set_vacc_outcome_params(outcome_path = outcomes_path,
                                           sim_start_date = sim_start,
                                           sim_end_date = sim_end,
                                           inference = FALSE,
                                           incl_geoid = NULL,
                                           scenario = vacc_scenario,
                                           v_dist="truncnorm",
                                           v_sd = 0.01, v_a = 0, v_b = 1,
                                           p_dist="truncnorm",
                                           p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1) %>%
        dplyr::filter(stringr::str_detect(name, "incidH", negate=TRUE))
    
    ## IncidC Shift

    incidC_dat <- set_incidC_shift(startdate = as.Date("2020-06-30"),
                                   enddate = sim_end, # TODO: allow specific geoids
                                   inference = TRUE,
                                   v_dist="truncnorm",
                                   v_mean=0.07, v_sd = 0.05, v_a = 0, v_b = 1,
                                   p_dist="truncnorm",
                                   p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1)
    
    # Bind and save df

    interventions <- mget(objects(pattern = "_dat$")) %>%
        bind_interventions(
            # npi_dat,
            #             seasonality_dat,
            #             localvar_dat,
            #             redux_dat,
            #             vacc_dat,
            #             variant_dat,
            #             outcome_dat,
                       # incidC_dat,
                        sim_start_date = sim_start,
                        sim_end_date = sim_end,
                        save_name = save_config_data)
    
    daily_mean_reduction(interventions,
                         plot=TRUE)
    
    if(opt$fixed){
        interventions <- interventions %>%
            dplyr::mutate(value_dist = "fixed",
                          dplyr::across(tidyselect::starts_with("pert_"), ~NA))
        
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
                dt = 0.25,
                census_year = 2019,
                base_path = "data",
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
                 seeding_file_type = "seed",
                 folder_path = "importation/minimal/",
                 lambda_file = lambda_file,
                 perturbation_sd = perturbation_sd)

    print_seir(alpha = 0.99,
              sigma = 1/5.2,
              gamma_dist = gamma_dist,
              gamma_val = 1/3.83,
              gamma_a = 1/4.5, 
              gamma_b = 1/3,
              R0s_dist = "fixed",
              R0s_val = 2.3,
              incl_vacc = TRUE,
              dose_transmission_dist = c("fixed","fixed", "fixed"),
              dose_transmission_val = c(0, 0, 0),
              dose_susceptibility_dist = c("fixed","fixed", "fixed"),
              dose_susceptibility_val = c(0, 0.5, 0.90),
              transitions_dist = c("fixed", "fixed"),
              transitions_val = c(0, 0.04))

    print_transmission_interventions(interventions,
                                     scenario = npi_scenario_name)

    print_outcomes(dat = interventions,
                  ifr = ifr,
                  outcomes_parquet_file = outcomes_parquet_file,
                  incidH_prob_dist="fixed",
                  incidH_prob_value=0.0175,
                  incidH_delay_dist="fixed",
                  incidH_delay_value=7,
                  incidH_duration_dist="fixed",
                  incidH_duration_value=7,
                  incidD_prob_dist="fixed",
                  incidD_prob_value=incidD_prob_value,
                  incidD_delay_dist="fixed",
                  incidD_delay_value=20,
                  incidICU_prob_dist="fixed",
                  incidICU_prob_value=0.167,
                  incidICU_delay_dist="fixed",
                  incidICU_delay_value=3,
                  incidICU_duration_dist="fixed",
                  incidICU_duration_value=8,
                  incidVent_prob_dist="fixed",
                  incidVent_prob_value=0.463,
                  incidVent_delay_dist="fixed",
                  incidVent_delay_value=1,
                  incidVent_duration_dist="fixed",
                  incidVent_duration_value=7,
                  incidC_prob_dist=incidC_prob_dist,
                  incidC_prob_value=incidC_prob_value,
                  incidC_prob_sd=.1,
                  incidC_prob_a=0,
                  incidC_prob_b=1,
                  incidC_perturbation = incidC_perturbation, 
                  incidC_prob_dist_pert=incidC_prob_dist_pert,
                  incidC_prob_value_pert=0,
                  incidC_prob_sd_pert=0.05,
                  incidC_prob_a_pert=-1,
                  incidC_prob_b_pert=1,
                  incidC_delay_value=7,
                  incidC_delay_dist="fixed")
    
        print_filtering_statistics(sims_per_slot = sims_per_slot,
                                   do_filtering = do_filtering,
                                   data_path = data_path,
                                   gt_source = "csse",
                                   stat_names = c("sum_deaths", "sum_confirmed"),
                                   aggregator = "sum",
                                   period = "1 weeks",
                                   sim_var = c("incidD", "incidC"),
                                   data_var = c("death_incid", "confirmed_incid"),
                                   remove_na = FALSE,
                                   add_one = c(FALSE, TRUE),
                                   ll_dist = c("sqrtnorm", "pois"),
                                   ll_param = .4)
        
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

