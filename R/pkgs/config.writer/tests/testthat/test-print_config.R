generate_config <- function(){
    interventions <- readr::read_csv("processed_intervention_data.csv")

    config_name <- file.path(tempdir(), "config_test.yml")

    sink(config_name)

    print_header(sim_name = "USA",
                 sim_start_date = "2020-01-01",
                 sim_end_date = "2021-08-07",
                 dt = 0.25,
                 n_simulations = 300,
                 sim_states = unique(interventions$USPS[!interventions$USPS %in% c("", "all") & !is.na(interventions$USPS)]),
                 setup_name = "usa_inference_territories_statelevel",
                 geodata = "geodata_territories_2019_statelevel.csv",
                 mobility = "mobility_territories_2011-2015_statelevel.csv")

    print_seeding(lambda_file = "data/minimal/seeding_territories.csv",
                  perturbation_sd = 3,
                  compartment = FALSE)

    print_seir(gamma_dist = "uniform",
               gamma_a = 1/4.5,
               gamma_b = 1/3,
               R0s_dist = "fixed",
               R0s_val = 2.3,
               incl_vacc = TRUE,
               theta_1_val = 0.5,
               theta_1_dist = "fixed", 
               theta_2_val = 0.9,
               theta_2_dist = "fixed",
               nu_2_val = 0.04,
               compartment = FALSE,
               vaccine_compartments = c("unvaccinated", "first_dose", "second_dose"))

    print_interventions(interventions,
                        scenario = "inference",
                        compartment = FALSE)

    print_outcomes(dat = interventions,
                   ifr = "med",
                   outcomes_parquet_file="usa-geoid-params-output_statelevel.parquet",
                   incidC_prob_value = c(0.4, 0.4, 0.4),
                   compartment = FALSE)

    print_filtering_statistics(sims_per_slot = 1000,
                               compartment = FALSE,
                               data_var = c("death_incid", "confirmed_incid"))

    print_filtering_hierarchical(npi_name = c("local_variance", "probability_incidI_incidC"),
                                 module = c("seir", "hospitalization"),
                                 geo_group_col = "USPS",
                                 transform = c("none", "logit"),
                                 compartment = FALSE)

    print_filtering_prior(dat = interventions,
                          npi_name = c("local_variance", "Seas_jan", "Seas_feb", "Seas_mar",
                                       "Seas_may", "Seas_jun", "Seas_jul", "Seas_aug", "Seas_sep",
                                       "Seas_oct", "Seas_nov", "Seas_dec"),
                          param_mean = NULL)

    sink()

    config <- yaml::read_yaml(config_name)

    unlink(config_name)

    return(config)
}


test_that("Config generation works", {
    test_config <- generate_config()
    config <- yaml::read_yaml("sample_config.yml")

    expect_equal(
        test_config,
        config
    )

})

