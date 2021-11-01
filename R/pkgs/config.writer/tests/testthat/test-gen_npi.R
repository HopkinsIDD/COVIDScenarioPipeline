
generate_processed <- function(geodata_path,
                               intervention_path,
                               sim_start,
                               sim_end,
                               vaccination_path,
                               variant_path_1,
                               variant_path_2,
                               vacc_scenario = 2,
                               outcomes_path
                               ){
    temp_name <- tempfile()
    geodata <- load_geodata_file(filename = geodata_path)

    npi_dat <- process_npi_shub(intervention_path = intervention_path,
                                geodata)

    npi_dat <- set_npi_params(intervention_file = npi_dat,
                              sim_start_date = sim_start,
                              sim_end_date = sim_end,
                              npi_cutoff_date=as.Date("2021-06-20"),
                              inference = TRUE,
                              v_dist = "truncnorm", v_mean=0.6, v_sd=0.05, v_a=0.0, v_b=0.9,
                              p_dist = "truncnorm", p_mean=0, p_sd=0.05, p_a=-1, p_b=1,
                              compartment = FALSE)

    seasonality_dat <- set_seasonality_params(sim_start_date = sim_start,
                                              sim_end_date = sim_end,
                                              inference = TRUE,
                                              template = "MultiTimeReduce",
                                              v_dist="truncnorm",
                                              v_mean = c(-0.2, -0.133, -0.067, 0, 0.067, 0.133, 0.2, 0.133, 0.067, 0, -0.067, -0.133),
                                              v_sd = 0.05, v_a = -1, v_b = 1,
                                              p_dist="truncnorm",
                                              p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1,
                                              compartment = FALSE) %>%
        dplyr::filter(stringr::str_detect(name, "_apr", negate=TRUE))

    localvar_dat <- set_localvar_params(sim_start_date = sim_start,
                                        sim_end_date = sim_end,
                                        inference = TRUE,
                                        v_dist="truncnorm",
                                        v_mean =  0, v_sd = 0.025, v_a = -1, v_b = 1,
                                        p_dist="truncnorm",
                                        p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1, compartment = FALSE)

    vacc_dat <- set_vacc_rates_params(vacc_path = vaccination_path,
                                      sim_end_date = sim_end,
                                      vacc_start_date="2021-01-01",
                                      incl_geoid = NULL,
                                      scenario = vacc_scenario,
                                      compartment = FALSE)

    variant_dat <- set_variant_params(variant_path = variant_path_1,
                                      variant_path_2 = variant_path_2,
                                      sim_start_date = sim_start,
                                      sim_end_date = sim_end,
                                      inference_cutoff_date = as.Date("2021-06-15"),
                                      compartment = FALSE, 
                                      b117_only = FALSE,
                                      state_level = FALSE,
                                      v_sd = 0.01,
                                      transmission_increase = 0.6,
                                      v_a = -1.5,
                                      v_b = 0)

    outcome_dat <- set_vacc_outcome_params(outcome_path = outcomes_path,
                                           sim_start_date = sim_start,
                                           sim_end_date = sim_end,
                                           inference = FALSE,
                                           incl_geoid = NULL,
                                           scenario = vacc_scenario,
                                           v_dist="truncnorm",
                                           v_sd = 0.01, v_a = 0, v_b = 1,
                                           p_dist="truncnorm",
                                           p_mean = 0, p_sd = 0.05, p_a = -1, p_b = 1,
                                           compartment = FALSE) %>%
        dplyr::filter(stringr::str_detect(name, "incidH", negate=TRUE))

    interventions <- bind_interventions(npi_dat,
                                        seasonality_dat,
                                        localvar_dat,
                                        vacc_dat,
                                        variant_dat,
                                        outcome_dat,
                                        sim_start_date = sim_start,
                                        sim_end_date = sim_end,
                                        save_name = temp_name)

    temp_name <- readr::read_csv(temp_name)

    return(temp_name)
}


test_that("Interventions processing works", {
    test_interventions <- generate_processed(geodata_path = "geodata.csv",
                                        intervention_path = "intervention.csv",
                                        sim_start = "2020-01-01",
                                        sim_end = "2021-08-07",
                                        vaccination_path = "vacc_rates.csv",
                                        variant_path_1 = "var1_fits.csv",
                                        variant_path_2 = "var2_fits.csv",
                                        vacc_scenario = 2,
                                        outcomes_path = "outcome_adj.csv")

    interventions <- readr::read_csv("processed_intervention_data.csv") %>%
        dplyr::filter(USPS %in% c("all", "KS", "DE", "") | geoid == "all") %>%
        dplyr::mutate(dplyr::across(pert_mean:pert_b,
                                    ~ifelse(stringr::str_detect(name, "variant") & start_date < as.Date("2021-06-15") |
                                                stringr::str_detect(name, "variant", negate = TRUE) , .x, NA_real_)),
                      pert_dist = ifelse(stringr::str_detect(name, "variant") & start_date < as.Date("2021-06-15") |
                                             stringr::str_detect(name, "variant", negate = TRUE) , pert_dist, NA_character_))

    expect_equal(
        test_interventions,
        interventions
    )

})

