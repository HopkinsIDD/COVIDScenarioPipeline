

# TEST NEW VERSION --------------------------------------------------------


library(covidImportation)

# First run the setup

dest <- "UT"
dest=c("CA","OR","WA","NV","AZ")
dest <- state.abb
setup_res <- covidImportation::setup_importations(dest=dest,
                               dest_type=c("state"), #,"city","airport", "country"),
                               dest_country="USA",
                               dest_aggr_level=c("airport"), #, "city", "state", "country", "metro"),
                               first_date = ISOdate(2019,12,1),
                               last_date = ISOdate(2020,3,15),
                               update_case_data=TRUE,
                               case_data_dir = "data/case_data",
                               output_dir = file.path("output", paste0("fullUSA","_", as.Date(Sys.Date()))),
                               check_saved_data=TRUE,
                               save_case_data=TRUE,
                               get_travel=TRUE,
                               n_top_dests=Inf,
                               travel_dispersion=3,
                               param_list=list(incub_mean_log=log(5.89),
                                               incub_sd_log=log(1.74),
                                               inf_period_nohosp_mean=15,
                                               inf_period_nohosp_sd=5,
                                               inf_period_hosp_mean_log=1.23,
                                               inf_period_hosp_sd_log=0.79,
                                               p_report_source=c(0.05, 0.25),
                                               shift_incid_days=-10,
                                               delta=1))


sim_res <- covidImportation::run_importations(
                             n_sim=2,
                             cores=2,
                             get_detection_time=FALSE,
                             travel_dispersion=3,
                             allow_travel_variance=FALSE,
                             print_progress=TRUE,
                             output_dir = file.path("output", paste0("fullUSA","_", as.Date(Sys.Date()))),
                             param_list=list(incub_mean_log=log(5.89),
                                             incub_sd_log=log(1.74),
                                             inf_period_nohosp_mean=15,
                                             inf_period_nohosp_sd=5,
                                             inf_period_hosp_mean_log=1.23,
                                             inf_period_hosp_sd_log=0.79,
                                             p_report_source=c(0.05, 0.25)))
