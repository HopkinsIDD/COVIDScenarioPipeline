library(covidImportation)
#debugonce(setup_and_run_importations)
t1 <- Sys.time()
WC_import <- setup_and_run_importations(dest="AZ",# c("CA","OR","WA","NV","AZ"),
                           dest_type=c("state"), #,"city","airport", "country"),
                           dest_country="USA",
                           dest_aggr_level=c("airport"), #, "city", "state", "country", "metro"),
                           project_name="WestCoast_import",
                           first_date = ISOdate(2019,12,1),
                           last_date = Sys.time(),
                           update_case_data=TRUE,
                           case_data_dir = "data/case_data",
                           check_saved_data=TRUE,
                           save_case_data=TRUE,
                           get_travel=TRUE,
                           n_sim=100,
                           cores=5,
                           n_top_dests=25,
                           get_detection_time=FALSE,
                           travel_dispersion=3,
                           allow_travel_variance=FALSE,
                           print_progress=TRUE,
                           param_list=list(incub_mean_log=log(5.89),
                                           incub_sd_log=log(1.74),
                                           inf_period_nohosp_mean=15,
                                           inf_period_nohosp_sd=5,
                                           inf_period_hosp_shape=0.75,
                                           inf_period_hosp_scale=5.367,
                                           p_report_source=c(0.05, 0.25),
                                           shift_incid_days=-10,
                                           delta=1))
print(paste0('Simulation required ', round(as.list(proc.time() - t1)$elapsed/60, 3), ' minutes'))

save(WC_import, "output/WC_import_100sims.rda")

(size_ <- format(object.size(WC_import), "Mb"))

nb_pars <- calc_nb_import_pars(WC_import$importation_sims$importation_sim, cores=6)
    
importation_sim <- UT_import$importation_sims$importation_sim















dest=c("CA","OR","WA","NV")
dest_type=c("state") #,"city","airport", "country"),
dest_country="USA"
dest_aggr_level=c("airport") #, "city", "state", "country", "metro"),
project_name="WestCoast_import"
version="global"
batch="1st"
first_date = ISOdate(2019,12,1)
last_date = Sys.time()
update_case_data=TRUE
case_data_dir = "data/case_data"
check_saved_data=TRUE
save_case_data=TRUE
get_travel=TRUE
end_date=Sys.Date()
n_sim=10
cores=4
get_detection_time=FALSE
travel_dispersion=3
allow_travel_variance=FALSE
print_progress=TRUE
get_nb_params = TRUE
param_list=list(incub_mean_log=log(5.89),
                incub_sd_log=log(1.74),
                inf_period_nohosp_mean=15,
                inf_period_nohosp_sd=5,
                inf_period_hosp_shape=0.75,
                inf_period_hosp_scale=5.367,
                p_report_source=c(0.05, 0.25),
                shift_incid_days=-10,
                delta=1)






test1 <- run_daily_import_model(input_data,
                                   travel_data_monthly,
                                   travel_data_daily,
                                   travel_dispersion=3,
                                   travel_restrictions=NULL,
                                   allow_travel_variance=FALSE,
                                   tr_inf_redux=0,
                                   get_detection_time=FALSE,
                                   time_inftotravel,
                                   time_inftodetect,
                                   project_name=NULL, batch=NULL, version=NULL,
                                   output_dir = file.path("output", paste0(paste(dest, collapse="+"),"_", as.Date(Sys.Date()))),
                                   param_list=list(incub_mean_log=log(5.89),
                                                   incub_sd_log=log(1.74),
                                                   inf_period_nohosp_mean=15,
                                                   inf_period_nohosp_sd=5,
                                                   inf_period_hosp_shape=0.75,
                                                   inf_period_hosp_scale=5.367))


















# testing
library(covidImportation)

incid_data <- get_incidence_data(first_date = ISOdate(2019,12,1),
                                 last_date = Sys.time(),
                                 update_case_data=TRUE,
                                 case_data_dir = "data/case_data",
                                 check_saved_data=TRUE,
                                 save_data=TRUE)



covidImportation:::update_JHUCSSE_github_data(last_date = Sys.time(),
                                              case_data_dir = "data/case_data",
                                              check_saved_data=TRUE,
                                              save_data=TRUE)





















# TEST NEW VERSION --------------------------------------------------------


library(covidImportation)

# First run the setup

dest <- "UT"
dest=c("CA","OR","WA","NV","AZ")
setup_res <- covidImportation::setup_importations(dest=dest,
                               dest_type=c("state"), #,"city","airport", "country"),
                               dest_country="USA",
                               dest_aggr_level=c("airport"), #, "city", "state", "country", "metro"),
                               first_date = ISOdate(2019,12,1),
                               last_date = Sys.time(),
                               update_case_data=TRUE,
                               case_data_dir = "data/case_data",
                               output_dir = file.path("output", paste0(paste(dest, collapse="+"),"_", as.Date(Sys.Date()))),
                               check_saved_data=TRUE,
                               save_case_data=TRUE,
                               get_travel=TRUE,
                               n_top_dests=Inf, 
                               travel_dispersion=3,
                               param_list=list(incub_mean_log=log(5.89),
                                               incub_sd_log=log(1.74),
                                               inf_period_nohosp_mean=15,
                                               inf_period_nohosp_sd=5,
                                               inf_period_hosp_shape=0.75,
                                               inf_period_hosp_scale=5.367,
                                               p_report_source=c(0.05, 0.25),
                                               shift_incid_days=-10,
                                               delta=1))


sim_res <- covidImportation::run_importations(
                             n_sim=10,
                             cores=5,
                             get_detection_time=FALSE,
                             travel_dispersion=3,
                             allow_travel_variance=FALSE,
                             print_progress=TRUE,
                             output_dir = file.path("output", paste0(paste(dest, collapse="+"),"_", as.Date(Sys.Date()))),
                             param_list=list(incub_mean_log=log(5.89),
                                             incub_sd_log=log(1.74),
                                             inf_period_nohosp_mean=15,
                                             inf_period_nohosp_sd=5,
                                             inf_period_hosp_shape=0.75,
                                             inf_period_hosp_scale=5.367,
                                             p_report_source=c(0.05, 0.25)))
