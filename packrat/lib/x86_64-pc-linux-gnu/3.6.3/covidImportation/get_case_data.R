
#Required package
devtools::install_github("HopkinsIDD/globaltoolboxlite")
#Installation
devtools::install_github("HopkinsIDD/covidImportation")

library(covidImportation)


## ~ Incidence data
incid_data_list <- get_incidence_data(first_date = ISOdate(2019,12,1),
                                      last_date = Sys.time(),
                                      update_case_data = TRUE,
                                      case_data_dir = "data/case_data",
                                      check_saved_data = TRUE,
                                      save_data = TRUE)

#Smoothed data -- currently smoothed for Chinese Province, US State, and otherwise country
incid_data <- incid_data_list$incid_data %>% dplyr::filter(source != "USA")
incid_data <- incid_data %>% rename(incid_est = cases_incid)

#Cleaned JHU CSSE data
jhucsse <- incid_data_list$jhucsse_case_data

#Cleaned JHU CSSE data, with cumulative cases and incident cases aggregated to State level for the US
jhucsse_state <- incid_data_list$jhucsse_case_data_state



# To look at the functions doing smoothing within the `get_incidence_data` function, look at:
# -- est_daily_incidence_corrected()

