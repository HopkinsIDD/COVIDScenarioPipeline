
## The purpose of this code is to take in determine how air importations to a specific US region will be distributed to surrounding counties to seed the SEIR epidemic model. 

airport_estimation <- function(
    states_of_interest=c("CA","NV","WA","OR","AZ"),
    regioncode="west-coast-AZ-NV",
    yr=2010,
    airport_codes_csv_file,
    airport_monthly_mean_travelers_csv_file,
    travelers_threshold=60000, ## airports must meet this average number of monthly travelers to be included
    airport_cluster_threshold=160, # units: km. Airports that are separated by Haversine distance
    nb_params_nocluster,
    local_dir="data/",
    plot=FALSE) {

  states_of_interest <- sort(states_of_interest)
  county_pops_shp_path <- get_county_pops(states_of_interest, regioncode, yr, local_dir=local_dir)
  airports_to_consider <- get_airports_to_consider(airport_codes_csv_file, airport_monthly_mean_travelers_csv_file, states_of_interest, travelers_threshold)
  airport_attribution <- do_airport_attribution(airports_to_consider,
                                                airport_cluster_threshold,
                                                shapefile_path,
                                                regioncode,
                                                yr=yr,
                                                local_dir=local_dir,
                                                plot=plot)
  nb_params <- update_nb_parameters(nb_params_no_cluster, airport_attribution, regioncode, local_dir=local_dir)
  return(nb_params)
}
