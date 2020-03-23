## Change importation model parameters so that they match the airport attribution clusters

update_nb_parameters <- function(nb_params_nocluster, airport_attribution, regioncode, local_dir="data/") {
  air_cl <- airport_attribution # read_csv(paste0("data/", regioncode, "/airport_attribution_", yr, ".csv"))
  nb_orig <- nb_params_nocluster # read_csv(paste0("data/", regioncode, "/import_nb_params_nocluster.csv"))

  cl_names <- air_cl %>%
      dplyr::filter(nchar(airport_iata)>3) %>%
      distinct(airport_iata) %>% unlist %>% unname
  cl_names_ls <- lapply(1:length(cl_names), function(i) {
      unlist(strsplit(cl_names[i], "_"))
  })

  ## assumes perhaps incorrectly that mu1/size1 == mu2/size2
  nb_params_cluster <- purrr::map_dfr(1:length(cl_names_ls), function(i){
    nb_orig %>%
      dplyr::filter(airport %in% cl_names_ls[[i]]) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(airport = paste(airport, collapse = "_"), size = sum(size), mu = sum(mu)) %>% 
      dplyr::ungroup()
  })

  nb_params_tot <- nb_orig %>% 
      dplyr::filter(!(airport %in% unlist(purrr::flatten(cl_names_ls)))) %>%
      dplyr::bind_rows(nb_params_cluster) %>%
      dplyr::mutate(date = as.character(date))

  write_csv(nb_params_tot, paste0(local_dir, "/", regioncode, "/import_nb_params.csv"))
  return(nb_params_tot)
}
