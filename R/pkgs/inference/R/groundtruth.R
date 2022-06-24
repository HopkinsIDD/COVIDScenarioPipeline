#' Function to load US COVID data from JHUCSSE
#' @param data_path Path where to write the data
#' @param cache logical indicating whether to cache the data (default = TRUE)
#' @param gt_source string indicating source of ground truth data. options include "csse" or "usafacts" (default csse)
#' @param gt_scale string indicating whether "US county" or "US state"-level data
#'
#' @return NULL
#'
#' @export
get_ground_truth_file <- function(data_path, cache = TRUE, gt_source = "csse", gt_scale = "US county", variant_filename = "data/variant/variant_props_long.csv") {
  data_dir <- dirname(data_path)
  if(!dir.exists(data_dir)){
    suppressWarnings(dir.create(data_dir,recursive=TRUE))
  }
  if(!(file.exists(data_path) & cache)){
    message(paste("*** Loading Data from", gt_source, "\n"))
    cases_deaths <- suppressMessages(covidcommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale, variables = c("Confirmed", "Deaths", "incidI", "incidDeath"), incl_unass = ifelse(gt_scale == "US state", TRUE, FALSE), adjust_for_variant = !is.null(variant_filename), variant_props_file = variant_filename))
    cases_deaths  <- dplyr::arrange(
      dplyr::rename(
        dplyr::mutate(
          cases_deaths,
          Update = lubridate::ymd(Update)
        ),
        date = Update,
      ),
      date,
      FIPS
    )
    readr::write_csv(cases_deaths, data_path)
    rm(cases_deaths)
    message("*** DONE Loading Data \n")
  } else {
    message("*** USING CACHED Data\n")
  }

  return()
}

#' Function to load US COVID data from USAfacts
#' @param data_path Path where to write the data
#'
#' @export
get_ground_truth <- function(data_path, fips_codes, fips_column_name, start_date, end_date, 
                             cache = TRUE, 
                             gt_source = "csse", gt_scale = "US county", 
                             variant_filename = "data/variant/variant_props_long.csv"){

  get_ground_truth_file(data_path = data_path, cache = cache, 
                        gt_source = gt_source, gt_scale = gt_scale, 
                        variant_filename = variant_filename)

  rc <- suppressMessages(readr::read_csv(
    data_path,
    col_types = list(FIPS = readr::col_character()),
  ))
  rc <- dplyr::filter(
    rc,
    FIPS %in% fips_codes,
    date >= start_date,
    date <= end_date
  )
  rc <- dplyr::right_join(
    rc,
    tidyr::expand_grid(
      FIPS = unique(rc$FIPS),
      date = unique(rc$date)
    )
  )
  rc <- rc %>% dplyr::arrange(FIPS, source, date)
  rc <- dplyr::mutate_if(rc, is.numeric, dplyr::coalesce, 0)
  names(rc)[names(rc) == "FIPS"] <- fips_column_name
  return(rc)
}
