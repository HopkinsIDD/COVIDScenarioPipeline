#' Function to load US COVID data from JHUCSSE
#' @param data_path Path where to write the data
#' @param cache logical indicating whether to cache the data (default = TRUE)
#' @param gt_source string indicating source of ground truth data. options include "csse" or "usafacts" (default csse)
#' @param gt_scale string indicating whether "US county" or "US state"-level data
#'
#' @return NULL
#'
#' @export
get_ground_truth_file <- function(data_path, cache = TRUE, gt_source = "csse", gt_scale = "US county", gt_vars = c("Confirmed", "Deaths", "incidI", "incidDeath"), new_vars = gt_vars, fips_column_name = "geoid", date_column_name = "date") {
  data_dir <- dirname(data_path)

  if(!dir.exists(data_dir)){
    suppressWarnings(dir.create(data_dir,recursive=TRUE))
  }

  if (length(gt_vars) != length(new_vars)) {
    stop(paste(
      "groundtruth variables and new variables should have the same number of elements, got:",
      "(", paste(gt_vars, collapse = ", "), ")",
      "and",
      "(", paste(new_vars, collapse = ", "), ")"
    ))
  }

  if(!isTRUE(all.equal(gt_vars, new_vars))){
    warning("new_vars is deprecated, please adjust data_var to match gt_column_name")
  }

  if(!(file.exists(data_path) & cache)){
    message(paste("*** Loading Data from", gt_source, "\n"))
    cases_deaths <- suppressMessages(covidcommon::get_groundtruth_from_source(
      source = gt_source,
      scale = gt_scale,
      variables = gt_vars,
      incl_unass = ifelse(gt_scale == "US state", TRUE, FALSE)
    ))
    cases_deaths <- dplyr::arrange(
      dplyr::mutate(
        cases_deaths,
        Update = lubridate::ymd(Update)
      ),
      Update
    )

    gt_vars <- c("Update", "FIPS", gt_vars)
    new_vars <- c(date_column_name, fips_column_name, new_vars)

    if(!all(gt_vars %in% names(cases_deaths))) {
      stop(paste(
        "Could not find all expected names.  Looking for",
        "(", paste(gt_vars, collapse = ", "), ")",
        "found",
        "(", paste(names(cases_deaths), collapse = ", "), ")"
      ))
    }
    if(!all(names(cases_deaths) %in% gt_vars)) {
      warning(paste(
        "Found more than the expected names.  Looking for",
        "(", paste(gt_vars, collapse = ", "), ")",
        "found",
        "(", paste(names(cases_deaths), collapse = ", "), ")",
        "extra",
        "(", paste(names(cases_deaths)[!(names(cases_deaths) %in% gt_vars)], collapse = ", "), ")"
      ))
    }
    names(cases_deaths)[names(cases_deaths) %in% gt_vars] <-
      setNames(new_vars, gt_vars)[names(cases_deaths)[names(cases_deaths) %in% gt_vars]]

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
get_ground_truth <- function(
  data_path,
  fips_codes = NULL,
  fips_column_name = "geoid",
  date_column_name = "date",
  start_date = NULL,
  end_date = NULL,
  cache = TRUE,
  gt_source = "csse",
  gt_scale = "US county",
  gt_vars = c("Confirmed", "Deaths", "incidI", "incidDeath"),
  new_vars = gt_vars
) {

  get_ground_truth_file(
    data_path = data_path,
    cache = cache,
    gt_source = gt_source,
    gt_scale = gt_scale,
    gt_vars = gt_vars,
    new_vars = new_vars,
    fips_column_name = fips_column_name,
    date_column_name = date_column_name
  )


  rc <- suppressMessages(readr::read_csv(data_path,col_types = list(FIPS = readr::col_character())))


  if(is.null(start_date)) {
    start_date <- min(rc$date)
  }
  if(is.null(end_date)) {
    end_date <- max(rc$date)
  }
  if (is.null(fips_codes)) {
    fips_codes <- unique(rc$fips_codes)
  }

  rc <- dplyr::filter(
    rc,
    !!rlang::sym(fips_column_name) %in% fips_codes,
    !!rlang::sym(date_column_name) >= start_date,
    !!rlang::sym(date_column_name) <= end_date
  )
  rc <- dplyr::right_join(
    rc,
    tidyr::expand_grid(
      FIPS = unique(rc$FIPS),
      date = unique(rc$date)
    )
  )
  rc <- dplyr::mutate_if(rc,is.numeric,dplyr::coalesce,0)
  names(rc)[names(rc) == "FIPS"] <- fips_column_name
  return(rc)
}
