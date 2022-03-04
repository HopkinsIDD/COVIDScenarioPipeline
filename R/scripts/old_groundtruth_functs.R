download_CSSE_US_data_OLD <- function (filename, url, value_col_name, incl_unassigned = FALSE) 
{
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    message(paste("Downloading", url, "to", filename))
    download.file(url, filename, "auto")
    csse_data <- readr::read_csv(filename, col_types = list(FIPS = readr::col_character())) %>% 
        tibble::as_tibble()
    if (incl_unassigned) {
        csse_data <- dplyr::filter(csse_data, !grepl("out of", 
                                                     Admin2, ignore.case = TRUE) & !grepl("princess", 
                                                                                          Province_State, ignore.case = TRUE) & !is.na(FIPS))
    }
    else {
        csse_data2 <- dplyr::filter(csse_data, !grepl("out of", 
                                                      Admin2, ignore.case = TRUE), !grepl("unassigned", 
                                                                                          Admin2, ignore.case = TRUE), !grepl("princess", 
                                                                                                                              Province_State, ignore.case = TRUE), !is.na(FIPS))
        pr_unassigned <- dplyr::filter(csse_data, grepl("unassigned", 
                                                        Admin2, ignore.case = TRUE), Province_State == "Puerto Rico")
        csse_data <- dplyr::bind_rows(csse_data2, pr_unassigned)
    }
    csse_data <- tidyr::pivot_longer(csse_data, cols = dplyr::contains("/"), 
                                     names_to = "Update", values_to = value_col_name) %>% 
        dplyr::mutate(Update = as.Date(lubridate::mdy(Update)), 
                      FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), 
                                                  ""), FIPS = ifelse(stringr::str_length(FIPS) <= 
                                                                         2, paste0(FIPS, "000"), stringr::str_pad(FIPS, 
                                                                                                                  5, pad = "0", side = "left")), FIPS = ifelse(stringr::str_sub(FIPS, 
                                                                                                                                                                                1, 3) == "900", paste0(stringr::str_sub(FIPS, 
                                                                                                                                                                                                                        4, 5), "000"), FIPS)) %>% dplyr::filter(as.Date(Update) <= 
                                                                                                                                                                                                                                                                    as.Date(Sys.time())) %>% dplyr::distinct()
    csse_data <- suppressWarnings(dplyr::mutate(csse_data, state_abb = state.abb[match(Province_State, 
                                                                                       state.name)]))
    csse_data <- suppressWarnings(dplyr::mutate(csse_data, source = ifelse(Province_State == 
                                                                               "District of Columbia", "DC", ifelse(is.na(state_abb) & 
                                                                                                                        Country_Region == "US", iso2, state_abb))))
    csse_data <- dplyr::select(csse_data, FIPS, source, Update, 
                               !!value_col_name)
    validation_date <- Sys.getenv("VALIDATION_DATE")
    if (validation_date != "") {
        print(paste("(DataUtils.R) Limiting CSSE US data to:", 
                    validation_date, sep = " "))
        csse_data <- dplyr::filter(csse_data, Update < validation_date)
    }
    return(csse_data)
}





get_CSSE_US_data_OLD <- function (case_data_filename = "data/case_data/jhucsse_us_case_data_crude.csv", 
                                  death_data_filename = "data/case_data/jhucsse_us_death_data_crude.csv", 
                                  incl_unassigned = FALSE, fix_negatives=TRUE) 
{
    CSSE_US_CASE_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
    CSSE_US_DEATH_DATA_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    csse_us_case <- download_CSSE_US_data_OLD(case_data_filename, 
                                              CSSE_US_CASE_DATA_URL, "Confirmed", incl_unassigned)
    csse_us_death <- download_CSSE_US_data_OLD(death_data_filename, 
                                               CSSE_US_DEATH_DATA_URL, "Deaths", incl_unassigned)
    csse_us_data <- dplyr::full_join(csse_us_case, csse_us_death) %>% 
        dplyr::select(Update, source, FIPS, Confirmed, Deaths) %>% 
        dplyr::arrange(source, FIPS, Update)
    csse_us_data <- dplyr::group_modify(dplyr::group_by(csse_us_data, 
                                                        FIPS), function(.x, .y) {
                                                            .x$incidI = c(.x$Confirmed[1], diff(.x$Confirmed))
                                                            .x$incidDeath = c(.x$Deaths[1], diff(.x$Deaths, ))
                                                            return(.x)
                                                        })
    if (fix_negatives){
        csse_us_data <- fix_negative_counts(csse_us_data, "Confirmed", 
                                            "incidI") %>% fix_negative_counts("Deaths", 
                                                                              "incidDeath")
    }
    csse_us_data <- aggregate_counties_to_state(csse_us_data, 
                                                "72")
    return(csse_us_data)
}
