





#' get_covidcast_data
#'
#' @param data_source 
#' @param geo_level 
#' @param signals 
#' @param limit_date 
#' @param fix_negatives 
#' @param weekly_data 
#' @param run_parallel 
#' @param n_cores 
#'
#' @return
#' @export
#'
#' @examples
get_covidcast_data <- function(
        geo_level = "state",
        signals = c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_cumulative_num", "confirmed_admissions_covid_1d"),
        limit_date = Sys.Date(),
        fix_negatives = TRUE,
        run_parallel = FALSE, 
        n_cores = 4){
    
    # Create dictionary
    # From the GitHub: https://github.com/reichlab/covid19-forecast-hub
    loc_dictionary <- read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
    loc_abbr <- loc_dictionary %>% dplyr::filter(!is.na(abbreviation))
    loc_dictionary <- loc_dictionary %>% 
        dplyr::mutate(state_fips = substr(location, 1, 2)) %>%
        dplyr::select(-abbreviation) %>%
        dplyr::full_join(loc_abbr %>% dplyr::select(abbreviation, state_fips=location))
    
    # in folder data-locations
    loc_dictionary_name <- suppressWarnings(
        setNames(c(rep(loc_dictionary$location_name, 2), "US", 
                   rep(loc_dictionary$location_name[-1], 2),
                   rep(loc_dictionary$location_name, 2), 
                   "New York"),
                 c(loc_dictionary$location, 
                   tolower(loc_dictionary$abbreviation), "US", 
                   na.omit(as.numeric(loc_dictionary$location)),
                   as.character(na.omit(
                       as.numeric(loc_dictionary$location))),
                   tolower(loc_dictionary$location_name),
                   toupper(loc_dictionary$location_name),
                   "new york state")))
    
    loc_dictionary_abbr <- setNames(loc_dictionary$abbreviation, loc_dictionary$location)
    loc_dictionary_pop <- setNames(loc_dictionary$population, loc_dictionary$location)
    
    # Set up start and end dates of data to pull
    # -- we pull the data in 6-month chunks to speed up and not overwhelm API for county-level
    
    if (geo_level=="county"){
        years_ <- lubridate::year("2020-01-01"):lubridate::year(limit_date)
        start_dates <- sort(c(lubridate::as_date(paste0(years_, "-01-01")), 
                              lubridate::as_date(paste0(years_, "-07-01"))))
        start_dates <- start_dates[start_dates<=limit_date]
        
        end_dates <- sort(c(lubridate::as_date(paste0(years_, "-06-30")),
                            lubridate::as_date(paste0(years_, "-12-31")), limit_date))
        end_dates <- end_dates[end_dates<=limit_date]
    } else {
        start_dates <- lubridate::as_date("2020-01-01")
        end_dates <- lubridate::as_date(limit_date)
    }
    
    # Set up parallelization to speed up
    if (run_parallel){
        doParallel::registerDoParallel(cores=n_cores)
        `%do_fun%` <- foreach::`%dopar%`
    } else {
        `%do_fun%` <- foreach::`%do%`
    }
    
    res <- foreach::foreach(x = signals, 
                            .combine = rbind, 
                            .packages = c("covidcast","dplyr","lubridate", "doParallel","foreach","vroom","purrr"),
                            .verbose = TRUE) %do_fun% {
                                
                                start_dates_ <- start_dates
                                if (x == "confirmed_admissions_covid_1d"){
                                    start_dates_[1] <- lubridate::as_date("2020-02-01")
                                } 
                                
                                # Call API to generate gold standard data from COVIDCast
                                df <- lapply(1:length(start_dates), 
                                             FUN = function(y=x){
                                                 covidcast::covidcast_signal(data_source = ifelse(grepl("admissions", x), "hhs", "jhu-csse"), 
                                                                             signal = x,
                                                                             geo_type = geo_level, 
                                                                             start_day = lubridate::as_date(start_dates_[y]), 
                                                                             end_day = lubridate::as_date(end_dates[y]))})
                                df <- data.table::rbindlist(df)
                                
                                if (geo_level=="state"){
                                    df <- df %>% mutate(state_abbr = toupper(geo_value)) %>%
                                        dplyr::select(-geo_value) %>%
                                        dplyr::left_join(loc_dictionary %>% 
                                                             dplyr::select(state_abbr=abbreviation, geo_value=location) %>% 
                                                             dplyr::filter(stringr::str_length(geo_value)==2))
                                }
                                df <- df %>% dplyr::rename(date = time_value)
                                
                                # Get cum hospitalizations
                                if (x == "confirmed_admissions_covid_1d"){
                                    df_cum <- df %>%
                                        dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
                                        dplyr::arrange(state_abbr, geo_value, date) %>%
                                        dplyr::group_by(data_source, signal, geo_value, state_abbr) %>%
                                        dplyr::mutate(value = cumsum(value)) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::mutate(signal = "confirmed_admissions_cum") 
                                    df <- rbind(df, df_cum)
                                }
                                
                                df %>% dplyr::select(signal, Update=date, source=state_abbr, FIPS=geo_value, value)
                            }
    
    res <- res %>%
        dplyr::mutate(signal = recode(signal, 
                                      "deaths_incidence_num"="incidDeath", 
                                      "deaths_cumulative_num"="Deaths", 
                                      "confirmed_incidence_num"="incidI", 
                                      "confirmed_cumulative_num"="Confirmed",
                                      "confirmed_admissions_covid_1d"="incidH",
                                      "confirmed_admissions_cum"="Hospitalizations")) %>%
        
        tidyr::pivot_wider(names_from = signal, values_from = value) %>%
        dplyr::mutate(Update=lubridate::as_date(Update),
                      FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""), # clean FIPS if numeric
                      FIPS = paste0(FIPS, "000")) %>%
        dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>%
        dplyr::distinct()
    
    validation_date <- Sys.getenv("VALIDATION_DATE")
    if ( validation_date != '' ) {
        print(paste("(DataUtils.R) Limiting CSSE US data to:", validation_date, sep=" "))
        res <- dplyr::filter(res, Update < validation_date)
    }
    
    res <- res %>% tibble::as_tibble()
    
    # Fix incidence counts that go negative and NA values or missing dates
    if (fix_negatives & any(c("Confirmed", "incidI", "Deaths", "incidDeath") %in% colnames(res))){
        res <- fix_negative_counts(res, "Confirmed", "incidI") %>%
            fix_negative_counts("Deaths", "incidDeath")
    }
    
    return(res)
}





#' Do variant adjustment 
#'
#' @param rc 
#' @param variant_props_file 
#' @param var_targets 
#'
#' @return
#' @export
#'
#' @examples
do_variant_adjustment <- function (rc, 
                                   variant_props_file = "data/variant/variant_props_long.csv", 
                                   var_targets = c("incidI", "Confirmed")) {
    outcome_column_names <- names(rc)[(names(rc) %in% var_targets)]
    variant_data <- readr::read_csv(variant_props_file) %>%     dplyr::filter(!is.na(source))
    rc <- rc %>% tidyr::pivot_longer(!!outcome_column_names, names_to = "outcome") %>% 
        dplyr::left_join(variant_data) %>% 
        dplyr::mutate(value = value * prop) %>% dplyr::select(-prop) %>% 
        dplyr::bind_rows(dplyr::mutate(tidyr::pivot_longer(rc, !!outcome_column_names, names_to = "outcome"), variant = "all variants")) %>% 
        tidyr::pivot_wider(names_from = c("outcome", "variant"), values_from = "value") %>% dplyr::bind_rows(rc)
    return(rc)
}


#' Do variant adjustment - version 2
#'
#' @param rc 
#' @param variant_props_file 
#' @param var_targets 
#'
#' @return
#' @export
#'
#' @examples
do_variant_adjustment2 <- function(rc, 
                                   variant_props_file = "data/variant/variant_props_long.csv", 
                                   var_targets = c("incidI","Confirmed")){
    #non_outcome_column_names <- c("FIPS", "Update", "source", )
    #outcome_column_names <- names(rc)[!(names(rc) %in% non_outcome_column_names)]
    outcome_column_names <- names(rc)[(names(rc) %in% var_targets)]
    
    variant_data <- readr::read_csv(variant_props_file) %>% 
        dplyr::filter(!is.na(source)) %>%
        tidyr::pivot_wider(names_from = variant, values_from = prop, names_prefix = "prop_")
    
    rc <- rc %>%
        dplyr::select(FIPS, Update, source, !!outcome_column_names) %>%
        dplyr::left_join(variant_data, by=c("Update", "source")) %>%
        tidyr::pivot_longer(cols = tidyselect::starts_with("prop"), names_to = "variant", values_to = "prop") %>%
        tidyr::pivot_longer(cols = !!outcome_column_names, names_to = "outcome", values_to = "value") %>% 
        dplyr::mutate(value = round(value*prop,0)) %>%
        dplyr::bind_rows(
            dplyr::mutate(
                rc %>%
                    # dplyr::select(geoid, date, source, !!outcome_column_names) %>%
                    tidyr::pivot_longer(-c(FIPS, Update, source), names_to = "outcome"),
                variant = "")) %>%
        dplyr::mutate(outcome = paste0(outcome, gsub("prop", "", variant))) %>%
        dplyr::select(-prop, -variant) %>%
        tidyr::pivot_wider(names_from = outcome, values_from = value)
    
    return(rc)
}


#' get_rawcoviddata_state_data_old
#'
#' @param fix_negatives 
#'
#' @export
#'
#' @examples
get_rawcoviddata_state_data <- function(fix_negatives = TRUE){
    
    # install the required package if not already
    is_rawcoviddata_available <- require("rawcoviddata")
    if (!is_rawcoviddata_available){
        devtools::install_github("lmullany/rawcoviddata", force = TRUE)
    }
    
    # Pull CSSE data using `rawcoviddata` package from Luke Mullany
    cdp <- rawcoviddata::cssedata(return_compact=T)
    state_dat <- rawcoviddata::get_state_from_cdp(cdp=cdp, state = NULL, fix_cumul = fix_negatives, type = c("mid"))
    
    loc_dictionary <- readr::read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv") %>%
        dplyr::rename(fips = location, USPS=abbreviation, Province_State=location_name, Pop2 = population) %>%
        dplyr::filter(stringr::str_length(fips)==2 & fips!="US") %>% 
        data.table::as.data.table()
    
    state_dat <- state_dat[loc_dictionary, on = .(USPS)]
    
    state_dat <- state_dat %>%
        dplyr::select(Update = Date, FIPS = fips, source = USPS, 
                      Confirmed = cumConfirmed, Deaths = cumDeaths, 
                      incidI = Confirmed, incidDeath = Deaths)
    
    state_dat <- state_dat %>%
        dplyr::mutate(Update=lubridate::as_date(Update),
                      FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""), # clean FIPS if numeric
                      FIPS = paste0(FIPS, "000")) %>%
        dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>%
        dplyr::distinct()
    
    validation_date <- Sys.getenv("VALIDATION_DATE")
    if ( validation_date != '' ) {
        print(paste("(DataUtils.R) Limiting CSSE US data to:", validation_date, sep=" "))
        state_dat <- dplyr::filter(state_dat, Update < validation_date)
    }
    
    # Fix incidence counts that go negative and NA values or missing dates
    if (fix_negatives){
        state_dat <- fix_negative_counts(state_dat, "Confirmed", "incidI") %>%
            fix_negative_counts("Deaths", "incidDeath")
    }
    
    return(state_dat)
}






# There are three different ways of dealing with negative incidence counts, specified by the type argument
# "high": highest estimate; this modifies the cumulative column to be the cummax of the previous cumulative counts
# "low": method: Assume all negative incidents are corrections to previous reports.
#               So, reduce previous reports to make incidents not decrease.
# "mid" method: Combination of both low and high
#               For consecutive negatives, the first negative will be modified so that the
#               cumulative is the cummax so far.
#               For the following negatives, use the low method: reduce previous reports.
# For get_USAFacts_data(), this is always the default ("mid"), but the other code is here for posterity.
fix_negative_counts_single_geoid <- function(.x,.y, incid_col_name, date_col_name, cum_col_name, type){
    original_names <- names(.x)
    
    .x <- dplyr::arrange(.x,!!rlang::sym(date_col_name))
    
    # Calculate new cumulative column
    calc_col_name <- paste(cum_col_name, type, sep="_")
    
    if (type == "high") {
        
        .x[[calc_col_name]] <- .x[[cum_col_name]]
        .x[[calc_col_name]][is.na(.x[[calc_col_name]]) ] <- 0 # Fixes NA values
        .x[[calc_col_name]] <- cummax(.x[[calc_col_name]])
        
    } else if (type == "low") {
        
        .x[[calc_col_name]] <- .x[[cum_col_name]]
        .x[[calc_col_name]][is.na(.x[[calc_col_name]]) ] <- Inf # Fixes NA values
        .x[[calc_col_name]] <- rev(cummin(rev(.x[[calc_col_name]])))
        .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- 0 # In case last thing in row is NA/infinity
        .x[[calc_col_name]] <- cummax(.x[[calc_col_name]]) # Replace 0's with cummax
        
    } else if (type == "mid") {
        
        .x[[calc_col_name]] <- .x[[cum_col_name]]
        
        ## In mid, do a local max followed by a global min (like "low")
        
        # Get indices where the cumulative count is bigger than the one after it
        # i.e. where incidence is negative next time
        unlagged <- .x[[calc_col_name]]
        unlagged[is.na(.x[[calc_col_name]]) ] <- -Inf # Fixes NA values
        lagged <- dplyr::lag(.x[[calc_col_name]])
        lagged[is.na(.x[[calc_col_name]]) ] <- Inf # Fixes NA values
        
        max_indices <- which(unlagged < lagged)
        .x[[calc_col_name]][max_indices] <- lagged[max_indices]
        
        # Global min
        .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- Inf
        .x[[calc_col_name]] <- rev(cummin(rev(.x[[calc_col_name]])))
        .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- 0
        .x[[calc_col_name]] <- cummax(.x[[calc_col_name]])
        
    } else {
        
        stop(paste("Invalid fix_negative_counts type. Must be ['low', 'med', 'high']. Actual: ", type))
        
    }
    
    .x[[cum_col_name]] <- .x[[calc_col_name]]
    
    # Recover incidence from the new cumulative
    .x[[incid_col_name]] <- c(.x[[calc_col_name]][[1]], diff(.x[[calc_col_name]]))
    
    .x <- .x[,original_names]
    for(col in names(.y)){
        .x[[col]] <- .y[[col]]
    }
    
    return(.x)
}

# Add missing dates, fix counts that go negative, and fix NA values
#
# See fix_negative_counts_single_geoid() for more details on the algorithm,
# specified by argument "type"
#' Title
#'
#' @param df 
#' @param cum_col_name 
#' @param incid_col_name 
#' @param date_col_name 
#' @param min_date 
#' @param max_date 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
fix_negative_counts <- function(
        df,
        cum_col_name,
        incid_col_name,
        date_col_name = "Update",
        min_date = min(df[[date_col_name]]),
        max_date = max(df[[date_col_name]]),
        type="mid" # "low" or "high"
){
    
    if(nrow(dplyr::filter(df, !!incid_col_name < 0)) > 0) {
        return(df)
    }
    
    df <- dplyr::group_by(df, FIPS,source)
    # Add missing dates
    df <- tidyr::complete(df, !!rlang::sym(date_col_name) := min_date + seq_len(max_date - min_date)-1)
    df <- dplyr::group_map(df, fix_negative_counts_single_geoid,
                           incid_col_name=incid_col_name,
                           date_col_name=date_col_name,
                           cum_col_name=cum_col_name,
                           type=type)
    df <- do.call(what=rbind, df)
    
    return(df)
}










get_groundtruth_from_source <- function(source = "csse", 
                                        scale = "US state", 
                                        source_file = NULL,
                                        variables = c("Confirmed", "Deaths", "incidI", "incidDeath"), 
                                        incl_unass = TRUE, 
                                        fix_negatives = TRUE,
                                        adjust_for_variant = FALSE, 
                                        variant_props_file = "data/variant/variant_props_long.csv"){
    
    if(source == "reichlab" & scale == "US county"){
        
        rc <- get_reichlab_cty_data()
        rc <- dplyr::select(rc, Update, FIPS, source, !!variables)
        rc <- tidyr::drop_na(rc, tidyselect::everything())
        
    } else if(source == "reichlab" & scale == "US state"){
        
        rc <- get_reichlab_st_data()
        rc <- dplyr::select(rc, Update, FIPS, source, !!variables)
        rc <- tidyr::drop_na(rc, tidyselect::everything())
        
    } else if(source == "usafacts" & scale == "US county"){
        
        rc <- get_USAFacts_data(tempfile(), tempfile(), incl_unassigned = incl_unass) %>%
            dplyr::select(Update, FIPS, source, !!variables) %>%
            tidyr::drop_na(tidyselect::everything()) %>%
            dplyr::ungroup()
        
    } else if(source == "usafacts" & scale == "US state"){
        
        rc <- get_USAFacts_data(tempfile(), tempfile(), incl_unassigned = incl_unass) %>%
            dplyr::select(Update, FIPS, source, !!variables) %>%
            dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
            dplyr::group_by(Update, FIPS, source) %>%
            dplyr::summarise_if(is.numeric, sum) %>%
            tidyr::drop_na(tidyselect::everything()) %>%
            dplyr::ungroup()
        
    } else if(source == "csse" & scale == "US county"){
        
        rc <- get_CSSE_US_data(tempfile(), tempfile(), incl_unassigned = incl_unass) %>%
            dplyr::select(Update, FIPS, source, !!variables)
        rc <- tidyr::drop_na(rc, c(Update, FIPS, source))
        #rc <- tidyr::drop_na(rc, tidyselect::everything())
        
    } else if(source == "csse" & scale == "US state"){
        
        rc <- get_CSSE_US_data(tempfile(), tempfile(), incl_unassigned = incl_unass) %>%
            dplyr::select(Update, FIPS, source, !!variables) %>%
            dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
            dplyr::group_by(Update, FIPS, source) %>%
            dplyr::summarise_if(is.numeric, sum) %>%
            dplyr::ungroup()
        rc <- tidyr::drop_na(rc, c(Update, FIPS, source))
        #rc <- tidyr::drop_na(rc, tidyselect::everything())
        
    } else if(source == "csse" & scale == "country"){
        
        rc <- get_CSSE_global_data()
        rc <- dplyr::select(rc, UID, iso2, iso3, Province_State, Country_Region, Latitude, Longitude, Update, source, !!variables)
        rc <- tidyr::drop_na(rc, c(Update, FIPS, source))
        #rc <- tidyr::drop_na(rc, tidyselect::everything())
        
    } else if(source == "csse" & scale == "complete"){
        
        us <- get_CSSE_US_matchGlobal_data()
        us <- dplyr::select(us, Update, UID, iso2, iso3, Latitude, Longitude, source, !!variables, Country_Region, Province_State, source)
        rc <- get_CSSE_global_data()
        rc <- dplyr::select(rc, Update, UID, iso2, iso3, Latitude, Longitude, source, !!variables, Country_Region, Province_State, source)
        rc <- dplyr::bind_rows(rc, us)
        rc <- tidyr::drop_na(rc, tidyselect::everything())
        warning(print(paste("The combination of ", source, "and", scale, "is not fully working. County-level US data may be missing from this data frame.")))
        
    } else if(source == "hhsCMU" & scale == "US state"){
        
        rc <- get_hhsCMU_cleanHosp_st_data()
        rc <- dplyr::select(rc, Update, FIPS, source, !!variables)
        rc <- tidyr::drop_na(rc, tidyselect::everything())
        
        
    } else if(source == "csse_lm" & scale == "US state"){
        
        variables_ <- variables[!grepl("incidh|hosp", tolower(variables))] # hosp not available 
        rc <- get_rawcoviddata_state_data(fix_negatives=fix_negatives) %>%
            dplyr::select(Update, FIPS, source, !!variables_)
        rc <- tidyr::drop_na(rc, c(Update, FIPS, source))
        #rc <- tidyr::drop_na(rc, tidyselect::everything())
        
    } else if(source == "covidcast" & scale == "US state"){
        
        # define covidcast signals
        
        signals <- NULL
        if (any(c("incidDeath", "Deaths") %in% variables)) signals <- c(signals, "deaths_incidence_num", "deaths_cumulative_num")
        if (any(c("incidI", "Confirmed") %in% variables)) signals <- c(signals, "confirmed_incidence_num", "confirmed_cumulative_num")
        if (any(grepl("hosp|incidH", variables))) signals <- c(signals, "confirmed_admissions_covid_1d")
        
        rc <- get_covidcast_data(geo_level = "state",
                                 signals = signals,
                                 limit_date = Sys.Date(),
                                 fix_negatives = fix_negatives,
                                 run_parallel = FALSE) %>%
            dplyr::select(Update, FIPS, source, !!variables)
        rc <- tidyr::drop_na(rc, c(Update, FIPS, source))
        #rc <- tidyr::drop_na(rc, tidyselect::everything()) 
        
    } else if(source == "file"){
        
        rc <- get_gt_file_data(source_file)
        
        # check that it has correct columns
        check_cols <- all(c("Update", "FIPS", "source", variables) %in% colnames(rc))
        stopifnot("Columns missing in source file. Must have [Update, FIPS, source] and desired variables." = all(check_cols))
        
        rc <- rc %>%
            dplyr::select(rc, Update, FIPS, source, !!variables)
        rc <- tidyr::drop_na(rc, c(Update, FIPS, source))
        #rc <- tidyr::drop_na(rc, tidyselect::everything()) 
        
    } else{
        warning(print(paste("The combination of ", source, "and", scale, "is not valid. Returning NULL object.")))
        rc <- NULL
    }
    
    # Drop NA rows (where all rows are NA for the outcomes of interest)
    drop_cols <- c("incidI", "incidDeath", "incidH") [c("incidI", "incidDeath", "incidH") %in% colnames(rc)]
    rc <- rc[rowSums(is.na(rc[,drop_cols])) != length(drop_cols), ]
    
    
    if (adjust_for_variant & any(c("incidI", "Confirmed") %in% variables)) {
        tryCatch({
            rc <- do_variant_adjustment2(rc, variant_props_file)
        }, error = function(e) {
            stop(paste0("Could not use variant file |", variant_props_file, "|, with error message", e$message()))
        })
    }
    
    return(rc)
    
}





clean_gt_forplots <- function(gt_data){
    
    gt_data <- as_tibble(gt_data) %>% 
        filter(source != "US")
    
    gt_long <- gt_data %>%
        pivot_longer(cols = -c(date, source, FIPS), names_to = "target", values_to = "incid") %>%
        group_by(source, FIPS, date, target)%>%
        summarise(incid = sum(incid))%>%
        ungroup() %>%
        filter(grepl("incid", target, ignore.case = TRUE))
    
    gt_long_tmp <- gt_long %>%
        as_tibble() %>%
        mutate(incid = fix_NAs(incid)) %>%
        group_by(source, FIPS, target) %>%
        arrange(date) %>%
        mutate(incid=cumsum(incid))%>%
        ungroup() %>%
        mutate(target = gsub("incid", "cum", target))
    
    gt_long <- gt_long %>% full_join(gt_long_tmp)
    rm(gt_long_tmp)
    
    gt_long_us <- gt_long %>%
        group_by(date, target)%>%
        summarise(incid=sum(incid, na.rm = TRUE)) %>%
        mutate(source="US")
    gt_long <- gt_long %>%
        bind_rows(gt_long_us)
    rm(gt_long_us)
    
    gt_long <- gt_long %>% mutate(target = gsub("Death", "D", target))
    
    # pivot back wide now with cum
    gt_data <- gt_long %>%
        pivot_wider(names_from = target, values_from = incid)
    
    gt_long <- gt_long %>%
        rename(time=date, USPS=source)
    gt_long <- gt_long %>%
        rename(geoid=FIPS, outcome_name = target, outcome = incid)
    
    gt_data <- gt_data %>%
        rename(geoid=FIPS, time=date, USPS=source)
    
    return(gt_data)
}