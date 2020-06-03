##' Functions originally present in HopkinsIDD/covidImportation
##'
##' Pull JHU CSSE GitHub data
##'
##' Pulls the JHUCSSE total case count data up to current date from GitHub.
##' This version checks what is already saved, and downloads those that are not.
##' Eventually, we would like automate this. This function 
##'
##' @param case_data_dir directory where daily reported case data files are stored by the function.
##' @param repull_all whether to repull all data to make sure it's up to date and catch error fixes
##' @return NA (saves a CSV of the current data to the data directory)
##'
##' @import httr dplyr
##' @importFrom lubridate mdy month day year
##' @importFrom readr read_csv write_csv
##'
##' @export
##'
pull_JHUCSSE_github_data <- function(case_data_dir = "data/case_data", repull_all=FALSE){
  
  # Create directory to hold all the data
  dir.create(case_data_dir, showWarnings = FALSE, recursive = FALSE)
  print(paste0("Pulled JHUCSSE data files are saved in ", case_data_dir, "."))
  
  # First get a list of files so we can get the latest one
  req <- httr::GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")
  
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  data_files <- grep(".csv", grep("csse_covid_19_data/csse_covid_19_daily_reports/", filelist, value=TRUE), value=TRUE)
  dates_ <- gsub("csse_covid_19_data/csse_covid_19_daily_reports/", "", data_files)
  dates_ <- gsub(".csv", "", dates_)
  dates_reformat_ <- as.POSIXct(dates_, format="%m-%d-%Y")
  dates_tocheck_ <- paste(lubridate::month(dates_reformat_),
                          lubridate::day(dates_reformat_),
                          lubridate::year(dates_reformat_), sep="-")
  
  file_prefix = "JHUCSSE Total Cases "
  
  # select list to download
  if (!repull_all){
    # Check which we have already
    files_in_dir <- list.files(case_data_dir, file_prefix)
    files_in_dir_dates <- gsub(file_prefix, "", files_in_dir)
    files_in_dir_dates <- gsub(".csv", "", files_in_dir_dates)
    tmp <- which.max(lubridate::mdy(files_in_dir_dates))
    files_in_dir_dates <- files_in_dir_dates[-tmp]
    
    # Remove ones we already have
    data_files <- data_files[!(dates_tocheck_ %in% files_in_dir_dates)]
    dates_tocheck_ <- dates_tocheck_[!(dates_tocheck_ %in% files_in_dir_dates)]
  }
  
  for (i in seq_len(length(data_files))){
    file_name_ <- data_files[i]   # file to pull
    date_ <- dates_tocheck_[i]     # date formatted for saving csv
    
    # Read in the file
    url_ <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",file_name_)
    case_data <- readr::read_csv(url(url_))
    
    # Save it
    readr::write_csv(case_data, file.path(case_data_dir, paste0(file_prefix, date_,".csv")))
  }
}

fix_column_names_and_update <- function(rc) {
  # Fix the different file column names
  colnames_ <- colnames(rc)
  colnames_[grepl("Province", colnames_)] <- "Province_State"
  colnames_[grepl("Country", colnames_)] <- "Country_Region"
  colnames_[grepl("Demised", colnames_)] <- "Deaths"
  colnames_[grepl("Update", colnames_)] <- "Update"
  colnames_[grepl("Lat", colnames_)] <- "Latitude"
  colnames_[grepl("Long", colnames_)] <- "Longitude"
  
  colnames(rc) <- colnames_
  
  rc <- rc %>% dplyr::mutate(Update=lubridate::parse_date_time(Update,
                                                               c("%m/%d/%Y %I:%M %p", "%m/%d/%Y %H:%M", "%m/%d/%y %I:%M %p","%m/%d/%y %H:%M", "%Y-%m-%d %H:%M:%S")))
  return(rc)
}

fix_locations <- function(rc) {
  # Fix Chinese provinces and autonomous regions
  rc <- rc %>%
    dplyr::mutate(Country_Region=replace(Country_Region, Country_Region=="China", "Mainland China")) %>%
    dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Macau", "Macau")) %>%
    dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Hong Kong", "Hong Kong")) %>%
    dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Taiwan", "Taiwan")) %>%
    dplyr::mutate(Province_State=ifelse(is.na(Province_State),Country_Region, Province_State))
  
  # Fix bad locations
  rc <- rc %>% dplyr::filter(!(Province_State %in% c("US"))) %>%
    dplyr::mutate(Province_State = ifelse(grepl("Chicago", Province_State), "Chicago, IL", Province_State)) %>%
    dplyr::mutate(Province_State = ifelse(grepl("Ningxia", Province_State), "Ningxia", Province_State)) %>%
    dplyr::mutate(Province_State = ifelse(Province_State=="Inner Mongolia", "Nei Mongol", Province_State)) %>%
    dplyr::mutate(Province_State = ifelse(Province_State=="Hong Kong", "HKG", Province_State))
  
  return(rc)
}

##'
##' Reads in the JHUCSSE total case count data up
##' until (and including) a given dat.
##'
##' @param last_date Date, the last time to consider data from
##' @param append_wiki logical, should we also append data from wikipedia
##' @param case_data_dir directory where daily reported case data files are stored by the function.
##' @param print_file_path logical whether or not to print the file path
##'
##' @return a data frame with the basic data.
##'
##' @import dplyr 
##' @importFrom lubridate mdy parse_date_time ymd_hms
##' @importFrom readr read_csv write_csv
##'
##' @export
##'
read_JHUCSSE_cases <- function(last_date=Sys.Date(),
                               append_wiki=TRUE,
                               case_data_dir = "data/case_data",
                               print_file_path=FALSE) {
  
  ## first get a list of all of the files in the directory
  ## starting with "JHUCSSE Total Cases"
  file_list <- list.files(case_data_dir,"JHUCSSE Total Cases",
                          full.names = TRUE)
  
  file_list <- rev(file_list)
  
  ##Now combine them into one data frame
  rc <- list()
  
  for (f in seq_along(file_list)) {
    if(print_file_path) print(file_list[f])
    tmp <- readr::read_csv(file_list[f])
    
    tmp <- fix_column_names_and_update(tmp)
    
    rc[[f]] <- tmp
  }
  rc <- data.table::rbindlist(rc, fill = TRUE)
  
  ##Now drop any after the date given
  rc <- rc %>% as.data.frame() %>% dplyr::mutate(Update = lubridate::ymd_hms(Update)) %>%
    dplyr::filter(as.Date(Update) <= as.Date(last_date))
  
  # Fix Chinese provinces, auonomous regions, and bad locations
  rc <- fix_locations(rc)
  
  if (append_wiki) {
    data("wikipedia_cases", package="covidcommon")
    rc <- dplyr::bind_rows(rc,wikipedia_cases)
  }
  # Remove any duplicate rows
  rc <- rc %>% dplyr::distinct()
  
  return(rc)
}

##'
##' Pull JHU CSSE GitHub data
##'
##' Pulls the JHUCSSE total case count data up to current date from GitHub.
##' This version checks what is already saved, and downloads those that are not.
##'
##' @param case_data_dir directory where daily reported case data files are stored by the function.
##' @param last_date last date for which to include case data
##' @param check_saved_data whether to check for existing saved case data
##' @param save_data whether to save the cleaned and combined data
##'
##' @import dplyr httr 
##' @importFrom lubridate mdy month day year parse_date_time ymd_hms
##' @importFrom readr read_csv write_csv
##' @importFrom data.table rbindlist
##'
##' @return NA (saves a CSV of the current data to the data directory)
##'
##' @export
##' 
update_JHUCSSE_github_data <- function(case_data_dir = "data/case_data",
                                       last_date=Sys.time(),
                                       check_saved_data=FALSE,
                                       save_data=FALSE){
  
  # Create directory to hold all the data
  if (check_saved_data | save_data){
    dir.create(case_data_dir, showWarnings = FALSE, recursive = TRUE)
    print(paste0("Combined data is saved in ", case_data_dir, "."))
  }
  
  # First get a list of files so we can get the latest one
  req <- httr::GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  data_files <- grep(".csv", grep("csse_covid_19_data/csse_covid_19_daily_reports/", filelist, value=TRUE), value=TRUE)
  dates_ <- gsub("csse_covid_19_data/csse_covid_19_daily_reports/", "", data_files)
  dates_ <- gsub(".csv", "", dates_)
  dates_reformat_ <- as.POSIXct(dates_, format="%m-%d-%Y")
  dates_tocheck_ <- paste(lubridate::month(dates_reformat_),
                          lubridate::day(dates_reformat_),
                          lubridate::year(dates_reformat_), sep="-")
  
  file_prefix = "JHUCSSE Total Cases "
  
  if (check_saved_data){
    
    # First check the data that comes with the package
    data('jhucsse_case_data', package = 'covidcommon')
    update_dates <- sort(unique(as.Date(jhucsse_case_data$Update)))
    tmp <- which.max(update_dates)
    update_dates <- update_dates[-tmp]
    update_dates <- paste(lubridate::month(update_dates),
                          lubridate::day(update_dates),
                          lubridate::year(update_dates), sep="-")
    
    # Check which we have already
    files_in_dir <- list.files(case_data_dir, file_prefix)
    files_in_dir_dates <- gsub(file_prefix, "", files_in_dir)
    files_in_dir_dates <- gsub(".csv", "", files_in_dir_dates)
    tmp <- which.max(lubridate::mdy(files_in_dir_dates))
    files_in_dir_dates <- files_in_dir_dates[-tmp]
    
    # check for previously combined data
    comb_file_in_dir <- file.exists(file.path(case_data_dir, "jhucsse_case_data.csv"))
    if (comb_file_in_dir){
      comb_data <- readr::read_csv(file.path(case_data_dir,"jhucsse_case_data.csv"))
      comb_data_in_dir_dates <- sort(unique(as.Date(comb_data$Update)))
      tmp <- which.max(comb_data_in_dir_dates)
      comb_data_in_dir_dates <- comb_data_in_dir_dates[-tmp]
      comb_data_in_dir_dates <- paste(lubridate::month(comb_data_in_dir_dates),
                                      lubridate::day(comb_data_in_dir_dates),
                                      lubridate::year(comb_data_in_dir_dates), sep="-")
    } else {
      comb_data_in_dir_dates <- NULL
    }
    
  } else {
    files_in_dir_dates <- comb_data_in_dir_dates <- update_dates <- NULL
  }
  
  # select list to download (minus the latest one as multiple updates to the data are made daily)
  dates_have_ <- unique(c(update_dates, files_in_dir_dates, comb_data_in_dir_dates))
  data_files <- data_files[!(dates_tocheck_ %in% dates_have_)]
  dates_tocheck_ <- dates_tocheck_[!(dates_tocheck_ %in% files_in_dir_dates)]
  
  # pull and combine data from github
  rc <- list()
  
  for (i in seq_len(length(data_files))){
    file_name_ <- data_files[i]   # file to pull
    date_ <- dates_tocheck_[i]     # date formatted for saving csv
    
    # Read in the file
    url_ <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",file_name_)
    case_data <- readr::read_csv(url(url_))
    case_data <- case_data %>% dplyr::mutate(FIPS = as.character(FIPS))
    
    case_data <- fix_column_names_and_update(case_data)
    
    case_data <- case_data %>% dplyr::mutate(FIPS = as.character(FIPS))
    rc[[i]] <- case_data
  }
  rc <- data.table::rbindlist(rc, fill = TRUE)
  
  ##Now drop any after the date given
  rc <- rc %>% as.data.frame() %>% mutate(Update = lubridate::ymd_hms(Update)) %>%
    dplyr::filter(as.Date(Update) <= as.Date(last_date))
  
  # Fix Chinese provinces, autonomous regions, and bad locations
  rc <- fix_location(rc)
  
  # merge with data from the package
  rc <- dplyr::bind_rows(jhucsse_case_data, rc) %>% dplyr::distinct()
  
  
  # Save if desired
  if (save_data){
    readr::write_csv(rc, file.path(case_data_dir,"jhucsse_case_data.csv"))
  }
  
  return(rc)
}



