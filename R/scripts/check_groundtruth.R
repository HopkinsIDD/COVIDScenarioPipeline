
library(tidyverse)
library(covidcommon)
library(inference)




# PULL DATA ---------------------------------------------------------------

signals <- c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_cumulative_num")
#signals <- c(signals,"confirmed_admissions_covid_1d")
variables = c("Confirmed", "Deaths", "incidI", "incidDeath")#, "incidH","Hospitalizations")

geo_level = "state"
signals = signals
limit_date = Sys.Date()
run_parallel = TRUE
n_cores=4
fix_negatives <- TRUE
incl_unass <- TRUE



# CSSe
rc_CSSE1 <- get_CSSE_US_data(tempfile(), tempfile(), incl_unassigned = incl_unass, fix_negatives = FALSE) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()

# CSSe
rc_CSSE1_fixnegs <- get_CSSE_US_data(tempfile(), tempfile(), incl_unassigned = incl_unass, fix_negatives = TRUE) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()


# CSSe
rc_CSSE1_old <- get_CSSE_US_data_OLD(tempfile(), tempfile(), incl_unassigned = incl_unass, fix_negatives = FALSE) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()

# CSSe
rc_CSSE1_fixnegs_old <- get_CSSE_US_data_OLD(tempfile(), tempfile(), incl_unassigned = incl_unass, fix_negatives = TRUE) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()

# csse LM
variables_ <- variables[!grepl("incidh|hosp", tolower(variables))] 
rc_csselm <- get_rawcoviddata_state_data(fix_negatives=fix_negatives) %>%
    dplyr::select(Update, FIPS, source, !!variables_) %>%
    tidyr::drop_na(tidyselect::everything())

# if (any(grepl("hosp|incid", tolower(variables)))){
#     variables_hosp <- variables[grepl("incidh|hosp", tolower(variables))] 
#     rc_hosp <- get_covidcast_data(geo_level = "state",
#                                   signals = "confirmed_admissions_covid_1d",
#                                   limit_date = Sys.Date(),
#                                   fix_negatives = fix_negatives,
#                                   run_parallel = FALSE) %>%
#         dplyr::select(Update, FIPS, source, !!variables_hosp) %>%
#         tidyr::drop_na(tidyselect::everything())
#     rc_csselm <- rc_csselm %>% dplyr::full_join(rc_hosp)
# }



loc_dictionary <- readr::read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv") %>%
    dplyr::rename(fips = location, USPS=abbreviation, Province_State=location_name, Pop2 = population) %>%
    dplyr::filter(stringr::str_length(fips)==2 & fips!="US") %>% 
    data.table::as.data.table()

cdp <- rawcoviddata::cssedata(return_compact=T)
state_dat_fixneg_pkg <- rawcoviddata::get_state_from_cdp(cdp=cdp, state = NULL, fix_cumul = TRUE, type = c("mid"))
state_dat <- rawcoviddata::get_state_from_cdp(cdp=cdp, state = NULL, fix_cumul = FALSE, type = c("mid"))

cowplot::plot_grid(
    state_dat %>%
        filter(USPS=="UT") %>%
        ggplot(aes(Date, cumConfirmed)) +
        geom_point(),
    state_dat %>%
        filter(USPS=="UT") %>%
        ggplot(aes(Date, Confirmed)) +
        geom_point(),
    nrow=1)


state_dat_fixneg_pkg <- state_dat_fixneg_pkg[loc_dictionary, on = .(USPS)]
state_dat_fixneg_pkg <- state_dat_fixneg_pkg %>%
    dplyr::select(Update = Date, FIPS = fips, source = USPS, 
                  Confirmed = cumConfirmed, Deaths = cumDeaths, 
                  incidI = Confirmed, incidDeath = Deaths) %>%
    dplyr::mutate(Update=lubridate::as_date(Update),
                  FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""), # clean FIPS if numeric
                  FIPS = paste0(FIPS, "000")) %>%
    dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>%
    dplyr::distinct()

state_dat <- state_dat[loc_dictionary, on = .(USPS)]
state_dat <- state_dat %>%
    dplyr::select(Update = Date, FIPS = fips, source = USPS, 
                  Confirmed = cumConfirmed, Deaths = cumDeaths, 
                  incidI = Confirmed, incidDeath = Deaths) %>%
    dplyr::mutate(Update=lubridate::as_date(Update),
                  FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""), # clean FIPS if numeric
                  FIPS = paste0(FIPS, "000")) %>%
    dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>%
    dplyr::distinct()

state_dat_nofixneg <- state_dat
state_dat_fixneg_jh <- state_dat %>% as_tibble() %>%
    fix_negative_counts("Confirmed", "incidI") %>%
    fix_negative_counts("Deaths", "incidDeath")

state_dat_nofixneg <- state_dat_nofixneg %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()
state_dat_fixneg_jh <- state_dat_fixneg_jh %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()
state_dat_fixneg_pkg <- state_dat_fixneg_pkg %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()





# Check negatives
sum(state_dat_fixneg_pkg$incidDeath<0, na.rm = TRUE)
sum(state_dat_nofixneg$incidDeath<0, na.rm = TRUE)
sum(state_dat_fixneg_jh$incidDeath<0, na.rm = TRUE)

sum(state_dat_fixneg_pkg$incidI<0, na.rm = TRUE)
sum(state_dat_nofixneg$incidI<0, na.rm = TRUE)
sum(state_dat_fixneg_jh$incidI<0, na.rm = TRUE)

# check cums
sum(state_dat_fixneg_pkg$incidDeath, na.rm = TRUE)
sum(state_dat_nofixneg$incidDeath, na.rm = TRUE)
sum(state_dat_fixneg_jh$incidDeath, na.rm = TRUE)

sum(state_dat_fixneg_pkg$incidI, na.rm = TRUE)
sum(state_dat_nofixneg$incidI, na.rm = TRUE)
sum(state_dat_fixneg_jh$incidI, na.rm = TRUE)







# COVIDcast
# define covidcast signals
signals <- c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_cumulative_num")
if (any(grepl("hosp", variables))){
    signals <- c(signals,"confirmed_admissions_covid_1d")
}   
rc_covidcast <- get_covidcast_data(geo_level = "state",
                                   signals = signals,
                                   limit_date = Sys.Date(),
                                   fix_negatives = fix_negatives,
                                   run_parallel = TRUE) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    tidyr::drop_na(tidyselect::everything())

rc_covidcast_nofixnegs <- get_covidcast_data(geo_level = "state",
                                             signals = signals,
                                             limit_date = Sys.Date(),
                                             fix_negatives = FALSE,
                                             run_parallel = TRUE) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    tidyr::drop_na(tidyselect::everything())








# COMPARE -----------------------------------------------------------------


rc_comp <- bind_rows(
    rc_CSSE1_fixnegs %>% mutate(type = "cssejh_fixnegs"),
    rc_CSSE1 %>% mutate(type = "cssejh_nofixnegs"),
    rc_CSSE1_fixnegs_old %>% mutate(type = "cssejh_fixnegs_old"),
    rc_CSSE1_old %>% mutate(type = "cssejh_nofixnegs_old"),
    rc_covidcast %>% mutate(type = "covidcast_fixnegs"),
    rc_covidcast_nofixnegs %>% mutate(type = "covidcast_nofixnegs"),
    state_dat_fixneg_pkg %>% mutate(type = "csselm_fixnegs"),
    state_dat_fixneg_jh %>% mutate(type = "csselm_jhfixnegs"),
    state_dat_nofixneg %>% mutate(type = "csselm_nofixnegs")
)


# Look at negative fixes

rc_comp2 <- bind_rows(
    rc_CSSE1_fixnegs %>% mutate(type = "cssejh", fixnegs = TRUE),
    rc_CSSE1 %>% mutate(type = "cssejh", fixnegs = FALSE),
    rc_CSSE1_fixnegs_old %>% mutate(type = "cssejh_old", fixnegs = TRUE),
    rc_CSSE1_old %>% mutate(type = "cssejh_old", fixnegs = FALSE),
    rc_covidcast %>% mutate(type = "covidcast", fixnegs = TRUE),
    rc_covidcast_nofixnegs %>% mutate(type = "covidcast", fixnegs = FALSE),
    state_dat_fixneg_pkg %>% mutate(type = "csselm", fixnegs = TRUE),
    state_dat_nofixneg %>% mutate(type = "csselm", fixnegs = FALSE)
) %>%
    pivot_longer(cols=c(Confirmed, Deaths, incidI, incidDeath), names_to = "target", values_to = "value") %>%
    group_by(FIPS, source, Update, type, target) %>%
    arrange(fixnegs) %>%
    summarise(value_prefix = value[fixnegs==FALSE],
              value_postfix = value[fixnegs==TRUE],
              value_diffs = value[fixnegs==TRUE] - value[fixnegs==FALSE]) 

rc_comp2 %>%
    filter(source %in% c("FL", "TN","MO","OK")) %>%
    filter(type=="cssejh", target=="incidI") %>%
    pivot_longer(cols=c(value_prefix, value_postfix), names_to = "prepost", values_to = "value") %>%
    ggplot(aes(x=Update, y=value, color=prepost)) +
    scale_color_discrete("Fixing Negatives") +    
    ggtitle("incidI - IDD code") +
    geom_point() +
    theme_bw() +
    facet_wrap(~source, nrow = 2, scales = "free_y")

rc_comp2 %>%
    filter(source %in% c("FL", "TN","MO","OK")) %>%
    filter(type=="cssejh", target=="Confirmed") %>%
    pivot_longer(cols=c(value_prefix, value_postfix), names_to = "prepost", values_to = "value") %>%
    ggplot(aes(x=Update, y=value, color=prepost)) +
    scale_color_discrete("Fixing Negatives") +
    ggtitle("COnfirmed - IDD code") +
    geom_point() +
    theme_bw() +
    facet_wrap(~source, nrow = 2, scales = "free_y")

rc_comp2 %>%
    filter(source %in% c("FL", "TN","MO","OK")) %>%
    filter(type=="cssejh", target=="incidDeath") %>%
    pivot_longer(cols=c(value_prefix, value_postfix), names_to = "prepost", values_to = "value") %>%
    ggplot(aes(x=Update, y=value, color=prepost)) +
    scale_color_discrete("Fixing Negatives") +
    ggtitle("IncidDeath - IDD code") +
    geom_point() +
    theme_bw() +
    facet_wrap(~source, nrow = 2, scales = "free_y")

rc_comp2 %>%
    filter(source %in% c("FL", "TN","MO","OK")) %>%
    filter(type=="cssejh_old", target=="incidDeath") %>%
    pivot_longer(cols=c(value_prefix, value_postfix), names_to = "prepost", values_to = "value") %>%
    ggplot(aes(x=Update, y=value, color=prepost)) +
    scale_color_discrete("Fixing Negatives") +
    ggtitle("IncidDeath - IDD county-fix code (old version)") +
    geom_point() +
    theme_bw() +
    facet_wrap(~source, nrow = 2, scales = "free_y")


rc_comp2 %>%
    filter(source %in% c("FL", "TN","MO","OK")) %>%
    filter(type=="csselm", target=="incidDeath") %>%
    pivot_longer(cols=c(value_prefix, value_postfix), names_to = "prepost", values_to = "value") %>%
    ggplot(aes(x=Update, y=value, color=prepost)) +
    scale_color_discrete("Fixing Negatives") +
    ggtitle("IncidDeath - RawCOVIDdata") +
    geom_point() +
    theme_bw() +
    facet_wrap(~source, nrow = 2, scales = "free_y")










rc_us <- rc_comp %>%
    group_by(Update, type) %>%
    summarise(across(c(Confirmed, Deaths, incidI, incidDeath), ~sum(.x, na.rm = TRUE))) %>%
    mutate(source="US", FIPS="US")

rc_us %>% ggplot(aes(Update, Confirmed, color=type)) +
    geom_point()



rc_us %>% 
    select(Update, type, incidI, incidDeath, source, FIPS) %>%
    group_by(Update, source, FIPS) %>%
    mutate(incidI = incidI[type=="covidcast_nofixnegs"] - incidI) %>%
    ungroup() %>%
    ggplot(aes(Update, incidI, color=type)) +
    geom_point()

rc_us %>% 
    select(Update, type, incidI, incidDeath, source, FIPS) %>%
    group_by(Update, source, FIPS) %>%
    mutate(incidI = incidI[type=="covidcast_nofixnegs"] - incidI) %>%
    ungroup() %>%
    group_by(type, source, FIPS) %>%
    summarise(incidIdiffs = sum(incidI, na.rm = TRUE))


rc_us %>% 
    select(Update, type, incidI, incidDeath, source, FIPS) %>%
    group_by(Update, source, FIPS) %>%
    mutate(incidI = incidI[type=="csselm_fixnegs"] - incidI) %>%
    ungroup() %>%
    ggplot(aes(Update, incidI, color=type)) +
    geom_point()

rc_us %>% 
    select(Update, type, incidI, incidDeath, source, FIPS) %>%
    group_by(Update, source, FIPS) %>%
    mutate(incidI = incidI[type=="csselm_fixnegs"] - incidI) %>%
    ungroup() %>%
    group_by(type, source, FIPS) %>%
    summarise(incidIdiffs = sum(incidI, na.rm = TRUE))

rc_us %>% 
    select(Update, type, incidI, incidDeath, source, FIPS) %>%
    group_by(type, source, FIPS) %>%
    summarise(incidI = sum(incidI, na.rm = TRUE))

rc_us %>% 
    select(Update, type, incidI, incidDeath, Confirmed, Deaths, source, FIPS) %>%
    group_by(type, source, FIPS) %>%
    summarise(incidI = sum(incidI, na.rm = TRUE),
              incidDeath = sum(incidDeath, na.rm = TRUE),
              Confirmed = sum(Confirmed, na.rm = TRUE),
              incidDeathsDeath = sum(Deaths, na.rm = TRUE))

# Look at total days with negatives
rc_comp %>%
    mutate(across(c(Confirmed, Deaths, incidI, incidDeath), ~.x<0)) %>%
    group_by(Update, type) %>%
    summarise(across(c(Confirmed, Deaths, incidI, incidDeath), ~sum(.x==1, na.rm = TRUE))) %>%
    mutate(source="US", FIPS="US")


select(Update, type, incidI, incidDeath, Confirmed, Deaths, source, FIPS) %>%
    group_by(type, source, FIPS) %>%
    summarise(incidI = sum(incidI<0, na.rm = TRUE),
              incidDeath = sum(incidDeath<0, na.rm = TRUE),
              Confirmed = sum(Confirmed<0, na.rm = TRUE),
              incidDeathsDeath = sum(Deaths<0, na.rm = TRUE))






# Figure out descrep in raw data

deaths_comp <- rc_comp %>%
    group_by(Update, source, type) %>%
    summarise(across(c(Confirmed, Deaths, incidI, incidDeath), ~sum(.x, na.rm = TRUE))) %>%
    select(Update, type, incidI, incidDeath, source) %>%
    group_by(type, source) %>%
    summarise(incidDeath = sum(incidDeath, na.rm = TRUE)) %>%
    pivot_wider(names_from = type, values_from = incidDeath)


incidI_comp <- rc_comp %>%
    group_by(Update, source, type) %>%
    summarise(across(c(Confirmed, Deaths, incidI, incidDeath), ~sum(.x, na.rm = TRUE))) %>%
    select(Update, type, incidI, incidDeath, source) %>%
    group_by(type, source) %>%
    summarise(incidI = sum(incidI, na.rm = TRUE)) %>%
    group_by(source) %>%
    mutate(incidI = incidI[type=="covidcast_nofixnegs"] - incidI) %>%
    pivot_wider(names_from = type, values_from = incidI)


rc_comp %>% 
    filter(source=="UT") %>%
    ggplot(aes(Update, incidI, color=type)) +
    geom_point()

rc_comp %>% 
    filter(source=="UT") %>%
    ggplot(aes(Update, Confirmed, color=type)) +
    geom_point()










rc_us %>% 
    select(Update, type, incidI, source, FIPS) %>%
    pivot_wider(names_from = type, values_from = incidI) %>%
    mutate(csse1 = covidcast-csse1,
           csselm = covidcast-csselm) %>% 
    pivot_longer(cols=c(-Update, -source, -FIPS), names_to = "type", values_to = "diffs") %>%
    ggplot(aes(Update, diffs, color=type)) +
    geom_point()

rc_us %>% 
    select(Update, type, incidI, source, FIPS) %>%
    ggplot(aes(Update, incidI, color=type)) +
    geom_point()

rc_comp %>% filter(source=="NY") %>%
    select(Update, type, incidI, source, FIPS) %>%
    ggplot(aes(Update, incidI, color=type)) +
    geom_point()
rc_comp %>% filter(source=="MO") %>%
    select(Update, type, incidI, source, FIPS) %>%
    ggplot(aes(Update, incidI, color=type)) +
    geom_point()

rc_us %>% 
    select(Update, type, incidDeath, source, FIPS) %>%
    pivot_wider(names_from = type, values_from = incidDeath) %>%
    mutate(csse1 = covidcast-csse1,
           csselm = covidcast-csselm) %>% 
    pivot_longer(cols=c(-Update, -source, -FIPS), names_to = "type", values_to = "diffs") %>%
    ggplot(aes(Update, diffs, color=type)) +
    geom_point()

rc_us %>% 
    select(Update, type, incidDeath, source, FIPS) %>%
    pivot_wider(names_from = type, values_from = incidDeath) %>%
    mutate(csse1 = covidcast-csse1,
           csselm = covidcast-csselm) %>% select(-covidcast) %>%
    pivot_longer(cols=c(-Update, -source, -FIPS), names_to = "type", values_to = "diffs") %>%
    ggplot(aes(Update, diffs, color=type)) +
    geom_point()

rc_us %>% 
    select(Update, type, Confirmed, source, FIPS) %>%
    pivot_wider(names_from = type, values_from = Confirmed) %>%
    mutate(csse1 = covidcast-csse1,
           csselm = covidcast-csselm) %>% select(-covidcast) %>%
    pivot_longer(cols=c(-Update, -source, -FIPS), names_to = "type", values_to = "diffs") %>%
    ggplot(aes(Update, diffs, color=type)) +
    geom_point()
