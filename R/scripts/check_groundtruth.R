


signals <- c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_cumulative_num")
signals <- c(signals,"confirmed_admissions_covid_1d")
variables = c("Confirmed", "Deaths", "incidI", "incidDeath", "incidH","Hospitalizations")

geo_level = "state"
signals = signals
limit_date = Sys.Date()
run_parallel = TRUE
n_cores=4
fix_negatives <- TRUE
incl_unass <- TRUE



# CSSe
rc_CSSE1 <- get_CSSE_US_data(tempfile(), tempfile(), incl_unassigned = incl_unass) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    dplyr::mutate(FIPS = paste0(stringr::str_sub(FIPS, 1, 2), "000")) %>%
    dplyr::group_by(Update, FIPS, source) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::drop_na(tidyselect::everything()) %>%
    dplyr::ungroup()

# csse LM

variables_ <- variables[!grepl("incidh|hosp", tolower(variables))] 
rc_csselm <- get_rawcoviddata_state_data(incl_unassigned=incl_unass, fix_negatives=fix_negatives) %>%
    dplyr::select(Update, FIPS, source, !!variables_) %>%
    tidyr::drop_na(tidyselect::everything())

if (any(grepl("hosp|incid", tolower(variables)))){
    
    variables_hosp <- variables[grepl("incidh|hosp", tolower(variables))] 
    rc_hosp <- get_covidcast_data(geo_level = "state",
                                  signals = "confirmed_admissions_covid_1d",
                                  limit_date = Sys.Date(),
                                  fix_negatives = fix_negatives,
                                  run_parallel = FALSE) %>%
        dplyr::select(Update, FIPS, source, !!variables_hosp) %>%
        tidyr::drop_na(tidyselect::everything())
    rc_csselm <- rc_csselm %>% dplyr::full_join(rc_hosp)
}

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
                                   run_parallel = FALSE) %>%
    dplyr::select(Update, FIPS, source, !!variables) %>%
    tidyr::drop_na(tidyselect::everything())


rc_comp <- rc_CSSE1 %>% mutate(type = "csse1") %>%
    bind_rows(rc_csselm %>% mutate(type = "csselm"),
              rc_covidcast %>% mutate(type = "covidcast"))

rc_us <- rc_comp %>%
    group_by(Update, type) %>%
    summarise(across(c(Confirmed, Deaths, incidI, incidDeath), ~sum(.x, na.rm = TRUE))) %>%
    mutate(source="US", FIPS="US")
#rc_comp <- rc_comp %>% bind_rows(rc_us)

rc_us %>% ggplot(aes(Update, Confirmed, color=type)) +
    geom_point()

rc_comp_diffs <- rc_comp %>% 
    select(Update, type, incidI, source, FIPS) %>%
    pivot_wider(names_from = type, values_from = incidI) %>%
    mutate(csse1 = covidcast-csse1,
           csselm = covidcast-csselm)
rc_comp_diffs %>% 
    filter(abs(csselm)>0) %>% View()





rc_us %>% 
    select(Update, type, incidI, source, FIPS) %>%
    pivot_wider(names_from = type, values_from = incidI) %>%
    mutate(csse1 = covidcast-csse1,
           csselm = covidcast-csselm) %>% select(-covidcast) %>%
    pivot_longer(cols=c(-Update, -source, -FIPS), names_to = "type", values_to = "diffs") %>%
    ggplot(aes(Update, diffs, color=type)) +
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
