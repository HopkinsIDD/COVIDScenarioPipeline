library(tidyverse)
tmp <- jsonlite::fromJSON(httr::content(httr::GET("https://raw.githubusercontent.com/hodcroftlab/covariants/master/cluster_tables/USAClusters_data.json")))
tmp3 <- as.data.frame(tmp[[1]][["Maine"]])

tmp4 <- lapply(names(tmp[[1]]), function(state_name, start_date = lubridate::ymd("2020-01-01"), end_date) {
  tmp3 <- tmp[[1]][[state_name]]
  tmp3 <- as.data.frame(tmp3)
  tmp3 %>%
    dplyr::mutate(week = lubridate::ymd(week)) %>%
    tidyr::pivot_longer(names(tmp3)[-1]) %>%
    dplyr::mutate(name = gsub("[.].*", "", gsub("^[^.]*[.][.]", "", name))) %>% 
    dplyr::mutate(
      name = c(
        Delta = "DELTA",
        "Alpha" = "ALPHA",
        # "Beta" = "ALPHA",
        "Epsilon" = "ALPHA",
        "Gamma" = "ALPHA",
        # "Iota" = "ALPHA",
        "Kappa" = "ALPHA",
        "Lambda" = "ALPHA",
        "X21H" = "ALPHA",
        "X20B" = "ALPHA",
        total_sequences="total_sequences",
        NULL
      )[name]
    ) %>%
    group_by(week, name) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    group_by(week) %>%
    group_modify(function(.x,.y){
      .x <- .x[!is.na(.x$name),]
      .x$value = .x$value / .x$value[.x$name == "total_sequences"];
      .x$value[.x$name == "total_sequences"] <- 2 - sum(.x$value)
      .x$name[.x$name == "total_sequences"] <- "WILD"
      return(.x)}
    ) %>%
    ungroup() %>%
    group_by(name) %>%
    arrange(week) %>%
    group_modify(function(.x,.y){
      
      date_range <- start_date + 0:(end_date - start_date)
      date_value <- sapply(date_range, function(y){
        acceptable_dates <- (dplyr::lead(.x$week > y, default = TRUE)) & (.x$week <= y)
        if (any(acceptable_dates)) {
          date_index <- which(acceptable_dates)
          lhs_date <- .x$week[date_index]
          rhs_date <- .x$week[date_index+1]
          lhs_value <- .x$value[date_index]
          rhs_value <- .x$value[date_index+1]
          return(lhs_value + as.numeric(y - lhs_date) / as.numeric(rhs_date - lhs_date) * (rhs_value - lhs_value))
        } else {
          return(0)
        }
      })
      return(tibble::tibble(date = date_range, value = date_value))
     
    }) %>%
    dplyr::mutate(state = state_name) %>%
    dplyr::rename(Update = date, state_name = state, variant = name, prop = value) %>%
    return()
}, start_date = lubridate::ymd("2020-01-01"), end_date = lubridate::today() + 30) %>%
  do.call(what = rbind)
tmp5 <- tmp4 %>%
  group_by(state_name) %>%
  group_modify(function(.x,.y){
    predictor <- .x %>%
      filter(variant == "DELTA", !is.na(prop), prop > 0.01) %>%
      mutate(prop = ifelse(prop == 1, 1 - 1e-10, prop)) %>%
      lm(formula=boot::logit(prop)~Update, na.action = "na.exclude")
    final_proportions <- .x %>%
      filter(!is.na(prop)) %>%
      filter(Update == max(Update)) %>%
      tidyr::pivot_wider(names_from=variant, values_from=prop) %>%
      dplyr::mutate(
        alpha_prop = ALPHA / (1 - DELTA),
        wild_prop = WILD / (1 - DELTA)
      )
     predictor$coefficients["(Intercept)"] <- boot::logit(final_proportions$DELTA) - predictor$coefficients["Update"] * as.numeric(final_proportions$Update)

    .x <- .x %>%
      tidyr::pivot_wider(names_from=variant, values_from=prop) %>%
      dplyr::mutate(
        DELTA_NEW = boot::inv.logit(predict(predictor, data.frame(Update = Update))),
        ALPHA_NEW = final_proportions$alpha_prop * (1 - DELTA_NEW), 
        WILD_NEW = final_proportions$wild_prop * (1 - DELTA_NEW), 
        DELTA = ifelse(is.na(DELTA), DELTA_NEW, DELTA),
        DELTA = ifelse(Update > lubridate::ymd("2021-03-01"), DELTA, 0), 
        DELTA = pmin(1,DELTA), 
        DELTA = pmax(0,DELTA), 
        ALPHA = ifelse(is.na(ALPHA) & (Update < lubridate::ymd("2021-01-01")), 0, ALPHA),
        ALPHA = ifelse(is.na(ALPHA) & (Update >= lubridate::ymd("2021-01-01")), ALPHA_NEW, ALPHA),
        ALPHA = pmin(1,ALPHA), 
        ALPHA = pmax(0,ALPHA), 
        WILD = 1 - ALPHA - DELTA,
        WILD = pmin(1,WILD), 
        WILD = pmax(0,WILD)
      ) %>% 
      dplyr::select(Update, WILD, ALPHA, DELTA) %>%
      tidyr::pivot_longer(c("WILD", "ALPHA", "DELTA"), names_to = "variant", values_to = "prop")
      
    return(.x)
  }) 

library(datasets)
state_names <- c(state.name, c("Guam", "Puerto Rico", "Virgin Islands", "Washington DC"))
state_2_letter <- c(state.abb, "GU", "PR", "VI", "DC")
state_abbreviator <- setNames(state_2_letter, state_names)
tmp5$source <- state_abbreviator[tmp5$state_name]


tmp5 %>%
  dplyr::filter(!is.na(prop)) %>%
  ggplot() +
  geom_line(aes(x=Update,y=prop, col=variant,group=paste(variant, source))) +
  facet_wrap(~source)


write.csv(x=dplyr::select(dplyr::ungroup(tmp5), source, Update, variant, prop), row.names = FALSE, file = "~/git/COVID19_USA/data/variant/variant_props_long.csv")

