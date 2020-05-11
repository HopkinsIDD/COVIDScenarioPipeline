
#' @name create_delay_frame
#' Append one (or two) column(s) to a data frame with the incident (and optionally current) counts for the number of people in a compartment if the incident count, time (and duration) are derived from distributions of columns of `data`.
#' @param data A data frame with columns geoid, time, and others as specified in `local_config`.  The geoid column should contain unique identifiers for time series.  The time should be a lubridate date.  The distributions in the config can have their values substituted with other columns of data, so any such columns must also be included.
#' @param name character() The name of the compartment we are appending to `data`.  This is used to create the names of the new columns (e.g., hospitalization).
#' @param local_config A list with two or three sub-lists.  incidence, delay, duration.  Each should be able to be passed to covidcommon::as_random_distribution after substituting in columns from `data`.  The incidence element should generate the number of people who enter the `name` compartment based on the state at the current time.  The delay element delay should keep track of how much time will pass from the current time until they enter `name`.  The duration element should keep track of how long they will stay in `name` before leaving.  The duration element is optional.
#' @return A data frame that is identical to data with one or two new columns.  The first new column `name`_incident is the number of people entering `name` at that time for that time series.  The second column `name`_current is the number of people currently in that compartment.
#' @export
create_delay_frame <- function(data, name, local_config){
  library(foreach)

  all_dates <- unique(data$time)
  all_geoids <- sort(unique(data$geoid))

  for(i in which(local_config$incidence %in% names(data))){
    local_config$incidence[[i]] <- data[[local_config$incidence[[i]]]]
  }

  for(i in which(local_config$delay %in% names(data))){
    local_config$delay[[i]] <- data[[local_config$delay[[i]]]]
  }

  for(i in which(local_config$duration %in% names(data))){
    local_config$duration[[i]] <- data[[local_config$duration[[i]]]]
  }

  incidence_draw <- covidcommon::as_random_distribution(local_config$incidence)(n=nrow(data))

  incident_time <- foreach(time = seq_len(length(incidence_draw))) %do% {
    if(time %% 100 == 0){print(time / length(incidence_draw))}
    if(incidence_draw[time] > 0){
      data.frame(
        geoid = data$geoid[[time]],
        time = data$time[[time]] + lubridate::days(round(
          covidcommon::as_random_distribution(local_config$delay)(incidence_draw[time])
        ))
      ) %>%
        dplyr::group_by(geoid,time) %>%
        dplyr::summarize(count = length(time)) %>%
        ungroup()
    } else {
      NULL
    }
  }
  incident_time <- do.call(dplyr::bind_rows,incident_time) %>%
    dplyr::filter(count > 0) %>%
    dplyr::group_by(geoid,time) %>%
    dplyr::summarize(count = sum(count))

  using_release <- "duration" %in% names(local_config)
  if(using_release){ #only set up current value for things that have durations
    release_time <- incident_time %>%
      dplyr::group_by(geoid,time) %>%
      dplyr::group_map(function(.x,.y){
        if(.x$count > 0){
          data.frame(
            geoid= .y$geoid,
            time = .y$time + lubridate::days(round(
              covidcommon::as_random_distribution(local_config$duration)(.x$count)
            ))
          ) %>%
            dplyr::group_by(geoid,time) %>%
            dplyr::summarize(count = length(time)) %>%
            dplyr::ungroup() %>%
            return
        } else {
          return(data.frame(
            geoid= .y$geoid,
            time = .y$time,
            count = 0
          ))
        }
      }) %>%
      do.call(what=rbind) %>%
      dplyr::group_by(geoid,time) %>%
      dplyr::summarize(count = sum(count))
  }

  all_geoid_dates_frame <- tidyr::expand_grid(data.frame(geoid = all_geoids),data.frame(time = all_dates))

  if(!using_release){
    release_time <- all_geoid_dates_frame[1,]
  }

  release_time$type <- "release"
  incident_time$type <- "incident"
  all_geoid_dates_frame$type <- "test"
  current_time <- dplyr::bind_rows(
    release_time,
    incident_time,
    all_geoid_dates_frame
  ) %>%
    dplyr::group_by(geoid,time) %>%
    tidyr::pivot_wider(
      c('geoid','time'),names_from=type,values_from=count,values_fn = list(count=sum), values_fill=c(count=0)
    ) %>%
    dplyr::mutate(
      change = incident - release
    ) %>%
    dplyr::group_by(geoid) %>%
    dplyr::arrange(time) %>%
    dplyr::group_modify(function(.x,.y){
      .x$current = cumsum(.x$change)
      return(.x[,c('time','current','incident')])
    }) %>%
    dplyr::filter(
      time %in% all_dates,
      geoid %in% all_geoids
    ) %>%
    dplyr::arrange(geoid,time) %>%
    ungroup()

  data <- dplyr::arrange(data,geoid,time)

  data[[paste(name,"incident",sep='_')]] <- current_time$incident
  if(using_release){
    data[[paste(name,"current",sep='_')]] <- current_time$current
  }

  return(data)
}

hosp_create_delay_frame <- function(X, p_X, data_, X_pars, varname) {
    X_ <- rbinom(length(data_[[X]]),data_[[X]],p_X)
    rc <- data.table::data.table(
      time = data_$time + round(exp(X_pars[1] + X_pars[2]^2 / 2)),
      uid = data_$uid,
      count = X_
    )
    names(rc)[3] <- paste0("incid",varname)
    return(rc)
}

##'
##' Data loading utility function for this package.
##'
#' Load the csv associated with a single scenario
#' @param scenario_dir The directory to load scenarios from
#' @param sim_id  integer Which file of the files in `scenario_dir`.
#' @param keep_compartments character Optional names of compartments to keep (and throw away others).  Default is to keep all compartments
#' @param time_filter_low lubridate::date time_filter_low Only keep rows that have time after this
#' @param time_filter_high lubridate::date time_filter_low Only keep rows that have time before this
#' @param geod_len The minimum length each geoid should be
#' @param padding_char If geoids need to be increased in length, which character should be used to add length
#' @param use_parquet Whether to save to parquet files rather than csvs
hosp_load_scenario_sim <- function(scenario_dir,
                                   sim_id,
                                   keep_compartments=NULL,
                                   time_filter_low = -Inf,
                                   time_filter_high = Inf,
                                   geoid_len = 0,
                                   padding_char = "0",
                                   use_parquet = FALSE
    ) {

    if (geoid_len > 0) {
      padfn <- function(x) {x%>% dplyr::mutate(geoid = str_pad(geoid,width=geoid_len,pad=padding_char))}
    } else {
      padfn <- function(x) {x}
    }

    file <- paste0(scenario_dir,'/',str_pad(sim_id, width=9, pad="0"),'.seir')
    if(use_parquet){
      file <- paste0(file,'.parquet')
      tmp <- arrow::read_parquet(file)
      if("POSIXct" %in% class(tmp$time)){
        tmp$time <- lubridate::as_date(tz="GMT",tmp$time)
      }
    } else {
      file <- paste0(file,'.csv')
      suppressMessages(tmp <- read_csv(file))
    }
    if (!is.null(keep_compartments)) {
      suppressMessages(tmp <- tmp %>% filter(comp %in% keep_compartments))
    }

    tmp <- #tmp[-1,] %>%
        tmp %>%
        filter(time <= time_filter_high & time >= time_filter_low) %>%
        pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>%
        mutate(sim_num = sim_id) %>%
        padfn
    return(tmp)
}

write_hosp_output <- function(root_out_dir, data_dir, dscenario_name, sim_id, res, use_parquet)
{
  # Write output
  outdir <- paste0(root_out_dir,'/', data_dir,'/')
  if(!dir.exists(outdir)){
    dir.create(outdir,recursive=TRUE)
  }
  outfile <- paste0(outdir,dscenario_name,"_death-",str_pad(sim_id, width=9, pad="0"),'.hosp')
  if(use_parquet){
    outfile <- paste0(outfile, '.parquet')
    arrow::write_parquet(res,outfile)
  } else {
    outfile <- paste0(outfile,'.csv')
    data.table::fwrite(res,outfile)
  }
}


##'
##' Build a set of sampled hospitalizations, deaths, and recoveries
##' from the incident infection data from the simulation model.
##'
##' @param p_hosp probability of hospitalization, among infections
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param p_ICU probability of needing the ICU among hospitalized patients
##' @param p_vent probability of needing a ventilator among ICU patients
##' @param data_dir Path to the directory that contains the CSV output of the simulation model
##' @param dscenario_name The name of the death scenario we are analyzing here (e.g., "highdeath", "meddeath", etc.)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_ICU_pars parameters for time from hospitalization to ICU
##' @param time_vent_pars parameters for time from ICU to time on ventilator
##' @param time_hosp_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' @param time_ICUdur_pars parameetrs for time of ICU duration
##' @param cores The number of CPU cores to run this model on in parallel
##' @param root_out_dir Path to the directory to write the outputs of this analysis
##' @param use_parquet Whether to save to parquet files rather than csvs
##'
##' @export
build_hospdeath_par <- function(p_hosp,
                                p_death,
                                p_ICU,
                                p_vent,
                                data_dir,
                                dscenario_name,
                                time_hosp_pars = c(1.23, 0.79),
                                time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                time_hosp_death_pars = c(log(11.25), log(1.15)),
                                time_disch_pars = c(log(11.5), log(1.22)),
                                time_ICUdur_pars = c(log(17.46), log(4.044)),
                                time_ventdur_pars = log(17),
                                cores=8,
                                root_out_dir='hospitalization',
                                use_parquet = FALSE,
                                start_sim = 1,
                                num_sims = -1) {

  n_sim <- ifelse(num_sims < 0, length(list.files(data_dir)), num_sims)
  print(paste("Creating cluster with",cores,"cores"))
  doParallel::registerDoParallel(cores)

  if(n_sim == 0){
    stop("No simulations selected to run")
  }

  print(paste("Running over",n_sim,"simulations"))

  pkgs <- c("dplyr", "readr", "data.table", "tidyr", "hospitalization")
  foreach::foreach(s=seq_len(n_sim), .packages=pkgs) %dopar% {
    sim_id <- start_sim + s - 1
    dat_ <- hosp_load_scenario_sim(data_dir,sim_id,
                                   keep_compartments = "diffI",
                                   geoid_len = 5,
                                   use_parquet = use_parquet) %>%
      mutate(hosp_curr = 0,
             icu_curr = 0,
             vent_curr = 0,
             uid = paste0(geoid, "-",sim_num)) %>%
      rename(incidI = N)

    # Add time things
    dat_H <- hosp_create_delay_frame('incidI',p_hosp,dat_,
                                     time_hosp_pars,"H")
    data_ICU <- hosp_create_delay_frame('incidH',p_ICU,dat_H,
                                        time_ICU_pars,"ICU")
    data_Vent <- hosp_create_delay_frame('incidICU',p_vent,data_ICU,
                                         time_vent_pars,"Vent")
    data_D <- hosp_create_delay_frame('incidH',p_death,dat_H,
                                      time_hosp_death_pars,"D")
    R_delay_ <- round(exp(time_disch_pars[1]))
    ICU_dur_ <- round(exp(time_ICUdur_pars[1]))
    Vent_dur_ <- round(exp(time_ventdur_pars[1]))

    stopifnot(is.character(dat_H$uid) && is.character(data_ICU$uid) &&
              is.character(data_Vent$uid) && is.character(data_D$uid) &&
              is.character(dat_$uid))
    res <- Reduce(function(x, y, ...) merge(x, y, all = TRUE, ...),
                  list(dat_, dat_H, data_ICU, data_Vent, data_D)) %>%
      replace_na(
        list(incidI = 0,
             incidH = 0,
             incidICU = 0,
             incidVent = 0,
             incidD = 0,
             vent_curr = 0,
             hosp_curr = 0)) %>%
      # get sim nums
      select(-geoid, -sim_num) %>%
      separate(uid, c("geoid", "sim_num"), sep="-", remove=FALSE) %>%
      mutate(date_inds = as.integer(time - min(time) + 1),
             geo_ind = as.numeric(as.factor(geoid))) %>%
      arrange(geo_ind, date_inds) %>%
      split(.$geo_ind) %>%
      purrr::map_dfr(function(.x){
        .x$hosp_curr <- cumsum(.x$incidH) - lag(cumsum(.x$incidH),
                                                n=R_delay_,default=0)
        .x$icu_curr <- cumsum(.x$incidICU) - lag(cumsum(.x$incidICU),
                                                 n=ICU_dur_,default=0)
        .x$vent_curr <- cumsum(.x$incidVent) - lag(cumsum(.x$incidVent),
                                                   n=Vent_dur_)
        return(.x)
      }) %>%
      replace_na(
        list(vent_curr = 0,
             icu_curr = 0,
             hosp_curr = 0)) %>%
      arrange(date_inds, geo_ind)
    write_hosp_output(root_out_dir, data_dir, dscenario_name, sim_id, res, use_parquet)
    NULL
  }
  doParallel::stopImplicitCluster()
}


##'
##' Build a set of sampled hospitalizations, deaths, and recoveries
##' from the incident infection data from the simulation model.
##'
##' @param prob_dat df of p_hosp, p_death, p_ICU, p_vent, GEOID
##' @param p_death probability of death, among infections (user-defined)
##' @param p_hosp_inf probability of hospitalization among infections (user-defined)
##' @param data_dir Path to the directory that contains the CSV output of the simulation model
##' @param dscenario_name The name of the scenario we are analyzing here (e.g., "highdeath", "meddeath", etc.)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_ICU_pars parameters for time from hospitalization to ICU
##' @param time_vent_pars parameters for time from ICU to time on ventilator
##' @param time_onset_death_pars parameters for time from onset to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' @param time_ICUdur_pars parameetrs for time of ICU duration
##' @param cores The number of CPU cores to run this model on in parallel
##' @param root_out_dir Path to the directory to write the outputs of this analysis
##'
##' @export
build_hospdeath_geoid_fixedIFR_par <- function(
  prob_dat,
  p_death,
  p_hosp_inf,
  data_dir,
  dscenario_name,
  time_hosp_pars = c(1.23, 0.79),
  time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
  time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
  time_onset_death_pars = c(log(11.25), log(1.15)),
  time_disch_pars = c(log(11.5), log(1.22)),
  time_ICUdur_pars = c(log(17.46), log(4.044)),
  time_ventdur_pars = log(17),
  cores=8,
  root_out_dir='hospitalization',
  use_parquet = FALSE,
  start_sim = 1,
  num_sims = -1
) {
  n_sim <- ifelse(num_sims < 0, length(list.files(data_dir)), num_sims)
  print(paste("Creating cluster with",cores,"cores"))
  doParallel::registerDoParallel(cores)

  if(n_sim == 0){
    stop("No simulations selected to run")
  }

  ## scale prob_dat to match defined IFR, p_hosp_inf
  prob_dat$p_death_inf_scaled <- prob_dat$rr_death_inf * p_death
  prob_dat$p_hosp_inf_scaled <- prob_dat$rr_hosp_inf * p_hosp_inf


  print(paste("Running over",n_sim,"simulations"))

  pkgs <- c("dplyr", "readr", "data.table", "tidyr", "hospitalization")
  foreach::foreach(s=seq_len(n_sim), .packages=pkgs) %dopar% {
    sim_id <- start_sim + s - 1
    dat_I <- hosp_load_scenario_sim(data_dir, sim_id,
                                   keep_compartments = "diffI",
                                   geoid_len=5,
                                   use_parquet = use_parquet) %>%
      mutate(hosp_curr = 0,
             icu_curr = 0,
             vent_curr = 0,
             uid = paste0(geoid, "-",sim_num)) %>%
      rename(incidI = N) %>%
      as.data.table()
    dat_ <- dat_I %>%
      left_join(prob_dat, by="geoid")

    # Add time things
    dat_H <- hosp_create_delay_frame('incidI',
                                     dat_$p_hosp_inf_scaled,
                                     dat_,
                                     time_hosp_pars,"H")
    data_ICU <- hosp_create_delay_frame('incidH',
                                        dat_$p_icu_hosp,
                                        dat_H,
                                        time_ICU_pars,"ICU")
    data_Vent <- hosp_create_delay_frame('incidICU',
                                         dat_$p_vent_icu,
                                         data_ICU,
                                         time_vent_pars,"Vent")
    data_D <- hosp_create_delay_frame('incidI',
                                      dat_$p_death_inf_scaled,
                                      dat_,
                                      time_onset_death_pars,"D")
    R_delay_ <- round(exp(time_disch_pars[1]))
    ICU_dur_ <- round(exp(time_ICUdur_pars[1]))
    Vent_dur_ <- round(exp(time_ventdur_pars[1]))

    stopifnot(is.data.table(dat_I) && is.data.table(dat_H) && is.data.table(data_ICU) && is.data.table(data_Vent) && is.data.table(data_D))

    # Using `merge` instead of full_join for performance reasons
    res <- Reduce(function(x, y, ...) merge(x, y, all = TRUE, ...),
                  list(dat_I, dat_H, data_ICU, data_Vent, data_D)) %>%
      replace_na(
        list(incidI = 0,
             incidH = 0,
             incidICU = 0,
             incidVent = 0,
             incidD = 0,
             vent_curr = 0,
             hosp_curr = 0)) %>%
      # get sim nums
      select(-geoid, -sim_num) %>%
      separate(uid, c("geoid", "sim_num"), sep="-", remove=FALSE) %>%
      mutate(date_inds = as.integer(time - min(time) + 1),
             geo_ind = as.numeric(as.factor(geoid))) %>%
      arrange(geo_ind, date_inds) %>%
      split(.$geo_ind) %>%
      purrr::map_dfr(function(.x){
        .x$hosp_curr <- cumsum(.x$incidH) - lag(cumsum(.x$incidH),
                                                n=R_delay_,default=0)
        .x$icu_curr <- cumsum(.x$incidICU) - lag(cumsum(.x$incidICU),
                                                 n=ICU_dur_,default=0)
        .x$vent_curr <- cumsum(.x$incidVent) - lag(cumsum(.x$incidVent),
                                                   n=Vent_dur_)
        return(.x)
      }) %>%
      replace_na(
        list(vent_curr = 0,
             icu_curr = 0,
             hosp_curr = 0)) %>%
      arrange(date_inds, geo_ind)

    write_hosp_output(root_out_dir, data_dir, dscenario_name, sim_id, res, use_parquet)
    NULL
  }
  doParallel::stopImplicitCluster()
}
