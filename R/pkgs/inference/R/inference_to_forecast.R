##' Functoin that takes the results of an inference run, a date
##' and a set of cumulative numbers and and results from the simulation.
##'
##' @param sim_data data from the simse to use. already aggregated to correct spatial scale
##' @param start_date the date to start from
##' @param cum_dat the cumulative data on start date. should have a column to join to sta on and cumDeaths
##' @param loc_column whihc column defines location
##'
##' @return a data frame with daily cumulative datra starting from start_date
##'
##'
##' @export
cum_death_forecast <- function (sim_data,
                               start_date,
                               cum_dat,
                               loc_column) {
    require(dplyr)

    rc <- sim_data %>%
        filter(time>start_date)%>%
        inner_join(cum_dat)%>%
        group_by(sim_num, !!sym(loc_column))%>%
        mutate(cum_deaths_corr = cumsum(incidD)+cumDeaths)%>%
        ungroup()

    
    return(rc)    
    
}

##'
##' Creates a merged forecast from a data object and a set of sims.
##' 
##' @param sim_data simulated data.
##' @param obs_data the observed data.
##' @param forecast_date the date to forecast from (i.e., last date from data)
##' @param aggregation the level of aggregation to use.
##' @param quants the quantiles to return
##' @param weights if not NA, the weights for the mean
##' @param loc_col the name of the  location column
##' 
##' @return a forecast with columns time (end day), quantile steps_ahead and deaths
##' 
##' @export
create_cum_death_forecast <- function(sim_data, 
                                 obs_data,
                                 forecast_date,
                                 aggregation="day",
                                 quants=c(.01,.025, seq(.05,.95,.5),.975,.99),
                                 weights=NA,
                                 loc_column="USPS") {
    
    ##Sanity checks
    if(forecast_date>max(obs_data$time)) {stop("forecast date must be within range of observed times")}
    if(forecast_date+1<min(sim_data$time)) {stop("no simulation support for first forecast date")}
    
    ##convert data to a cumdeath forecast.
    start_deaths <- obs_data%>%
        filter(time==forecast_date)%>%
        select(!!sym(loc_column),cumDeaths)
    
    forecast_sims <- cum_death_forecast(sim_data,
                                        forecast_date,
                                        start_deaths,
                                        loc_column)
    
    ##aggregated data to the right scale
    if (aggregation=="day") {
        ##NOOP
    } else {
        stop("unknown aggregatoin period")
    }
    
    rc <- forecast_sims%>%
        group_by(time, !!sym(loc_column))%>% 
         summarize(x=list(enframe(c(quantile(cum_deaths_corr, probs=c(0.01, 0.025,
                                                                seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
                                           mean=mean(cum_deaths_corr)),
                                            "quantile","cumDeaths"))) %>%
                    unnest(x)
    
    
    ##Append on the the other deaths.
    rc<-dplyr::bind_rows(rc,
            obs_data%>%
                select(time, !!sym(loc_column), cumDeaths)%>%
                mutate(quantile="data"))
    
    rc<- rc%>%
        mutate(steps_ahead=as.numeric(time-forecast_date))
    
    return(rc)
    
}

