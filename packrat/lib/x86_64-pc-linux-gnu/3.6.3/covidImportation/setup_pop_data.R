
# SETUP POPULATION DATA - FROM WORLDPOP

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)


# CHINA PROVINCE DATA -----------------------------------------------------

make_all_pop_data <- function(){
    
    
    china_pop_data <- read_csv("data-raw/China_popsize_census_2010.csv")
    china_pop_data <- china_pop_data %>% rename(loc = province, population=pop)
    china_pop_data <- china_pop_data %>% mutate(country = "China", country_code="CHN") %>% rename(pop = population)
    write_csv(china_pop_data, "data-raw/china_province_pop_data.csv")
    
    
    # COUNTRY DATA
    
    country_pop_url <- "https://pkgstore.datahub.io/JohnSnowLabs/population-figures-by-country/population-figures-by-country-csv_csv/data/630580e802a621887384f99527b68f59/population-figures-by-country-csv_csv.csv"
    country_pop_data <- readr::read_csv(country_pop_url)
    
    country_pop_data <- country_pop_data %>% gather(key="year", value="pop", -Country, -Country_Code) %>% 
        mutate(year = as.integer(substr(year, 6,9)))
    
    #take only the most recent year
    country_pop_data <- country_pop_data %>% rename(country = Country, country_code=Country_Code) %>% 
        filter(!is.na(year) & !is.na(pop)) %>%
        group_by(country, country_code) %>%
        filter(year == max(year)) %>% ungroup()
    
    
    
    # US STATE DATA
    # - source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html
    us_state_pop <- readr::read_csv("data-raw/us_state_pop.csv") %>% rename(state_name=state)
    us_state_pop <- full_join(us_state_pop, 
                              data.frame(state=state.abb, state_name=state.name)) %>%
        mutate(state = as.character(state))
    us_state_pop$state[grepl("District", us_state_pop$state_name, ignore.case = TRUE)] <- "DC"
    us_state_pop$state[grepl("Puerto", us_state_pop$state_name, ignore.case = TRUE)] <- "PR"
    us_state_pop <- us_state_pop %>% rename(pop = population)
    
    
    
    # MERGE
    
    colnames(country_pop_data)
    colnames(china_pop_data)
    colnames(us_state_pop)
    
    # ~ Country and China Province
    pop_data <- full_join(country_pop_data %>% rename(country_name=country, country=country_code) %>% mutate(location = country) %>% select(-year), 
                          china_pop_data %>% rename(country_name=country, country=country_code), 
                          by=c("location"="loc", "pop"="pop", "country"="country", "country_name")) %>% mutate(pop = as.integer(pop))
    
    # ~ US State
    colnames(pop_data)
    pop_data <- full_join(pop_data, 
                          us_state_pop %>%
                              mutate(country_name="United States of America", country="USA") %>% 
                              rename(location = state))
    
    # fix some randoms 
    other_pops <- read_csv("data-raw/other_locations_pop.csv")
    pop_data <- bind_rows(pop_data, other_pops)
    pop_data <- pop_data %>% rename(source = location)
    

    write_csv(pop_data, "data/pop_data.csv")
    
}    



# Function to get population data wanted
##'
##' Return the population of the country or Chinese province of interest.
##' 
##' @param loc The location or locations of which population is needed. This can be a vector.
##'
##' @return a named vector of populations of locations
##' 
get_population <- function(loc = "Singapore"){
    pop_data <- readr::read_csv("data/pop_data.csv", col_types = cols())
    
    match_ <- match(loc, pop_data$location)
    
    pops <- as.integer(pop_data$population[match_])
    names(pops) <- loc
    
    return(pops)
}

## EXAMPLE:
# get_population(c("Aruba", "India", "Germany"))





# Loading World Pop data
# {load("../Data/WorldPop_longform_1960.to.2017.RData")
#     WorldPop <- WorldPop %>% mutate(Country.Code = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
#         filter(!is.na(Country.Code))
#     
#     # if (!any(WorldPop$Year == 2016)) WorldPop <- WorldPop %>% union(filter(WorldPop, Year == 2015) %>% mutate(Year = 2016))     # Assuming 2015 population for 2016
#     # Extrapolating for pop of 2017
#     for (this.country in unique(WorldPop$Country)) {
#         # print(this.country)
#         
#         Fit <- lm(Population ~ Year, data = filter(WorldPop, Country == this.country, Year >= 2008))
#         
#         Correlate.vars <- data.frame(Country = this.country,
#                                      Country.Code = countrycode(this.country, origin = "country.name", destination = "iso3c"),
#                                      Year = 2018:2019)
#         
#         Preds <- predict(Fit, newdata = Correlate.vars)
#         
#         New.Rows <- Correlate.vars %>% mutate(Population = round(Preds))
#         
#         WorldPop <- WorldPop %>% bind_rows(New.Rows)
#     }
#     Years_pop <- sort(unique(WorldPop$Year))
#     Countries_pop <- str_replace_all(WorldPop$Country, " ", ".") %>% unique
#     Country.Codes_pop <- WorldPop$Country.Code %>% unique}
