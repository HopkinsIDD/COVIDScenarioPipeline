













##' 
##' Get population distribution and aggregate it to 10 year age groups
##' 
##' @param country country of interest
##' 
##' 
get_age_pop <- function(country){
    
    require(stringi)
    #require(globaltoolbox)
    
    pop_data <- read_csv("data/WPP2019_POP.csv")
    pop_data <- pop_data %>% 
        mutate(country_clean = stringi::stri_trans_general(location, "Latin-ASCII")) %>%
        filter(tolower(country_clean) == tolower(country)) %>% filter(year==max(year))
    
    # print for a double check
    print(pop_data$location)
    pop_data <- pop_data[,-(1:4)] %>% select(-country_clean)
    dat <- as.numeric(pop_data)
    names(dat) <- colnames(pop_data)
    return(dat)
}





##' 
##' Get population distribution and aggregate it to 10 year age groups
##' 
##' @param country country of interest
##' 
get_p_severe <- function(country="China"){
    
    #  population by age
    nage_ <- get_age_pop(country) * 1000 
    nage_[8] <- sum(nage_[8:11])
    nage_ <- nage_[1:8] 
    pr_age10_ <- nage_ / sum(nage_)
    
    p_severe_tmp <- prob
    for(i in 1:nrow(prob)){
        p_severe_tmp[i,] <- prob[i,] * pr_age10_
    }
    p_severe_ <- rowSums(p_severe_tmp)
    
    fit_ <- fitdistrplus::fitdist(p_severe_, "gamma", "mle")
    
    
    p_severe_ <- list(mean=mean(p_severe_), 
                      ll=quantile(p_severe_, .025),
                      ul=quantile(p_severe_, .975),
                      q25=quantile(p_severe_, .25),
                      q75=quantile(p_severe_, .75),
                      shape = coef(fit_)["shape"],
                      rate = coef(fit_)["rate"])
    
    return(p_severe_)
}



