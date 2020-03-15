


library(tidyverse)



get_severe_age_shenzhen <- function( ){
    
    sym_dat <- data.frame(
        age_cat=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70+"),
        mild=c(7,3,13,22,15,21,17,4),
        moderate=c(13,9,21,64,40,46,49,12),
        severe=c(0,0,0,1,5,7,20,2)
    )
    fev_dat <- data.frame(
        age_cat=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70+"),
        no=c(6,3,3,13,6,10,16,4),
        yes=c(14,9,31,74,54,64,70,14)
    )
    
    
    dat <- full_join(sym_dat, fev_dat)
    dat <- dat %>% as.data.frame() %>% mutate(tot = yes+no)
    dat <- dat %>% mutate(not_severe = tot-severe)
    dat <- dat %>% mutate(p_severe = severe/tot)
    
    # Use stan
    
    library(rstanarm)
    
    t_prior <- student_t(df = 7, location = 0, scale = 2.5, autoscale = FALSE)
    fit1 <- stan_glm(cbind(severe, tot-severe) ~ age_cat, data = dat, 
                     family = binomial(link = "logit"), 
                     prior = t_prior, prior_intercept = t_prior,  
                     cores = 2, seed = 12345)
    
    PPD <- posterior_predict(fit1)
    prob <- PPD
    for(i in 1:nrow(PPD)){
        prob[i,] <- PPD[i,] / dat$tot
    }
    
    write_csv(prob %>% as.data.frame(), "data/severe_age_prob.csv")
    return(prob)
    
}










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
    
    # Load prob(severe | age) from shenzhen
    prob <- read_csv("data/severe_age_prob.csv")
    
    
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



