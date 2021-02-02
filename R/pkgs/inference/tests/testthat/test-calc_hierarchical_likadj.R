context("calc_hierarchical_likadj")



test_that("penalty is  based on selected stat", {
    npi1 <- runif(6,-1,1)
    npi2 <- runif (6,-1,1)

    ##makes data frame with stats
    infer_frame <- data.frame(geoid=rep(c("01001","01002","01003",
                                          "06001", "06002", "06003"),2),
                              npi_name=rep(c("npi 1", "npi 2"), each=6),
                              reduction=c(npi1,npi2))




    ##make geodata dataframe
    geodata <- data.frame(geoid=c("01001","01002","01003",
                                  "06001", "06002","06003"),
                          USPS=rep(c("HI","CA"), each=3))



    tmp <- npi1[1:3]
    tmp2 <- npi1[4:6]
    expect_that(calc_hierarchical_likadj("npi 1",infer_frame,
                                         geodata,"USPS")$likadj,
                equals(c(dnorm(tmp,mean(tmp), max(sd(tmp),.1), log=TRUE),
                         dnorm(tmp2,mean(tmp2), max(sd(tmp2),.1), log=TRUE))))


    tmp <- npi2[1:3]
    tmp2 <- npi2[4:6]
    expect_that(calc_hierarchical_likadj("npi 2",infer_frame,geodata,"USPS")$likadj,
                equals(c(dnorm(tmp,mean(tmp), max(sd(tmp),.1), log=TRUE),
                         dnorm(tmp2,mean(tmp2), max(sd(tmp2),.1), log=TRUE))))

})



test_that("NPIs with equal values have highe LL than npis with different values", {

    npi1 <- runif(6,-1,1)
    npi2 <- rep(runif (1,-1,1),6)

    ##makes data frame with stats
    infer_frame <- data.frame(geoid=rep(c("01001","01002","01003",
                                          "06001", "06002", "06003"),2),
                              npi_name=rep(c("npi 1", "npi 2"), each=6),
                              reduction=c(npi1,npi2))




    ##make geodata dataframe
    geodata <- data.frame(geoid=c("01001","01002","01003",
                                  "06001", "06002","06003"),
                          USPS=rep(c("HI","CA"), each=3))




    val1 <- sum(calc_hierarchical_likadj("npi 1",infer_frame,
                                         geodata,"USPS")$likadj)

    val2 <- sum(calc_hierarchical_likadj("npi 2",infer_frame,
                                         geodata,"USPS")$likadj)

    expect_that(val1,
                is_less_than(val2))

})



test_that("Groups with equal values have highe LL than npis with different values", {

    npi1 <- c(runif(3,-1,1), rep(runif(1,-1,1),3))
    npi2 <- c(rep(runif(1,-1,1),3),runif(3,-1,1))

    ##makes data frame with stats
    infer_frame <- data.frame(geoid=rep(c("01001","01002","01003",
                                          "06001", "06002", "06003"),2),
                              npi_name=rep(c("npi 1", "npi 2"), each=6),
                              reduction=c(npi1,npi2))




    ##make geodata dataframe
    geodata <- data.frame(geoid=c("01001","01002","01003",
                                  "06001", "06002","06003"),
                          USPS=rep(c("HI","CA"), each=3))




    res1 <- calc_hierarchical_likadj("npi 1",infer_frame,
                                     geodata,"USPS")%>%
        inner_join(geodata)

    valHI <- sum(res1$likadj[res1$USPS=="HI"])
    valCA <- sum(res1$likadj[res1$USPS=="CA"])


    expect_that(valHI,
                is_less_than(valCA))

    res2 <- calc_hierarchical_likadj("npi 2",infer_frame,
                                     geodata,"USPS")%>%
        inner_join(geodata)

    valHI <- sum(res2$likadj[res1$USPS=="HI"])
    valCA <- sum(res2$likadj[res1$USPS=="CA"])


    expect_that(valHI,
                is_more_than(valCA))

})



test_that("equal values use minimum variance", {
    npi1 <- rep(1,3)

    ##makes data frame with stats
    infer_frame <- dplyr::tibble(geoid=c("01001","01002","01003"),
                              npi_name=rep("npi 1", 3),
                              reduction=npi1)




    ##make geodata dataframe
    geodata <- dplyr::tibble(geoid=c("01001","01002","01003",
                                  "06001", "06002","06003"),
                          USPS=rep(c("HI","CA"), each=3))


    expect_that(calc_hierarchical_likadj("npi 1",infer_frame,
                                         geodata,"USPS",
                                         min_sd=.2)$likadj,
                equals(dnorm(npi1, mean(npi1), .2, log=TRUE)))
})



test_that("transforms give the appropriate likelihoods", {
    

    val<- runif(3,0,1)

    # val <-  c(0.25698943, 0.23411552, 0.09412548)
    ##makes data frame with stats
    infer_frame <- dplyr::tibble(geoid=c("01001","01002","01003"),
                              npi_name=rep("val1", each=3),
                              value=val)


    ##make geodata dataframe
    geodata <- dplyr::tibble(geoid=c("01001","01002","01003",
                                  "06001", "06002","06003"),
                          USPS=rep(c("HI","CA"), each=3))


    ##no trandform
    untrans <- calc_hierarchical_likadj("val1", infer_frame, geodata, "USPS",
                                        stat_col="value",
                                        transform="none")

    
    expect_equal(untrans$likadj,
                  dnorm(val, mean(val), max(sd(val), .1), log=TRUE))
    

    ##logit transmform
    logit <-  calc_hierarchical_likadj("val1", infer_frame, geodata, "USPS",
                                        stat_col="value",
                                        transform="logit")

    
    tmp <- log(val/(1-val))


    expect_equal(logit$likadj,
                 dnorm(tmp, mean(tmp), max(sd(tmp, .1)), log=TRUE))
    

    ##nonsense transform
    expect_error(  calc_hierarchical_likadj("val1", infer_frame, geodata, "USPS",
                                        stat_col="value",
                                        transform="nonesense"))
    
})


test_that("sensible things are returned whern there is only 1 geoid in a location", {
    
    val<- runif(4,0,1)

    ##makes data frame with stats
    infer_frame <- dplyr::tibble(geoid=c("01001", "06001", "06002","06003"),
                              npi_name=rep("val1", 4),
                              value=val)


    ##make geodata dataframe
    geodata <- dplyr::tibble(geoid=c("01001","01002","01003",
                                  "06001", "06002","06003"),
                          USPS=rep(c("HI","CA"), each=3))


    ##no trandform
    adj <- calc_hierarchical_likadj("val1", infer_frame, geodata, "USPS",
                                        stat_col="value",
                                        transform="none")

    
    
    ##print(adj)
    
    ##make sure that the one geoid thing is zero
    expect_true(!is.na(adj$likadj[adj$geoid=="01001"]))
    
})


test_that("logit transform does not blow up on 0 or 1", {
    val<- runif(3,0,1)
    val[1] <- 0
    val[2] <- 1

    ##makes data frame with stats
    infer_frame <- dplyr::tibble(geoid=c("01001","01002","01003"),
                              npi_name=rep("val1", each=3),
                              value=val)


    ##make geodata dataframe
    geodata <- dplyr::tibble(geoid=c("01001","01002","01003",
                                  "06001", "06002","06003"),
                          USPS=rep(c("HI","CA"), each=3))


    logit <-  calc_hierarchical_likadj("val1", infer_frame, geodata, "USPS",
                                       stat_col="value",
                                       transform="logit")
    
 
    expect_false(is.nan(max(logit$likadj)))
    expect_false(is.nan(min(logit$likadj)))
       

})
