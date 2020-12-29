context("accept_reject_new_seeding_npis")


test_that("all blocks are accpeted when all proposals are better",{
    seed_orig <- data.frame(place=c(rep("A",5),rep("B",5),rep("C",5)),
                            date=16:30,
                            value=1:15)

    seed_prop <- data.frame(place=c(rep("A",5),rep("B",5),rep("C",5)),
                            date=16:30,
                            value=(1:15)*10)


    npis_orig <- data.frame(geoid=c(rep("A",3),rep("B",3),rep("C",3)),
                            name=rep(c("X","Y","Z"),3),
                            value=1:9)

    npis_prop <- data.frame(geoid=c(rep("A",3),rep("B",3),rep("C",3)),
                            name=rep(c("X","Y","Z"),3),
                            value=(1:9)*10)


    hpar_orig <- npis_orig
    hpar_orig$value <- runif(nrow(hpar_orig))
    hpar_prop <- npis_prop
    hpar_prop$value <- runif(nrow(hpar_prop))


    orig_lls <- data.frame(geoid=c("A","B","C"),ll=rep(-10,3))
    prop_lls <-  data.frame(geoid=c("A","B","C"),ll=rep(-9,3))


    tmp <- accept_reject_new_seeding_npis(
      seeding_orig = seed_orig,
      seeding_prop = seed_prop,
      snpi_orig = npis_orig,
      snpi_prop = npis_prop,
      hnpi_orig = npis_orig,
      hnpi_prop = npis_prop,
      hpar_orig = hpar_orig,
      hpar_prop = hpar_prop,
      orig_lls = orig_lls,
      prop_lls = prop_lls
    )



    expect_that(tmp$seeding$value, equals(seed_prop$value))
    expect_that(tmp$snpi$value, equals(npis_prop$value))
    expect_that(tmp$hnpi$value, equals(npis_prop$value))
    expect_that(tmp$hpar$value, equals(hpar_prop$value))
    expect_that(tmp$lls$ll, equals(prop_lls$ll))



})


test_that("all blocks are rejected when all proposals are 1x10^12 times worse",{
    seed_orig <- data.frame(place=c(rep("A",5),rep("B",5),rep("C",5)),
                            date=16:30,
                            value=1:15)

    seed_prop <- data.frame(place=c(rep("A",5),rep("B",5),rep("C",5)),
                            date=16:30,
                            value=(1:15)*10)


    npis_orig <- data.frame(geoid=c(rep("A",3),rep("B",3),rep("C",3)),
                            name=rep(c("X","Y","Z"),3),
                            value=1:9)

    npis_prop <- data.frame(geoid=c(rep("A",3),rep("B",3),rep("C",3)),
                            name=rep(c("X","Y","Z"),3),
                            value=(1:9)*10)


    hpar_orig <- npis_orig
    hpar_orig$value <- runif(nrow(hpar_orig))
    hpar_prop <- npis_prop
    hpar_prop$value <- runif(nrow(hpar_prop))



    orig_lls <- data.frame(geoid=c("A","B","C"),ll=rep(-1,3))
    prop_lls <-  data.frame(geoid=c("A","B","C"),ll=rep(-13,3))


    tmp <- accept_reject_new_seeding_npis(
      seeding_orig = seed_orig,
      seeding_prop = seed_prop,
      snpi_orig = npis_orig,
      snpi_prop = npis_prop,
      hnpi_orig = npis_orig,
      hnpi_prop = npis_prop,
      hpar_orig = hpar_orig,
      hpar_prop = hpar_prop,
      orig_lls = orig_lls,
      prop_lls = prop_lls
    )


    expect_that(tmp$seeding$value, equals(seed_orig$value))
    expect_that(tmp$snpi$value, equals(npis_orig$value))
    expect_that(tmp$hpar$value, equals(hpar_orig$value))
    expect_that(tmp$lls$ll, equals(orig_lls$ll))



})



test_that("only middle block is accepted when appropriate",{
    seed_orig <- data.frame(place=c(rep("A",5),rep("B",5),rep("C",5)),
                            date=16:30,
                            value=1:15)

    seed_prop <- data.frame(place=c(rep("A",5),rep("B",5),rep("C",5)),
                            date=16:30,
                            value=(1:15)*10)


    npis_orig <- data.frame(geoid=c(rep("A",3),rep("B",3),rep("C",3)),
                            name=rep(c("X","Y","Z"),3),
                            value=1:9)

    npis_prop <- data.frame(geoid=c(rep("A",3),rep("B",3),rep("C",3)),
                            name=rep(c("X","Y","Z"),3),
                            value=(1:9)*10)



    hpar_orig <- npis_orig
    hpar_orig$value <- runif(nrow(hpar_orig))
    hpar_prop <- npis_prop
    hpar_prop$value <- runif(nrow(hpar_prop))


    orig_lls <- data.frame(geoid=c("A","B","C"),ll=rep(-2,3))
    prop_lls <-  data.frame(geoid=c("A","B","C"),ll=rep(-15,3))
    prop_lls$ll[prop_lls$geoid=="B"] <- -1


    tmp <- accept_reject_new_seeding_npis(
      seeding_orig = seed_orig,
      seeding_prop = seed_prop,
      snpi_orig = npis_orig,
      snpi_prop = npis_prop,
      hnpi_orig = npis_orig,
      hnpi_prop = npis_prop,
      hpar_orig = hpar_orig,
      hpar_prop = hpar_prop,
      orig_lls = orig_lls,
      prop_lls = prop_lls
    )

    sd_inds <- which(seed_orig$place!="B")
    npi_inds <- which(npis_orig$geoid!="B")
    ll_inds <- which(prop_lls$geoid!="B")

    expect_that(tmp$seeding$value[sd_inds], equals(seed_orig$value[sd_inds]))
    expect_that(tmp$snpi$value[npi_inds], equals(npis_orig$value[npi_inds]))
    expect_that(tmp$hpar$value[npi_inds], equals(hpar_orig$value[npi_inds]))
    expect_that(tmp$lls$ll[ll_inds], equals(orig_lls$ll[ll_inds]))


    sd_inds <- which(seed_orig$place=="B")
    npi_inds <- which(npis_orig$geoid=="B")
    ll_inds <- which(prop_lls$geoid=="B")

    expect_that(tmp$seeding$value[sd_inds], equals(seed_prop$value[sd_inds]))
    expect_that(tmp$snpi$value[npi_inds], equals(npis_prop$value[npi_inds]))
    expect_that(tmp$hpar$value[npi_inds], equals(hpar_prop$value[npi_inds]))
    expect_that(tmp$lls$ll[ll_inds], equals(prop_lls$ll[ll_inds]))



})
