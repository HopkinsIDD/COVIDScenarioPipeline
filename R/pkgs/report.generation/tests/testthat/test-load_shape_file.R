test_that("Shapefile is valid", {
  dirname <- tempdir()
  setwd(dirname)
  
  fname <- "djkewlkmdkf!!!."
  expect_error({
    load_shape_file(filename = fname)
  }, "does not exist")

  badfile <- "filename.csv"
  write.csv(data.frame(), file = badfile)

  expect_error({
    load_shape_file(
      filename = badfile
    )
  })

  badfile2 <- paste0(dirname, "filename1.shp")
  sf::st_write(sf::st_sf(sf::st_sfc(sf::st_point(matrix(0,ncol=2,nrow=1)))), badfile2) 

  expect_error({
    load_shape_file(
      filename = badfile2
    )
  }, "does not have a column named geoid")

  goodfile <- paste0(dirname, "filename2.shp")
  sf::st_write(sf::st_sf(sf::st_sfc(sf::st_point(matrix(0,ncol=2,nrow=1)))) %>% dplyr::mutate(geoid="0"), goodfile) 

  expect_silent({
    load_shape_file(
      filename = goodfile
    )
  })

})


test_that("Shapefile geoid padding works", {

  dirname <- tempdir()
  setwd(dirname)
  
  goodfile <- "filename3.shp"
  sf::st_write(sf::st_sf(sf::st_sfc(sf::st_point(matrix(0,ncol=2,nrow=1)))) %>% dplyr::mutate(geoid="b"), goodfile) 

  expect_equal({
    shp <- load_shape_file(filename = goodfile, geoid_len = 0)
    nchar(as.character(shp$geoid[1]))
  }, 1)

  expect_equal({
    shp <- load_shape_file(filename = goodfile, geoid_len = 5)
    nchar(shp$geoid[1])
  }, 5)

  expect_equal({
    shp <- load_shape_file(filename = goodfile, geoid_len = 5, geoid_pad = "a")
    shp$geoid[1]
  }, "aaaab")

  expect_error({
    shp <- load_shape_file(filename = goodfile, geoid_len = 2, geoid_pad = NA)
  }, "Invalid geoid_pad value")

  expect_error({
    shp <- load_shape_file(filename = goodfile, geoid_len = 5, geoid_pad = 12)
  }, "Invalid geoid_pad value")

})


test_that("Shapefile lowercasing works", {

  dirname <- tempdir()
  goodfile <- "filename4.shp"
  sf::st_write(sf::st_sf(sf::st_sfc(sf::st_point(matrix(0,ncol=2,nrow=1)))) %>% dplyr::mutate(GEOID="b", Altcol="00"), goodfile) 

  expect_output({
    shp <- load_shape_file(filename = goodfile, to_lower = TRUE)
    print(names(shp))
  }, "geoid")

})
