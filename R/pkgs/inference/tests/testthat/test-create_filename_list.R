context("initial MCMC setup")

test_that("create_filename_list produces a file of each type",{
  expect_error({
    create_filename_list("run_id","prefix",1,"type","extension")
  },NA)
  expect_equal({
    gsub(".*[.]","",create_filename_list("run_id","prefix",1,"type","extension")[['type_filename']])
  },"extension")
  expect_true({
    all(sapply(c("run_id","prefix","type","extension"),function(x){grepl(x,create_filename_list("run_id","prefix",1,"type","extension")[['type_filename']])}))
  },"extension")

  expect_error({
    create_filename_list("run_id","prefix",1)
  },NA)

  expect_error({
    create_filename_list("run_id","prefix",1,c("a","b","c"),c("csv","parquet","fake"))
  },NA)
  expect_equal({
    names(create_filename_list("run_id","prefix",1,c("a","b","c"),c("csv","parquet","fake")))
  },c("a_filename","b_filename","c_filename"))

  expect_equal({
    rc <- create_filename_list("run_id","prefix",1,c("a","b","c"),c("csv","parquet","fake"))
    rc <- gsub(".*[.]","",rc)
    rc <- unname(rc)
    rc
  },c("csv","parquet","fake"))

  expect_equal({
    rc <- create_filename_list("run_id","prefix",1,c("a","b","c"),c("csv","parquet","fake"))
    rc <- gsub("[.][^.]*$","",rc)
    rc <- gsub(".*[.]","",rc)
    rc <- unname(rc)
    rc
  },c("a","b","c"))
})
