context("iterateAccept")

test_that("iterateAccept accepts better likelihoods",{
  ll_ref <- data.frame(
    ll = -1
  )
  ll_new <- data.frame(
    ll = -.99
  )
  ll_col <- 'll'
  N <- 10000
  expect_equal({
    all(sapply(seq_len(N),function(x){iterateAccept(ll_ref,ll_new,ll_col)}))
  },TRUE)
})

test_that("iterateAccept sometimes accepts worse likelihoods",{
  ll_ref <- data.frame(
    ll = -1
  )
  ll_new <- data.frame(
    ll = -.99
  )
  ll_col <- 'll'
  N <- 10000
  expect_lt(
    abs(mean(sapply(seq_len(N),function(x){iterateAccept(ll_new,ll_ref,ll_col)})) - exp(ll_ref$ll - ll_new$ll)),
    4.5e-3
  )

  ll_ref <- data.frame(
    ll = -1
  )
  ll_new <- data.frame(
    ll = -.9
  )
  ll_col <- 'll'
  N <- 10000
  expect_lt(
    abs(mean(sapply(seq_len(N),function(x){iterateAccept(ll_new,ll_ref,ll_col)})) - exp(ll_ref$ll - ll_new$ll)),
    1.15e-2
  )
})
