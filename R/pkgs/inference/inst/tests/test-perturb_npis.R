context("perturb_npis")

 CONFIG_STR <-
"interventions:
  scenarios:
    - local_variance_LockdownWideEffect_post50_firstcase_10x
  settings:
    local_variance:
      template: ReduceR0
      value:
        distribution: truncnorm
        mean: 0
        sd: 0.15
        a: -.5
        b: .5
      perturbation:
        distribution: truncnorm
        mean: 0
        sd: .1
        a: -1
        b: 1
    local_variance2:
      template: ReduceR0
      value:
        distribution: truncnorm
        mean: 0
        sd: 0.05
        a: -.2
        b: .2
      perturbation:
        distribution: truncnorm
        mean: 0
        sd: .05
        a: -1
        b: 1"


test_that("npi_petubations actaully perturbs", {
    library(truncnorm)

    config <- yaml::read_yaml(text=CONFIG_STR)

    N <- 10000
    npis <- data.frame(npi_name=rep(c("local_variance","local_variance2"),each=N),
                       geoid=rep(c("A","B"),N),
                       reduction=rep(0,2*N))

    tmp <- perturb_npis(npis, config$interventions$settings)

    expect_that(prod(tmp$reduction==npis$reduction),
                equals(0))
})

test_that("npi pertubations always stay within the bounds specified by the distirbution", {
    library(truncnorm)

    config <- yaml::read_yaml(text=CONFIG_STR)

    N <- 10000
    npis <- data.frame(npi_name=rep(c("local_variance","local_variance2"),each=N),
                       geoid=rep(c("A","B"),N),
                       reduction=rep(0,2*N))

    print(covidcommon::as_random_distribution(config$intervention$settings$local_variance$perturbation))
    tmp <- perturb_npis(npis, config$intervention$settings)

    expect_that(prod(tmp$reduction[1:N]>=config$interventions$settings$local_variance$a),
                equals(1))

     expect_that(prod(tmp$reduction[1:N]<=config$interventions$settings$local_variance$b),
                equals(1))

    expect_that(prod(tmp$reduction[(N+1):(2*N)]>=config$interventions$settings$local_variance2$a),
                equals(1))

    expect_that(prod(tmp$reduction[(N+1):(2*N)]<=config$interventions$settings$local_variance2$b),
                equals(1))

})
