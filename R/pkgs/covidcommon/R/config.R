##'
##'Map of configuration values, constructed via a call to `load_config`
##'
config <- NA

##'
##'Returns a map of configuration loaded from the config YAML
##'@param fname Load configuration from fname (optional, otherwise loads from CONFIG_PATH env var)
##'@example config$parameters_seir$gamma
##'
load_config <- function(fname) {
  require(yaml)

  if (missing(fname)) {
    fname <- Sys.getenv("CONFIG_PATH")
  }
  if (file.exists(fname)) {
    return(yaml.load_file(fname))
  } else {
    return(NA)
  }
}

##'
##'Evaluates an expression, returning a numeric value
##'@example as_evaled_expression(c("2+2", "9*9")) -> (4, 81)
##'
##'@param l the object (scalar or vector) to evaluate
##'@return a float evaluation of the expression
##'
as_evaled_expression <- function(l) {
  .as_evaled_expression <- function(obj) {
    if (is.numeric(obj)) {
      return(obj)
    } else if (is.character(obj)) {
      return(as.numeric(safe_eval(parse(text=obj))))
    } else {
      stop("expected numeric or string expression")
    }
  }

  return(unlist(lapply(l, .as_evaled_expression)))
}

##'
##'Evaluates an expression, returning a numeric value
##'@example as_evaled_expression("2+2") -> 4
##'
##'@param obj the string to evaluate
##'@return a float evaluation of the expression
##'
as_random_distribution <- function(obj) {
  require(purrr)

  if (obj$distribution == "uniform") {
    return(purr::partial(runif, min=as_evaled_expression(obj$low), max=as_evaled_expression(obj$high)))
  } else if (obj$distribution == "poisson") {
    return(purrr::partial(rpois, lambda=as_evaled_expression(obj$lam)))
  } else if (obj$distribution == "binomial") {
    return(purr::partial(rbinom, size=as_evaled_expression(obj$n), prob=as_evaled_expression(obj$p)))
  } else {
      stop("unknown distribution")
  }
}

.onLoad <- function(libname, pkgname) {
  config <<- load_config()
}
