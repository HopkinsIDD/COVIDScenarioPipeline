##' An environment with only arithmetic operations
.safe_env <- new.env(parent = emptyenv())

##' Initializes .safe_env to include only arithmetic operations
##'
##' @return None
##'
##' @examples
##' if (is.null(.safe_env$min)) init_safe_env()
##'
init_safe_env = function() {
  safe_f <- c(
    methods::getGroupMembers("Math"),
    methods::getGroupMembers("Arith"),
    methods::getGroupMembers("Compare"),
    "<-", "{", "(", "min", "max", "pmin", "pmax",
    "seq", ":", "seq.default", "seq.int"
  )

  for (f in safe_f) {
    .safe_env[[f]] <- get(f, "package:base")
  }
}

##'
##' Safer version of eval that only allows arthimetic operations
##'
##' @param call An unevaluated expression
##'
##' @examples
##' safe_eval(parse(text="1+1"))
##'
##' @export
safe_eval <- function(call) {
  if (is.null(.safe_env$min)) init_safe_env()
  eval(call, env=.safe_env)
}
