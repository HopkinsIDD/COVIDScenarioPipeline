.safe_env <- new.env(parent = emptyenv())

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
##'Safer version of eval that only allows arthimetic operations
##'
safe_eval <- function(call) {
  if (is.null(.safe_env$min)) init_safe_env()
  eval(call, env=.safe_env)
}
