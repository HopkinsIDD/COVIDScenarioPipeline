# To be promoted as a demo?
library(svUnit)

options(svUnit.excludeList = NULL)

foo <- function(x, y = 2)
  x * y

#is.test(foo)    # No
# Create test cases for this function
test(foo) <- function() {
  checkEqualsNumeric(4, foo(2))
  checkEqualsNumeric(5, foo(2, 3)) # Should fail
  checkTrue(is.test(foo))
  checkTrue(is.test(test(foo)))
  checkIdentical(attr(foo, "test"), test(foo))
  checkException(foo(2, dfgfg))
  #DEACTIVATED("My deactivation message")
  checkException(foo("bb"))
}

test_simple <- svTest(function() {
  checkTrue(1 == 1, "test1")
  checkTrue(1 == 2, "test2")
  checkTrue(1 == var, "test3")
})

# This is for an example!
clearLog()
imax <- 3
jmax <- 100
l <- 50
Rprof()
for (i in 1:imax) {
  .LogTest <- paste("Test", i, sep = "")
  .LogTag <- paste("#", i, sep = "")
  res <- system.time({
    for (j in 1:jmax)
      checkTrue(i <= j, "My test")
  }, gcFirst = TRUE)[3]
  print(res)
  flush.console()
}
Rprof(NULL)
summaryRprof()$by.self
summary(.Log[[.LogTest]])
rm(.LogTest, .LogTag)
unlink("Rprof.out")

Rprof()
system.time(
  for (i in 1:100)
    runTest(test.bar))[3]
Rprof(NULL)
summaryRprof()
unlink("Rprof.out")
