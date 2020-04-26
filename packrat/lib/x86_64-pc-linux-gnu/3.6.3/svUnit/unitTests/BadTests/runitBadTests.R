# runitBadTests.R test suite
# by Ph. Grosjean <phgrosjean@sciviews.org>

# Create a few objects we need for tests

# An R object (matrix)
mat <- matrix(rnorm(4), ncol = 2)

# Create very simple test cases for matrix 'mat'
testmat <- svTest(function() {
  checkEqualsNumeric(2, nrow(mat))
  checkTrue(is.numeric(mat))
})

# An example function without test case
foo <- function(x)
  x

# Another function with a test associated
bar <- function(x)
  x^2

testbar <- svTest(function() {
  checkEqualsNumeric(4, bar(2))
  checkException(bar("xx"))
})
test(bar) <- testbar

# The test cases
.setUp <- function() {
  # Executed before each test function
  # ...
}

.tearDown <- function() {
  # Executed after each test function
  # ...
}

testBadTests <- function() {
  # These tests should fail
  checkEquals(c(x = 2), 2, "Check a double is equal to a named double")
  checkEqualsNumeric(2, 3, "Check if two different numbers are equal")
  checkIdentical(2, sqrt(2)^2, "Check a double is exactly the expected value")
  checkTrue(FALSE, "Check if FALSE is TRUE")
  checkException(log(10), "log(10) produces and exception")

  # These tests should generate en error
  checkEquals(fff(2), 2, "Wrong expression in checkEquals()")
  checkEqualsNumeric(fff(2), 2, "Wrong expression in checkEqualsNumeric()")
  checkIdentical(2, fff(2), "Wrong expression in checkIdentical()")
  checkTrue(fff(), "Wrong expression in checkTrue()")
}
