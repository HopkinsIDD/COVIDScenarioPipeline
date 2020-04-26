## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("svUnit")

## -----------------------------------------------------------------------------
library(svUnit)
Square <- function(x) return(x^2)
test(Square) <- function() {
  checkEqualsNumeric(9, Square(3))
  checkEqualsNumeric(10, Square(3))   # This intentionally fails
  checkEqualsNumeric(9, SSSquare(3))  # This raises error
  checkEqualsNumeric(c(1, 4, 9), Square(1:3))
  checkException(Square("xx"))
}
clearLog()
(runTest(Square))

## -----------------------------------------------------------------------------
library(svUnit)
# Create two R functions that include their own test cases
Square <- function(x) return(x^2)
test(Square) <- function() {
  checkEqualsNumeric(9, Square(3))
  checkEqualsNumeric(c(4, 9), Square(2:3))
  checkException(Square("xx"))
}

Cube <- function(x) return(x^3)
test(Cube) <- function() {
  checkEqualsNumeric(27, Cube(3))
  checkEqualsNumeric(c(8, 28), Cube(2:3))
  checkException(Cube("xx"))
}

# Add a separate test case
test_Integrate <- svTest(function() {
  checkTrue(1 < 2, "check1")
  v <- c(1, 2, 3)  # The reference
  w <- 1:3         # The value to compare to the reference
  checkEquals(v, w)
})

## -----------------------------------------------------------------------------
clearLog()
runTest(Square)
runTest(test_Integrate)
Log()

## -----------------------------------------------------------------------------
runTest(Cube)
Log()

## -----------------------------------------------------------------------------
clearLog()
checkEqualsNumeric(1, log(exp(1)))
checkException(log("a"))
checkTrue(1 == 2)
Log()

## -----------------------------------------------------------------------------
# Clear test exclusion list for running all test suites
options(svUnit.excludeList = NULL)
# Clear the logger
clearLog()
# Run all currently defined tests
runTest(svSuiteList(), name = "AllTests")
# Get some statistics
stats(Log())[, 1:3]
# A slightly different presentation than with print
summary(Log())
# Metadata collected on the machine where tests are run
metadata(Log())
# List content of the log
ls(Log())

## -----------------------------------------------------------------------------
myTest <- Log()$testCube
class(myTest)
myTest
summary(myTest)
stats(myTest)

## -----------------------------------------------------------------------------
ls(Log())
rm(test_R, envir = Log())
ls(Log())

## -----------------------------------------------------------------------------
test_function <- function() {
  checkTrue(1 < 2, "check1")
  v <- c(1, 2, 3)  # The reference
  w <- 1:3         # The object to compare to the reference
  checkEqualsNumeric(v, w)
}
# Turn this function into a test
test_function <- as.svTest(test_function)
is.svTest(test_function)

## -----------------------------------------------------------------------------
clearLog()
runTest(test_function)
Log()

## -----------------------------------------------------------------------------
# A very simple function
Square <- function(x) return(x^2)

# A test case to associate with the Square() function
test(Square) <- function() {
  checkEqualsNumeric(9, Square(3))
  checkEqualsNumeric(c(1, 4, 9), Square(1:3))
  checkException(Square("xx"))
}
is.test(Square)  # Does this object contain tests?

## -----------------------------------------------------------------------------
test(Square)

## -----------------------------------------------------------------------------
runTest(Square)
Log()  # Remember we didn't clear the log!

## ---- eval=FALSE--------------------------------------------------------------
#  # Create a test unit on disk and view its content
#  unit <- makeUnit(Square)
#  file.show(unit, delete.file = TRUE)

## -----------------------------------------------------------------------------
example(unitTests.svUnit)

## -----------------------------------------------------------------------------
# Reset default exclusion list
options(svUnit.excludeList = c("package:sv", "package:RUnit"))
# List all currently available tests
svSuiteList()

## -----------------------------------------------------------------------------
# Clear exclusion list
options(svUnit.excludeList = NULL)
svSuiteList()

## -----------------------------------------------------------------------------
(mySuite <- svSuiteList())

## ---- eval=FALSE--------------------------------------------------------------
#  myUnit <- makeUnit(mySuite, name = "ExampleTests")
#  file.show(myUnit, delete.file = TRUE)

## -----------------------------------------------------------------------------
clearLog()
runTest(mySuite)
summary(Log())

