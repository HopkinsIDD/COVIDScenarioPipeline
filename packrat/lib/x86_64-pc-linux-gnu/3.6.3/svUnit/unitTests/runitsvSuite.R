# runitsvSuite.R test suite
# by Ph. Grosjean <phgrosjean@sciviews.org>
# Run it simply by example(unitTests.svUnit)

# Create a few objects we need for our tests

# Save current exclusion list and clear it
oex <- getOption("svUnit.excludeList")

# Create a very simple 'svTest' object
test_R <- svTest(function() {
  checkTrue(1 < 2)
})

# The test cases
.setUp <- function() {
  # Executed before each test function
  # Remove temporarily the exclusion list for our tests
  options(svUnit.excludeList = NULL)

  # Create an object with associated tests in .GlobalEnv
  foo <- function(x)
    return(x)
  test(foo) <- function() {
    checkEquals(2, foo(2),            "foo(2) returns 2")
    checkException(foo("x"),          "foo(\"x\") raises an exception")
  }
  svSuite.foo <<- foo   # Place a copy of 'foo' in .GlobalEnv

  # Create an object without associated tests in .GlobalEnv
  svSuite.bar <<- function(x)
    return(x^2)

  # Create an integration test in .globalEnv
  test_svSuite <<- svTest(function() {
    checkTrue(1 == 1,                  "example test: 1 == 1")
    checkException(nonexisting + 1,    "exception when using non existing var")
  })
}

.tearDown <- function() {
  # Executed after each test function
  # Restore previous exclusion list
  options(svUnit.excludeList = oex)
  # Remove our object with tests in .GlobalEnv
  rm(svSuite.foo, svSuite.bar, test_svSuite, envir = .GlobalEnv)
}

testsvSuite <- function() {
  checkTrue(is.svSuite(svSuite("svSuite.foo")),
    "svSuite(\"svSuite.foo\") returns a 'svSuite' object")
  checkTrue(is.svSuite(as.svSuite("svSuite.foo")),
    "as.svSuite(\"svSuite.foo\") returns a 'svSuite' object")
  checkTrue(is.svSuite(svSuite("svSuite.bar")),
    "svSuite(\"svSuite.bar\") returns a 'svSuite' object")
  checkTrue(is.svSuite(svSuite("test_svSuite")),
    "svSuite(\"test_svSuite\") returns a 'svSuite' object")
  checkTrue(is.svSuite(print(svSuite("test_svSuite"))),
    "print(svSuite) returns a 'svSuite' object invisibly")
  checkTrue(is.svSuite(svSuite("nonexisting")),
    "svSuite(\"nonexisting\") returns a 'svSuite' object")
  checkException(svSuite(nonexisting),
    "svSuite(nonexisting) raises an exception")
}

testsvSuiteList <- function() {
  checkTrue(is.svSuite(svSuiteList()),
    "svSuiteList() returns a 'svSuite' object")
  checkTrue("package:svUnit" %in% svSuiteList(),
    "svSuiteList() lists 'svSuite' package")
  checkTrue("package:svUnit (VirtualClass)" %in% svSuiteList(),
    "svSuiteList() lists 'VirtualClass' suite")
  checkTrue("test(svSuite.foo)" %in% svSuiteList(),
    "svSuiteList() lists objects with tests")
  checkTrue("test_svSuite" %in% svSuiteList(),
    "svSuiteList() lists 'svTest' objects")
  checkTrue("test_R" %in% svSuiteList(pos = parent.frame()),
    "svSuiteList() uses 'pos' correctly")
}
