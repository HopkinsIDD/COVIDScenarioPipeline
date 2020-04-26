# runit.VirtualClass.R test suite
# Just one (little bit more complex) example taken from RUnit

# Test setup
if (FALSE) {
  # Not really needed, but can be handy when writing tests
  library("svUnit")
}

# Package 'methods' is usually loaded, but make sure it is
if (!require(methods))
  stop("Package 'methods' is required!")

# Define class (not exported yet by the program, and defined in .GlobalEnv!)
className <- "MyVirtualBaseClass"
setClass(className,
  representation("VIRTUAL",
    x = "numeric",
    y = "numeric",
    description = "character"),
  validity = NULL,
  sealed   = FALSE,
  where    = .GlobalEnv)

if (!isGeneric("getX")) {
  setGeneric("getX", function(object, ...) standardGeneric("getX"),
    useAsDefault = TRUE, where = .GlobalEnv, valueClass = "numeric")
}

setMethod("getX", signature = className, function(object) return(object@x),
  where = .GlobalEnv)

if (!isGeneric("setX<-")) {
  setGeneric("setX<-", function(object, value) standardGeneric("setX<-"),
    useAsDefault = TRUE, where = .GlobalEnv)
}

setMethod("setX<-",
  signature = signature(object = className, value = "numeric"),
  function(object, value) {
    if (length(value) < 1)
      stop("value has to contain at least one element.")
    if (any(is.na(value)))
      stop("value may not contain NA(s).")
    object@x <- value
    object
  }, where = .GlobalEnv)


# Test functions
.setUp <- function() {
  # Executed before each test function
  # ...
}

.tearDown <- function() {
  # Executed after each test function
  # ...
}

testCreateClass <- function() {
  setClass("A", contains = "numeric", where = .GlobalEnv)
  a <- new("A")
  checkTrue(validObject(a))
  removeClass("A", where = .GlobalEnv)	# Better to use on.exit() here!
  checkException(new("A"))
}

testMyVirtualBaseClass.getX <- function() {
  testClassName <- "MyDerivedTestClass"
  setClass(testClassName,
    representation("MyVirtualBaseClass"),
    validity = NULL,
    sealed   = FALSE,
    where    = .GlobalEnv)

  on.exit(removeClass(testClassName, where = .GlobalEnv))

  # system constructor
  this <- new(testClassName)

  # constructor call succeeded?
  checkTrue(is(this, testClassName))

  ret <- getX(this)
  checkTrue(is(ret, "numeric"))
  # class default
  checkEquals(numeric(0), ret)
}

testMyVirtualBaseClass.setX <- function() {
  testClassName <- "MyDerivedTestClass"
  setClass(testClassName,
    representation("MyVirtualBaseClass"),
    validity = NULL,
    sealed   = FALSE,
    where    = .GlobalEnv)

  on.exit(removeClass(testClassName, where = .GlobalEnv))

  # System constructor
  this <- new(testClassName)

  # Constructor call succeeded?
  checkTrue(is(this, testClassName))

  testSeq <- 1:23
  setX(this) <- testSeq
  ret <- getX(this)
  checkTrue(is(ret, "numeric"))
  checkEquals(testSeq, ret)

  # Error handling
  checkException(setX(this) <- numeric(0))
  checkException(setX(this) <- as.numeric(NA))
  checkException(setX(this) <- c(1:4, NA))
}
