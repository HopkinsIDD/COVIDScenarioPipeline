# svUnit 1.0.3

- During the conversion to Roxygen2, the topic 'svUnit' was lost . Corrected.

- A new 'pkgdown' web site is added at https://www.sciviews.org/svUnit.

# svUnit 1.0.2

- http://www.r-project.org changed into https://www.r-project.org in svUnit vignette (required by CRAN). 

# svUnit 1.0.1

- Corrections in URLs in the svUnit vignettes (canonical forms for CRAN packages like https://CRAN.R-project.org/package=<pkg_name> + another buggy URL).

# svUnit 1.0.0

- No further problems found in two years => version bumped to 1.0.0.

- `NEWS` file reworked and renamed `NEWS.md`.

- The vignette is reworked into an R Markdown format.

- The functions documentation is reworked in roxygen2 format (v7).

- A pkgdown site is created to better document the svUnit package.

# svUnit 0.7-13

- Development moved to Github; Travis CI and appveyor added.

- Dependency to optional XML package is now managed correctly through `requireNamespace()` and `XML::xxx()` calls.

# svUnit 0.7-12

- Rework of Author field in `DESCRIPTION` file.

# svUnit 0.7-11

- Temporary objects are now saved in `SciViews:TempEnv` instead of `TempEnv`.

- `runTest.svSuite()` does not create `.TestSuiteEnv` into `.GlobalEnv` any more. This object is now stored in `SciViews:TempEnv` (CRAN does not allow to assign in `.GlobalEnv`).

# svUnit 0.7-10

- In `runTest.svSuite()`, a `<<-` assignation is replaced by `assign(..., envir = .GlobalEnv)`.

- Vignette svUnit is now moved to `\vignettes` subdirectory. `DESCRIPTION` and `NEWS` files are reworked.

# svUnit 0.7-9

- Vignette svUnit translated into LyX 2.0-0.

# svUnit 0.7-8

- Option added to skip instead of fail missing documentation examples.

# svUnit 0.7-7

- The test list was only repeatedly running the last test defined. Fixed.

# svUnit 0.7-6

- Refer to last test environment through a local identifier. Closes #1327.

- Strip attributes from context fields when saving them temporarily.

# svUnit 0.7-5

- XML-encoding entities in `protocol_junit.svTestData()`. Closes #1147.

# svUnit 0.7-4

- Backquotes set to test name in evaluation instruction, allowing to use syntactically incorrect names for tests.

# svUnit 0.7-3

- Loading of svUnit sometimes failed during checking of the SciViews-K Unit plugin installation in Komodo Edit/IDE. Corrected. Thanks Claudia Beleites.

- The `unitTest.svUnit` man page now uses `require(svUnit)` in a mechanism that ignores the tests in case svUnit is not installed in a machine where R CMD check is run.

- `errorLog()` now briefly reports statistics on all test run when used in `interactive()` mode. This is more appropriate when the tests are run by using `example(unitTests.<mypackage>)`, while it does not change original behavior (silent execution of the tests, except in case of failure or error) during the R CMD check process.

- The package vignette is updated to reflect these changes.

# svUnit 0.7-2

- Added a `unitname=` argument in `runTest.svSuite()` to select one test unit to run in the test suite. Thanks to Thomas Wutzler for submitting this patch.

# svUnit 0.7-1

- Upgrade to the Komodo SciViews-K Unit plugin version 0.7-1.

# svUnit 0.7-0

- Typo correction in `guiTestReport.Rd`.

# svUnit 0.6-3

- The task callback mechanism introduced in svSocket 0.9-48 is now used to all running the task after R code send by socket clients is executed.

# svUnit 0.6-2

- There is now a vignette for the svUnit package.

# svUnit 0.6-1

- Correction of a bug in Windows relative to path separators `\\` versus `/`, especially using `tempdir()`.

- Correction of a bug that prevented to list`runit*.R` files in the unitTests subdirectory of a package under Windows.

# svUnit 0.6-0

- The package does not depends any more on RUnit. It has his own `checkXXX()` function (they are compatible with those in RUnit 0.4.17, except that here the `checkTrue()` function is vectorized, but they operate very differently),

- svUnit functions and objects are renamed `svSuite`, and there is a reworking of objects to end with `svTest`, `svSuite`, `svTestData` and `svSuiteData`, and many new methods for those objects (`print()`, `summary()`, `stats()`, `metadata()`, `makeUnit()`, `runTest()`).

- `print.svSuiteData()` and `summary.svSuiteData()` are completely reworked.

- `svSuiteList()` now accepts additional, dirs, manages an exclusion list and has an argument `loadPackages =` to force loading packages provided in the list.

- Names for test units is now `runit<name>.R`, like in the RUnit package. Same for the test functions which are `test<name>.R` (used to be `runit.<name>.R` and `test.<name>.R`).

- The temporary directory is now emptied from old `runit*.R` files before making new ones, but not any more after running tests. That way, we avoid running old test definitions, while keeping the latest one available for inspection.

- `koUnit_xxx()` functions have been added to manipulate the R Unit GUI in Komodo from within R.

# svUnit 0.5-0

- Added `guiTestFeedback()` and `guiTestReport()` functions.

- A couple of other little changes.

# svUnit 0.4-0

- First version compiled as a package and distributed on R-Forge.
