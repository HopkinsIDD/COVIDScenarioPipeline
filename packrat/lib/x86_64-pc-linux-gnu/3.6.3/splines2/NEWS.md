# splines2 0.2.8

## Bug fixes

* Fixed inconsistency of argument `df` for piece-wise constant bases when
  `knots = NULL`.


# splines2 0.2.7

## Minor changes

* Updated tests for R development version.


# splines2 0.2.6

## Minor changes

* Added checks for any internal knot incorrectly placed outside of the boundary
  knots and added warnings for users' reference.


# splines2 0.2.5

## Minor changes

* Added more tests and increased code coverage.

## Bug fixes

* Fixed evaluation of derivatives of M-splines for a single value. Thanks Ina
  Jazic for reporting the bug and providing possible fix.

* Fixed `deriv.cSpline` method for derivatives of order greater than two when
  `scale = TRUE`.


# splines2 0.2.4

## New features

* Added function `dbs` generating derivative of given order of B-splines. It is
  a similar function with `splines::splineDesign`. However, it provides a more
  user-friendly interface and more consistent handling on `NA`'s.

* Added `deriv` methods for derivatives of given order of any existing
  **splines2** object that can be generated currently.

## Major changes

* Added argument `derivs` to function `mSpline` and `iSpline` for derivatives.

* Changed all the classes of object generated for a better dispatching on
  methods.

## Minor changes

* Added tests for all major functions with the help of package **testthat**.

## Bug fixes

* Fixed the generation of splines without any internal knot.


# splines2 0.2.3

## Bug fixes

* Fixed one-piece constant basis for M-splines.


# splines2 0.2.2

## Bug fixes

* Fixed the NA's handling in all the functions constructing spline bases.


# splines2 0.2.1

## New features

* Added function `bSpline` generating B-spline basis allowing zero degree or
  piecewise constant basis based on function `bs` in package **splines**.

* Introduced function `bSpline` to allow M-splines of degree zero.

* Added function `cSpline` constructing convex spline (C-spline) basis.

* Added `predict` methods for `bSpline2` object and `cSpline` object generated
  by `bSpline` and `cSpline`, respectively.

* Added `print` methods for all **splines2** objects developed so far.

## Major changes

* Improved the function `iSpline` to construct I-spline basis directly from
  B-spline basis instead of M-spline basis.

## Minor changes

* Updated all CRAN URL to a canonical form suggested.


# splines2 0.1.0

## New features

* The first version of **splines2** providing functions constructing M-spline,
  I-spline, and integral of B-spline basis.
