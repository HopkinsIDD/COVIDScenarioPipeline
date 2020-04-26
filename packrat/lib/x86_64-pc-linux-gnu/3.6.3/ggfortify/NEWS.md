## ggfortify 0.4.10

* This is a minor release of the package with fixes in tests to be compatible with `survival` (>=3.1-12).

## ggfortify 0.4.9

* This is a minor release of the package with fixes for CRAN check results.

## ggfortify 0.4.8

* This is a minor release of the package with fixes to support new major release of `survival` (>=3.1-7).

## ggfortify 0.4.7

* This is a minor release of the package with a couple of fixes for CRAN check results.

## ggfortify 0.4.6

* Fixed CRAN check results for `R-devel` regarding changes in the default method for generating from a discrete uniform distribution used in `sample()`.

## ggfortify 0.4.5

* Updated tests to work with ggplot2 v2.3.0.

## ggfortify 0.4.4

* Added `label.show.legend` argument to hide/show the legend of text labels.
* `ggmultiplot` arranges a single `ggplot` instance with layout that can now be specified by the user.
* Enhanced unit tests: 1) CRAN MKL build can pass; tests are skipped if the required packages are not installed; several test checks are more robust to upstream package changes.

## ggfortify 0.4.3

* Fixed incorrect y-axis label for plotting `cv.glmnet` objects.
* Removed incorrect y-axis label with `scales::percent` for `ggdistribution`.
* Added support of `autoplot.lm()` for character variables, thanks to @lselzer.
* Fixed tests for `forecast::ets()` to be compatible with newer version of `forecast` package, thanks to @robjhyndman.
* Added support for arbitrary functions in `autoplot.survfit()`, thanks to @jonathon-love.

## ggfortify 0.4.2

* Removed `gglagplot` in favor of `forecast::gglagplot`.
* Added support of `survival::survfitms`, thanks to @yoursdearboy.
* Temporarily dropped support for `xts` and `timeSeries` objects. We will try fix them in the next release.

## ggfortify 0.4.1

* Survival plot now displays legend as it is (without prefix / reordering), thanks to @DarioS.

## ggfortify 0.4.0

* Support `ROCR::performance`, thanks to @austin3dickey
* PCA allows to choose the components to plot, thanks to @lselzer.
* LM autoplot fails when a formula contains polynomial regression.
* GLM autoplot fails when a family function is binomial.

## ggfortify 0.3.0

* Our white paper is on R Journal. Type `citation("ggfortify")` for more info.
* Support `splines::basis`.
* Support several `raster` ojects, such as `RasterBrick`, `RasterCommon`, `RasterLayer`,
  and `RasterStack`.
* `ggmultiplot` objects can now be saved correctly via `ggplot2::ggsave()`.

## ggfortify 0.2.0

* Components of `prcomp` and `princomp` are now scaled. Scaling can be disabled by
  specifying `scale = 0`.
* `autoplot.t` now supports stacked option.
* Support `maps` and `sp` packages.
* Support `ggrepel`.
* Support `ggbiplot`.
* More compatible with recent version of `forecast`, `dplyr`, `tidyr`, etc.

## ggfortify 0.1.0

* Compatible with ggplot2 v2.0.0.
* Support `glmnet`.
* Support `multiplots` from a list of classes supported by autoplot.
* Support `ggmultiplot` extraction getter / setter.
* Support `ggmultiplot` arithmetics.

## ggfortify 0.0.4

* First release on CRAN.
