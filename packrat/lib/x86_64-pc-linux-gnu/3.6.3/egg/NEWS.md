# egg 0.5 (2019-07-13)
 
## NEW FEATURES

* added `theme_presentation`
* improved the settings in `theme_article`
* added `tag_facet_outside` to tag facet rows and columns

## BUG FIX

* corrected logic in `ggarrange` which was failing for non-trivial layouts with more than 4 plots
* News file was referring to gridExtra
* `symmetrise_scale` no longer worked (again) due to internal changes in ggplot2; this functionality is not achieved through `symmetric_range` passed as limits in the scale

# egg 0.4.2 (2018-11-02)
 
## NEW FEATURES

* added theme_article

# egg 0.4.1 (2018-07-23)
 
## NEW FEATURES

* added tag_facet

## BUG FIX

* fixed aspect ratio for facetted plots was causing a problem in ggarrange; it now works again but the aspect ratio is not fully respected, as space between panels is ignored in the calculation

# egg 0.4.0 (2018-06-18)
 
## NEW FEATURES

* added geom_custom
* ggarrange gains labels

## CLEANUP

* improved vignettes

## BUG FIX

* fixed aspect ratio somewhat better respected for facetted plots in ggarrange (not perfect, as space between panels is ignored in the calculation)

# egg 0.2.0 (2017-09-12) 

* initial CRAN release

