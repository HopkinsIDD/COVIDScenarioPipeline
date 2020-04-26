## ---- include=FALSE-----------------------------------------------------------
library(flextable)
library(magrittr)
library(htmltools)
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>", 
  eval = !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))
)

## -----------------------------------------------------------------------------
qflextable(head(iris)) %>% 
  color(~ Sepal.Length < 5, color = "orange", ~ Sepal.Width + Petal.Length ) %>% 
  color(~ Sepal.Length > 4.99, ~ Sepal.Length, color = "red")

## -----------------------------------------------------------------------------
ft <- flextable(head(iris, n = 10))
ft <- color(ft, 
            i = ~ Sepal.Length < 5, 
            j = ~ Sepal.Length + Sepal.Width, 
            color = "orange")
ft

## -----------------------------------------------------------------------------
dat <- head(iris, n = 10)
ft <- flextable(dat)
ft <- color(ft, j = "Sepal.Length", color = "orange", part = "all")
ft <- bold(ft, j = c("Sepal.Width", "Species"), bold = TRUE)
ft

## -----------------------------------------------------------------------------
ft <- flextable(head(iris, n = 10))
ft <- color(ft, i = 1:3, j = 1:3, color = "orange")
ft

## -----------------------------------------------------------------------------
dat <- head(iris, n = 10)
ft <- flextable(dat)

ft <- color(ft, i = rep(c(TRUE, FALSE), 5), color = "orange")
ft

## -----------------------------------------------------------------------------
border <- officer::fp_border()
ft <- flextable(head(iris, n = 10))
ft <- vline(ft, j = c('Sepal.Length', 'Sepal.Width'), border = border, part = "all")
ft

## -----------------------------------------------------------------------------
ft <- color(ft, i = 1, color = "red", part = "header")
ft

## -----------------------------------------------------------------------------
ft <- color(ft, i = ~ Sepal.Length < 5, 
               j = c('Petal.Length', 'Petal.Width'), 
               color = "red", part = "body")
ft

