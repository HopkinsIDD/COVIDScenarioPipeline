## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, echo=FALSE, message=FALSE---------------------------------
library(officer)
library(flextable)

## -----------------------------------------------------------------------------
dat <- head(mtcars, n = 10)
dat[3:7, 1] <- NA
dat[, 2] <- dat[, 6] * 1000000

ft <- flextable(dat)
ft
num_keys <- c("mpg", "disp", "drat", "wt", "qsec")
int_keys <- c("cyl", "hp", "vs", "am", "gear", "carb")

ft <- colformat_num(x = ft, col_keys = num_keys, big.mark = ",", digits = 2, na_str = "missing")
ft <- colformat_int(x = ft, col_keys = int_keys, big.mark = ",")
autofit(ft)

## -----------------------------------------------------------------------------
ft <- flextable(head(mtcars, n = 10 ), 
                   col_keys = c("gear", "mpg", "qsec"))
ft <- set_formatter(ft, 
    mpg = function(x) sprintf("%.04f", x),
    gear = function(x) sprintf("%.0f gears", x)
  )
ft <- theme_booktabs(ft)
ft <- autofit(ft)
ft

## -----------------------------------------------------------------------------
myft <- flextable( head(mtcars), 
  col_keys = c("am", "separator", "gear", "mpg", "drat", "qsec" ))
myft <- bold(myft, part = "header")
myft <- border(myft, border = fp_border( width = 0), 
  border.top = fp_border(), border.bottom = fp_border(), 
  part = "all")
myft <- align(myft, align = "right", part = "all" )
myft <- border(myft, j = ~ separator, border = fp_border(width=0), part = "all")
myft <- width(myft, j = ~ separator, width = .1)
myft

## -----------------------------------------------------------------------------
myft <- compose( 
  myft, j = "mpg", 
  value = as_paragraph(
    "mpg value is ", 
    as_chunk(sprintf("%.01f", mpg), props = fp_text(color = "red", bold = TRUE) ) )
  )
myft <- autofit(myft)
myft

## -----------------------------------------------------------------------------
myft <- compose( 
  myft, j = "mpg", 
  value = as_paragraph(
    "mpg value is ", 
    as_chunk(sprintf("%.01f", mpg), props = fp_text(color = "red", bold = TRUE) ), 
    " with ",
    as_chunk(sprintf("# %.0f", carb), props = fp_text(color = "gray", italic = TRUE) )
    )
  )

myft <- autofit(myft)
myft

## -----------------------------------------------------------------------------
myft <- compose( 
  myft, j = "mpg", part = "header",
  value = as_paragraph(
    "Miles/(US) gallon ", 
    as_chunk("* with num of carb.", props = fp_text(color = "gray", vertical.align = "superscript") )
    )
  )

myft <- autofit(myft)
myft

## -----------------------------------------------------------------------------
img.file <- file.path( R.home("doc"), "html", "logo.jpg" )

myft <- compose( myft, i = ~ qsec > 18, j = "qsec", 
  value = as_paragraph(as_image( src = img.file, width = .20, height = .15))
)
myft <- autofit(myft)
myft

## -----------------------------------------------------------------------------
myft <- flextable( head(iris, n = 10 ))

myft <- compose( myft, j = 1,
  value = as_paragraph(
    minibar(value = Sepal.Length, max = max(Sepal.Length))
  ),
  part = "body")

autofit(myft)

## -----------------------------------------------------------------------------
myft <- flextable( head(iris, n = 10 ))

myft <- compose( myft, j = 1,
  value = as_paragraph(
    linerange(value = Sepal.Length, max = max(Sepal.Length))
  ),
  part = "body")

autofit(myft)

## -----------------------------------------------------------------------------
data <- structure(list(Species = structure(1:3, .Label = c("setosa", 
"versicolor", "virginica"), class = "factor"), col1 = c(5.006, 
5.936, 6.588)), class = "data.frame", row.names = c(NA, -3L))

ft <- flextable(data)
ft
ft <- compose(ft, part = "header", j = "Species", 
    value = as_paragraph(as_i(as_b("Species"))))
ft <- compose(ft, part = "header", j = "col1", 
    value = as_paragraph(as_b("µ"), as_sup("blah")))
ft

## -----------------------------------------------------------------------------
ft <- flextable(head(iris))
ft <- footnote( ft, i = 1, j = 1:3,
            value = as_paragraph(
              c("This is footnote one",
                "This is footnote two",
                "This is footnote three")
            ),
            ref_symbols = c("a", "b", "c"),
            part = "header")
ft <- valign(ft, valign = "bottom", part = "header")
autofit(ft)

