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
data <- iris[c(1:3, 51:53, 101:104),]
data

## ----warning=FALSE, echo=FALSE------------------------------------------------
library(flextable)
typology <- data.frame(
  col_keys = c( "Sepal.Length", "Sepal.Width", "Petal.Length",
                "Petal.Width", "Species" ),
  what = c("Sepal", "Sepal", "Petal", "Petal", "Species"),
  measure = c("Length", "Width", "Length", "Width", "Species"),
  stringsAsFactors = FALSE )

ft <- flextable(
  data, 
  col_keys = c("Species", "sep_1", "Sepal.Length", "Sepal.Width", 
               "sep_2",  "Petal.Length", "Petal.Width" ) )

ft <- set_header_df(ft, mapping = typology, key = "col_keys" )
ft <- merge_h(ft, part = "header")
ft <- merge_v(ft, j = "Species", part = "body")
ft <- merge_v(ft, j = "Species", part = "header")
ft <- theme_booktabs(ft)
ft <- empty_blanks(ft)
ft <- fix_border_issues(ft)
autofit(ft) 

## ----warning=FALSE------------------------------------------------------------
library(flextable)
library(officer)

myft <- flextable(
  head(mtcars), 
  col_keys = c("am", "carb", "gear", "mpg", "drat" ))
myft

## ----warning=FALSE------------------------------------------------------------
myft <- theme_vanilla(myft)
myft

## ----warning=FALSE------------------------------------------------------------
myft <- merge_v(myft, j = c("am", "carb") )
myft <- set_header_labels( myft, carb = "# carb." )
myft <- autofit(myft)
myft

## -----------------------------------------------------------------------------
myft <- italic(myft, j = 1)
myft <- bg(myft, bg = "#C90000", part = "header")
myft <- color(myft, color = "white", part = "header")
myft <- color(myft, ~ drat > 3.5, ~ drat, color = "red")
myft <- bold(myft, ~ drat > 3.5, ~ drat, bold = TRUE)
myft <- autofit(myft)
myft

## -----------------------------------------------------------------------------
ft <- flextable(head(mtcars))
ft <- autofit(ft)
ft

## ----echo=FALSE---------------------------------------------------------------
code <- pre(
  code(
    "---",
    "title: 'flextable formatting'", 
    "output: rmarkdown::html_document", 
    "---",
    "", 
    "", 
    "> this is how to print a flextable in a R Markdown document", 
    "", "", 
    "```{r}", 
    "library(flextable)", 
    "ft <- flextable(head(mtcars))", 
    "ft <- autofit(ft)", 
    "ft", 
    "```"
  )
)
knitr::knit_print(knitr::asis_output(as.character(code)))

## ----eval=FALSE---------------------------------------------------------------
#  docx_file <- tempfile(fileext = ".docx")
#  pptx_file <- tempfile(fileext = ".pptx")
#  # docx_file <- "example.docx"
#  # pptx_file <- "example.pptx"
#  save_as_docx("my table" = ft, path = docx_file)
#  save_as_docx("my table" = ft, path = pptx_file)

## ----eval=FALSE---------------------------------------------------------------
#  print(ft, preview = "docx")
#  print(ft, preview = "pptx")

## -----------------------------------------------------------------------------
library(officer)

## ----eval=FALSE---------------------------------------------------------------
#  ppt <- read_pptx()
#  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
#  ppt <- ph_with(ppt, value = ft, location = officer::ph_location_left())
#  
#  print(ppt, target = "example.pptx")

## ----eval=FALSE---------------------------------------------------------------
#  doc <- read_docx()
#  doc <- body_add_flextable(doc, value = ft)
#  print(doc, target = "example.docx")

## ----eval=FALSE---------------------------------------------------------------
#  img_file <- tempfile(fileext = ".png")
#  save_as_image(ft, path = img_file)

