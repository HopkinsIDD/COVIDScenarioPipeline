## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>")

## ----warning=FALSE, echo=FALSE, message=FALSE---------------------------------
library(officer)
library(flextable)

## -----------------------------------------------------------------------------
myft <- flextable(head(iris))
myft

## -----------------------------------------------------------------------------
myft <- flextable(head(iris)) 
myft <- bold(myft, part = "header") # bold header
myft

## -----------------------------------------------------------------------------
myft <- fontsize(myft, part = "header", size = 12) 
myft

## -----------------------------------------------------------------------------
myft <- color(myft, color = "#E4C994")
myft

## -----------------------------------------------------------------------------
myft <- italic(myft, i = ~ Sepal.Length > 5, 
         j = ~ Sepal.Length + Sepal.Width, italic = TRUE)
myft

## -----------------------------------------------------------------------------
# light gray as background color for header
myft <-  bg(myft, bg = "#E4C994", part = "header")
# dark gray as background color for body
myft <-  bg(myft, bg = "#333333", part = "body")
myft

## -----------------------------------------------------------------------------
myft <- align( myft, align = "center", part = "all" )
myft

## -----------------------------------------------------------------------------
myft <- padding( myft, padding = 3, part = "all" )
myft

## -----------------------------------------------------------------------------
myft <- font(myft, j = "Species", fontname = "Times")
myft <- fontsize(myft, j = "Species", size = 14)
myft

## -----------------------------------------------------------------------------
ft <- flextable(head(iris))
ft <- rotate(ft, rotation = "tbrl", align = "center", part = "header")
ft <- align(ft, align = "right", part = "header")
ft <- valign(ft, valign = "center", part = "header")
ft <- align(ft, align = "center", part = "body")

## ----echo=FALSE---------------------------------------------------------------
ft

## -----------------------------------------------------------------------------
library(officer)
big_border = fp_border(color="orange", width = 2)
border_v = fp_border(color="gray")
border_h = fp_border(color="gray")

dat <- iris[c(1:2, 51:52, 101:102),]
ft <- flextable(dat)
ft <- border_remove(x = ft)
ft <- border_outer(ft, part="all", border = big_border )
ft <- border_inner_h(ft, part="all", border = border_h )
ft <- border_inner_v(ft, part="all", border = border_v )
ft

## -----------------------------------------------------------------------------
dat <- iris[c(1:2, 51:52, 101:102),]
ft <- flextable(dat)
ft <- border_remove( ft )

big_b <- fp_border(color="gray70", width = 3)
std_b <- fp_border(color="orange", style = "dashed")

ft <- vline( ft, border = std_b, part = "all" )
ft <- vline_left( ft, border = big_b, part = "all" )
ft <- vline_right( ft, border = big_b, part = "all" )
ft <- hline( ft, border = std_b )
ft <- hline_bottom( ft, border = big_b )
ft <- hline_top( ft, border = big_b, part = "all" )
ft

## ----warning=FALSE, message=FALSE---------------------------------------------
dat <- iris[c(1:2, 51:52, 101:102),]
ft <- flextable(dat)
ft <- fontsize(ft, size = 14, part = "all")
ft <- color(ft, i = ~ Sepal.Length < 5 & Petal.Length > 1.3, 
        j = ~ Petal.Width + Species, 
        color="red")
ft <- italic(ft, i = ~ Sepal.Length > 5)
ft <- bold(ft, i = 4, j = "Sepal.Length")
ft

## ----warning=FALSE, message=FALSE---------------------------------------------
row_id <- with(dat, Sepal.Length < 5 & Petal.Length > 1.3 )
col_id <- c("Petal.Width", "Species")

ft <- color(ft, i = row_id, j = col_id, color="red") 

ft

## -----------------------------------------------------------------------------
library(officer)
def_cell <- fp_cell(border = fp_border(color="#00C9C9"))
def_par <- fp_par(text.align = "center")
def_text <- fp_text(color="#999999", italic = TRUE)
def_text_header <- update(color="black", def_text, bold = TRUE)

ft <- flextable(head(mtcars, n = 10 ))
ft <- style( ft, pr_c = def_cell, pr_p = def_par, pr_t = def_text, part = "all")  
ft

ft <- style( ft, pr_t = def_text_header, part = "header")  
ft

