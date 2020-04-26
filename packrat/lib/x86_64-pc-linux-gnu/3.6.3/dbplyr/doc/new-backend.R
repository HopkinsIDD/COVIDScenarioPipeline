## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup, message = FALSE---------------------------------------------------
library(dplyr)
library(DBI)

## -----------------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
DBI::dbWriteTable(con, "mtcars", mtcars)

tbl(con, "mtcars")

## -----------------------------------------------------------------------------
#' @export
db_desc.PostgreSQLConnection <- function(x) {
  info <- dbGetInfo(x)
  host <- if (info$host == "") "localhost" else info$host

  paste0("postgres ", info$serverVersion, " [", info$user, "@",
    host, ":", info$port, "/", info$dbname, "]")
}

