pkgname <- "constrainedKriging"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('constrainedKriging')
assign(".oldSearch", search(), pos = 'CheckExEnv')

cleanEx()
nameEx("CKrige")
flush(stderr()); flush(stdout())

data(meuse,meuse.blocks)

plot(meuse.blocks)
suppressWarnings(preCK_1  <- preCKrige(newdata = meuse.blocks, model = covmodel("exponential", 0.05, 0.15, scale = 192.5), pwidth = 75, pheight = 75))
plot(preCK_1, 59)

### define neighbours
library(spdep)
neighbours <- poly2nb(meuse.blocks)
class(neighbours)
class(neighbours) <- "list"
preCK_2 <- preCKrige(newdata = meuse.blocks, neighbours = neighbours,
    model = covmodel("exponential", 0.05, 0.15, scale = 192.5),
    pwidth = 75, pheight = 75)

plot(preCK_1, 59)
