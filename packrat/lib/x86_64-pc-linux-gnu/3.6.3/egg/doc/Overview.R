## ----setup, echo=FALSE, results='hide', message=FALSE--------------------
library(egg)
library(grid)
library(gridExtra)
library(gtable)
library(knitr)
opts_chunk$set(message = FALSE,
fig.width = 7,
fig.height = 3)

## ----layout--------------------------------------------------------------
p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
p2 <- qplot(mpg, data = mtcars) + ggtitle("title")
p3 <- qplot(mpg, data = mtcars, geom = "dotplot")
p4 <-
p1 + facet_wrap( ~ carb, nrow = 1) + theme(legend.position = "none") +
ggtitle("facetted plot")
pl <- lapply(list(p1, p2, p3, p4), expose_layout, FALSE, FALSE)
grid.arrange(
grobs = pl,
widths = c(1.2, 1, 1),
layout_matrix = rbind(c(1, 2, 3),
c(4, 4, 4))
)

## ----panel, fig.height=3.5-----------------------------------------------
  p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
p2 <- p1 + facet_wrap( ~ carb, nrow = 1)
grid.arrange(grobs = lapply(
list(p1, p2),
set_panel_size,
width = unit(2, "cm"),
height = unit(1, "in")
))

## ----frame---------------------------------------------------------------
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()

p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap(~ cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme()

p3 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_grid(. ~ cyl, scales = "free")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

fg1 <- gtable_frame(g1, debug = TRUE)
fg2 <- gtable_frame(g2, debug = TRUE)
fg12 <-
  gtable_frame(gtable_rbind(fg1, fg2),
               width = unit(2, "null"),
               height = unit(1, "null"))
fg3 <-
  gtable_frame(
    g3,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = TRUE
  )
grid.newpage()
combined <- gtable_cbind(fg12, fg3)
grid.draw(combined)

## ----ggarrange-----------------------------------------------------------
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()+ theme_article() + theme(legend.position = 'top') 
p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap(~ cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme_article()
  
ggarrange(p1, p2, widths = c(1.5,2))

## ----ggarrangelayout-----------------------------------------------------
p <- ggplot()
ggarrange(p, p, p, widths = c(3, 1), heights = c(5, 1))

## ----ggarrangelabels-----------------------------------------------------
ggarrange(p1, p2, p3, ncol=2,
          labels = c("A", "b)", "iii."), 
          label.args = list(gp=gpar(font=4), x=unit(1,"line"), hjust=0))

## ----tagfacet------------------------------------------------------------
d = data.frame(
  x = 1:90,
  y = rnorm(90),
  red = rep(letters[1:3], 30),
  blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))

p <- ggplot(d) +
  geom_point(aes(x = x, y = y)) +
  facet_grid(red ~ blue)
  
tag_facet(p)
tag_facet_outside(p)

## ----themes--------------------------------------------------------------
d = data.frame(
  x = 1:90,
  y = rnorm(90),
  red = rep(letters[1:3], 30),
  blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))

p <- ggplot(d) +
  geom_point(aes(x = x, y = y)) +
  facet_grid(red ~ blue)

p + theme_article()

## ----symmetrise----------------------------------------------------------
df = data.frame(x = c(1, 2),
                y = c(5, 0.2),
                group = c(1, 2))
p <- ggplot(df, aes(x = x, y = y)) + 
  geom_point() + 
  facet_wrap( ~ group, scale =
                "free")
p + scale_y_continuous(limits = symmetric_range)

## ----custompics----------------------------------------------------------
codes <- data.frame(country = c("nz","ca","ar","fr","gb","es"))
codes$y <- runif(nrow(codes))

gl <- lapply(codes$country, 
                    function(.x) png::readPNG(system.file("flags", 
                                             paste0(.x,".png"),
                                             package="egg")))

codes$raster <- I(gl)


ggplot(codes, aes(x = country, y = y)) + 
  geom_point() + 
  geom_custom(data = codes, aes(data=raster), 
              grob_fun = rasterGrob, 
              fun_params = list(height=unit(1,"cm"))) +
  scale_y_continuous(breaks=NULL, "") +
  theme(panel.grid = element_blank())

## ----customgrobs---------------------------------------------------------
codes$raster <- I(lapply(codes$raster, function(x) rasterGrob(x, height=unit(1,"cm"))))

ggplot(codes, aes(x = country, y = y)) + 
  geom_point() +
  geom_custom(data = codes, aes(data=raster), 
              grob_fun = identity)

## ----customgrobcoord-----------------------------------------------------
custom_grob <- function(data, x=0.5,y=0.5){
  grob(data=data,x=x,y=y, cl="custom")
}
preDrawDetails.custom <- function(x){
  pushViewport(viewport(x=x$x,y=x$y))
}
postDrawDetails.custom <- function(x){
  upViewport()
}
drawDetails.custom <- function(x, recording=FALSE, ...){
  grid.rect(mean(x$data$x), mean(x$data$y), 
            width=diff(range(x$data$x)), 
            height=diff(range(x$data$y)))
  grid.lines(x$data$x, x$data$y, gp=gpar(col=x$data$col,lwd=2), default.units = "native")
}

d <- data.frame(x=rep(1:3, 4), f=rep(letters[1:4], each=3))
gl <- lapply(1:4, function(ii){
  data.frame(x=seq(0.4,0.6,length=10),
             y = runif(10,0.45,0.55),
             col = hcl(h = seq(0,300,length=nrow(d)))[ii],
             stringsAsFactors = FALSE)
})
subplots <- data.frame(f=letters[1:4], data = I(gl))
str(subplots)

ggplot(d, aes(f,x)) +
  facet_wrap(~f, nrow=1)+
  coord_polar() +
  geom_point()+
  geom_custom(data = subplots, aes(data = data, x = f, y = 2), 
              grob_fun = custom_grob) 

