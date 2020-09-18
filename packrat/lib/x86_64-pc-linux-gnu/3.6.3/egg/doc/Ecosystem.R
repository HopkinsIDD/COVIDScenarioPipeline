## ----setup, echo=FALSE, results='hide', message=FALSE--------------------
library(knitr)
library(gridExtra)
library(egg)
library(gtable)
library(grid)
opts_chunk$set(
  message = FALSE,
  fig.width = 6,
  fig.height = 3,
  cache = FALSE
)

## ----basic, echo=2:6, fig.height=5, fig.cap="A few plots that we want to organise on a page."----
set.seed(123)
library(ggplot2)
p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
p2 <- qplot(mpg, data = mtcars) + ggtitle("title")
p3 <- qplot(mpg, data = mtcars, geom = "dotplot")
p4 <-
  p1 + facet_wrap( ~ carb, nrow = 1) + theme(legend.position = "none") +
  ggtitle("facetted plot")
library(gridExtra)
grid.arrange(p1,
             p2,
             p3,
             p4,
             layout_matrix = rbind(c(1, 2, 3),
                                   c(4, 4, 4)),
             widths = c(1.2, 1, 1))

## ----arrange1, echo=TRUE, fig.height=3, fig.cap="Basic usage of `grid.arrange()`"----
grid.arrange(p1, p2, nrow = 1)

## ----arrange2, echo=-c(1:2), fig.width=4, fig.height=3, fig.cap="Illustrating further arguments of `grid.arrange()`, namely  `layout_matrix` and relative widths."----
cols <- c(
  "#FBB4AE",
  "#B3CDE3",
  "#CCEBC5",
  "#DECBE4",
  "#FED9A6",
  "#FFFFCC",
  "#E5D8BD",
  "#FDDAEC"
)
gl <- lapply(1:4, function(x)
  grobTree(rectGrob(gp = gpar(fill = cols[x])), textGrob(paste("plot", x))))
grid.arrange(
  grobs = gl,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 2, NA),
                        c(3, 3, 4))
)

## ----inset, fig.width=4, fig.height=3, fig.cap="Plot inset."-------------
g <- ggplotGrob(qplot(1, 1) +
                  theme(plot.background = element_rect(colour = "black")))
qplot(1:10, 1:10) +
  annotation_custom(
    grob = g,
    xmin = 1,
    xmax = 5,
    ymin = 5,
    ymax = 10
  ) +
  annotation_custom(
    grob = rectGrob(gp = gpar(fill = "white")),
    xmin = 7.5,
    xmax = Inf,
    ymin = -Inf,
    ymax = 5
  )

## ----plotstructure, echo=FALSE, fig.width=6, fig.height=3, fig.cap="Colour-coded structure of examplar ggplot layouts. Note how the panels (red) vary in size from plot to plot, as they accommodate the other graphical components."----

pl <- lapply(list(p1, p2, p3, p4), expose_layout, FALSE, FALSE)
layouts <- arrangeGrob(
  grobs = pl,
  widths = c(1.2, 1, 1),
  layout_matrix = rbind(c(1, 2, 3),
                        c(4, 4, 4))
)

ids <-
  c("background", "panel", "axis", "lab", "guide", "strip", "title")
cols <-
  c(
    "grey95",
    "#FBB4AE",
    "#B3CDE3",
    "#CCEBC5",
    "#DECBE4",
    "#FED9A6",
    "#FFFFCC",
    "#E5D8BD",
    "#FDDAEC"
  )

leg <- lapply(ids, textGrob, hjust = 0, x = 0.1)
legend <- gtable_matrix(
  "legend",
  matrix(leg, ncol = 1),
  widths = 1.2 * grobWidth(leg[[1]]),
  heights = unit(rep(1, length(leg)), "line")
)
legend <- gtable_add_cols(legend, unit(1, "line"), 0)

legend <- gtable_add_grob(legend,
                          t = seq_along(leg),
                          l = 1,
                          lapply(cols[seq_along(ids)], function(x) rectGrob(gp = gpar(fill = x, col = NA))))

grid.arrange(layouts, legend, 
             widths = unit.c(unit(1, "null"),
                             1.2 * sum(legend$widths)))


## ----rbind, fig.height=4, fig.width=4, fig.cap="Aligning plot panels. Note that the two y axes have different widths."----
library(gtable)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)

## ----egg, fig.height=3, fig.width=6--------------------------------------
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()+ theme_article() + theme(legend.position = 'top') 
p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap(~ cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme_article()
  
ggarrange(p1, p2, widths = c(1.5,2))

## ----titles--------------------------------------------------------------
grid.arrange(
  p3,
  p3,
  p3,
  nrow = 1,
  top = "Title of the page",
  bottom = textGrob(
    "this footnote is right-justified",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## ----legend, echo=TRUE, fig.height=4-------------------------------------
grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }

grid_arrange_shared_legend(p1, p2)


## ----tables--------------------------------------------------------------
grid.arrange(
  tableGrob(mtcars[1:4, 1:4]),
  p2,
  ncol = 2,
  widths = c(1.5, 1),
  clip = FALSE
)

## ----comparison, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'----
tabl <- "
| Package           | Function(s)   | ggsave compat. | alignment  |
|-------------------|:-------------:|:------:|:----------:|
| grid              | `viewport`, `grid.layout`      |   no   | no         |
| [gridExtra][1]    | `grid.arrange`  |   yes  | no         |
| [(r cookbook)][2] | `multiplot`     |   no   | no         |
| [gtable][3]       | `rbind`, `cbind`|   yes  | yes        |
| [cowplot][4]      | `plot_grid`     |   yes* | yes*       |
| [multipanelfigure][5]    | `multi_panel_figure`     |   yes  | yes        |
| [egg][6]          | `ggarrange`     |   yes  | yes        |
| [patchwork][7]    | `plot_layout`     |   yes  | yes        |
"
cat(tabl) 

## ----comp----------------------------------------------------------------
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1:2))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(2, 2))

