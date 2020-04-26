## ----setup, echo=FALSE, results='hide', warning=FALSE, error=FALSE, message=FALSE, cache=FALSE----
library(knitr)
opts_chunk$set(
  cache       = TRUE,
  autodep     = TRUE,
  echo        = FALSE,
  warning     = FALSE,
  error       = FALSE,
  message     = FALSE,
  out.width   = 700,
  fig.width   = 12,
  fig.height  = 8,
  dpi         = 300,
  cache.path  = "cache/ggrepel/",
  fig.path    = "figures/ggrepel/",
  pngquant    = "--speed=1 --quality=0-10",
  concordance = TRUE
)
knit_hooks$set(
  pngquant = hook_pngquant
)
library(gridExtra)
library(ggplot2)
theme_set(theme_classic(base_size = 18) %+replace% theme(
  axis.line.y = element_line(colour = "black", size = 0.2),
  axis.line.x = element_line(colour = "black", size = 0.2),
  axis.ticks = element_line(colour = "black", size = 0.2)
))

## ----comparison, echo=TRUE, fig.width=9, fig.height=4-------------------------
library(ggrepel)
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

p <- ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red")

p1 <- p + geom_text() + labs(title = "geom_text()")

p2 <- p + geom_text_repel() + labs(title = "geom_text_repel()")

gridExtra::grid.arrange(p1, p2, ncol = 2)

## ----install-cran, echo=TRUE, eval=FALSE--------------------------------------
#  install.packages("ggrepel")

## ----install-github, echo=TRUE, eval=FALSE------------------------------------
#  # Use the devtools package
#  # install.packages("devtools")
#  devtools::install_github("slowkow/ggrepel")
#  
#  # Or use the install-github.me service
#  source("https://install-github.me/slowkow/ggrepel")

## ----empty_string, echo=TRUE, fig.width=5, fig.height=4-----------------------
set.seed(42)

dat2 <- subset(mtcars, wt > 3 & wt < 4)
# Hide all of the text labels.
dat2$car <- ""
# Let's just label these items.
ix_label <- c(2,3,16)
dat2$car[ix_label] <- rownames(dat2)[ix_label]

ggplot(dat2, aes(wt, mpg, label = car)) +
  geom_point(color = ifelse(dat2$car == "", "grey50", "red")) +
  geom_text_repel()

## ----point_padding_na, echo=TRUE, fig.width=5, fig.height=4-------------------
set.seed(42)
ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red") +
  geom_text_repel(point.padding = NA)

## ----xlim, echo=TRUE, fig.width=5, fig.height=4-------------------------------
set.seed(42)

# All labels should be to the right of 3.
x_limits <- c(3, NA)

ggplot(dat, aes(wt, mpg, label = car, color = factor(cyl))) +
  geom_vline(xintercept = x_limits, linetype = 3) +
  geom_point() +
  geom_label_repel(
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
    force = 10,
    xlim  = x_limits
  ) +
  scale_color_discrete(name = "cyl")

## ----direction_x, echo=TRUE, fig.width=9, fig.height=3------------------------
set.seed(42)

ggplot(mtcars, aes(x = wt, y = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  geom_text_repel(
    nudge_y      = 0.05,
    direction    = "x",
    angle        = 90,
    vjust        = 0,
    segment.size = 0.2
  ) +
  xlim(1, 6) +
  ylim(1, 0.8) +
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )

## ----neat-offset-x, echo=TRUE, fig.width=7, fig.height=4----------------------
set.seed(42)

dat <- mtcars
dat$car <- rownames(dat)

ggplot(dat, aes(qsec, mpg, label = car)) +
  geom_text_repel(
    data          = subset(dat, mpg > 30),
    nudge_y       = 36 - subset(dat, mpg > 30)$mpg,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "x"
  ) +
  geom_point(color = ifelse(dat$mpg > 30, "red", "black")) +
  scale_x_continuous(expand = c(0.05, 0.05)) +
  scale_y_continuous(limits = c(NA, 36))

## ----direction_y, echo=TRUE, fig.width=10, fig.height=8-----------------------
set.seed(42)

p <- ggplot(mtcars, aes(y = wt, x = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  ylim(1, 5.5) +
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    plot.title   = element_text(hjust = 0.5)
  )

p1 <- p +
  xlim(1, 1.375) +
  geom_text_repel(
    nudge_x      = 0.15,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0")

p2 <- p + 
  xlim(1, 1.375) +
  geom_text_repel(
    nudge_x      = 0.2,
    direction    = "y",
    hjust        = 0.5,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0.5 (default)")

p3 <- p +
  xlim(0.25, 1) +
  scale_y_continuous(position = "right") +
  geom_text_repel(
    nudge_x      = -0.35,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 1")

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

## ----neat-offset-y, echo=TRUE, fig.width=7, fig.height=4----------------------
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_text_repel(
    data          = subset(dat, wt > 3),
    nudge_x       = 3.5 - subset(dat, wt > 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0
  ) +
  geom_text_repel(
    data          = subset(dat, wt < 3),
    nudge_x       = 2.7 - subset(dat, wt < 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 1
  ) +
  scale_x_continuous(
    breaks = c(2.5, 2.75, 3, 3.25, 3.5),
    limits = c(2.4, 3.8)
  ) +
  geom_point(color = "red")

## ----polar, echo=TRUE, fig.width=5, fig.height=4------------------------------
set.seed(42)

mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$mpg < 25] <- ""

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), label = label)) +
  coord_polar(theta = "x") +
  geom_point(size = 2) +
  scale_color_discrete(name = "cyl") +
  geom_text_repel(show.legend = FALSE) + # Don't display "a" in the legend.
  theme_bw(base_size = 18)

## ----math, echo=TRUE, fig.width=5, fig.height=4-------------------------------
d <- data.frame(
  x    = c(1, 2, 2, 1.75, 1.25),
  y    = c(1, 3, 1, 2.65, 1.25),
  math = c(
    NA,
    "integral(f(x) * dx, a, b)",
    NA,
    "lim(f(x), x %->% 0)",
    NA
  )
)

ggplot(d, aes(x, y, label = math)) +
  geom_point() +
  geom_label_repel(
    parse       = TRUE, # Parse mathematical expressions.
    size        = 8,
    box.padding = 2
  )

## ----animated, echo=TRUE, eval=FALSE------------------------------------------
#  # This chunk of code will take a minute or two to run.
#  library(ggrepel)
#  library(animation)
#  
#  plot_frame <- function(n) {
#    set.seed(42)
#    p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
#      geom_point(color = "red") +
#      geom_text_repel(
#        size = 5, force = 3, max.iter = n
#      ) +
#      theme_minimal(base_size = 16)
#    print(p)
#  }
#  
#  saveGIF(
#    lapply(ceiling(1.75^(1:12)), function(i) {
#      plot_frame(i)
#    }),
#    interval   = 0.20,
#    ani.width  = 800,
#    ani.heigth = 600,
#    movie.name = "animated.gif"
#  )

## ----session_info, echo=TRUE--------------------------------------------------
sessionInfo()

