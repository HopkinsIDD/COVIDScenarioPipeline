## ----global_options, include=FALSE, eval=FALSE--------------------------------
#  library(knitr)
#  opts_chunk$set(fig.width=8, fig.height=6, fig.path='figures/lm-', warning=FALSE)

## ---- message = FALSE, fig.width = 5, fig.height = 5, eval=FALSE--------------
#  library(ggfortify)
#  autoplot(lm(Petal.Width ~ Petal.Length, data = iris), label.size = 3)

## ---- message = FALSE, fig.width = 7, fig.height = 5, eval=FALSE--------------
#  par(mfrow = c(1, 2))
#  m <- lm(Petal.Width ~ Petal.Length, data = iris)
#  
#  autoplot(m, which = 1:6, ncol = 3, label.size = 3)

## ---- message = FALSE, fig.width = 5, fig.height = 6, eval=FALSE--------------
#  m <- glm(Murder ~ Assault + UrbanPop + Rape,
#           family = gaussian, data = USArrests)
#  
#  autoplot(m, which = 1:6, label.size = 3)

## ---- message = FALSE, fig.width = 5, fig.height = 5, eval=FALSE--------------
#  class(autoplot(m))
#  autoplot(m, label.size = 3) + theme_bw()

## ---- message = FALSE, fig.width = 8, fig.height = 5, eval=FALSE--------------
#  autoplot(m, which = 1:6, colour = 'dodgerblue3',
#           smooth.colour = 'black', smooth.linetype = 'dashed',
#           ad.colour = 'blue',
#           label.size = 3, label.n = 5, label.colour = 'blue',
#           ncol = 3)

## ---- message = FALSE, fig.width = 8, fig.height = 5, eval=FALSE--------------
#  autoplot(lm(Petal.Width ~ Petal.Length, data = iris), data = iris,
#           colour = 'Species', label.size = 3)

## ---- message = FALSE, fig.width = 5, fig.height = 3, eval=FALSE--------------
#  library(glmnet)
#  data(QuickStartExample)
#  fit <- glmnet::glmnet(x, y)
#  autoplot(fit)
#  
#  fit <- glmnet::cv.glmnet(x, y)
#  autoplot(fit, colour = 'blue')

