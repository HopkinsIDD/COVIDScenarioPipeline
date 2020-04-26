## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=4, fig.path='figures/dist-', warning=FALSE)

## ---- message = FALSE---------------------------------------------------------
library(ggfortify)
ggdistribution(dnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)

## ---- message = FALSE---------------------------------------------------------
ggdistribution(pnorm, seq(-3, 3, 0.1), mean = 0, sd = 1, colour = 'red')
ggdistribution(dpois, seq(0, 20), lambda = 9, fill = 'blue')

## ---- message = FALSE---------------------------------------------------------
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 7, colour = 'blue')
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 9, colour = 'green', p = p)
ggdistribution(dchisq, seq(0, 20, 0.1), df = 11, colour = 'red', p = p)

## ---- message = FALSE---------------------------------------------------------
autoplot(density(rnorm(1:50)), fill = 'green')

