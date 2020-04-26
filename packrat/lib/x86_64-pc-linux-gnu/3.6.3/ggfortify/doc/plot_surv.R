## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=3, fig.path='figures/surv-', warning=FALSE)

## ---- message = FALSE---------------------------------------------------------
library(ggfortify)
library(survival)
fit <- survfit(Surv(time, status) ~ sex, data = lung)
autoplot(fit)

## ---- message = FALSE---------------------------------------------------------
autoplot(fit, surv.linetype = 'dashed', conf.int = FALSE,
         censor.shape = '*', censor.size = 5, facets = TRUE, ncol = 2)

autoplot(survfit(Surv(time, status) ~ 1, data = lung), surv.colour = 'orange', censor.colour = 'red')

autoplot(survfit(Surv(time, status) ~ sex, data = lung), fun = 'event')

d.coxph <- survfit(coxph(Surv(time, status) ~ sex, data = lung))
autoplot(d.coxph, surv.linetype = 'dashed', surv.colour = 'blue',
         conf.int.fill = 'dodgerblue3', conf.int.alpha = 0.5, censor = FALSE)

## ---- fig.width = 8, fig.height = 4-------------------------------------------
autoplot(aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung))

