### R code from vignette source 'paper2JSS.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: par4vignette
###################################################
options(digits = 4, prompt="R> ", 
        SweaveHooks=list(fig=function() par(mar=c(5.1, 4.1, 1.1, 2.1))))
set.seed(1234)


###################################################
### code chunk number 2: datgroundbeef
###################################################
library("fitdistrplus")
data("groundbeef")
str(groundbeef)


###################################################
### code chunk number 3: figgroundbeef.echo (eval = FALSE)
###################################################
## plotdist(groundbeef$serving, histo = TRUE, demp = TRUE)


###################################################
### code chunk number 4: figgroundbeefplot
###################################################
getOption("SweaveHooks")[["fig"]]()
plotdist(groundbeef$serving, histo = TRUE, demp = TRUE)


###################################################
### code chunk number 5: descgroundbeef.echo (eval = FALSE)
###################################################
## descdist(groundbeef$serving, boot = 1000)


###################################################
### code chunk number 6: descgroundbeefplot
###################################################
getOption("SweaveHooks")[["fig"]]()
descdist(groundbeef$serving, boot = 1000)


###################################################
### code chunk number 7: fitgroundbeef.weibull
###################################################
fw <- fitdist(groundbeef$serving, "weibull")
summary(fw)


###################################################
### code chunk number 8: fitgroundbeef.echo
###################################################
fg <- fitdist(groundbeef$serving, "gamma")
fln <- fitdist(groundbeef$serving, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)


###################################################
### code chunk number 9: fitgroundbeef
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mfrow=c(2, 2))
denscomp(list(fw, fln, fg), legendtext=c("Weibull", "lognormal", "gamma"))
qqcomp(list(fw, fln, fg), legendtext=c("Weibull", "lognormal", "gamma"))
cdfcomp(list(fw, fln, fg), legendtext=c("Weibull", "lognormal", "gamma"))
ppcomp(list(fw, fln, fg), legendtext=c("Weibull", "lognormal", "gamma"))


###################################################
### code chunk number 10: fitendo.echo
###################################################
data("endosulfan")
ATV <-endosulfan$ATV
fendo.ln <- fitdist(ATV, "lnorm")
library("actuar")
fendo.ll <- fitdist(ATV, "llogis", start = list(shape = 1, scale = 500))
fendo.P <- fitdist(ATV, "pareto", start = list(shape = 1, scale = 500))
fendo.B <- fitdist(ATV, "burr", start = list(shape1 = 0.3, shape2 = 1, 
  rate = 1))
cdfcomp(list(fendo.ln, fendo.ll, fendo.P, fendo.B), 
  xlogscale = TRUE, ylogscale = TRUE, 
  legendtext = c("lognormal", "loglogistic", "Pareto", "Burr"))


###################################################
### code chunk number 11: fitendo
###################################################
getOption("SweaveHooks")[["fig"]]()
cdfcomp(list(fendo.ln, fendo.ll, fendo.P, fendo.B), xlogscale = TRUE,
        ylogscale = TRUE,legendtext = c("lognormal","loglogistic","Pareto","Burr"))


###################################################
### code chunk number 12: quantilefitdist
###################################################
quantile(fendo.B, probs = 0.05)
quantile(ATV, probs = 0.05)


###################################################
### code chunk number 13: fendo.gof.print
###################################################
gofstat(list(fendo.ln, fendo.ll, fendo.P, fendo.B),
  fitnames = c("lnorm", "llogis", "Pareto", "Burr"))


###################################################
### code chunk number 14: fitBurr.boot.echo
###################################################
bendo.B <- bootdist(fendo.B, niter = 1001)


###################################################
### code chunk number 15: fitBurr.boot.results
###################################################
summary(bendo.B)
plot(bendo.B)


###################################################
### code chunk number 16: fitBurrbootplot
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(bendo.B)


###################################################
### code chunk number 17: fitATV.lnorm.quantile
###################################################
quantile(bendo.B, probs = 0.05)


###################################################
### code chunk number 18: fitATV.lnorm.quantileb
###################################################
quantile(bendo.B, probs = 0.05)


###################################################
### code chunk number 19: mge.gofcomp.echo
###################################################
fendo.ln.ADL <- fitdist(ATV, "lnorm", method = "mge", gof = "ADL")
fendo.ln.AD2L <- fitdist(ATV, "lnorm", method = "mge", gof = "AD2L")
cdfcomp(list(fendo.ln, fendo.ln.ADL, fendo.ln.AD2L), 
  xlogscale = TRUE, ylogscale = TRUE,
  main = "Fitting a lognormal distribution",
  xlegend = "bottomright", 
  legendtext = c("MLE","Left-tail AD", "Left-tail AD 2nd order"))


###################################################
### code chunk number 20: mgegofcompplot
###################################################
getOption("SweaveHooks")[["fig"]]()
cdfcomp(list(fendo.ln, fendo.ln.ADL, fendo.ln.AD2L),
  xlogscale = TRUE, ylogscale = TRUE,
  main = "Fitting a lognormal distribution",
  legendtext = c("MLE","Left-tail AD", "Left-tail AD 2nd order"),
xlegend = "bottomright")


###################################################
### code chunk number 21: quantilefitdist
###################################################
(HC5.estimates <- c(
  empirical = as.numeric(quantile(ATV, probs = 0.05)), 
  Burr = as.numeric(quantile(fendo.B, probs = 0.05)$quantiles), 
  lognormal_MLE = as.numeric(quantile(fendo.ln, probs = 0.05)$quantiles), 
  lognormal_AD2 = as.numeric(quantile(fendo.ln.ADL, 
    probs = 0.05)$quantiles), 
  lognormal_AD2L = as.numeric(quantile(fendo.ln.AD2L, 
    probs = 0.05)$quantiles)))


###################################################
### code chunk number 22: danish.mme
###################################################
data("danishuni")
str(danishuni)
fdanish.ln.MLE <- fitdist(danishuni$Loss, "lnorm")
fdanish.ln.MME <- fitdist(danishuni$Loss, "lnorm", method = "mme", 
  order = 1:2)
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.MME), 
  legend = c("lognormal MLE", "lognormal MME"), 
  main = "Fitting a lognormal distribution", 
  xlogscale = TRUE, datapch = 20)


###################################################
### code chunk number 23: danishmmeplot
###################################################
getOption("SweaveHooks")[["fig"]]()
library("actuar")
fdanish.P.MLE <- fitdist(danishuni$Loss, "pareto", 
  start=list(shape=10, scale=10), lower = 2+1e-6, upper = Inf)
memp <- function(x, order) sum(x^order)/length(x)
fdanish.P.MME <- fitdist(danishuni$Loss, "pareto", 
    method="mme", order=1:2, 
memp="memp", start=list(shape=10, scale=10), lower=c(2+1e-6,2+1e-6), 
upper=c(Inf,Inf))
par(mfrow=c(1, 2))
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.MME), 
        legend=c("lognormal MLE", "lognormal MME"), main="Fitting a lognormal distribution",
        xlogscale=TRUE, datapch=20)
cdfcomp(list(fdanish.P.MLE, fdanish.P.MME), 
        legend=c("Pareto MLE", "Pareto MME"), main="Fitting a Pareto distribution",
        xlogscale=TRUE, datapch=20)


###################################################
### code chunk number 24: danish.mme.pareto
###################################################
library("actuar")
fdanish.P.MLE <- fitdist(danishuni$Loss, "pareto",  
  start = list(shape = 10, scale = 10), lower = 2+1e-6, upper = Inf)
memp <- function(x, order) sum(x^order)/length(x)
fdanish.P.MME <- fitdist(danishuni$Loss, "pareto", method = "mme", 
  order = 1:2, memp = "memp", start = list(shape = 10, scale = 10), 
  lower = c(2+1e-6, 2+1e-6), upper = c(Inf, Inf))
cdfcomp(list(fdanish.P.MLE, fdanish.P.MME), 
  legend = c("Pareto MLE", "Pareto MME"), 
  main = "Fitting a Pareto distribution", 
  xlogscale = TRUE, datapch = ".")
gofstat(list(fdanish.ln.MLE, fdanish.P.MLE, 
  fdanish.ln.MME, fdanish.P.MME),
  fitnames = c("lnorm.mle", "Pareto.mle", "lnorm.mme", "Pareto.mme"))


###################################################
### code chunk number 25: danish.qme.echo
###################################################
fdanish.ln.QME1 <- fitdist(danishuni$Loss, "lnorm", method = "qme", 
  probs = c(1/3, 2/3))
fdanish.ln.QME2 <- fitdist(danishuni$Loss, "lnorm", method = "qme", 
  probs = c(8/10, 9/10))
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.QME1, fdanish.ln.QME2), 
  legend = c("MLE", "QME(1/3, 2/3)", "QME(8/10, 9/10)"), 
  main = "Fitting a lognormal distribution", 
  xlogscale = TRUE, datapch = 20)


###################################################
### code chunk number 26: danishqmeplot
###################################################
getOption("SweaveHooks")[["fig"]]()
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.QME1, fdanish.ln.QME2), 
        legend=c("MLE", "QME(1/3, 2/3)", "QME(8/10, 9/10)"), main="Fitting a lognormal distribution",
        xlogscale=TRUE, datapch=20)


###################################################
### code chunk number 27: optimmethod.gamma
###################################################
data("groundbeef")
fNM <- fitdist(groundbeef$serving, "gamma", optim.method = "Nelder-Mead")
fBFGS <- fitdist(groundbeef$serving, "gamma", optim.method = "BFGS") 
fSANN <- fitdist(groundbeef$serving, "gamma", optim.method = "SANN")
fCG <- try(fitdist(groundbeef$serving, "gamma", optim.method = "CG", 
  control = list(maxit = 10000)))
if(class(fCG) == "try-error")
  fCG <- list(estimate = NA)


###################################################
### code chunk number 28: optimmethod.customgenoud
###################################################
mygenoud <- function(fn, par, ...) 
{
   require(rgenoud)
   res <- genoud(fn, starting.values = par, ...)        
   standardres <- c(res, convergence = 0)
   return(standardres)
}


###################################################
### code chunk number 29: optimmethod.customgenoud.fitdist
###################################################
fgenoud <- mledist(groundbeef$serving, "gamma", custom.optim = mygenoud, 
  nvars = 2, max.generations = 10, Domains = cbind(c(0,0), c(10,10)), 
  boundary.enforcement = 1, hessian = TRUE, print.level = 0, P9 = 10)
cbind(NM = fNM$estimate,
  BFGS = fBFGS$estimate,
  SANN = fSANN$estimate,
  CG = fCG$estimate,
  fgenoud = fgenoud$estimate)


###################################################
### code chunk number 30: datsalinity
###################################################
data("salinity")
str(salinity)


###################################################
### code chunk number 31: plotsalinity2.echo
###################################################
plotdistcens(salinity, NPMLE = FALSE)


###################################################
### code chunk number 32: plotsalinity
###################################################
getOption("SweaveHooks")[["fig"]]()
plotdistcens(salinity,Turnbull = FALSE)


###################################################
### code chunk number 33: fitsalinity.echo
###################################################
fsal.ln <- fitdistcens(salinity, "lnorm")
fsal.ll <- fitdistcens(salinity, "llogis",
  start = list(shape = 5, scale = 40))
summary(fsal.ln)
summary(fsal.ll)


###################################################
### code chunk number 34: fitsalinity.cdfcomp.echo (eval = FALSE)
###################################################
## par(mfrow=c(2, 2))
## cdfcompcens(list(fsal.ln, fsal.ll), 
##   legendtext = c("lognormal", "loglogistic "))
## qqcompcens(fsal.ln, legendtext = "lognormal")
## ppcompcens(fsal.ln, legendtext = "lognormal")
## qqcompcens(list(fsal.ln, fsal.ll), legendtext = c("lognormal", "loglogistic "),
##            main = "Q-Q plot with 2 dist.")


###################################################
### code chunk number 35: fitsalinitycdfcompplot
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mfrow=c(2, 2))
cdfcompcens(list(fsal.ln, fsal.ll),
    legendtext=c("lognormal", "loglogistic "))
qqcompcens(fsal.ln, legendtext = "lognormal")
ppcompcens(fsal.ln, legendtext = "lognormal")
qqcompcens(list(fsal.ln, fsal.ll), legendtext = c("lognormal", "loglogistic "),
           main = "Q-Q plot with 2 dist.")


###################################################
### code chunk number 36: dattoxocara
###################################################
data("toxocara")
str(toxocara)


###################################################
### code chunk number 37: fittoxocara.poisnbinom
###################################################
(ftoxo.P <- fitdist(toxocara$number, "pois"))
(ftoxo.nb <- fitdist(toxocara$number, "nbinom"))


###################################################
### code chunk number 38: fittoxocara.poisnbinom.echo
###################################################
par(mfrow = c(1,2))
denscomp(list(ftoxo.P, ftoxo.nb), 
  legendtext = c("Poisson", "negative binomial"), fitlty = 1)
cdfcomp(list(ftoxo.P, ftoxo.nb), 
  legendtext = c("Poisson", "negative binomial"), fitlty = 1)


###################################################
### code chunk number 39: fittoxocarapoisnbinomplot
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mfrow = c(1,2))
denscomp(list(ftoxo.P, ftoxo.nb), 
  legendtext = c("Poisson", "negative binomial"), fitlty = 1)
cdfcomp(list(ftoxo.P, ftoxo.nb), 
  legendtext = c("Poisson", "negative binomial"), fitlty = 1)


###################################################
### code chunk number 40: fittoxocara.poisnbinom.gof
###################################################
gofstat(list(ftoxo.P, ftoxo.nb), 
  fitnames = c("Poisson", "negative binomial"))


