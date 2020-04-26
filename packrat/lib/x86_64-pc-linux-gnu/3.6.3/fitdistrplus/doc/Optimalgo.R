## ----setup, echo=FALSE, message=FALSE, warning=FALSE---------------------
require(fitdistrplus)
require(knitr) #for kable() function
set.seed(12345)
options(digits = 3)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  fitbench <- function(data, distr, method, grad=NULL, control=list(trace=0, REPORT=1, maxit=1000), lower=-Inf, upper=+Inf, ...)

## ---- echo=FALSE---------------------------------------------------------
fitbench <- fitdistrplus:::fitbench

## ------------------------------------------------------------------------
lnL <- function(par, fix.arg, obs, ddistnam) 
  fitdistrplus:::loglikelihood(par, fix.arg, obs, ddistnam) 
grlnlbeta <- fitdistrplus:::grlnlbeta

## ---- fig.height=4, fig.width=4------------------------------------------
#(1) beta distribution
n <- 200
x <- rbeta(n, 3, 3/4)
grlnlbeta(c(3, 4), x) #test
hist(x, prob=TRUE)
lines(density(x), col="red")
curve(dbeta(x, 3, 3/4), col="green", add=TRUE)
legend("topleft", lty=1, col=c("red","green"), leg=c("empirical", "theoretical"))

## ------------------------------------------------------------------------
ctr <- list(trace=0, REPORT=1, maxit=1000)

## ------------------------------------------------------------------------
unconstropt <- fitbench(x, "beta", "mle", grad=grlnlbeta, lower=0)

## ------------------------------------------------------------------------
dbeta2 <- function(x, shape1, shape2, log)
  dbeta(x, exp(shape1), exp(shape2), log=log)
#take the log of the starting values
startarg <- lapply(fitdistrplus:::start.arg.default(x, "beta"), log)
#redefine the gradient for the new parametrization
grbetaexp <- function(par, obs, ...) 
    grlnlbeta(exp(par), obs) * exp(par)
    

expopt <- fitbench(x, distr="beta2", method="mle", grad=grbetaexp, start=startarg) 
#get back to original parametrization
expopt[c("fitted shape1", "fitted shape2"), ] <- exp(expopt[c("fitted shape1", "fitted shape2"), ])

## ---- results='asis', echo=FALSE-----------------------------------------
kable(unconstropt[, grep("G-", colnames(unconstropt), invert=TRUE)], digits=3)

## ---- results='asis', echo=FALSE-----------------------------------------
kable(unconstropt[, grep("G-", colnames(unconstropt))], digits=3)

## ---- results='asis', echo=FALSE-----------------------------------------
kable(expopt[, grep("G-", colnames(expopt), invert=TRUE)], digits=3)

## ---- results='asis', echo=FALSE-----------------------------------------
kable(expopt[, grep("G-", colnames(expopt))], digits=3)

## ---- fig.width=4, fig.height=4------------------------------------------
llsurface(min.arg=c(0.1, 0.1), max.arg=c(7, 3), 
          plot.arg=c("shape1", "shape2"), nlev=25,
          plot.np=50, data=x, distr="beta", back.col = FALSE)
points(unconstropt[1,"BFGS"], unconstropt[2,"BFGS"], pch="+", col="red")
points(3, 3/4, pch="x", col="green")

## ---- fig.width=4, fig.height=4------------------------------------------
b1 <- bootdist(fitdist(x, "beta", method="mle", optim.method="BFGS"), niter=100, parallel="snow", ncpus=2)
summary(b1)
plot(b1)
abline(v=3, h=3/4, col="red", lwd=1.5)

## ------------------------------------------------------------------------
grlnlNB <- function(x, obs, ...)
{
  m <- x[1]
  p <- x[2]
  n <- length(obs)
  c(sum(psigamma(obs+m)) - n*psigamma(m) + n*log(p),
    m*n/p - sum(obs)/(1-p))
}

## ---- fig.height=4, fig.width=4------------------------------------------
#(1) beta distribution
n <- 200
trueval <- c("size"=10, "prob"=3/4, "mu"=10/3)
x <- rnbinom(n, trueval["size"], trueval["prob"])

hist(x, prob=TRUE, ylim=c(0, .3))
lines(density(x), col="red")
points(min(x):max(x), dnbinom(min(x):max(x), trueval["size"], trueval["prob"]), col="green")
legend("topleft", lty=1, col=c("red","green"), leg=c("empirical", "theoretical"))

## ------------------------------------------------------------------------
ctr <- list(trace=0, REPORT=1, maxit=1000)
unconstropt <- fitbench(x, "nbinom", "mle", grad=grlnlNB, lower=0)
unconstropt <- rbind(unconstropt, "fitted prob"=unconstropt["fitted mu",] / (1+unconstropt["fitted mu",]))

## ------------------------------------------------------------------------
dnbinom2 <- function(x, size, prob, log)
  dnbinom(x, exp(size), 1/(1+exp(-prob)), log=log)
#transform starting values
startarg <- fitdistrplus:::start.arg.default(x, "nbinom")
startarg$mu <- startarg$size / (startarg$size+startarg$mu)
startarg <- list(size=log(startarg[[1]]), prob=log(startarg[[2]]/(1-startarg[[2]])))

#redefine the gradient for the new parametrization
Trans <- function(x)
  c(exp(x[1]), plogis(x[2]))
grNBexp <- function(par, obs, ...) 
    grlnlNB(Trans(par), obs) * c(exp(par[1]), plogis(x[2])*(1-plogis(x[2])))

expopt <- fitbench(x, distr="nbinom2", method="mle", grad=grNBexp, start=startarg) 
#get back to original parametrization
expopt[c("fitted size", "fitted prob"), ] <- apply(expopt[c("fitted size", "fitted prob"), ], 2, Trans)

## ---- results='asis', echo=FALSE-----------------------------------------
kable(unconstropt[, grep("G-", colnames(unconstropt), invert=TRUE)], digits=3)

## ---- results='asis', echo=FALSE-----------------------------------------
kable(unconstropt[, grep("G-", colnames(unconstropt))], digits=3)

## ---- results='asis', echo=FALSE-----------------------------------------
kable(expopt[, grep("G-", colnames(expopt), invert=TRUE)], digits=3)

## ---- results='asis', echo=FALSE-----------------------------------------
kable(expopt[, grep("G-", colnames(expopt))], digits=3)

## ---- fig.width=4, fig.height=4------------------------------------------
llsurface(min.arg=c(5, 0.3), max.arg=c(15, 1), 
          plot.arg=c("size", "prob"), nlev=25,
          plot.np=50, data=x, distr="nbinom", back.col = FALSE)
points(unconstropt["fitted size","BFGS"], unconstropt["fitted prob","BFGS"], pch="+", col="red")
points(trueval["size"], trueval["prob"], pch="x", col="green")

## ---- fig.width=4, fig.height=4------------------------------------------
b1 <- bootdist(fitdist(x, "nbinom", method="mle", optim.method="BFGS"), niter=100, parallel="snow", ncpus=2)
summary(b1)
plot(b1)
abline(v=trueval["size"], h=trueval["mu"], col="red", lwd=1.5)

