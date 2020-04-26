## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=3, fig.path='figures/ts-', warning=FALSE)

## ---- message = FALSE---------------------------------------------------------
library(ggfortify)
autoplot(AirPassengers)

## -----------------------------------------------------------------------------
autoplot(AirPassengers, ts.colour = 'red', ts.linetype = 'dashed')

## ----message = FALSE----------------------------------------------------------
library(vars)
data(Canada)
autoplot(Canada)

## -----------------------------------------------------------------------------
autoplot(Canada, facets = FALSE)

## ---- message = FALSE, eval = FALSE-------------------------------------------
#  library(xts)
#  autoplot(as.xts(AirPassengers), ts.colour = 'green')
#  
#  library(timeSeries)
#  autoplot(as.timeSeries(AirPassengers), ts.colour = ('dodgerblue3'))

## ---- message = FALSE---------------------------------------------------------
autoplot(AirPassengers, ts.geom = 'bar', fill = 'blue')

## ---- message = FALSE---------------------------------------------------------
autoplot(AirPassengers, ts.geom = 'ribbon', fill = 'green')

## ---- message = FALSE---------------------------------------------------------
autoplot(AirPassengers, ts.geom = 'point', shape = 3)

## ---- message = FALSE---------------------------------------------------------
mts <- ts(data.frame(a = c(1, 2, 3, 4, 4, 3), b = c(3, 2, 3, 2, 2, 1)), start = 2010)
autoplot(mts, ts.geom = 'bar', facets = FALSE)

## ---- message = FALSE---------------------------------------------------------
autoplot(mts, ts.geom = 'bar', facets = FALSE, stacked = TRUE)

## ---- message = FALSE---------------------------------------------------------
autoplot(mts, ts.geom = 'ribbon', facets = FALSE)

## ---- message = FALSE---------------------------------------------------------
autoplot(mts, ts.geom = 'ribbon', facets = FALSE, stacked = TRUE)

## ---- message = FALSE---------------------------------------------------------
library(forecast)
d.arima <- auto.arima(AirPassengers)
d.forecast <- forecast(d.arima, level = c(95), h = 50)
autoplot(d.forecast)

## -----------------------------------------------------------------------------
autoplot(d.forecast, ts.colour = 'firebrick1', predict.colour = 'red',
         predict.linetype = 'dashed', conf.int = FALSE)

## -----------------------------------------------------------------------------
library(vars)
d.vselect <- VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
d.var <- VAR(Canada, p = d.vselect, type = 'const')

## -----------------------------------------------------------------------------
autoplot(predict(d.var, n.ahead = 50), ts.colour = 'dodgerblue4',
         predict.colour = 'blue', predict.linetype = 'dashed')

## ----message = FALSE----------------------------------------------------------
library(changepoint)
autoplot(cpt.meanvar(AirPassengers))

## -----------------------------------------------------------------------------
autoplot(cpt.meanvar(AirPassengers), cpt.colour = 'blue', cpt.linetype = 'solid')

## ----message = FALSE----------------------------------------------------------
library(strucchange)
autoplot(breakpoints(Nile ~ 1), ts.colour = 'blue', ts.linetype = 'dashed',
         cpt.colour = 'dodgerblue3', cpt.linetype = 'solid')

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dlm)
form <- function(theta){
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))
}

model <- form(dlmMLE(Nile, parm = c(1, 1), form)$par)
filtered <- dlmFilter(Nile, model)

autoplot(filtered)

## -----------------------------------------------------------------------------
autoplot(filtered, ts.linetype = 'dashed', fitted.colour = 'blue')

## -----------------------------------------------------------------------------
smoothed <- dlmSmooth(filtered)
class(smoothed)
autoplot(smoothed)

## -----------------------------------------------------------------------------
p <- autoplot(filtered)
autoplot(smoothed, ts.colour = 'blue', p = p)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(KFAS)
model <- SSModel(
  Nile ~ SSMtrend(degree=1, Q=matrix(NA)), H=matrix(NA)
)
 
fit <- fitSSM(model=model, inits=c(log(var(Nile)),log(var(Nile))), method="BFGS")
smoothed <- KFS(fit$model)
autoplot(smoothed)

## -----------------------------------------------------------------------------
filtered <- KFS(fit$model, filtering="mean", smoothing='none')
autoplot(filtered)

## -----------------------------------------------------------------------------
trend <- signal(smoothed, states="trend")
class(trend)

## -----------------------------------------------------------------------------
p <- autoplot(filtered)
autoplot(trend, ts.colour = 'blue', p = p)

## ---- message = FALSE---------------------------------------------------------
autoplot(stl(AirPassengers, s.window = 'periodic'), ts.colour = 'blue')

## ---- fig.width = 4, fig.height = 2-------------------------------------------
autoplot(acf(AirPassengers, plot = FALSE))

## ---- fig.width = 4, fig.height = 2-------------------------------------------
autoplot(acf(AirPassengers, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma')

## ---- fig.width = 4, fig.height = 2-------------------------------------------
autoplot(spec.ar(AirPassengers, plot = FALSE))

## ---- message = FALSE, fig.width=3, fig.height=3------------------------------
ggcpgram(arima.sim(list(ar = c(0.7, -0.5)), n = 50))

## ---- message = FALSE, fig.width = 5, fig.height = 5--------------------------
library(forecast)
ggtsdiag(auto.arima(AirPassengers))

## ---- message = FALSE, fig.width = 4, fig.height = 4--------------------------
ggfreqplot(AirPassengers)
ggfreqplot(AirPassengers, freq = 4)

