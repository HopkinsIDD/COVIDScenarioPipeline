## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=4, fig.path='figures/basics-', warning=FALSE)

## ---- message = FALSE---------------------------------------------------------
library(ggfortify)

## -----------------------------------------------------------------------------
autoplot(AirPassengers)

## -----------------------------------------------------------------------------
autoplot(AirPassengers, ts.colour = 'blue')

## -----------------------------------------------------------------------------
p <- autoplot(AirPassengers)
class(p)
# plot as it is
p

# add title and labels
p + ggtitle('AirPassengers') + xlab('Year') + ylab('Passengers')

## -----------------------------------------------------------------------------
# these common options are supported as keywords 
autoplot(AirPassengers, title = 'AirPassengers', xlab = 'Year', ylab = 'Passengers')

## -----------------------------------------------------------------------------
set.seed(1)
p <- autoplot(kmeans(iris[-5], 3), data = iris)
# plot as it is
p

# change colour mapping
p + scale_colour_brewer()

## -----------------------------------------------------------------------------
df <- fortify(kmeans(iris[-5], 3), data = iris)
head(df)

ggplot(df, aes(x= cluster, fill = cluster)) + geom_bar()

## ---- message = FALSE, fig.width = 9, fig.height = 2.5, eval=FALSE------------
#  res <- lm(Volume ~ Girth, data = trees)
#  mp <- autoplot(res, ncol = 4)
#  mp

## ---- message = FALSE, fig.width = 9, fig.height = 2.5, eval=FALSE------------
#  class(mp)
#  mp + theme_bw()

## ---- message = FALSE, fig.width = 9, fig.height = 5, eval=FALSE--------------
#  mp +
#    (ggplot(trees, aes(Girth, Volume)) + geom_point()) +
#    (ggplot(trees, aes(Girth, Height)) + geom_point())

## ---- message = FALSE, fig.width = 9, fig.height = 2.5, eval=FALSE------------
#  mp[2:3]

## ---- message = FALSE, fig.width = 2.25, fig.height = 2.5, eval=FALSE---------
#  mp[[1]]

## ---- message = FALSE, fig.width = 9, fig.height = 2.5, eval=FALSE------------
#  mp[2:3] <- mp[2:3] + theme_bw()
#  mp

## ---- message = FALSE, fig.width = 9, fig.height = 2.5------------------------
res <- lapply(c(3, 4, 5), function(x) kmeans(iris[-5], x))
autoplot(res, data = iris[-5], ncol = 3)

## ---- message = FALSE, fig.width = 8, fig.height = 2.5------------------------
library(survival)
res <- list(a = survfit(Surv(time, status) ~ 1, data = lung),
            b = survfit(Surv(time, status) ~ sex, data = lung))
autoplot(res)

## ---- message = FALSE, fig.width = 8, fig.height = 2.5------------------------
library(vars)
data(Canada)
res <- list(a = Canada, b = AirPassengers)
autoplot(res)

## ---- message = FALSE, fig.width = 4.5, fig.height = 2.5----------------------
p1 <- ggplot(iris, aes(Petal.Width, Petal.Length)) + geom_point()
p2 <- ggplot(iris, aes(Petal.Width, Petal.Length)) + geom_point()
new('ggmultiplot', plots = list(p1, p2))

## ---- message = FALSE, fig.width = 2.25, fig.height = 5-----------------------
new('ggmultiplot', plots = list(p1, p2), ncol = 1)

