## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=3, fig.path='figures/pca-', warning=FALSE)

## ---- message = FALSE---------------------------------------------------------
library(ggfortify)
df <- iris[1:4]
pca_res <- prcomp(df, scale. = TRUE)

autoplot(pca_res)

## ---- message = FALSE---------------------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species')

## ---- message = FALSE---------------------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species', label = TRUE, label.size = 3)

## ---- message = FALSE---------------------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species', shape = FALSE, label.size = 3)

## ---- message = FALSE---------------------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species', loadings = TRUE)

## ---- message = FALSE---------------------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

## ---- message = FALSE---------------------------------------------------------
autoplot(pca_res, scale = 0)

## ---- message = FALSE---------------------------------------------------------
d.factanal <- factanal(state.x77, factors = 3, scores = 'regression')
autoplot(d.factanal, data = state.x77, colour = 'Income')
autoplot(d.factanal, label = TRUE, label.size = 3,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)

## ---- message = FALSE---------------------------------------------------------
set.seed(1)
autoplot(kmeans(USArrests, 3), data = USArrests)

autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3)

## ---- message = FALSE---------------------------------------------------------
library(cluster)
autoplot(clara(iris[-5], 3))

## -----------------------------------------------------------------------------
autoplot(fanny(iris[-5], 3), frame = TRUE)

## -----------------------------------------------------------------------------
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')

## ---- message = FALSE---------------------------------------------------------
library(lfda)

# Local Fisher Discriminant Analysis (LFDA)
model <- lfda(iris[-5], iris[, 5], r = 3, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')

## ---- message = FALSE---------------------------------------------------------
# Semi-supervised Local Fisher Discriminant Analysis (SELF)
model <- self(iris[-5], iris[, 5], beta = 0.1, r = 3, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')

## ---- message = FALSE---------------------------------------------------------
autoplot(eurodist)

## -----------------------------------------------------------------------------
autoplot(cmdscale(eurodist, eig = TRUE))

## -----------------------------------------------------------------------------
autoplot(cmdscale(eurodist, eig = TRUE), label = TRUE, label.size = 3)

## ---- message = FALSE---------------------------------------------------------
library(MASS)
autoplot(isoMDS(eurodist), colour = 'orange', size = 4, shape = 3)

## ---- message = FALSE---------------------------------------------------------
autoplot(sammon(eurodist), shape = FALSE, label.colour = 'blue', label.size = 3)

