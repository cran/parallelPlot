## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ----setup--------------------------------------------------------------------
library(parallelPlot)

## -----------------------------------------------------------------------------
parallelPlot(iris)

## -----------------------------------------------------------------------------
parallelPlot(iris, cssRules = list("g" = "visibility: hidden"))

## -----------------------------------------------------------------------------
parallelPlot(iris, cssRules = list("g.slider" = "visibility: hidden"))

