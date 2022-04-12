## ---- include = FALSE---------------------------------------------------------
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
parallelPlot(iris, refColumnDim = "Species")

## -----------------------------------------------------------------------------
parallelPlot(iris, refColumnDim = "Species", categoricalCS = "Set1")

## -----------------------------------------------------------------------------
parallelPlot(iris, refColumnDim = "Sepal.Length")

## -----------------------------------------------------------------------------
parallelPlot(iris, refColumnDim = "Sepal.Length", continuousCS = "YlOrRd")

## -----------------------------------------------------------------------------
parallelPlot(mtcars)

## -----------------------------------------------------------------------------
categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
parallelPlot(mtcars, categorical = categorical, refColumnDim = "cyl")

## -----------------------------------------------------------------------------
categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
inputColumns <- c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
parallelPlot(mtcars, categorical = categorical, inputColumns = inputColumns, refColumnDim = "cyl")

## -----------------------------------------------------------------------------
histoVisibility <- rep(TRUE, ncol(iris))
parallelPlot(iris, histoVisibility = histoVisibility)

## -----------------------------------------------------------------------------
invertedAxes <- rep(FALSE, ncol(iris))
invertedAxes[2] <- TRUE
parallelPlot(iris, invertedAxes = invertedAxes)

## -----------------------------------------------------------------------------
histoVisibility <- rep(TRUE, ncol(iris))
cutoffs <- list(list(c(6, 7)), NULL, NULL, NULL, c("virginica", "setosa"))
parallelPlot(iris, histoVisibility = histoVisibility, cutoffs = cutoffs)

## -----------------------------------------------------------------------------
parallelPlot(iris, refRowIndex = 1)

## -----------------------------------------------------------------------------
parallelPlot(iris, refColumnDim = "Species", rotateTitle = TRUE)

## -----------------------------------------------------------------------------
columnLabels <- gsub("\\.", "<br>", colnames(iris))
parallelPlot(iris, refColumnDim = "Species", columnLabels = columnLabels)

## -----------------------------------------------------------------------------
parallelPlot(iris, cssRules = list(
    "svg" = "background: white", # Set background of plot to white
    ".tick text" = c("fill: red", "font-size: 1.8em") # Set text of axes ticks red and greater
))

## -----------------------------------------------------------------------------
parallelPlot(iris, sliderPosition = list(
  dimCount = 3, # Number of columns to show
  startingDimIndex = 2 # Index of first shown column
))
# Visible columns starts at second column and three columns are represented.

## -----------------------------------------------------------------------------
parallelPlot(iris, refColumnDim = "Species", controlWidgets = TRUE)

