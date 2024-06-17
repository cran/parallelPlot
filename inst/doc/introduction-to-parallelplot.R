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
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
parallelPlot(mtcars, categorical = categorical, refColumnDim = "cyl")

## -----------------------------------------------------------------------------
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
parallelPlot(
  mtcars,
  categorical = categorical,
  refColumnDim = "cyl",
  categoriesRep = "EquallySizedBoxes"
)

## -----------------------------------------------------------------------------
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
parallelPlot(
  mtcars,
  categorical = categorical,
  refColumnDim = "cyl",
  arrangeMethod = "fromLeft"
)

## -----------------------------------------------------------------------------
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
parallelPlot(
  mtcars,
  categorical = categorical,
  refColumnDim = "cyl",
  arrangeMethod = "fromBoth"
)

## -----------------------------------------------------------------------------
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
inputColumns <- c("mpg", "disp", "drat", "qsec", "am", "gear", "carb")
parallelPlot(
  mtcars,
  categorical = categorical,
  inputColumns = inputColumns,
  refColumnDim = "cyl"
)

## -----------------------------------------------------------------------------
histoVisibility <- rep(TRUE, ncol(iris))
parallelPlot(iris, histoVisibility = histoVisibility)

## -----------------------------------------------------------------------------
invertedAxes <- c("Sepal.Width")
parallelPlot(iris, invertedAxes = invertedAxes)

## -----------------------------------------------------------------------------
histoVisibility <- names(iris) # same as `rep(TRUE, ncol(iris))`
cutoffs <- list(Sepal.Length = list(c(6, 7)), Species = c("virginica", "setosa"))
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
  "svg" = "background: #C2C2C2", # Set background of plot to grey
  ".axisLabel" = c("fill: red", "font-size: 1.8em"), # Set title of axes red and greater
  ".tick text" = "font-size: 1.8em", # Set text of axes ticks greater
  ".plotGroup path" = "opacity: 0.25", # Make lines less opaque
  ".xValue" = "color: orange", # Set color for x values in tooltip
  ".xName" = "display: table" # Trick to have x values on a new line
))

## -----------------------------------------------------------------------------
parallelPlot(iris, sliderPosition = list(
  dimCount = 3, # Number of columns to show
  startingDimIndex = 2 # Index of first shown column
))

## -----------------------------------------------------------------------------
parallelPlot(iris, refColumnDim = "Species", controlWidgets = TRUE, height = 350)

