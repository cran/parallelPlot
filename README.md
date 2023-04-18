# parallelPlot

'parallelPlot' is: 
- an interactive parallel coordinates plot (thanks to the [d3.js](https://d3js.org/));
- an HTML widget for R that render in various contexts including the R console, 'R Markdown' documents, and 'Shiny' web applications (thanks to the [htmlwidgets](https://www.htmlwidgets.org/) package).

## Installation

You can install this package from CRAN, or the development version from GitHub:

``` r
# CRAN version
install.packages('parallelPlot')

# Or Github version
if (!require('devtools')) install.packages('devtools')
devtools::install_gitlab(host = 'https://gitlab.com', repo = 'drti/parallelPlot', subdir = 'htmlwidget')
```

## Example

``` r
library(parallelPlot)
categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
parallelPlot(mtcars, categorical = categorical, refColumnDim = "cyl")
```