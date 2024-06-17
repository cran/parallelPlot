# parallelPlot

'parallelPlot' is: 
- an interactive parallel coordinates plot (thanks to the [d3.js](https://d3js.org/));
- an HTML widget for R that render in various contexts including the R console, 'R Markdown' documents, and 'Shiny' web applications (thanks to the [htmlwidgets](https://www.htmlwidgets.org/) package);
- a Shiny widget that can be updated programmatically and whose some actions can trigger reactions.

As an example, a Shiny application is available on [shinyapps.io](https://detocs.shinyapps.io/ScatterPlotMatrix-ParallelPlot-Link-DataFromFile/).

## Installation

You can install this package from CRAN, or the development version from GitLab:

``` r
# CRAN version
install.packages('parallelPlot')

# Or GitLab version
if (!require('devtools')) install.packages('devtools')
devtools::install_gitlab(host = 'https://gitlab.com', repo = 'drti/parallelPlot', subdir = 'htmlwidget')
```

## Example

``` r
library(parallelPlot)
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
parallelPlot(mtcars, categorical = categorical, refColumnDim = "cyl")
```

See also Shiny testing app:
[https://detocs.shinyapps.io/ParallelPlot-Testing-App/](https://detocs.shinyapps.io/ScatterPlotMatrix-ParallelPlot-Link-DataFromFile/)
whose source code is available in [test\testingApp\Shiny-ParallelPlotTest.R](https://ifpen-gitlab.appcollaboratif.fr/detocs/scatterplotmatrix/-/blob/master/test/testingApp/Shiny-ScatterPlotMatrix-ParallelPlot-CsvFile.R)
