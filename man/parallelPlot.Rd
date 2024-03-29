% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallelPlot.R
\name{parallelPlot}
\alias{parallelPlot}
\title{htmlwidget for d3.js parallel coordinate plot}
\usage{
parallelPlot(
  data,
  categorical = NULL,
  categoriesRep = "EquallySpacedLines",
  arrangeMethod = "fromRight",
  inputColumns = NULL,
  keptColumns = NULL,
  histoVisibility = NULL,
  invertedAxes = NULL,
  cutoffs = NULL,
  refRowIndex = NULL,
  refColumnDim = NULL,
  rotateTitle = FALSE,
  columnLabels = NULL,
  continuousCS = "Viridis",
  categoricalCS = "Category10",
  eventInputId = NULL,
  editionMode = "EditionOff",
  controlWidgets = FALSE,
  cssRules = NULL,
  sliderPosition = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL
)
}
\arguments{
\item{data}{data.frame with data to use in the chart.}

\item{categorical}{List of list (one for each data column) containing the name of available categories,
or \code{NULL} if column corresponds to continuous data;
\code{NULL} is allowed, meaning all columns are continuous.}

\item{categoriesRep}{Within a category column, the height assigned to each category can either be:
equal for each category (\code{EquallySizedBoxes});
or calculated to reflect the proportion of lines passing through each category (\code{EquallySpacedLines}).}

\item{arrangeMethod}{Within a category box:
the position of lines can be calculated to minimize crossings on the left of the box (\code{fromLeft});
the position of lines can be calculated to minimize crossings on the right (\code{fromRight}, default behavior);
lines can be split in two points to minimize crossings on the left and on the right (\code{fromBoth}).
To turn this ordering off (for example for performance reasons),
`arrangeMethod` can also be set to \code{fromNone}.}

\item{inputColumns}{List of boolean (one for each data column), \code{TRUE} for an input column, \code{FALSE} for an output column;
\code{NULL} is allowed, meaning all columns are inputs.}

\item{keptColumns}{List of boolean (one for each data column), \code{FALSE} if column has to be ignored;
\code{NULL} is allowed, meaning all columns are available.}

\item{histoVisibility}{List of boolean (one for each data column), \code{TRUE} if an histogram must be displayed;
\code{NULL} is allowed, meaning no histogram must be displayed.}

\item{invertedAxes}{List of boolean (one for each data column), \code{TRUE} if orientation of axis must be inverted;
\code{NULL} is allowed, meaning no axis must be inverted.}

\item{cutoffs}{List of list (one for each data column) of list (one for each cutoff)
containing two values (min and max values defining the cutoff)
or \code{NULL} if there is no cutoff to apply;
\code{NULL} is allowed, meaning all columns are without cutoff.}

\item{refRowIndex}{Index of the sample row which has to appear horizontal;
\code{NULL} is allowed, meaning there is no row to use as reference.}

\item{refColumnDim}{Name of the reference column (used to determine the color to attribute to a row);
\code{NULL} is allowed, meaning there is no coloring to apply.}

\item{rotateTitle}{\code{TRUE} if column title must be rotated.}

\item{columnLabels}{List of string (one for each data column) to display in place of column name found in data,
or \code{NULL} if there is no alternative name;
\code{NULL} is allowed, meaning all columns are without alternative name;
\code{<br>} can be used to insert line breaks.}

\item{continuousCS}{Name of the color Scale to use for continuous data;
supported names: "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault",
"Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", "PuBuGn","PuBu",
"PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd";
default value is \code{Viridis}.}

\item{categoricalCS}{Name of the color Scale to use for categorical data;
supported names: Category10, Accent, Dark2, Paired, Set1;
default value is \code{Category10}.}

\item{eventInputId}{When plot event occurred, reactive input to write to; \code{NULL} is allowed, default value is 'plotEvent'.}

\item{editionMode}{Supported edition modes: \code{EditionOff, EditionOnDrag, EditionOnDragEnd}; default value is \code{EditionOff} .}

\item{controlWidgets}{Tells if some widgets must be available to control plot;
\code{NULL} is allowed, meaning that '!HTMLWidgets.shinyMode' is to use;
default value is \code{FALSE}.}

\item{cssRules}{CSS rules to add.
Must be a named list of the form list(selector = declarations),
where selector is a valid CSS selector and declarations is a string or vector of declarations.}

\item{sliderPosition}{Set initial position of slider, specifying which columns interval is visible.
Default value is \code{NULL} which is equivalent to:
  list(
    dimCount = 8,
    startingDimIndex = 1
  )}

\item{width}{Integer in pixels defining the width of the widget.}

\item{height}{Integer in pixels defining the height of the widget.}

\item{elementId}{Unique \code{CSS} selector id for the widget.}
}
\value{
An object of class \code{htmlwidget} that will intelligently print itself into HTML in a variety of contexts
  including the R console, within R Markdown documents, and within Shiny output bindings.
}
\description{
htmlwidget for d3.js parallel coordinate plot
}
\examples{
 if(interactive()) {
   library(parallelPlot)

   categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
   parallelPlot(mtcars, categorical = categorical, refColumnDim = "cyl")
   # 'cyl' and four last columns have a box representation for its categories

   histoVisibility <- rep(TRUE, ncol(iris))
   parallelPlot(iris, histoVisibility = histoVisibility)
   # An histogram is displayed for each column

   histoVisibility <- rep(TRUE, ncol(iris))
   cutoffs <- list(list(c(6, 7)), NULL, NULL, NULL, c("virginica", "setosa"))
   parallelPlot(iris, histoVisibility = histoVisibility, cutoffs = cutoffs)
   # Cut lines are shaded; an histogram is displayed considering only kept lines

   parallelPlot(iris, refRowIndex = 1)
   # Axes are shifted vertically in such a way that first trace of the dataset looks horizontal

   columnLabels <- gsub("\\\\.", "<br>", colnames(iris))
   parallelPlot(iris, refColumnDim = "Species", columnLabels = columnLabels)
   # Given names are displayed in place of dataset column names; <br> is used to insert line breaks

   parallelPlot(iris, cssRules = list(
       "svg" = "background: white",
       ".tick text" = c("fill: red", "font-size: 1.8em")
   ))
   # Background of plot is white and text of axes ticks is red and greater
 }

}
