% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallelPlot.R
\name{setCutoffs}
\alias{setCutoffs}
\title{Cutoffs values}
\usage{
setCutoffs(id, cutoffs)
}
\arguments{
\item{id}{output variable to read from (id which references the requested plot)}

\item{cutoffs}{Vector of list (one for each data column) of vector (one for each cutoff)
containing two values for continuous input (min and max value defining the cutoff),
or one value for categorical input (name of the category to keep),
or \code{NULL} if there is no cutoff to apply;
\code{NULL} is allowed, meaning all columns are without cutoff.
A named list can also be provided to only indicate which columns must be assigned
to a new cutoff.}
}
\value{
No return value, called from shiny applications for side effects.
}
\description{
Tells which cutoffs to use for each column.
}
\details{
It's possible to filter some lines by defining cutoffs to apply to columns.
}
\examples{
 if(interactive() && require(shiny)) {
   library(shiny)
   library(parallelPlot)

   ui <- fluidPage(
     sliderInput("brushSlider", "Brush for 'Sepal.Length' column:",
       min = 4, max = 8, step = 0.1, value = c(4, 8)),
     p("The slider controls the rows which are kept by cutoff (others are shaded)"),
     parallelPlotOutput("parPlot")
   )

   server <- function(input, output, session) {
     output$parPlot <- renderParallelPlot({
       parallelPlot(iris)
     })
     observeEvent(input$brushSlider, {
       cutoffs <- list()
       cutoffs["Sepal.Length"] <- list(list(input$brushSlider))
       parallelPlot::setCutoffs("parPlot", cutoffs)
     })
   }

   shinyApp(ui, server)
 }

}
