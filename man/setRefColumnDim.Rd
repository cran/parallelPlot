% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallelPlot.R
\name{setRefColumnDim}
\alias{setRefColumnDim}
\title{Line coloring}
\usage{
setRefColumnDim(id, dim)
}
\arguments{
\item{id}{output variable to read from (id which references the requested plot)}

\item{dim}{Name of the reference column (used to determine the color to attribute to a row);
\code{NULL} is allowed, meaning there is no coloring to apply.}
}
\value{
No return value, called from shiny applications for side effects.
}
\description{
Tells which column is used to determine the color to attribute to each row.
}
\examples{
 if(interactive() && require(shiny)) {
   library(shiny)
   library(parallelPlot)

   ui <- fluidPage(
     selectInput(
       "refColumnDimSelect",
       "Reference column:",
       choices = list(
         "None" = 1, "First" = 2, "Second" = 3
       ),
       selected = "None"
     ),
     p("Selector controls the colomn used to determine the color to attribute to rows"),
     parallelPlotOutput("parPlot")
   )

   server <- function(input, output, session) {
     output$parPlot <- renderParallelPlot({
       categorical <-
         list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
       parallelPlot(mtcars, categorical = categorical)
     })
     observeEvent(input$refColumnDimSelect, {
       choice <- as.numeric(input$refColumnDimSelect)
       refColumnDim <- list(NULL, colnames(mtcars)[1], colnames(mtcars)[2])[[choice]]
       parallelPlot::setRefColumnDim("parPlot", refColumnDim)
     })
   }

   shinyApp(ui, server)
 }

}
