#' htmlwidget for d3.js parallel coordinate plot
#'
#' @param data data.frame with data to use in the chart.
#' @param categorical List of list (one for each data column) containing the name of available categories, or \code{NULL} if column corresponds to continuous data; \code{NULL} is allowed, meaning all columns are continuous.
#' @param inputColumns List of boolean (one for each data column), \code{TRUE} for an input column, \code{FALSE} for an output column; \code{NULL} is allowed, meaning all columns are inputs.
#' @param keptColumns List of boolean (one for each data column), \code{FALSE} if column has to be ignored; \code{NULL} is allowed, meaning all columns are available.
#' @param histoVisibility List of boolean (one for each data column), \code{TRUE} if an histogram must be displayed; \code{NULL} is allowed, meaning no histogram must be displayed.
#' @param cutoffs List of list (one for each data column) of list (one for each cutoff) containing two values (min and max values defining the cutoff) or \code{NULL} if there is no cutoff to apply; \code{NULL} is allowed, meaning all columns are without cutoff.
#' @param refRowIndex Index of the sample row which has to appear horizontal; \code{NULL} is allowed, meaning there is no row to use as reference.
#' @param refColumnDim Name of the reference column (used to determine the color to attribute to a row); \code{NULL} is allowed, meaning there is no coloring to apply.
#' @param rotateTitle \code{TRUE} if column title must be rotated.
#' @param columnLabels List of string (one for each data column) to display in place of column name found in data, or \code{NULL} if there is no alternative name; \code{NULL} is allowed, meaning all columns are without alternative name; \code{<br>} can be used to insert line breaks.
#' @param continuousCS Name of the color Scale to use for continuous data (supported names: \code{Blues, RdBu, YlGnBu, YlOrRd, Reds}; default value is \code{Blues}).
#' @param categoricalCS Name of the color Scale to use for categorical data (supported names: \code{Category10, Accent, Dark2, Paired, Set1}; default value is \code{Category10}).
#' @param eventInputId When plot event occurred, reactive input to write to; \code{NULL} is allowed, default value is 'plotEvent'.
#' @param editionMode Supported edition modes: \code{EditionOff, EditionOnDrag, EditionOnDragEnd}; default value is \code{EditionOff} .
#' @param width Integer in pixels defining the width of the widget.
#' @param height Integer in pixels defining the height of the widget.
#' @param elementId Unique \code{CSS} selector id for the widget.
#'
#' @return An object of class \code{htmlwidget} that will intelligently print itself into HTML in a variety of contexts
#' including the R console, within R Markdown documents, and within Shiny output bindings.
#'
#' @examples
#'  if(interactive()) {
#'    library(parallelPlot)
#'
#'    categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
#'    parallelPlot(mtcars, categorical = categorical, refColumnDim = "cyl")
#'    # 'cyl' and four last columns have a box representation for its categories
#'
#'    histoVisibility <- rep(TRUE, ncol(iris))
#'    parallelPlot(iris, histoVisibility = histoVisibility)
#'    # An histogram is displayed for each column
#'
#'    histoVisibility <- rep(TRUE, ncol(iris))
#'    cutoffs <- list(list(c(6, 7)), NULL, NULL, NULL, c("virginica", "setosa"))
#'    parallelPlot(iris, histoVisibility = histoVisibility, cutoffs = cutoffs)
#'    # Cut traces are greyed; an histogram is displayed considering only kept traces
#'
#'    parallelPlot(iris, refRowIndex = 1)
#'    # Axes are shifted vertically in such a way that first trace of the dataset looks horizontal
#'
#'    columnLabels <- gsub("\\.", "<br>", colnames(iris))
#'    parallelPlot(iris, refColumnDim = "Species", columnLabels = columnLabels)
#'    # Given names are displayed in place of dataset column names; <br> is used to insert line breaks
#'  }
#'
#' @import htmlwidgets
#'
#' @export
parallelPlot <- function(
  data, 
  categorical = NULL, 
  inputColumns = NULL, 
  keptColumns = NULL, 
  histoVisibility = NULL, 
  cutoffs = NULL, 
  refRowIndex = NULL, 
  refColumnDim = NULL, 
  rotateTitle = FALSE, 
  columnLabels = NULL, 
  continuousCS = 'Blues', 
  categoricalCS = 'Category10', 
  eventInputId = NULL, 
  editionMode = 'EditionOff',
  width = NULL, 
  height = NULL, 
  elementId = NULL
) {

  args <- checkArgs(
    list(
      data = data,
      categorical = categorical,
      inputColumns = inputColumns,
      keptColumns = keptColumns,
      histoVisibility = histoVisibility,
      cutoffs = cutoffs,
      refRowIndex = refRowIndex,
      refColumnDim = refColumnDim,
      rotateTitle = rotateTitle,
      columnLabels = columnLabels,
      continuousCS = continuousCS,
      categoricalCS = categoricalCS,
      eventInputId = eventInputId,
      editionMode = editionMode
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'parallelPlot',
    args,
    width = width,
    height = height,
    package = 'parallelPlot',
    elementId = elementId
  )
}

checkArgs <- function(args) {
  return (
    checkEventInputId(
      checkCategoricalCS(
        checkContinuousCS(
          checkColumnLabels(
            checkColumnLabels(
              checkRotateTitle(
                checkRefColumnDim(
                  checkRefRowIndex(
                    checkCutoffs(
                      checkHistoVisibility(
                        checkInputColumns(
                          checkKeptColumns(
                            checkCategorical(
                              checkData(args)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

checkData <- function(args) {
  if (!is.data.frame(args$data) && !is.matrix(args$data)) {
    stop("'data' must be a dataframe")
  }
  return(args)
}

checkCategorical <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$categorical) && !is.list(args$categorical)) {
    message("'categorical' must be a list")
    args["categorical"] <- list(NULL)
  }
  if (!is.null(args$categorical)) {
    args$categorical <- as.list(args$categorical)
    if (colCount != length(args$categorical)) {
      message("Length of 'categorical' must be equal to the number of columns of 'data'")
      args["categorical"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$categorical))) {
        if (!is.null(args$categorical[[i]])) {
          if (is.vector(args$categorical[[i]])) {
            args$categorical[[i]] <- as.list(args$categorical[[i]])
          }
          else {
            message(paste("categorical", i, "must be a vector"))
            args[["categorical"]][i] <- list(NULL)
          }
        }
      }
    }
  }
  else {
    # Try some automatic generations
    categorical <- lapply(seq_len(ncol(args$data)), function(icol) {
      if (is.factor(args$data[, icol])) {
        categories <- as.list(levels(args$data[, icol]))
        if (length(categories) < 8) {
          return(categories)
        }
      }
      return(NULL)
    })
    args["categorical"] <- list(categorical)
  }
  return(args)
}

checkKeptColumns <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$keptColumns) && !is.vector(args$keptColumns)) {
    message("'keptColumns' must be a vector")
    args["keptColumns"] <- list(NULL)
  }
  if (!is.null(args$keptColumns)) {
    if (colCount != length(args$keptColumns)) {
      message("Length of 'keptColumns' must be equal to the number of columns of 'data'")
      args["keptColumns"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$keptColumns))) {
        if (!is.logical(args$keptColumns[[i]])) {
          message(paste("keptColumns", i, "must be of logical type"))
          args[["keptColumns"]][i] <- TRUE
        }
      }
    }
  }
  return(args)
}

checkInputColumns <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$inputColumns) && !is.vector(args$inputColumns)) {
    message("'inputColumns' must be a vector")
    args["inputColumns"] <- list(NULL)
  }
  if (!is.null(args$inputColumns)) {
    if (colCount != length(args$inputColumns)) {
      message("Length of 'inputColumns' must be equal to the number of columns of 'data'")
      args["inputColumns"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$inputColumns))) {
        if (!is.logical(args$inputColumns[[i]])) {
          message(paste("inputColumns", i, "must be of logical type"))
          args[["inputColumns"]][i] <- TRUE
        }
      }
    }
  }
  return(args)
}

checkHistoVisibility <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$histoVisibility) && !is.vector(args$histoVisibility)) {
    message("'histoVisibility' must be a vector")
    args["histoVisibility"] <- list(NULL)
  }
  if (!is.null(args$histoVisibility)) {
    if (colCount != length(args$histoVisibility)) {
      message("Length of 'histoVisibility' must be equal to the number of columns of 'data'")
      args["histoVisibility"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$histoVisibility))) {
        if (!is.logical(args$histoVisibility[[i]])) {
          message(paste("histoVisibility", i, "must be of logical type"))
          args[["histoVisibility"]][i] <- FALSE
        }
      }
    }
  }
  return(args)
}

checkCutoffs <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$cutoffs) && !is.list(args$cutoffs)) {
    message("'cutoffs' must be a list")
    args["cutoffs"] <- list(NULL)
  }
  if (!is.null(args$cutoffs)) {
    if (colCount != length(args$cutoffs)) {
      message("Length of 'cutoffs' must be equal to the number of columns of 'data'")
      args["cutoffs"] <- list(NULL)
    }
    else {
      # need to know what is the type of columns (continuous or categorical) => call 'checkCategorical'
      args <- checkCategorical(args)
      for (i in seq_len(length(args$cutoffs))) {
        ## if cutoffs are provided for current column
        if (!is.null(args$cutoffs[[i]])) {
          if (is.vector(args$cutoffs[[i]])) {
            args$cutoffs[[i]] <- as.list(args$cutoffs[[i]])
            # if column type is continuous
            if (is.null(args$categorical[[i]])) {
              for (co in args$cutoffs[[i]]) {
                if (is.vector(co)) {
                  co <- as.list(co)
                  if (!is.numeric(unlist(co))) {
                    message(paste("cutoffs", i, "contains a no-numeric interval:", toString(co)))
                    args[["cutoffs"]][i] <- list(NULL)
                    break;
                  }
                  if (length(co) != 2) {
                    message(paste("cutoffs", i, "contains an interval not defined by two values:", toString(co)))
                    args[["cutoffs"]][i] <- list(NULL)
                    break;
                  }
                }
                else {
                  message(paste("cutoffs", i, "contains an interval not defined by a vector:", toString(co)))
                  args[["cutoffs"]][i] <- list(NULL)
                  break;
                }
              }
            }
            else {
              # check if 'categorical' and 'cutoffs' use same category names
              categories <- args$categorical[[i]]
              colCutoffs <- args$cutoffs[[i]]
              cutDiff <- setdiff(colCutoffs, categories)
              if (length(cutDiff) != 0) {
                message(paste("cutoffs", i, "references unknown categories:", toString(cutDiff)))
              }
            }
          }
          else {
            message(paste("cutoffs", i, "must be a list"))
            args[["cutoffs"]][i] <- list(NULL)
          }
        }
      }
    }
  }
  return(args)
}

checkRefRowIndex <- function(args) {
  rowCount <- nrow(args$data)
  if (!is.null(args$refRowIndex) && !is.numeric(args$refRowIndex)) {
    message("'refRowIndex' must be of integer type")
    args["refRowIndex"] <- list(NULL)
  }
  if (is.numeric(args$refRowIndex) && (args$refRowIndex < 1 || args$refRowIndex > rowCount)) {
    message(paste("refRowIndex:", args$refRowIndex, "must be a valid row index, it must be in range:", paste0("[1, ", rowCount, "]")))
    args["refRowIndex"] <- list(NULL)
  }
  return(args)
}
  
checkRefColumnDim <- function(args) {
  colNames <- colnames(args$data)
  if (!is.null(args$refColumnDim) && is.na(match(args$refColumnDim, colNames))) {
    message(paste("refColumnDim:", args$refColumnDim, "must be a valid column dimension, it must be one of:", toString(colNames)))
    args["refColumnDim"] <- list(NULL)
  }
  return(args)
}
  
checkRotateTitle <- function(args) {
  if (!is.logical(args$rotateTitle)) {
    message("'rotateTitle' must be of logical type")
    args["rotateTitle"] <- FALSE
  }
  return(args)
}

checkColumnLabels <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$columnLabels) && !is.vector(args$columnLabels)) {
    message("'columnLabels' must be a vector")
    args["columnLabels"] <- list(NULL)
  }
  if (!is.null(args$columnLabels)) {
    if (colCount != length(args$columnLabels)) {
      message("Length of 'columnLabels' must be equal to the number of columns of 'data'")
      args["columnLabels"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$columnLabels))) {
        if (!is.character(args$columnLabels[[i]])) {
          message(paste("columnLabels", i, "must be of character type"))
          args[["columnLabels"]][i] <- list(NULL)
        }
      }
    }
  }
  return(args)
}
  
checkContinuousCS <- function(args) {
  continuousCSList = c("Blues", "RdBu", "YlGnBu", "YlOrRd", "Reds")
  if (is.na(match(args$continuousCS, continuousCSList))) {
    message(paste("continuousCS:", args$continuousCS, "must be a valid continuous color scale name, it must be one of:", toString(continuousCSList)))
    args$continuousCS <- continuousCSList[1]
  }
  return(args)
}

checkCategoricalCS <- function(args) {
  categoricalCSList = c("Category10", "Accent", "Dark2", "Paired", "Set1")
  if (is.na(match(args$categoricalCS, categoricalCSList))) {
    message(paste("categoricalCS:", args$categoricalCS, "must be a valid categorical color scale name, it must be one of:", toString(categoricalCSList)))
    args["categoricalCS"] <- categoricalCSList[1]
  }
  return(args)
}
  
checkEventInputId <- function(args) {
  if (!is.null(args$eventInputId) && !is.character(args$eventInputId)) {
    message("'eventInputId' must be of character type")
    args["eventInputId"] <- list(NULL)
  }
  return(args)
}

#' Shiny bindings for parallelPlot
#'
#' Output and render functions for using parallelPlot within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a parallelPlot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @return An output or render function that enables the use of the widget within Shiny applications.
#'
#' @name parallelPlot-shiny
#'
#' @export
parallelPlotOutput <- function(outputId, width = '100%', height = '600px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'parallelPlot', width, height, package = 'parallelPlot')
}

#' @rdname parallelPlot-shiny
#' @export
renderParallelPlot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, parallelPlotOutput, env, quoted = TRUE)
}

#' Traces colors
#'
#' Tells which color scale to use when reference column is of type continuous.
#'
#' If a column is defined as the reference (for example by clicking on its header), a color scale is associated to this column.
#' Available color scale ids are: `Blues`, `RdBu`, `YlGnBu`, `YlOrRd`, `Reds`.
#'
#' @param id output variable to read from (id which references the requested plot)
#' @param continuousCsId one of the available color scale ids
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(parallelPlot)
#'
#'    ui <- fluidPage(
#'        selectInput("continuousCsSelect", "Continuous Color Scale:", 
#'            choices = list("Blues" = "Blues", "RdBu" = "RdBu", "YlGnBu" = "YlGnBu", 
#'                            "YlOrRd" = "YlOrRd", "Reds" = "Reds"), selected = "Blues"),
#'        p("The selector controls the colors used when reference column is of type continuous"),
#'        parallelPlotOutput("parPlot")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$parPlot <- renderParallelPlot({
#'            parallelPlot(iris, refColumnDim = "Sepal.Length")
#'        })
#'        observeEvent(input$continuousCsSelect, {
#'            parallelPlot::setContinuousColorScale("parPlot", input$continuousCsSelect)
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setContinuousColorScale <- function(id, continuousCsId) {
  method <- "setContinuousColorScale"
  callJS()
}

#' Traces colors
#'
#' Tells which color scale to use when reference column is of type categorical.
#'
#' If a column is defined as the reference (for example by clicking on its header), a color scale is associated to this column.
#' Available color scale ids are: `Category10`, `Accent`, `Dark2`, `Paired`, `Set1`.
#'
#' @param id output variable to read from (id which references the requested plot)
#' @param categoricalCsId one of the available color scale ids
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(parallelPlot)
#'
#'    ui <- fluidPage(
#'        selectInput("categoricalCsSelect", "Categorical Color Scale:", 
#'            choices = list("Category10" = "Category10", "Accent" = "Accent", "Dark2" = "Dark2", 
#'                            "Paired" = "Paired", "Set1" = "Set1"), selected = "Category10"),
#'        p("The selector controls the colors used when reference column is of type categorical"),
#'        parallelPlotOutput("parPlot")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$parPlot <- renderParallelPlot({
#'            parallelPlot(data = iris, refColumnDim = "Species")
#'        })
#'        observeEvent(input$categoricalCsSelect, {
#'            parallelPlot::setCategoricalColorScale("parPlot", input$categoricalCsSelect)
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCategoricalColorScale <- function(id, categoricalCsId) {
  method <- "setCategoricalColorScale"
  callJS()
}

#' Histograms visibility
#'
#' Tells which columns have to be displayed with histograms.
#'
#' @param id output variable to read from (id which references the requested plot)
#' @param histoVisibility Vector of boolean (one for each data column), \code{TRUE} if an histogram must be displayed; \code{NULL} is allowed, meaning no histogram must be displayed. A named list can also be provided to only indicate which columns must be assigned to a new display.
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(parallelPlot)
#'
#'    ui <- fluidPage(
#'        checkboxInput("histCB", "Histogram Visibility", FALSE),
#'        p("The check box controls the visibility of histograms"),
#'        parallelPlotOutput("parPlot")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$parPlot <- renderParallelPlot({
#'            parallelPlot(iris)
#'        })
#'        observeEvent(input$histCB, {
#'            histoVisibility <- rep(input$histCB, ncol(iris))
#'            parallelPlot::setHistoVisibility("parPlot", histoVisibility)
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setHistoVisibility <- function(id, histoVisibility) {
  method <- "setHistoVisibility"
  callJS()
}

#' Cutoffs values
#'
#' Tells which cutoffs to use for each column.
#'
#' It's possible to filter some traces by defining cutoffs to apply to columns.
#'
#' @param id output variable to read from (id which references the requested plot)
#' @param cutoffs Vector of list (one for each data column) of vector (one for each cutoff) containing two values for continuous input (min and max value defining the cutoff), or one value for categorical input (name of the category to keep), or \code{NULL} if there is no cutoff to apply; \code{NULL} is allowed, meaning all columns are without cutoff. A named list can also be provided to only indicate which columns must be assigned to a new cutoff.
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(parallelPlot)
#'
#'    ui <- fluidPage(
#'        sliderInput("brushSlider", "Brush for 'Sepal.Length' column:", 
#'            min = 4, max = 8, step = 0.1, value = c(4, 8)),
#'        p("The slider controls the rows which are kept by cutoff (others are greyed)"),
#'        parallelPlotOutput("parPlot")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$parPlot <- renderParallelPlot({
#'            parallelPlot(iris)
#'        })
#'        observeEvent(input$brushSlider, {
#'            cutoffs <- list()
#'            cutoffs["Sepal.Length"] <- list(list(input$brushSlider))
#'            parallelPlot::setCutoffs("parPlot", cutoffs)
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCutoffs <- function(id, cutoffs) {
  method <- "setCutoffs"
  callJS()
}

#' Column visibitity
#'
#' Tells which columns have to be visible.
#'
#' @param id output variable to read from (id which references the requested plot)
#' @param keptColumns Vector of boolean (one for each data column), \code{FALSE} if column has to be hidden. A named list can also be provided to only indicate which columns must be assigned to a new visibility.
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(parallelPlot)
#'
#'    ui <- fluidPage(
#'        checkboxInput("hideColumnsCB", "Hide last columns", FALSE),
#'        p("The check box controls the visibility of the two last columns"),
#'        parallelPlotOutput("parPlot")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$parPlot <- renderParallelPlot({
#'            parallelPlot(mtcars)
#'        })
#'        observeEvent(input$hideColumnsCB, {
#'            keptColumns <- sapply(1:ncol(mtcars), function(i) {
#'                return(ifelse(input$hideColumnsCB, ncol(mtcars) - i >= 2, TRUE))
#'            })
#'            parallelPlot::setKeptColumns("parPlot", keptColumns)
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setKeptColumns <- function(id, keptColumns) {
  method <- "setKeptColumns"
  callJS()
}

#' Plot attributes
#'
#' Asks to retrieve the value of an attribute. 
#'
#' Available attributes are 'Cutoffs', 'SelectedTraces' and 'ReferenceColumn'.
#' Result will be sent through a reactive input.
#'
#' @param id output variable to read from (id which references the requested plot)
#' @param attrType which value is requested. 
#' @param valueInputId reactive input to write to. 
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(parallelPlot)
#'
#'    ui <- fluidPage(
#'        actionButton("getSelectedTracesAction", "Retrieve Selected Traces"),
#'        p("The button displays the list of uncutted rows (use brush to reduce it)"),
#'        parallelPlotOutput("parPlot")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$parPlot <- renderParallelPlot({
#'            parallelPlot(iris)
#'        })
#'        observeEvent(input$getSelectedTracesAction, {
#'            attributeType <- "SelectedTraces"
#'            parallelPlot::getValue("parPlot", attributeType, "MySelectedTraces")
#'        })
#'        observeEvent(input$MySelectedTraces, {
#'            showModal(modalDialog(
#'                title = "Selected Traces",
#'                toString(input$MySelectedTraces)
#'            ))
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
getValue <- function(id, attrType, valueInputId) {
  method <- "getValue"
  callJS()
}

#' Row edition
#'
#' Asks to change a row. 
#'
#' @param id output variable to read from (id which references the requested plot)
#' @param rowIndex index of the changed row.
#' @param newValues list of new values to attribute to the row (list associating a value to a column identifier). 
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(parallelPlot)
#'
#'    ui <- fluidPage(
#'        sliderInput("rowValueSlider", "Value for 'Sepal.Length' of first row:", 
#'            min = 4, max = 8, step = 0.1, value = iris[["Sepal.Length"]][1]),
#'        p("The slider controls the new value to assign to the 'Sepal.Length' of the first row"),
#'        parallelPlotOutput("parPlot")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$parPlot <- renderParallelPlot({
#'            parallelPlot(iris)
#'        })
#'        observeEvent(input$rowValueSlider, {
#'            newValues <- iris[1,]
#'            newValues[["Sepal.Length"]] <- input$rowValueSlider
#'            parallelPlot::changeRow("parPlot", 1, newValues)
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
changeRow <- function(id, rowIndex, newValues) {
  method <- "changeRow"
  callJS()
}

callJS <- function() {
  message <- Filter(function(x) !is.symbol(x), as.list(parent.frame(1)))
  session <- shiny::getDefaultReactiveDomain()
  method <- paste0("parallelPlot:", message$method)
  session$sendCustomMessage(method, message)
}