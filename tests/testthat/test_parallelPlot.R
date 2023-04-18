describe("options", {
    # use mtcars dataset
    data(mtcars, iris)

    # ***************
    # 'data' argument
    # ***************
    describe("data", {
        it("should accept a dataframe", {
            data <- data.frame()
            expect_equal(
                parallelPlot(data)$x$data,
                data
            )
        })

        it("should accept a matrix", {
            data <- matrix()
            expect_equal(
                parallelPlot(data)$x$data,
                data
            )
        })

        it("should refuse invalid value and throw an error", {
            expect_error(
                parallelPlot(data = list()),
                "'data' must be a dataframe"
            )
        })
    })

    # **********************
    # 'categorical' argument
    # **********************
    describe("categorical", {
        it("should keep unchanged a valid value", {
            categorical <- list(
                NULL,
                list(4, 6, 8),
                NULL, NULL, NULL, NULL, NULL,
                list(0, 1),
                list(0, 1),
                list(3:5),
                list(1:8)
            )
            expect_equal(
                parallelPlot(mtcars, categorical = categorical)$x$categorical,
                categorical
            )
        })
        it("should fix invalid type and print a message", {
            categorical <- 1
            expect_message(
                out <- parallelPlot(mtcars, categorical = categorical),
                "'categorical' must be a list"
            )
            expect_equal(
                out$x$categorical,
                rep(list(NULL), ncol(mtcars))
            )
        })
        it("should fix invalid elements and print a message", {
            categorical <- list(
                NULL,
                data.frame(),
                NULL, NULL, NULL, NULL, NULL,
                list(0, 1),
                list(0, 1),
                list(3:5),
                list(1:8)
            )
            fixed <- categorical
            fixed[2] <- list(NULL)
            expect_message(
                out <- parallelPlot(mtcars, categorical = categorical),
                "categorical 2 must be a vector"
            )
            expect_equal(
                out$x$categorical,
                fixed
            )
        })
        it("should accept vector type for elements (by coercing to list)", {
            categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
            fixed <- vapply(
                categorical,
                function(cat) ifelse(is.null(cat), list(NULL), list(as.list(cat))),
                list(1)
            )
            expect_equal(
                parallelPlot(mtcars, categorical = categorical)$x$categorical,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            categorical <- list()
            expect_message(
                out <- parallelPlot(mtcars, categorical = categorical),
                "Length of 'categorical' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$categorical,
                NULL
            )
        })
        it("should initialize using factor elements", {
            expect_equal(
                parallelPlot(iris)$x$categorical,
                list(NULL, NULL, NULL, NULL, list("setosa", "versicolor", "virginica"))
            )
        })
    })

    # ***********************
    # 'categoriesRep' argument
    # ***********************
    describe("categoriesRep", {
        categoriesRepList <- c("EquallySpacedLines", "EquallySizedBoxes")
        for (cr in categoriesRepList) {
            it(paste("should keep unchanged a valid value", cr), {
                expect_equal(
                    parallelPlot(mtcars, categoriesRep = cr)$x$categoriesRep,
                    cr
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- parallelPlot(mtcars, categoriesRep = 1),
                paste("must be a valid categories representation type, it must be one of:", toString(categoriesRepList))
            )
            expect_equal(
                out$x$categoriesRep,
                categoriesRepList[1]
            )
        })
    })

    # ***********************
    # 'arrangeMethod' argument
    # ***********************
    describe("arrangeMethod", {
        arrangeMethodList <- c("fromLeft", "fromRight", "fromBoth", "fromNone")
        for (cr in arrangeMethodList) {
            it(paste("should keep unchanged a valid value", cr), {
                expect_equal(
                    parallelPlot(mtcars, arrangeMethod = cr)$x$arrangeMethod,
                    cr
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- parallelPlot(mtcars, arrangeMethod = 1),
                paste("must be a valid arrange method, it must be one of:", toString(arrangeMethodList))
            )
            expect_equal(
                out$x$arrangeMethod,
                arrangeMethodList[1]
            )
        })
    })

    # **********************
    # 'keptColumns' argument
    # **********************
    describe("keptColumns", {
        it("should keep unchanged a valid value", {
            keptColumns <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            expect_equal(
                parallelPlot(mtcars, keptColumns = keptColumns)$x$keptColumns,
                keptColumns
            )
        })
        it("should fix invalid type and print a message", {
            keptColumns <- data.frame()
            expect_message(
                out <- parallelPlot(mtcars, keptColumns = keptColumns),
                "'keptColumns' must be a vector"
            )
            expect_equal(
                out$x$keptColumns,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            keptColumns <- list(TRUE, "false", TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            fixed <- keptColumns
            fixed[2] <- TRUE
            expect_message(
                out <- parallelPlot(mtcars, keptColumns = keptColumns),
                "keptColumns 2 must be of logical type"
            )
            expect_equal(
                out$x$keptColumns,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            keptColumns <- list()
            expect_message(
                out <- parallelPlot(mtcars, keptColumns = keptColumns),
                "Length of 'keptColumns' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$keptColumns,
                NULL
            )
        })
    })

    # ***********************
    # 'inputColumns' argument
    # ***********************
    describe("inputColumns", {
        it("should keep unchanged a valid value", {
            inputColumns <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            expect_equal(
                parallelPlot(mtcars, inputColumns = inputColumns)$x$inputColumns,
                inputColumns
            )
        })
        it("should fix invalid type and print a message", {
            inputColumns <- data.frame()
            expect_message(
                out <- parallelPlot(mtcars, inputColumns = inputColumns),
                "'inputColumns' must be a vector"
            )
            expect_equal(
                out$x$inputColumns,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            inputColumns <- list(TRUE, "false", TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            fixed <- inputColumns
            fixed[2] <- TRUE
            expect_message(
                out <- parallelPlot(mtcars, inputColumns = inputColumns),
                "inputColumns 2 must be of logical type"
            )
            expect_equal(
                out$x$inputColumns,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            inputColumns <- list()
            expect_message(
                out <- parallelPlot(mtcars, inputColumns = inputColumns),
                "Length of 'inputColumns' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$inputColumns,
                NULL
            )
        })
    })

    # **************************
    # 'histoVisibility' argument
    # **************************
    describe("histoVisibility", {
        it("should keep unchanged a valid value", {
            histoVisibility <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            expect_equal(
                parallelPlot(mtcars, histoVisibility = histoVisibility)$x$histoVisibility,
                histoVisibility
            )
        })
        it("should fix invalid type and print a message", {
            histoVisibility <- data.frame()
            expect_message(
                out <- parallelPlot(mtcars, histoVisibility = histoVisibility),
                "'histoVisibility' must be a vector"
            )
            expect_equal(
                out$x$histoVisibility,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            histoVisibility <- list(FALSE, "true", FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
            fixed <- histoVisibility
            fixed[[2]] <- FALSE

            expect_message(
                out <- parallelPlot(mtcars, histoVisibility = histoVisibility),
                "histoVisibility 2 must be of logical type"
            )
            expect_equal(
                out$x$histoVisibility,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            histoVisibility <- list()
            expect_message(
                out <- parallelPlot(mtcars, histoVisibility = histoVisibility),
                "Length of 'histoVisibility' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$histoVisibility,
                NULL
            )
        })
    })

    # ***********************
    # 'invertedAxes' argument
    # ***********************
    describe("invertedAxes", {
        it("should keep unchanged a valid value", {
            invertedAxes <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            expect_equal(
                parallelPlot(mtcars, invertedAxes = invertedAxes)$x$invertedAxes,
                invertedAxes
            )
        })
        it("should fix invalid type and print a message", {
            invertedAxes <- data.frame()
            expect_message(
                out <- parallelPlot(mtcars, invertedAxes = invertedAxes),
                "'invertedAxes' must be a vector"
            )
            expect_equal(
                out$x$invertedAxes,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            invertedAxes <- list(FALSE, "true", FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
            fixed <- invertedAxes
            fixed[[2]] <- FALSE

            expect_message(
                out <- parallelPlot(mtcars, invertedAxes = invertedAxes),
                "invertedAxes 2 must be of logical type"
            )
            expect_equal(
                out$x$invertedAxes,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            invertedAxes <- list()
            expect_message(
                out <- parallelPlot(mtcars, invertedAxes = invertedAxes),
                "Length of 'invertedAxes' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$invertedAxes,
                NULL
            )
        })
    })

    # **************************
    # 'cutoffs' argument
    # **************************
    describe("cutoffs", {
        it("should keep unchanged a valid value", {
            cutoffs <- list(list(list(6, 7)), NULL, NULL, NULL, list("virginica", "setosa"))
            expect_equal(
                parallelPlot(iris, cutoffs = cutoffs)$x$cutoffs,
                cutoffs
            )
        })

        it("should fix invalid type and print a message", {
            cutoffs <- 1
            expect_message(
                out <- parallelPlot(mtcars, cutoffs = cutoffs),
                "'cutoffs' must be a list"
            )
            expect_equal(
                out$x$cutoffs,
                NULL
            )
        })

        it("should fix invalid length and print a message", {
            cutoffs <- list()
            expect_message(
                out <- parallelPlot(mtcars, cutoffs = cutoffs),
                "Length of 'cutoffs' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$cutoffs,
                NULL
            )
        })

        it("should fix invalid continuous type and print a message", {
            cutoffs <- list(NULL, list(NULL, NULL), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
            fixed <- cutoffs
            fixed[2] <- list(NULL)
            expect_message(
                out <- parallelPlot(mtcars, cutoffs = cutoffs),
                "cutoffs 2 contains an interval not defined by a vector"
            )
            expect_equal(
                out$x$cutoffs,
                fixed
            )
        })

        it("should fix no-numerical intervals and print a message", {
            cutoffs <- list(list(list("6", "7")), NULL, NULL, NULL, list("virginica", "setosa"))
            fixed <- cutoffs
            fixed[1] <- list(NULL)
            expect_message(
                out <- parallelPlot(iris, cutoffs = cutoffs),
                "cutoffs 1 contains a no-numeric interval"
            )
            expect_equal(
                out$x$cutoffs,
                fixed
            )
        })

        it("should fix intervals not defined by two values and print a message", {
            cutoffs <- list(list(list(6, 7, 8)), NULL, NULL, NULL, list("virginica", "setosa"))
            fixed <- cutoffs
            fixed[1] <- list(NULL)
            expect_message(
                out <- parallelPlot(iris, cutoffs = cutoffs),
                "cutoffs 1 contains an interval not defined by two values"
            )
            expect_equal(
                out$x$cutoffs,
                fixed
            )
        })

        it("should fix invalid categorical type and print a message", {
            cutoffs <- list(list(c(6, 7)), NULL, NULL, NULL, data.frame())
            fixed <- cutoffs
            fixed[5] <- list(NULL)
            expect_message(
                out <- parallelPlot(iris, cutoffs = cutoffs),
                "cutoffs 5 must be a list"
            )
            expect_equal(
                out$x$cutoffs,
                fixed
            )
        })

        it("should warn for unknown categories", {
            cutoffs <- list(list(list(6, 7)), NULL, NULL, NULL, list("virginica", "setosaaaa"))
            expect_message(
                out <- parallelPlot(iris, cutoffs = cutoffs),
                "cutoffs 5 references unknown categories"
            )
            expect_equal(
                out$x$cutoffs,
                cutoffs
            )
        })
    })

    # ***********************
    # 'refRowIndex' argument
    # ***********************
    describe("refRowIndex", {
        it("should keep unchanged a valid value", {
            refRowIndex <- 1
            expect_equal(
                parallelPlot(iris, refRowIndex = refRowIndex)$x$refRowIndex,
                refRowIndex
            )
        })
        it("should fix invalid type and print a message", {
            refRowIndex <- data.frame()
            expect_message(
                out <- parallelPlot(iris, refRowIndex = refRowIndex),
                "'refRowIndex' must be of integer type"
            )
            expect_equal(
                out$x$refRowIndex,
                NULL
            )
        })
        it("should fix invalid value and print a message", {
            refRowIndex <- 0
            expect_message(
                out <- parallelPlot(iris, refRowIndex = refRowIndex),
                "must be a valid row index, it must be in range"
            )
            expect_equal(
                out$x$refRowIndex,
                NULL
            )
        })
    })

    # ***********************
    # 'refColumnDim' argument
    # ***********************
    describe("refColumnDim", {
        it("should keep unchanged a valid value", {
            refColumnDim <- "cyl"
            expect_equal(
                parallelPlot(mtcars, refColumnDim = refColumnDim)$x$refColumnDim,
                refColumnDim
            )
        })
        it("should fix invalid value and print a message", {
            expect_message(
                out <- parallelPlot(mtcars, refColumnDim = 1),
                paste("must be a valid column dimension, it must be one of:", toString(colnames(mtcars)))
            )
            expect_equal(
                out$x$refColumnDim,
                NULL
            )
        })
    })

    # **********************
    # 'rotateTitle' argument
    # **********************
    describe("rotateTitle", {
        it("should keep unchanged a valid value TRUE", {
            expect_equal(
                parallelPlot(data.frame(), rotateTitle = TRUE)$x$rotateTitle,
                TRUE
            )
        })

        it("should keep unchanged a valid value FALSE", {
            expect_equal(
                parallelPlot(data.frame(), rotateTitle = FALSE)$x$rotateTitle,
                FALSE
            )
        })

        it("should fix invalid value and print a message", {
            expect_message(
                out <- parallelPlot(data.frame(), rotateTitle = 1),
                "'rotateTitle' must be of logical type"
            )
            expect_equal(
                out$x$rotateTitle,
                FALSE
            )
        })
    })

    # ***********************
    # 'columnLabels' argument
    # ***********************
    describe("columnLabels", {
        it("should keep unchanged a valid value", {
            columnLabels <- gsub("\\.", "<br>", colnames(iris))
            expect_equal(
                parallelPlot(iris, columnLabels = columnLabels)$x$columnLabels,
                columnLabels
            )
        })
        it("should fix invalid type and print a message", {
            columnLabels <- data.frame()
            expect_message(
                out <- parallelPlot(mtcars, columnLabels = columnLabels),
                "'columnLabels' must be a vector"
            )
            expect_equal(
                out$x$columnLabels,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            columnLabels <- list(
                "Col 1", FALSE, "Col 3", "Col 4", "Col 5", "Col 6",
                "Col 7", "Col 8", "Col 9", "Col 10", "Col 11"
            )
            fixed <- columnLabels
            fixed[2] <- list(NULL)
            expect_message(
                out <- parallelPlot(mtcars, columnLabels = columnLabels),
                "columnLabels 2 must be of character type"
            )
            expect_equal(
                out$x$columnLabels,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            columnLabels <- list()
            expect_message(
                out <- parallelPlot(mtcars, columnLabels = columnLabels),
                "Length of 'columnLabels' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$columnLabels,
                NULL
            )
        })
    })

    # ***********************
    # 'continuousCS' argument
    # ***********************
    describe("continuousCS", {
        continuousCSList <- c(
            "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault",
            "Blues", "Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
            "PuBuGn", "PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"
        )
        for (cs in continuousCSList) {
            it(paste("should keep unchanged a valid value", cs), {
                expect_equal(
                    parallelPlot(mtcars, continuousCS = cs)$x$continuousCS,
                    cs
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- parallelPlot(mtcars, continuousCS = 1),
                paste("must be a valid continuous color scale name, it must be one of:", toString(continuousCSList))
            )
            expect_equal(
                out$x$continuousCS,
                continuousCSList[1]
            )
        })
    })

    # ************************
    # 'categoricalCS' argument
    # ************************
    describe("categoricalCS", {
        categoricalCSList <- c("Category10", "Accent", "Dark2", "Paired", "Set1")
        for (cs in categoricalCSList) {
            it(paste("should keep unchanged a valid value", cs), {
                expect_equal(
                    parallelPlot(mtcars, categoricalCS = cs)$x$categoricalCS,
                    cs
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- parallelPlot(mtcars, categoricalCS = 1),
                paste("must be a valid categorical color scale name, it must be one of:", toString(categoricalCSList))
            )
            expect_equal(
                out$x$categoricalCS,
                categoricalCSList[1]
            )
        })
    })

    # ************************
    # 'eventInputId' argument
    # ************************
    describe("eventInputId", {
        it("should keep unchanged a valid value", {
            eventInputId <- "testingInputId"
            expect_equal(
                parallelPlot(mtcars, eventInputId = eventInputId)$x$eventInputId,
                eventInputId
            )
        })
        it("should fix invalid value and print a message", {
            expect_message(
                out <- parallelPlot(mtcars, eventInputId = 1),
                "'eventInputId' must be of character type"
            )
            expect_equal(
                out$x$eventInputId,
                NULL
            )
        })
    })

})
