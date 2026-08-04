#' Data Prep tab module
#'
#' Column selection, row filtering and numeric transformations. This module
#' owns the pipeline from the raw upload to the processed dataset that every
#' other tab analyses.
#'
#' Prep state is held in a `reactiveValues` store rather than read straight
#' from the inputs, so that a selection survives the re-render of its own
#' `uiOutput` and is reset cleanly when a new file is uploaded.
#'
#' @param id Module id.
#' @param raw_data A reactive returning the uploaded data frame.
#' @return `mod_prep_ui()` returns a `tabItem`. `mod_prep_server()` returns a
#'   reactive holding the processed data frame.
#' @name mod_prep
NULL

#' @rdname mod_prep
#' @export
mod_prep_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "prep",
    fluidRow(
      box(title = "Column Selection", width = 6, status = "primary",
          solidHeader = TRUE,
          uiOutput(ns("colKeepUI")),
          helpText("Deselect columns to drop them from all downstream analysis.")),
      box(title = "Row Filter", width = 6, status = "primary", solidHeader = TRUE,
          uiOutput(ns("filterVarUI")),
          uiOutput(ns("filterValuesUI")),
          checkboxInput(ns("filterKeepNA"),
                        "Retain rows with missing filter values", FALSE),
          helpText("Filter rows by the values of a chosen column."))
    ),
    fluidRow(
      box(title = "Transform Numeric Variables", width = 12, status = "info",
          solidHeader = TRUE,
          fluidRow(
            column(4, uiOutput(ns("transformVarUI"))),
            column(4, selectInput(ns("transformType"), "Transformation:",
                                  choices = transform_choices(), selected = "none")),
            column(4, actionButton(ns("applyTransform"), "Apply Transformation",
                                   icon = icon("bolt"), class = "btn-warning",
                                   style = "margin-top:25px;"))
          ),
          helpText("Transformed variables are appended as new columns (e.g. log_yield).",
                   "Log and Box-Cox require positive values; square root allows zero.",
                   "Reset using the button below."))
    ),
    fluidRow(
      box(title = "Processed Data", width = 12, status = "success",
          solidHeader = TRUE,
          fluidRow(
            column(3, verbatimTextOutput(ns("prepSummary"))),
            column(3, actionButton(ns("resetPrep"), "Reset All Prep",
                                   icon = icon("undo"), class = "btn-default",
                                   style = "margin-top:25px;")),
            column(3, downloadButton(ns("downloadProcessed"), "Download Processed CSV",
                                     class = "btn-success", style = "margin-top:25px;")),
            column(3)
          ),
          br(),
          DT::DTOutput(ns("processedPreview")))
    )
  )
}

#' @rdname mod_prep
#' @export
mod_prep_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prep_state <- reactiveValues(
      keep_cols   = NULL,
      filter_var  = "None",
      filter_vals = NULL,
      added_cols  = list()
    )

    reset_state <- function(cols) {
      prep_state$keep_cols   <- cols
      prep_state$filter_var  <- "None"
      prep_state$filter_vals <- NULL
      prep_state$added_cols  <- list()
    }

    # Raw data plus any transformation columns the user has added.
    augmented_data <- reactive({
      df <- raw_data()
      req(df)
      for (nm in names(prep_state$added_cols)) df[[nm]] <- prep_state$added_cols[[nm]]
      df
    })

    observeEvent(raw_data(), {
      reset_state(names(raw_data()))
    }, ignoreNULL = TRUE)

    observeEvent(input$resetPrep, {
      req(raw_data())
      reset_state(names(raw_data()))
      updateSelectInput(session, "filterVar", selected = "None")
      updateSelectInput(session, "transformVar", selected = character(0))
      updateSelectInput(session, "transformType", selected = "none")
      updateCheckboxInput(session, "filterKeepNA", value = FALSE)
      showNotification("Data prep reset.", type = "message")
    })

    # ---- Column selection ----------------------------------------------------
    output$colKeepUI <- renderUI({
      req(augmented_data())
      choices <- names(augmented_data())
      checkboxGroupInput(ns("keepCols"), "Keep columns:",
                         choices = choices,
                         selected = intersect(prep_state$keep_cols %||% choices, choices),
                         inline = TRUE)
    })

    observeEvent(input$keepCols, {
      req(raw_data())
      if (!length(input$keepCols)) {
        showNotification("Keep at least one column in the processed dataset.",
                         type = "warning")
        updateCheckboxGroupInput(session, "keepCols", selected = prep_state$keep_cols)
        return()
      }
      prep_state$keep_cols <- input$keepCols
    }, ignoreNULL = FALSE)

    # ---- Row filter ----------------------------------------------------------
    output$filterVarUI <- renderUI({
      req(augmented_data())
      selectInput(ns("filterVar"), "Filter variable:",
                  choices = c("None", names(augmented_data())),
                  selected = prep_state$filter_var)
    })

    output$filterValuesUI <- renderUI({
      req(augmented_data(), input$filterVar)
      if (identical(input$filterVar, "None")) return(NULL)
      x <- augmented_data()[[input$filterVar]]

      if (is.numeric(x)) {
        finite_x <- x[is.finite(x)]
        if (!length(finite_x)) {
          return(helpText("This variable has no finite values to filter."))
        }
        rng <- range(finite_x)
        span <- diff(rng)
        slider_limits <- if (span == 0) rng + c(-0.5, 0.5) else rng
        stored <- prep_state$filter_vals
        stored_ok <- !is.null(stored) && length(stored) == 2 && is.numeric(stored) &&
          all(is.finite(stored)) &&
          stored[1] >= slider_limits[1] && stored[2] <= slider_limits[2]
        sliderInput(ns("filterRange"), "Keep rows where value is within:",
                    min = slider_limits[1], max = slider_limits[2],
                    value = if (stored_ok) stored else rng,
                    step = max(diff(slider_limits) / 100, .Machine$double.eps^0.5))
      } else {
        vals <- sort(unique(as.character(x[!is.na(x)])))
        selectInput(ns("filterLevels"), "Keep rows where value is one of:",
                    choices = vals,
                    selected = intersect(prep_state$filter_vals %||% vals, vals),
                    multiple = TRUE)
      }
    })

    observeEvent(input$filterVar, {
      if (!identical(prep_state$filter_var, input$filterVar)) {
        prep_state$filter_vals <- NULL
      }
      prep_state$filter_var <- input$filterVar
    }, ignoreNULL = FALSE)

    observeEvent(input$filterRange, {
      req(input$filterVar, !identical(input$filterVar, "None"))
      if (is.numeric(augmented_data()[[input$filterVar]])) {
        prep_state$filter_vals <- input$filterRange
      }
    })

    observeEvent(input$filterLevels, {
      req(input$filterVar, !identical(input$filterVar, "None"))
      if (!is.numeric(augmented_data()[[input$filterVar]])) {
        prep_state$filter_vals <- input$filterLevels
      }
    })

    # ---- Transformations -----------------------------------------------------
    output$transformVarUI <- renderUI({
      req(augmented_data())
      selectInput(ns("transformVar"), "Variable:",
                  choices = names(numeric_columns(augmented_data())))
    })

    observeEvent(input$applyTransform, {
      req(augmented_data(), input$transformVar, input$transformType)
      result <- apply_transform(augmented_data()[[input$transformVar]],
                                input$transformType)

      if (!is.null(result$message)) {
        showNotification(result$message,
                         type = if (result$ok) "message" else "warning")
      }
      if (!result$ok) return()

      new_name <- paste0(input$transformType, "_", input$transformVar)
      prep_state$added_cols[[new_name]] <- result$values
      if (!(new_name %in% prep_state$keep_cols)) {
        prep_state$keep_cols <- c(prep_state$keep_cols, new_name)
      }
      showNotification(paste("Added column:", new_name), type = "message")
    })

    # ---- Processed dataset ---------------------------------------------------
    working_data <- reactive({
      df <- augmented_data()
      req(df)

      fv <- prep_state$filter_var
      if (!is.null(fv) && !identical(fv, "None") && fv %in% names(df)) {
        x <- df[[fv]]
        keep_rows <- if (is.numeric(x) && length(prep_state$filter_vals) == 2) {
          !is.na(x) & x >= prep_state$filter_vals[1] & x <= prep_state$filter_vals[2]
        } else if (!is.null(prep_state$filter_vals)) {
          !is.na(x) & as.character(x) %in% prep_state$filter_vals
        } else {
          rep(TRUE, nrow(df))
        }
        if (isTRUE(input$filterKeepNA)) keep_rows <- keep_rows | is.na(x)
        df <- df[keep_rows, , drop = FALSE]
      }

      if (!is.null(prep_state$keep_cols)) {
        df <- df[, intersect(prep_state$keep_cols, names(df)), drop = FALSE]
      }
      df
    })

    output$prepSummary <- renderPrint({
      req(working_data())
      cat("Rows:", nrow(working_data()), "\n")
      cat("Cols:", ncol(working_data()), "\n")
      cat("Added:", length(prep_state$added_cols), "\n")
    })

    output$processedPreview <- DT::renderDT({
      req(working_data())
      DT::datatable(
        utils::head(working_data(), 200),
        options = list(pageLength = 10, scrollX = TRUE, scrollY = "350px"),
        rownames = FALSE, class = "cell-border stripe"
      )
    })

    output$downloadProcessed <- downloadHandler(
      filename = function() paste0("processed_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(working_data(), file, row.names = FALSE)
    )

    working_data
  })
}
