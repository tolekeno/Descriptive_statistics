#' Regression tab module
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param numeric_data A reactive returning its numeric columns.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_regression_ui()` returns a `tabItem`. `mod_regression_server()`
#'   is called for its side effects.
#' @name mod_regression
NULL

#' @rdname mod_regression
#' @export
mod_regression_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "regression",
    fluidRow(
      box(title = "Model Specification", width = 12, status = "primary",
          solidHeader = TRUE,
          column(3, uiOutput(ns("regYUI"))),
          column(4, uiOutput(ns("regXUI"))),
          column(3, checkboxInput(ns("regStd"), "Standardise predictors", FALSE)),
          column(2, actionButton(ns("fitReg"), "Fit Model", icon = icon("play"),
                                 class = "btn-success", style = "margin-top:25px;"))
      )
    ),
    fluidRow(
      box(title = "Coefficients", width = 7, status = "info", solidHeader = TRUE,
          DT::DTOutput(ns("regCoefs")),
          br(),
          downloadButton(ns("downloadRegCoefs"), "Download Coefficients CSV")),
      box(title = "Fit Statistics", width = 5, status = "info", solidHeader = TRUE,
          verbatimTextOutput(ns("regFitStats")))
    ),
    fluidRow(
      box(title = "Diagnostic Plots", width = 12, status = "warning",
          solidHeader = TRUE,
          plotOutput(ns("regDiag"), height = 620),
          br(),
          downloadButton(ns("downloadRegDiag"), "Download Diagnostics"))
    )
  )
}

#' @rdname mod_regression
#' @export
mod_regression_server <- function(id, working_data, numeric_data, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$regYUI <- renderUI({
      req(numeric_data())
      selectInput(ns("regY"), "Response (Y):", choices = names(numeric_data()))
    })

    output$regXUI <- renderUI({
      req(working_data())
      selectInput(ns("regX"), "Predictors (X):",
                  choices = setdiff(names(working_data()), input$regY %||% ""),
                  multiple = TRUE)
    })

    reg_fit <- eventReactive(input$fitReg, {
      req(input$regY, input$regX)
      fit_regression(working_data(), input$regY, input$regX,
                     standardise = isTRUE(input$regStd))
    })

    output$regCoefs <- DT::renderDT({
      rf <- reg_fit()
      if (!is.null(rf$error)) {
        return(DT::datatable(data.frame(Message = rf$error), rownames = FALSE))
      }
      tb <- round_numeric(tidy_regression(rf, conf.int = TRUE), 4)
      tb$Sig <- sig_stars(tb$p.value)
      DT::formatStyle(
        DT::datatable(tb, rownames = FALSE,
                      options = list(pageLength = 15, scrollX = TRUE)),
        "p.value",
        backgroundColor = DT::styleInterval(0.05, c("#d5f5e3", "#fadbd8"))
      )
    })

    output$regFitStats <- renderPrint({
      rf <- reg_fit()
      if (!is.null(rf$error)) {
        cat(rf$error)
        return()
      }
      cat(paste(regression_fit_stats(rf), collapse = "\n"))
    })

    reg_diag_plot <- reactive({
      rf <- reg_fit()
      if (!is.null(rf$error)) return(NULL)
      regression_diagnostics(rf, settings$colors(), settings$gg_theme())
    })

    output$regDiag <- renderPlot({
      p <- reg_diag_plot()
      validate(need(!is.null(p), "Fit a model to see diagnostic plots."))
      print(p)
    }, res = 110)

    output$downloadRegCoefs <- downloadHandler(
      filename = function() paste0("regression_coefs_", Sys.Date(), ".csv"),
      content  = function(file) {
        rf <- reg_fit()
        if (!is.null(rf$error)) return()
        utils::write.csv(tidy_regression(rf, conf.int = TRUE), file, row.names = FALSE)
      }
    )

    output$downloadRegDiag <- downloadHandler(
      filename = function() {
        paste0("regression_diag_", Sys.Date(), ".", settings$export()$format)
      },
      content = function(file) {
        p <- reg_diag_plot()
        if (is.null(p)) return()
        settings$save(p, file)
      }
    )
  })
}
