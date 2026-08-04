#' Correlations tab module
#'
#' Three linked views over the same variable selection: a significance-marked
#' correlation matrix, a scatter plot matrix, and an interactive pairwise
#' scatter.
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param numeric_data A reactive returning its numeric columns.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_correlations_ui()` returns a `tabItem`.
#'   `mod_correlations_server()` is called for its side effects.
#' @name mod_correlations
NULL

#' @rdname mod_correlations
#' @export
mod_correlations_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "correlations",
    fluidRow(
      box(title = "Correlation Controls", width = 12, status = "primary",
          solidHeader = TRUE,
          column(3, radioButtons(ns("relationType"), "Display:",
                                 choices = c("Correlation Matrix"  = "corr",
                                             "Scatter Plot Matrix" = "scatter",
                                             "Pairwise Scatter"    = "pair"),
                                 selected = "corr", inline = FALSE)),
          column(3, selectInput(ns("corrMethod"), "Method:",
                                choices = corr_method_choices(), selected = "pearson")),
          column(3, selectInput(ns("corrOrder"), "Ordering:",
                                choices = corr_order_choices(), selected = "hclust")),
          column(3,
                 checkboxInput(ns("showSig"), "Show significance stars", TRUE),
                 checkboxInput(ns("holmAdj"), "Holm-adjust p-values", TRUE))
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = "input.relationType == 'corr'", ns = ns,
        box(title = "Correlation Matrix", width = 12, status = "info",
            solidHeader = TRUE,
            plotOutput(ns("corrPlot"), height = 620),
            br(),
            downloadButton(ns("downloadCorr"), "Download Plot"),
            downloadButton(ns("downloadCorrCSV"), "Download r + p Tables"))
      ),
      conditionalPanel(
        condition = "input.relationType == 'scatter'", ns = ns,
        box(title = "Scatter Plot Matrix", width = 12, status = "info",
            solidHeader = TRUE,
            plotOutput(ns("scatterMatrix"), height = 720),
            br(),
            downloadButton(ns("downloadScatter"), "Download Plot"))
      ),
      conditionalPanel(
        condition = "input.relationType == 'pair'", ns = ns,
        box(title = "Pairwise Scatter", width = 12, status = "info",
            solidHeader = TRUE,
            fluidRow(
              column(3, uiOutput(ns("xVarSelect"))),
              column(3, uiOutput(ns("yVarSelect"))),
              column(3, uiOutput(ns("colorVarSelect"))),
              column(3,
                     checkboxInput(ns("showRegression"), "Regression line", TRUE),
                     checkboxInput(ns("showEquation"), "Equation & R2", TRUE))
            ),
            plotly::plotlyOutput(ns("pairwisePlot"), height = 580))
      )
    )
  )
}

#' @rdname mod_correlations
#' @export
mod_correlations_server <- function(id, working_data, numeric_data, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$xVarSelect <- renderUI({
      req(numeric_data())
      selectInput(ns("xVar"), "X Variable:", choices = names(numeric_data()))
    })

    output$yVarSelect <- renderUI({
      req(numeric_data())
      nd <- numeric_data()
      selectInput(ns("yVar"), "Y Variable:", choices = names(nd),
                  selected = names(nd)[min(2, ncol(nd))])
    })

    output$colorVarSelect <- renderUI({
      req(working_data())
      selectInput(ns("colorVar"), "Color by:",
                  choices = c("None", names(working_data())), selected = "None")
    })

    corr_objs <- reactive({
      req(numeric_data())
      selected <- intersect(settings$variables() %||% character(), names(numeric_data()))
      validate(need(length(selected) >= 2,
                    "Select at least two numeric variables in the sidebar."))
      mat <- drop_unusable_for_cor(numeric_data()[, selected, drop = FALSE])
      validate(need(
        ncol(mat) >= 2,
        "At least two selected variables must have non-zero variance and three observations."
      ))
      corr_objects(mat, method = input$corrMethod, holm = isTRUE(input$holmAdj))
    })

    render_corrplot <- function() {
      draw_corrplot(
        corr_objs(),
        method    = input$corrMethod %||% "pearson",
        order     = input$corrOrder %||% "hclust",
        colors    = settings$colors(),
        font_size = settings$font_size(),
        show_sig  = isTRUE(input$showSig)
      )
    }

    output$corrPlot <- renderPlot({ render_corrplot() })

    output$downloadCorr <- downloadHandler(
      filename = function() {
        paste0("correlation_", settings$palette_name(), "_",
               settings$export()$dpi, "dpi_", Sys.Date(), ".",
               settings$export()$format)
      },
      content = function(file) {
        open_export_device(file, settings$export())
        on.exit(grDevices::dev.off(), add = TRUE)
        render_corrplot()
      }
    )

    output$downloadCorrCSV <- downloadHandler(
      filename = function() paste0("correlation_rp_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(corr_export_table(corr_objs()), file, row.names = FALSE)
      }
    )

    scatter_plot_obj <- reactive({
      req(numeric_data())
      selected <- intersect(settings$variables() %||% character(), names(numeric_data()))
      validate(need(length(selected) >= 2,
                    "Select at least two numeric variables in the sidebar."))
      scatter_matrix_plot(
        numeric_data()[, selected, drop = FALSE],
        colors    = settings$colors(),
        gg_theme  = settings$gg_theme(),
        font_size = settings$font_size()
      )
    })

    output$scatterMatrix <- renderPlot({ scatter_plot_obj() }, res = 100)

    output$downloadScatter <- downloadHandler(
      filename = function() {
        paste0("scatter_matrix_", settings$palette_name(), "_",
               settings$export()$dpi, "dpi_", Sys.Date(), ".",
               settings$export()$format)
      },
      content = function(file) settings$save(scatter_plot_obj(), file)
    )

    output$pairwisePlot <- plotly::renderPlotly({
      req(input$xVar, input$yVar, working_data())
      df <- working_data()
      pairs_ok <- sum(stats::complete.cases(df[, c(input$xVar, input$yVar)]))
      validate(need(pairs_ok >= 2,
                    "The selected variables need at least two complete pairs."))
      p <- pairwise_scatter_plot(
        df,
        x_var = input$xVar, y_var = input$yVar, color_var = input$colorVar,
        show_regression = isTRUE(input$showRegression),
        show_equation   = isTRUE(input$showEquation),
        colors    = settings$colors(),
        gg_theme  = settings$gg_theme(),
        font_size = settings$font_size()
      )
      plotly::layout(plotly::ggplotly(p, tooltip = c("x", "y")),
                     font = list(size = settings$font_size()))
    })
  })
}
