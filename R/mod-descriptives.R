#' Descriptive Statistics tab module
#'
#' @param id Module id.
#' @param numeric_data A reactive returning the numeric columns of the working
#'   data.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_descriptives_ui()` returns a `tabItem`.
#'   `mod_descriptives_server()` returns a list of two reactives, `summary`
#'   and `extended`, which the analysis bundle also writes out.
#' @name mod_descriptives
NULL

#' @rdname mod_descriptives
#' @export
mod_descriptives_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "statistics",
    fluidRow(
      box(title = "Descriptive Statistics (all numeric variables)", width = 12,
          status = "primary", solidHeader = TRUE,
          DT::DTOutput(ns("summaryTable")),
          br(),
          downloadButton(ns("downloadStats"), "Download Summary CSV"))
    ),
    fluidRow(
      box(title = "Extended Statistics (selected variables)", width = 12,
          status = "info", solidHeader = TRUE,
          DT::DTOutput(ns("extendedStatsTable")),
          br(),
          downloadButton(ns("downloadExt"), "Download Extended CSV"))
    )
  )
}

#' @rdname mod_descriptives
#' @export
mod_descriptives_server <- function(id, numeric_data, settings) {
  moduleServer(id, function(input, output, session) {

    summary_stats <- reactive({
      req(numeric_data())
      validate(need(ncol(numeric_data()) > 0, "No numeric variables are available."))
      describe_numeric(numeric_data(), extended = FALSE)
    })

    extended_stats <- reactive({
      req(numeric_data())
      selected <- intersect(settings$variables() %||% character(), names(numeric_data()))
      validate(need(length(selected) > 0, "Select at least one numeric variable."))
      describe_numeric(numeric_data()[, selected, drop = FALSE], extended = TRUE)
    })

    output$summaryTable <- DT::renderDT({
      DT::datatable(summary_stats(),
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE, class = "cell-border stripe")
    })

    output$extendedStatsTable <- DT::renderDT({
      DT::formatStyle(
        DT::datatable(extended_stats(),
                      options = list(pageLength = 15, scrollX = TRUE),
                      rownames = FALSE, class = "cell-border stripe"),
        "Skewness",
        backgroundColor = DT::styleInterval(
          c(-1, -0.5, 0.5, 1),
          c("#f4cccc", "#fce5cd", "#ccffcc", "#fce5cd", "#f4cccc")
        )
      )
    })

    output$downloadStats <- downloadHandler(
      filename = function() paste0("statistics_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(summary_stats(), file, row.names = FALSE)
    )

    output$downloadExt <- downloadHandler(
      filename = function() paste0("extended_stats_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(extended_stats(), file, row.names = FALSE)
    )

    list(summary = summary_stats, extended = extended_stats)
  })
}
