#' Overview tab module
#'
#' Headline counts, the structure dump, and missingness / variable-type plots
#' for the processed dataset.
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param numeric_data A reactive returning its numeric columns.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_overview_ui()` returns a `tabItem`. `mod_overview_server()` is
#'   called for its side effects.
#' @name mod_overview
NULL

#' @rdname mod_overview
#' @export
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "overview",
    fluidRow(
      valueBoxOutput(ns("nRowsBox"), width = 3),
      valueBoxOutput(ns("nColsBox"), width = 3),
      valueBoxOutput(ns("numericColsBox"), width = 3),
      valueBoxOutput(ns("missingBox"), width = 3)
    ),
    fluidRow(
      box(title = "Dataset Structure", width = 6, status = "primary",
          solidHeader = TRUE, verbatimTextOutput(ns("dataStructure"))),
      box(title = "Missing Values by Variable", width = 6, status = "warning",
          solidHeader = TRUE, plotOutput(ns("missingPlot"), height = 300))
    ),
    fluidRow(
      box(title = "Variable Types", width = 12, status = "info",
          solidHeader = TRUE, plotOutput(ns("varTypesPlot"), height = 260))
    )
  )
}

#' @rdname mod_overview
#' @export
mod_overview_server <- function(id, working_data, numeric_data, settings) {
  moduleServer(id, function(input, output, session) {

    output$nRowsBox <- renderValueBox({
      req(working_data())
      valueBox(format(nrow(working_data()), big.mark = ","), "Rows",
               icon = icon("database"), color = "blue")
    })

    output$nColsBox <- renderValueBox({
      req(working_data())
      valueBox(ncol(working_data()), "Columns",
               icon = icon("columns"), color = "green")
    })

    output$numericColsBox <- renderValueBox({
      req(numeric_data())
      valueBox(ncol(numeric_data()), "Numeric Columns",
               icon = icon("hashtag"), color = "purple")
    })

    output$missingBox <- renderValueBox({
      req(working_data())
      df <- working_data()
      pct <- round(sum(is.na(df)) / prod(dim(df)) * 100, 1)
      valueBox(paste0(pct, "%"), "Missing Values",
               icon = icon("exclamation-triangle"),
               color = if (pct > 10) "red" else if (pct > 5) "yellow" else "green")
    })

    output$dataStructure <- renderPrint({
      req(working_data())
      utils::str(working_data())
    })

    output$missingPlot <- renderPlot({
      req(working_data())
      df <- working_data()
      md <- missing_summary(df)
      if (sum(md$Missing_Count) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No missing values.",
                     size = 6, color = "darkgreen", fontface = "bold") +
            theme_void()
        )
      }
      ggplot(md, aes(x = stats::reorder(.data$Variable, .data$Missing_Percent),
                     y = .data$Missing_Percent)) +
        geom_col(fill = settings$colors()[1], alpha = 0.85, width = 0.7) +
        geom_text(aes(label = sprintf("%.1f%%", .data$Missing_Percent)),
                  hjust = -0.1, size = 4, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
        labs(x = NULL, y = "Missing (%)") +
        settings$gg_theme()
    })

    output$varTypesPlot <- renderPlot({
      req(working_data())
      types <- vapply(working_data(), function(x) class(x)[1], character(1))
      vt <- as.data.frame(table(Type = types), stringsAsFactors = FALSE)
      names(vt) <- c("Type", "n")
      ggplot(vt, aes(x = stats::reorder(.data$Type, .data$n), y = .data$n,
                     fill = .data$Type)) +
        geom_col(alpha = 0.85, show.legend = FALSE, width = 0.7) +
        geom_text(aes(label = .data$n), hjust = -0.3, size = 5, fontface = "bold") +
        coord_flip() +
        scale_fill_manual(values = expand_palette(settings$colors(), nrow(vt))) +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
        labs(x = NULL, y = "Number of Variables", title = "Variable Types") +
        settings$gg_theme()
    })
  })
}
