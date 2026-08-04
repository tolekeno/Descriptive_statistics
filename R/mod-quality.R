#' Data Quality tab module
#'
#' Missingness, IQR-based outlier counts and a Holm-adjusted normality
#' assessment.
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param numeric_data A reactive returning its numeric columns.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_quality_ui()` returns a `tabItem`. `mod_quality_server()` is
#'   called for its side effects.
#' @name mod_quality
NULL

#' @rdname mod_quality
#' @export
mod_quality_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "quality",
    fluidRow(
      box(title = "Missing Values Summary", width = 6, status = "warning",
          solidHeader = TRUE, DT::DTOutput(ns("missingTable"))),
      box(title = "Outlier Detection (IQR)", width = 6, status = "danger",
          solidHeader = TRUE, DT::DTOutput(ns("outlierTable")))
    ),
    fluidRow(
      box(title = "Normality Assessment", width = 12, status = "info",
          solidHeader = TRUE,
          helpText(paste("Shapiro-Wilk p-values are adjusted across variables using",
                         "Holm's method. A non-significant result does not prove",
                         "normality.")),
          DT::DTOutput(ns("normalityTable")))
    ),
    fluidRow(
      box(title = "Missing Values by Variable", width = 12, status = "warning",
          solidHeader = TRUE, plotOutput(ns("missingBarPlot"), height = 420))
    )
  )
}

#' @rdname mod_quality
#' @export
mod_quality_server <- function(id, working_data, numeric_data, settings) {
  moduleServer(id, function(input, output, session) {

    output$missingTable <- DT::renderDT({
      req(working_data())
      DT::formatStyle(
        DT::datatable(missing_summary(working_data()),
                      options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE),
        "Missing_Percent",
        backgroundColor = DT::styleInterval(
          c(5, 10, 20), c("lightgreen", "yellow", "orange", "red")
        )
      )
    })

    output$outlierTable <- DT::renderDT({
      req(numeric_data())
      validate(need(ncol(numeric_data()) > 0, "No numeric variables are available."))
      DT::formatStyle(
        DT::datatable(outlier_summary(numeric_data()),
                      options = list(pageLength = 10), rownames = FALSE),
        "Outlier_Percent",
        backgroundColor = DT::styleInterval(
          c(1, 5, 10), c("lightgreen", "yellow", "orange", "red")
        )
      )
    })

    output$normalityTable <- DT::renderDT({
      req(numeric_data())
      validate(need(ncol(numeric_data()) > 0, "No numeric variables are available."))
      dt <- DT::datatable(normality_table(numeric_data()),
                          options = list(pageLength = 10), rownames = FALSE)
      dt <- DT::formatStyle(
        dt, "Interpretation",
        backgroundColor = DT::styleEqual(
          c("No evidence against normality",
            "Evidence against normality",
            "Not tested (requires 3-5000 non-constant values)"),
          c("lightgreen", "lightcoral", "lightyellow")
        )
      )
      DT::formatStyle(dt, "Shapiro_p_Holm",
                      backgroundColor = DT::styleInterval(
                        0.05, c("lightcoral", "lightgreen")
                      ))
    })

    output$missingBarPlot <- renderPlot({
      req(working_data())
      md <- missing_summary(working_data())
      md <- md[md$Missing_Percent > 0, , drop = FALSE]

      if (nrow(md) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No missing values.",
                     size = 8, color = "darkgreen", fontface = "bold") +
            theme_void()
        )
      }

      ggplot(md, aes(x = stats::reorder(.data$Variable, .data$Missing_Percent),
                     y = .data$Missing_Percent, fill = .data$Missing_Percent)) +
        geom_col(alpha = 0.85, width = 0.7) +
        geom_text(aes(label = sprintf("%.1f%%", .data$Missing_Percent)),
                  hjust = -0.1, size = settings$font_size() / 3.2, fontface = "bold") +
        coord_flip() +
        scale_fill_gradient(low = settings$colors()[3], high = settings$colors()[1],
                            name = "Missing (%)") +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
        labs(x = NULL, y = "Missing (%)", title = "Missing data by variable") +
        settings$gg_theme()
    })
  })
}
