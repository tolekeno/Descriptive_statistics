#' Group Comparisons tab module
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param numeric_data A reactive returning its numeric columns.
#' @param cat_vars A reactive returning the names of categorical columns.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_groups_ui()` returns a `tabItem`. `mod_groups_server()` is
#'   called for its side effects.
#' @name mod_groups
NULL

#' @rdname mod_groups
#' @export
mod_groups_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "groups",
    fluidRow(
      box(title = "Group Comparison Setup", width = 12, status = "primary",
          solidHeader = TRUE,
          column(3, uiOutput(ns("gcYUI"))),
          column(3, uiOutput(ns("gcGroupUI"))),
          column(3, selectInput(ns("gcTest"), "Test:",
                                choices = group_test_choices(), selected = "auto")),
          column(3, selectInput(ns("gcPlot"), "Plot Type:",
                                choices = c("Boxplot" = "box",
                                            "Violin + Boxplot" = "violinbox",
                                            "Strip + Mean +/- SE" = "meanse"),
                                selected = "violinbox"))
      )
    ),
    fluidRow(
      box(title = "Test Result", width = 6, status = "info", solidHeader = TRUE,
          verbatimTextOutput(ns("gcTestOut"))),
      box(title = "Group Summary", width = 6, status = "info", solidHeader = TRUE,
          DT::DTOutput(ns("gcSummary")))
    ),
    fluidRow(
      box(title = "Pairwise / Post-hoc", width = 12, status = "warning",
          solidHeader = TRUE, DT::DTOutput(ns("gcPostHoc")))
    ),
    fluidRow(
      box(title = "Comparison Plot", width = 12, status = "primary",
          solidHeader = TRUE,
          plotOutput(ns("gcPlot"), height = 520),
          br(),
          downloadButton(ns("downloadGC"), "Download Comparison Plot"))
    )
  )
}

#' @rdname mod_groups
#' @export
mod_groups_server <- function(id, working_data, numeric_data, cat_vars, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$gcYUI <- renderUI({
      req(numeric_data())
      selectInput(ns("gcY"), "Numeric response (Y):", choices = names(numeric_data()))
    })

    output$gcGroupUI <- renderUI({
      req(cat_vars())
      choices <- setdiff(cat_vars(), input$gcY %||% "")
      if (!length(choices)) {
        return(helpText("No categorical grouping column is available."))
      }
      selectInput(ns("gcGroup"), "Grouping variable:", choices = choices)
    })

    gc_data <- reactive({
      req(input$gcY, input$gcGroup, working_data())
      df <- working_data()
      d <- data.frame(y = df[[input$gcY]], g = as.factor(df[[input$gcGroup]]))
      d <- d[!is.na(d$y) & !is.na(d$g), , drop = FALSE]
      d$g <- droplevels(d$g)
      validate(need(nrow(d) > 0, "No complete observations for this combination."))
      d
    })

    gc_test_obj <- reactive({
      group_test_report(gc_data(), input$gcTest %||% "auto")
    })

    output$gcTestOut <- renderPrint({
      cat(paste(gc_test_obj()$text, collapse = "\n"))
    })

    output$gcSummary <- DT::renderDT({
      DT::datatable(group_summary(gc_data()), rownames = FALSE,
                    options = list(pageLength = 10))
    })

    output$gcPostHoc <- DT::renderDT({
      ph <- gc_test_obj()$posthoc
      if (is.null(ph)) {
        return(DT::datatable(data.frame(Note = "No pairwise table for this test."),
                             rownames = FALSE))
      }
      DT::datatable(ph, rownames = FALSE, options = list(pageLength = 15))
    })

    gc_plot_obj <- reactive({
      d <- gc_data()
      colors <- expand_palette(settings$colors(), nlevels(d$g))

      p <- ggplot(d, aes(x = .data$g, y = .data$y, fill = .data$g))
      p <- switch(
        input$gcPlot %||% "violinbox",
        "box" = p + geom_boxplot(alpha = 0.8, outlier.size = 1.5, linewidth = 0.6),
        "violinbox" = p +
          geom_violin(alpha = 0.65, trim = FALSE, linewidth = 0.5) +
          geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA,
                       linewidth = 0.4),
        "meanse" = p +
          geom_jitter(aes(color = .data$g), width = 0.15, alpha = 0.4, size = 1.5) +
          stat_summary(fun = mean, geom = "point", shape = 21, size = 4,
                       color = "black", fill = "white") +
          stat_summary(fun.data = mean_se, geom = "errorbar",
                       width = 0.15, linewidth = 0.8, color = "black") +
          scale_color_manual(values = colors, guide = "none")
      )

      p +
        scale_fill_manual(values = colors, guide = "none") +
        labs(x = input$gcGroup, y = input$gcY,
             title = paste(input$gcY, "by", input$gcGroup)) +
        settings$gg_theme()
    })

    output$gcPlot <- renderPlot({ gc_plot_obj() }, res = 110)

    output$downloadGC <- downloadHandler(
      filename = function() {
        paste0("group_", safe_filename(input$gcY), "_by_",
               safe_filename(input$gcGroup), "_", Sys.Date(), ".",
               settings$export()$format)
      },
      content = function(file) settings$save(gc_plot_obj(), file)
    )
  })
}
