#' Distributions tab module
#'
#' One panel per selected variable, or a single faceted panel across all of
#' them. The per-variable panels and their download handlers are created
#' dynamically because the number of selected variables is not known until
#' runtime.
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param numeric_data A reactive returning its numeric columns.
#' @param cat_vars A reactive returning the names of categorical columns.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_distributions_ui()` returns a `tabItem`.
#'   `mod_distributions_server()` is called for its side effects.
#' @name mod_distributions
NULL

#' @rdname mod_distributions
#' @export
mod_distributions_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "distributions",
    fluidRow(
      box(title = "Plot Controls", width = 12, status = "primary", solidHeader = TRUE,
          column(3, radioButtons(ns("plotType"), "Plot Type:",
                                 choices = dist_plot_choices(), selected = "hist")),
          column(3,
                 checkboxInput(ns("showStats"), "Show Statistics", TRUE),
                 checkboxInput(ns("showCI"), "95% CI for the mean (density)", FALSE),
                 checkboxInput(ns("showOutliers"), "Highlight Outliers", TRUE)),
          column(3,
                 checkboxInput(ns("facetPlots"), "Facet All Variables", FALSE),
                 uiOutput(ns("groupVarUI"))),
          column(3, uiOutput(ns("facetByUI")))
      )
    ),
    fluidRow(uiOutput(ns("distributionPlotsUI")))
  )
}

#' @rdname mod_distributions
#' @export
mod_distributions_server <- function(id, working_data, numeric_data, cat_vars, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$groupVarUI <- renderUI({
      selectInput(ns("groupVar"), "Group/color by (categorical):",
                  choices = c("None", cat_vars()), selected = "None")
    })

    output$facetByUI <- renderUI({
      if (!isTRUE(input$facetPlots)) return(NULL)
      selectInput(ns("facetBy"), "Additional facet (categorical):",
                  choices = c("None", cat_vars()), selected = "None")
    })

    current_opts <- reactive({
      dist_options(
        plot_type     = input$plotType %||% "hist",
        show_stats    = isTRUE(input$showStats),
        show_ci       = isTRUE(input$showCI),
        show_outliers = isTRUE(input$showOutliers),
        group_var     = input$groupVar,
        colors        = settings$colors(),
        gg_theme      = settings$gg_theme(),
        font_size     = settings$font_size()
      )
    })

    selected_vars <- reactive({
      req(numeric_data())
      intersect(settings$variables() %||% character(), names(numeric_data()))
    })

    faceted_plot_obj <- reactive({
      req(input$facetPlots)
      vars_sel <- selected_vars()
      validate(need(length(vars_sel) > 0, "Select at least one numeric variable."))
      build_faceted_plot(working_data(), vars_sel, input$facetBy, current_opts())
    })

    output$distributionPlotsUI <- renderUI({
      req(length(selected_vars()) > 0)
      if (isTRUE(input$facetPlots)) {
        return(fluidRow(
          box(title = "Faceted Distribution Plots", width = 12,
              status = "primary", solidHeader = TRUE,
              plotOutput(ns("facetedPlot"), height = 620),
              br(),
              downloadButton(ns("downloadFaceted"), "Download Plot"))
        ))
      }
      boxes <- lapply(seq_along(selected_vars()), function(i) {
        sn <- paste0("v", i)
        box(title = selected_vars()[[i]], width = 4, status = "primary",
            solidHeader = TRUE,
            plotOutput(ns(paste0("plot_", sn)), height = 380),
            br(),
            downloadButton(ns(paste0("dl_", sn)), "Download",
                           class = "btn-sm btn-default"))
      })
      do.call(fluidRow, boxes)
    })

    output$facetedPlot <- renderPlot({ faceted_plot_obj() }, res = 110)

    output$downloadFaceted <- downloadHandler(
      filename = function() {
        paste0("faceted_", input$plotType, "_", settings$palette_name(), "_",
               Sys.Date(), ".", settings$export()$format)
      },
      content = function(file) settings$save(faceted_plot_obj(), file)
    )

    # Bind one renderPlot and one downloadHandler per selected variable.
    observe({
      req(!isTRUE(input$facetPlots))
      local_vars <- selected_vars()
      req(length(local_vars) > 0)
      lapply(seq_along(local_vars), function(i) {
        local({
          var <- local_vars[[i]]
          sn  <- paste0("v", i)
          output[[paste0("plot_", sn)]] <- renderPlot({
            build_dist_plot(working_data(), var, current_opts())
          }, res = 110)
          output[[paste0("dl_", sn)]] <- downloadHandler(
            filename = function() {
              paste0(safe_filename(var), "_", input$plotType, "_",
                     settings$palette_name(), ".", settings$export()$format)
            },
            content = function(file) {
              settings$save(build_dist_plot(working_data(), var, current_opts()), file)
            }
          )
        })
      })
    })
  })
}
