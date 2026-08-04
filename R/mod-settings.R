#' Plot styling and export settings module
#'
#' Owns every control that affects how figures look and how they are exported,
#' plus the shared numeric-variable selector. The server half returns these as
#' reactives so each analysis module can consume them without reaching into
#' another module's inputs.
#'
#' @param id Module id.
#' @param numeric_data A reactive returning the numeric columns of the working
#'   data, used to populate the variable selector.
#' @return `mod_settings_ui()` returns a UI tag list. `mod_settings_server()`
#'   returns a list of reactives: `variables`, `colors`, `gg_theme`,
#'   `font_size`, `export`, plus a `save` function that writes a ggplot to a
#'   file using the current export settings.
#' @name mod_settings
NULL

#' @rdname mod_settings
#' @export
mod_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Plot Styling",
       style = "padding-left:15px;font-weight:bold;color:#3c8dbc;"),
    uiOutput(ns("varSelectSidebar")),
    selectInput(ns("colorPalette"), "Color Palette:",
                choices = names(publication_palettes()), selected = "Nature"),
    selectInput(ns("plotTheme"), "Plot Theme:",
                choices = plot_theme_choices(), selected = "minimal"),
    sliderInput(ns("baseFontSize"), "Base Font Size:",
                min = 8, max = 22, value = 13, step = 1),
    sliderInput(ns("plotDPI"), "Export DPI:",
                min = 150, max = 600, value = 300, step = 50),
    checkboxInput(ns("useGridLines"), "Show Grid Lines", TRUE),
    checkboxInput(ns("useBoldText"), "Bold Titles", TRUE),
    hr(),
    h5("Export Dimensions",
       style = "padding-left:15px;color:#3c8dbc;font-weight:bold;"),
    fluidRow(
      column(6, numericInput(ns("exportWidth"), "W (in)",
                             value = 9, min = 3, max = 20, step = 0.5)),
      column(6, numericInput(ns("exportHeight"), "H (in)",
                             value = 6, min = 3, max = 16, step = 0.5))
    ),
    selectInput(ns("exportFormat"), "Export Format:",
                choices = export_format_choices(), selected = "png")
  )
}

#' @rdname mod_settings
#' @export
mod_settings_server <- function(id, numeric_data) {
  moduleServer(id, function(input, output, session) {

    output$varSelectSidebar <- renderUI({
      req(numeric_data())
      nd <- numeric_data()
      if (ncol(nd) == 0) return(helpText("No numeric variables found"))
      previous <- isolate(input$variables)
      selected <- intersect(previous %||% character(), names(nd))
      if (!length(selected)) selected <- names(nd)[seq_len(min(3, ncol(nd)))]
      selectInput(session$ns("variables"), "Numeric Variables:",
                  choices = names(nd), multiple = TRUE, selected = selected)
    })

    colors <- reactive(get_palette(input$colorPalette))

    gg_theme <- reactive({
      build_plot_theme(
        theme_name  = input$plotTheme %||% "minimal",
        base_size   = input$baseFontSize %||% 13,
        grid_lines  = isTRUE(input$useGridLines),
        bold_titles = isTRUE(input$useBoldText)
      )
    })

    export <- reactive({
      list(
        format = input$exportFormat %||% "png",
        width  = input$exportWidth  %||% 9,
        height = input$exportHeight %||% 6,
        dpi    = input$plotDPI      %||% 300
      )
    })

    list(
      variables  = reactive(input$variables),
      palette_name = reactive(input$colorPalette %||% "Nature"),
      colors     = colors,
      gg_theme   = gg_theme,
      font_size  = reactive(input$baseFontSize %||% 13),
      export     = export,
      save       = function(plot, file) save_plot(plot, file, export())
    )
  })
}
