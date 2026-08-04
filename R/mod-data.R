#' Data upload module
#'
#' Sidebar controls for choosing a file, an Excel sheet and a delimiter.
#'
#' @param id Module id.
#' @return `mod_data_ui()` returns a UI tag list. `mod_data_server()` returns a
#'   reactive holding the uploaded data frame.
#' @name mod_data
NULL

#' @rdname mod_data
#' @export
mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload Data File",
              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"),
              buttonLabel = "Browse...", placeholder = "No file selected"),
    uiOutput(ns("sheetPickerUI")),
    selectInput(ns("delim"), "CSV / TSV Delimiter:",
                choices = delimiter_choices(), selected = "auto")
  )
}

#' @rdname mod_data
#' @export
mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$sheetPickerUI <- renderUI({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (!ext %in% c("xlsx", "xls") || !has_pkg("readxl")) return(NULL)
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath),
                         error = function(e) NULL)
      if (is.null(sheets)) return(NULL)
      selectInput(session$ns("xlsxSheet"), "Excel Sheet:",
                  choices = sheets, selected = sheets[1])
    })

    raw_data <- reactive({
      req(input$file)
      tryCatch(
        read_tabular(
          path      = input$file$datapath,
          file_name = input$file$name,
          delim     = input$delim %||% "auto",
          sheet     = input$xlsxSheet %||% 1
        ),
        error = function(e) {
          showNotification(paste("Error reading file:", conditionMessage(e)),
                           type = "error")
          NULL
        }
      )
    })

    raw_data
  })
}
