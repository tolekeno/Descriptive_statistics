#' Data Upload tab module
#'
#' Explains the supported formats and previews the raw, unprocessed upload.
#'
#' @param id Module id.
#' @param raw_data A reactive returning the uploaded data frame.
#' @return `mod_upload_ui()` returns a `tabItem`. `mod_upload_server()` is
#'   called for its side effects.
#' @name mod_upload
NULL

#' @rdname mod_upload
#' @export
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "upload",
    tags$section(
      class = "ds-intro",
      h1("Explore your data"),
      p("Upload a dataset from the sidebar to preview it here.",
        "Use Data Prep to filter rows or transform variables before analysis.")
    ),
    fluidRow(
      box(title = "Supported file formats", width = 12, status = "info", solidHeader = TRUE,
          uiOutput(ns("formatHelp")))
    ),
    fluidRow(
      box(title = "Data preview", width = 12, status = "primary",
          solidHeader = TRUE, DT::DTOutput(ns("dataPreviewTable")))
    )
  )
}

#' @rdname mod_upload
#' @export
mod_upload_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {

    output$formatHelp <- renderUI({
      excel_ok <- has_pkg("readxl")
      HTML(paste0(
        "<p style='font-size:14px;'>Supports <b>CSV, TSV, semicolon-delimited</b>",
        if (excel_ok) " and <b>Excel (.xlsx / .xls)</b>" else "",
        " files.</p>",
        if (!excel_ok) {
          paste0("<p style='color:#a94442;font-size:13px;'><i>Install the ",
                 "<code>readxl</code> package to enable Excel support.</i></p>")
        } else {
          ""
        }
      ))
    })

    output$dataPreviewTable <- DT::renderDT({
      req(raw_data())
      DT::datatable(
        raw_data(),
        options  = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"),
        rownames = TRUE, class = "cell-border stripe"
      )
    })
  })
}
