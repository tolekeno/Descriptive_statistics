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
    fluidRow(
      box(title = "Data Upload", width = 12, status = "info", solidHeader = TRUE,
          uiOutput(ns("formatHelp")))
    ),
    fluidRow(
      box(title = "Data Preview", width = 12, status = "primary",
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
        },
        "<p style='font-size:13px;color:#555;'>Use the Data Prep tab to filter, ",
        "transform and export a cleaned dataset.</p>"
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
