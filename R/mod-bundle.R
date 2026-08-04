#' Analysis bundle module
#'
#' The sidebar download button. Writes several rectangular CSV files into a zip
#' archive rather than stacking incompatible tables into one malformed CSV.
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param descriptives The list returned by [mod_descriptives_server()].
#' @return `mod_bundle_ui()` returns a download button.
#'   `mod_bundle_server()` is called for its side effects.
#' @name mod_bundle
NULL

#' @rdname mod_bundle
#' @export
mod_bundle_ui <- function(id) {
  ns <- NS(id)
  downloadButton(ns("downloadReport"), "Download Analysis Bundle (.zip)",
                 class = "btn-primary btn-block")
}

#' @rdname mod_bundle
#' @export
mod_bundle_server <- function(id, working_data, descriptives) {
  moduleServer(id, function(input, output, session) {

    output$downloadReport <- downloadHandler(
      filename = function() paste0("analysis_bundle_", Sys.Date(), ".zip"),
      content  = function(file) {
        df <- working_data()
        req(df)

        bundle_dir <- tempfile("analysis_bundle_")
        dir.create(bundle_dir)
        on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)

        utils::write.csv(df, file.path(bundle_dir, "processed_data.csv"),
                         row.names = FALSE)
        utils::write.csv(descriptives$summary(),
                         file.path(bundle_dir, "descriptive_statistics.csv"),
                         row.names = FALSE)

        # Extended statistics only exist once variables have been selected.
        ext <- tryCatch(descriptives$extended(), error = function(e) NULL)
        if (!is.null(ext)) {
          utils::write.csv(ext, file.path(bundle_dir, "extended_statistics.csv"),
                           row.names = FALSE)
        }

        utils::write.csv(missing_summary(df),
                         file.path(bundle_dir, "missing_values.csv"),
                         row.names = FALSE)

        writeLines(
          c(
            "Publication Statistics Dashboard analysis bundle",
            paste("Created:", Sys.time()),
            paste("Rows in processed data:", nrow(df)),
            paste("Columns in processed data:", ncol(df)),
            "",
            paste("Statistical results should be interpreted alongside study",
                  "design and model assumptions.")
          ),
          file.path(bundle_dir, "README.txt")
        )

        zip::zipr(zipfile = file,
                  files = list.files(bundle_dir, full.names = TRUE))
      }
    )
  })
}
