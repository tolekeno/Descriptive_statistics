#' Launch the dashboard
#'
#' @param max_upload_mb Maximum upload size in megabytes.
#' @param launch.browser Open the app in the default browser?
#' @param ... Further arguments passed to [shiny::shinyApp()].
#' @return A Shiny app object; when called interactively the app is run.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(max_upload_mb = 100, launch.browser = TRUE, ...) {
  old <- options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  on.exit(options(old), add = TRUE)

  shinyApp(
    ui      = app_ui(),
    server  = app_server,
    options = list(launch.browser = launch.browser),
    ...
  )
}
