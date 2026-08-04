#' Dashboard server
#'
#' Wires the modules together. The data flow is linear:
#' `mod_data` produces the raw upload, `mod_prep` turns it into the processed
#' `working_data`, and every analysis module consumes that plus the shared
#' `settings`.
#'
#' @param input,output,session Standard Shiny server arguments.
#' @return Invisibly `NULL`; called for its side effects.
#' @export
app_server <- function(input, output, session) {

  raw_data     <- mod_data_server("data")
  working_data <- mod_prep_server("prep", raw_data)

  numeric_data <- reactive({
    req(working_data())
    numeric_columns(working_data())
  })

  cat_vars <- reactive({
    req(working_data())
    col_classes(working_data())$categorical
  })

  settings <- mod_settings_server("settings", numeric_data)

  mod_upload_server("upload", raw_data)
  mod_overview_server("overview", working_data, numeric_data, settings)

  descriptives <- mod_descriptives_server("descriptives", numeric_data, settings)

  mod_distributions_server("distributions", working_data, numeric_data,
                           cat_vars, settings)
  mod_groups_server("groups", working_data, numeric_data, cat_vars, settings)
  mod_correlations_server("correlations", working_data, numeric_data, settings)
  mod_pca_server("pca", working_data, numeric_data, cat_vars, settings)
  mod_regression_server("regression", working_data, numeric_data, settings)
  mod_quality_server("quality", working_data, numeric_data, settings)

  mod_bundle_server("bundle", working_data, descriptives)

  invisible(NULL)
}
