#' Dashboard CSS
#'
#' @return A `tags$head` element.
#' @keywords internal
app_css <- function() {
  tags$head(tags$style(HTML("
    .box-title         { font-weight:bold; font-size:15px; }
    .content-wrapper   { background-color:#f4f6f9; }
    .box               { border-radius:5px; box-shadow:0 2px 4px rgba(0,0,0,.12); }
    .btn-primary       { background-color:#3c8dbc; border-color:#3c8dbc; }
    .btn-primary:hover { background-color:#357ca5; border-color:#357ca5; }
    .shiny-notification { width:360px; }
    .control-label     { color:#2f3b45; }
    .help-block        { color:#66727c; }
    pre                { font-size:12px; }
  ")))
}

#' Dashboard user interface
#'
#' Assembles the sidebar (navigation plus the upload and styling modules) and
#' the body (one `tabItem` per analysis module).
#'
#' @return A `shinydashboard::dashboardPage()`.
#' @export
app_ui <- function() {
  dashboardPage(
    skin = "blue",

    dashboardHeader(title = "Publication Statistics Dashboard", titleWidth = 340),

    dashboardSidebar(
      width = 320,
      sidebarMenu(
        id = "tabs",
        menuItem("Data Upload",       tabName = "upload",        icon = icon("upload")),
        menuItem("Overview",          tabName = "overview",      icon = icon("dashboard")),
        menuItem("Data Prep",         tabName = "prep",          icon = icon("filter")),
        menuItem("Descriptive Stats", tabName = "statistics",    icon = icon("table")),
        menuItem("Distributions",     tabName = "distributions", icon = icon("chart-bar")),
        menuItem("Group Comparisons", tabName = "groups",        icon = icon("balance-scale")),
        menuItem("Correlations",      tabName = "correlations",  icon = icon("project-diagram")),
        menuItem("PCA",               tabName = "pca",           icon = icon("vector-square")),
        menuItem("Regression",        tabName = "regression",    icon = icon("chart-line")),
        menuItem("Data Quality",      tabName = "quality",       icon = icon("check-circle"))
      ),
      hr(),
      mod_data_ui("data"),
      hr(),
      mod_settings_ui("settings"),
      hr(),
      mod_bundle_ui("bundle")
    ),

    dashboardBody(
      app_css(),
      tabItems(
        mod_upload_ui("upload"),
        mod_overview_ui("overview"),
        mod_prep_ui("prep"),
        mod_descriptives_ui("descriptives"),
        mod_distributions_ui("distributions"),
        mod_groups_ui("groups"),
        mod_correlations_ui("correlations"),
        mod_pca_ui("pca"),
        mod_regression_ui("regression"),
        mod_quality_ui("quality")
      )
    )
  )
}
