#' Dashboard CSS
#'
#' @return A `tags$head` element.
#' @keywords internal
app_css <- function() {
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = paste0("https://fonts.googleapis.com/css2?",
                    "family=Instrument+Sans:wght@400;500;600;700&display=swap")
    ),
    tags$style(HTML("
      :root {
        --ds-navy: #172b42;
        --ds-navy-light: #243e57;
        --ds-canvas: #eef3f6;
        --ds-line: #d6e0e8;
        --ds-ink: #172e43;
        --ds-muted: #4c6173;
        --ds-teal: #08756c;
      }

      body, h1, h2, h3, h4, .main-header .logo {
        font-family: 'Instrument Sans', 'Segoe UI', sans-serif;
      }
      body { color: var(--ds-ink); font-size: 14px; }
      .skin-blue .main-header .logo,
      .skin-blue .main-header .logo:hover,
      .skin-blue .main-header .navbar {
        background: var(--ds-navy);
      }
      .skin-blue .main-header .logo {
        text-align: left;
        padding-left: 22px;
        font-size: 19px;
        font-weight: 700;
      }
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background: var(--ds-navy-light);
      }
      .skin-blue .main-sidebar { background: var(--ds-navy); }
      .skin-blue .sidebar a { color: #e3edf5; }
      .skin-blue .sidebar-menu > li > a {
        margin: 2px 12px;
        padding: 8px 12px;
        border-left: 3px solid transparent;
        border-radius: 4px;
        font-weight: 500;
      }
      .skin-blue .sidebar-menu > li:hover > a,
      .skin-blue .sidebar-menu > li.active > a {
        background: var(--ds-navy-light);
        border-left-color: #79e0ca;
        color: #fff;
      }
      .sidebar-menu > li > a > .fa { margin-right: 6px; }
      .sidebar > .shiny-input-container,
      .sidebar > .shiny-html-output,
      .sidebar > .shiny-bound-output,
      .sidebar > .download-link {
        margin-left: 16px;
        width: calc(100% - 32px);
      }
      .sidebar .control-label, .sidebar .checkbox label { color: #edf5fb; }
      .sidebar .help-block { color: #bed0df; }
      .sidebar hr { border-color: #3b536a; margin: 16px; }
      .ds-sidebar-heading {
        color: #bce9e3;
        font-weight: 700;
        margin: 16px 16px 10px;
      }
      .sidebar .ds-export-dimensions { margin-inline: 1px; }

      .content-wrapper, .right-side { background: var(--ds-canvas); }
      .content { padding: 24px; }
      .box {
        border: 1px solid var(--ds-line);
        border-radius: 8px;
        box-shadow: 0 2px 3px rgba(23, 46, 67, .04);
      }
      .box-header,
      .box.box-solid[class*='box-'] > .box-header {
        padding: 14px 16px;
        border-bottom: 1px solid var(--ds-line);
        border-radius: 7px 7px 0 0;
        background: #f8fafc;
        color: var(--ds-ink);
      }
      .box.box-solid[class*='box-'] { border-color: var(--ds-line); }
      .box.box-solid[class*='box-'] > .box-header .btn,
      .box.box-solid[class*='box-'] > .box-header a { color: var(--ds-ink); }
      .box-header .box-title { font-size: 16px; font-weight: 700; line-height: 1.3; }
      .box-body { padding: 16px; }
      .control-label { color: var(--ds-ink); font-weight: 600; }
      .help-block { color: var(--ds-muted); }
      .form-control, .selectize-input, .input-group-addon { border-color: #bacbd8; }
      .form-control:focus, .selectize-input.focus { border-color: var(--ds-teal); }
      .btn { border-radius: 5px; font-weight: 600; }
      .btn-primary, .btn-success {
        background: var(--ds-teal);
        border-color: var(--ds-teal);
        color: #fff;
      }
      .btn-primary:hover, .btn-primary:focus,
      .btn-success:hover, .btn-success:focus {
        background: #075e58;
        border-color: #075e58;
        color: #fff;
      }
      a:focus-visible, button:focus-visible, input:focus-visible,
      select:focus-visible, [tabindex]:focus-visible {
        outline: 2px solid #168c9c;
        outline-offset: 2px;
      }
      .sidebar a:focus-visible, .main-header a:focus-visible {
        outline-color: #79e0ca;
        outline-offset: -2px;
      }
      .shiny-notification { width: min(360px, calc(100vw - 32px)); }
      pre {
        background: #f5f8fa;
        color: var(--ds-ink);
        border-color: var(--ds-line);
        font-size: 12px;
      }

      .ds-intro {
        background: #dfeeed;
        border: 1px solid #bfd8d6;
        border-left: 5px solid var(--ds-teal);
        border-radius: 5px;
        margin-bottom: 20px;
        padding: 24px clamp(18px, 4vw, 40px);
      }
      .ds-intro h1 {
        margin: 0 0 8px;
        font-size: clamp(24px, 3vw, 36px);
        font-weight: 700;
        letter-spacing: -.035em;
      }
      .ds-intro p { margin: 0; max-width: 64ch; color: var(--ds-muted); }
      @media (max-width: 767px) {
        .content { padding: 14px; }
        .box-body { padding: 12px; }
        .ds-intro { padding: 18px; }
      }
    "))
  )
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

    dashboardHeader(title = "Publication Statistics Dashboard", titleWidth = 320),

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
