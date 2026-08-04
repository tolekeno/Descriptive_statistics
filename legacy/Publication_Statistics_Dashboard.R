# =============================================================================
#  Publication-Quality Statistics Dashboard (Enhanced)
# -----------------------------------------------------------------------------
#  A full-featured EDA & reporting app for tabular data.
#
#  New in this version:
#    - CSV / TSV / semicolon / Excel upload (with sheet picker)
#    - Data Prep tab: row filter, column select, transformations, export
#    - Group Comparisons tab: t-test / ANOVA+Tukey / KW+pairwise Wilcox
#    - PCA tab with scree, biplot, loadings table, scores export
#    - Regression tab with coefficient table & 4 diagnostic plots
#    - Correlation significance matrix with stars / Holm adjustment
#    - Group colouring & faceting in distribution plots
#    - Multi-format export: PNG / PDF / SVG / TIFF at chosen DPI
#    - Several bug fixes (%||% ordering, Q-Q ggsave, corrplot dev handling, ...)
# =============================================================================

required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "tidyr",
  "viridis", "corrplot", "GGally", "moments", "scales", "plotly",
  "ggthemes", "broom", "FactoMineR", "factoextra"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    paste0(
      "Install the missing packages before running this app:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  invisible(lapply(required_packages, library, character.only = TRUE))
)

options(shiny.maxRequestSize = 100 * 1024^2)

# Optional dependencies ---------------------------------------------------------
has_readxl  <- requireNamespace("readxl",  quietly = TRUE)
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
has_gridextra <- requireNamespace("gridExtra", quietly = TRUE)

# Null-coalescing operator (define early, before any use) -----------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Publication colour palettes ---------------------------------------------------
publication_palettes <- list(
  "Nature"    = c("#E64B35","#4DBBD5","#00A087","#3C5488","#F39B7F","#8491B4","#91D1C2","#DC0000"),
  "Science"   = c("#3B4992","#EE0000","#008B45","#631879","#008280","#BB0021","#5F559B","#A20056"),
  "Lancet"    = c("#00468B","#ED0000","#42B540","#0099B4","#925E9F","#FDAF91","#AD002A","#ADB6B6"),
  "JAMA"      = c("#374E55","#DF8F44","#00A1D5","#B24745","#79AF97","#6A6599","#80796B","#B24745"),
  "NEJM"      = c("#BC3C29","#0072B5","#E18727","#20854E","#7876B1","#6F99AD","#FFDC91","#EE4C97"),
  "Economist" = c("#6794a7","#014d64","#01a2d9","#7ad2f6","#00887d","#76c0c1","#7c260b","#ee8f71"),
  "Tableau10" = c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F","#EDC948","#B07AA1","#FF9DA7"),
  "Okabe-Ito" = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000"),
  "Viridis"   = viridis(8),
  "Plasma"    = viridis(8, option = "plasma"),
  "Cividis"   = viridis(8, option = "cividis")
)

# Small helpers -----------------------------------------------------------------

# Expand a palette safely when a grouping variable has more than eight levels.
expand_palette <- function(colors, n) {
  if (n <= 0) return(character())
  if (n <= length(colors)) return(colors[seq_len(n)])
  grDevices::colorRampPalette(colors)(n)
}

# Return a simple ggplot message instead of an opaque plotting error.
message_plot <- function(text) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = text, size = 5, color = "grey30") +
    xlim(0, 1) + ylim(0, 1) + theme_void()
}

safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "variable")
}

# Delimiter detection based on the first non-empty records. This also detects
# semicolon- and pipe-delimited files when "Auto" is selected.
detect_delimiter <- function(path) {
  lines <- readLines(path, n = 10, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(",")
  candidates <- c(",", "\t", ";", "|")
  counts <- vapply(candidates, function(z) {
    mean(vapply(lines, function(x) lengths(regmatches(x, gregexpr(z, x, fixed = TRUE))), numeric(1)))
  }, numeric(1))
  if (max(counts) == 0) "," else candidates[which.max(counts)]
}

# Classify columns as numeric / categorical (treats low-cardinality ints as cat)
col_classes <- function(df, cat_unique_max = 20) {
  types <- sapply(df, function(x) {
    if (is.numeric(x)) {
      z <- unique(x[!is.na(x)])
      is_whole <- length(z) > 0 && all(abs(z - round(z)) < sqrt(.Machine$double.eps))
      low_cardinality <- length(z) >= 2 && length(z) <= cat_unique_max &&
        length(z) <= max(5, nrow(df) / 4)
      if (is_whole && low_cardinality) "categorical" else "numeric"
    } else if (is.logical(x)) "categorical"
    else if (is.factor(x) || is.character(x)) "categorical"
    else "other"
  })
  list(
    numeric     = names(types)[types == "numeric"],
    categorical = names(types)[types == "categorical"],
    all         = names(df)
  )
}

# Significance stars
sig_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*",
                              ifelse(p < 0.1,   ".", "")))))
}

# Compute cor + p-value matrix
cor_pmat <- function(mat, method = "pearson") {
  n <- ncol(mat); p <- matrix(NA_real_, n, n); r <- matrix(NA_real_, n, n)
  rownames(p) <- colnames(p) <- rownames(r) <- colnames(r) <- colnames(mat)
  diag(r) <- 1
  diag(p) <- 0
  if (n < 2) return(list(r = r, p = p))
  for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    ok <- complete.cases(mat[, c(i, j), drop = FALSE])
    if (sum(ok) < 3) next
    ct <- tryCatch(
      suppressWarnings(cor.test(mat[ok, i], mat[ok, j], method = method)),
      error = function(e) NULL
    )
    if (is.null(ct)) next
    r[i, j] <- r[j, i] <- unname(ct$estimate)
    p[i, j] <- p[j, i] <- ct$p.value
  }
  list(r = r, p = p)
}

describe_numeric <- function(df, extended = FALSE) {
  bind_rows(lapply(names(df), function(v) {
    x <- df[[v]]
    z <- x[!is.na(x)]
    n <- length(z)
    avg <- if (n) mean(z) else NA_real_
    sdev <- if (n > 1) sd(z) else NA_real_
    q <- if (n) quantile(z, c(0.25, 0.75), names = FALSE) else c(NA_real_, NA_real_)

    base <- data.frame(
      Variable = v,
      N = n,
      Missing = sum(is.na(x)),
      Mean = avg,
      Median = if (n) median(z) else NA_real_,
      SD = sdev,
      Min = if (n) min(z) else NA_real_,
      Q1 = q[1],
      Q3 = q[2],
      Max = if (n) max(z) else NA_real_,
      IQR = if (n) q[2] - q[1] else NA_real_,
      check.names = FALSE
    )

    if (!extended) return(base)

    data.frame(
      Variable = v,
      N = n,
      Missing = sum(is.na(x)),
      Mean = avg,
      Median = if (n) median(z) else NA_real_,
      SD = sdev,
      SE = if (n > 1) sdev / sqrt(n) else NA_real_,
      `CV (%)` = if (n > 1 && is.finite(avg) && abs(avg) > sqrt(.Machine$double.eps))
        100 * sdev / avg else NA_real_,
      Skewness = if (n >= 3 && length(unique(z)) > 1) moments::skewness(z) else NA_real_,
      Kurtosis = if (n >= 4 && length(unique(z)) > 1) moments::kurtosis(z) else NA_real_,
      Range = if (n) diff(range(z)) else NA_real_,
      check.names = FALSE
    )
  })) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

# =============================================================================
#  UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Publication Statistics Dashboard", titleWidth = 340),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(id = "tabs",
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
    fileInput("file", "Upload Data File",
              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"),
              buttonLabel = "Browse...", placeholder = "No file selected"),
    uiOutput("sheetPickerUI"),
    selectInput("delim", "CSV / TSV Delimiter:",
                choices = c("Auto" = "auto", "Comma" = ",", "Tab" = "\t",
                            "Semicolon" = ";", "Pipe" = "|"),
                selected = "auto"),
    hr(),
    h4("Plot Styling", style = "padding-left:15px;font-weight:bold;color:#3c8dbc;"),
    uiOutput("varSelectSidebar"),
    selectInput("colorPalette", "Color Palette:",
                choices = names(publication_palettes), selected = "Nature"),
    selectInput("plotTheme", "Plot Theme:",
                choices = c("Minimal"   = "minimal",
                            "Classic"   = "classic",
                            "Light"     = "light",
                            "Gray"      = "gray",
                            "BW"        = "bw",
                            "Economist" = "economist",
                            "Stata"     = "stata"),
                selected = "minimal"),
    sliderInput("baseFontSize", "Base Font Size:", min = 8,  max = 22, value = 13, step = 1),
    sliderInput("plotDPI",      "Export DPI:",     min = 150, max = 600, value = 300, step = 50),
    checkboxInput("useGridLines", "Show Grid Lines", TRUE),
    checkboxInput("useBoldText",  "Bold Titles",    TRUE),
    hr(),
    h5("Export Dimensions", style = "padding-left:15px;color:#3c8dbc;font-weight:bold;"),
    fluidRow(
      column(6, numericInput("exportWidth",  "W (in)", value = 9, min = 3, max = 20, step = 0.5)),
      column(6, numericInput("exportHeight", "H (in)", value = 6, min = 3, max = 16, step = 0.5))
    ),
    selectInput("exportFormat", "Export Format:",
                choices = c("PNG" = "png", "PDF" = "pdf",
                            "TIFF" = "tiff", "SVG" = "svg"),
                selected = "png"),
    hr(),
    downloadButton("downloadReport", "Download Analysis Bundle (.zip)",
                   class = "btn-primary btn-block")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .box-title       { font-weight:bold; font-size:15px; }
      .content-wrapper { background-color:#f4f6f9; }
      .box             { border-radius:5px; box-shadow:0 2px 4px rgba(0,0,0,.12); }
      .btn-primary     { background-color:#3c8dbc; border-color:#3c8dbc; }
      .btn-primary:hover { background-color:#357ca5; border-color:#357ca5; }
      .shiny-notification { width:360px; }
      .control-label { color:#2f3b45; }
      .help-block { color:#66727c; }
      pre              { font-size:12px; }
    "))),
    
    tabItems(
      
      # ── Upload ───────────────────────────────────────────────────────────────
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Data Upload", width = 12, status = "info", solidHeader = TRUE,
                    HTML(paste0(
                      "<p style='font-size:14px;'>Supports <b>CSV, TSV, semicolon-delimited</b>",
                      if (has_readxl) " and <b>Excel (.xlsx / .xls)</b>" else "",
                      " files.</p>",
                      if (!has_readxl)
                        "<p style='color:#a94442;font-size:13px;'><i>Install the <code>readxl</code> package to enable Excel support.</i></p>"
                      else "",
                      "<p style='font-size:13px;color:#555;'>Use the Data Prep tab to filter, transform and export a cleaned dataset.</p>"
                    ))
                )
              ),
              fluidRow(
                box(title = "Data Preview", width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("dataPreviewTable"))
              )
      ),
      
      # ── Overview ─────────────────────────────────────────────────────────────
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("nRowsBox",       width = 3),
                valueBoxOutput("nColsBox",       width = 3),
                valueBoxOutput("numericColsBox", width = 3),
                valueBoxOutput("missingBox",     width = 3)
              ),
              fluidRow(
                box(title = "Dataset Structure", width = 6, status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("dataStructure")),
                box(title = "Missing Values by Variable", width = 6, status = "warning", solidHeader = TRUE,
                    plotOutput("missingPlot", height = 300))
              ),
              fluidRow(
                box(title = "Variable Types", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("varTypesPlot", height = 260))
              )
      ),
      
      # ── Data Prep ────────────────────────────────────────────────────────────
      tabItem(tabName = "prep",
              fluidRow(
                box(title = "Column Selection", width = 6, status = "primary", solidHeader = TRUE,
                    uiOutput("colKeepUI"),
                    helpText("Deselect columns to drop them from all downstream analysis.")),
                box(title = "Row Filter", width = 6, status = "primary", solidHeader = TRUE,
                    uiOutput("filterVarUI"),
                    uiOutput("filterValuesUI"),
                    checkboxInput("filterKeepNA", "Retain rows with missing filter values", FALSE),
                    helpText("Filter rows by the values of a chosen column."))
              ),
              fluidRow(
                box(title = "Transform Numeric Variables", width = 12, status = "info", solidHeader = TRUE,
                    fluidRow(
                      column(4, uiOutput("transformVarUI")),
                      column(4, selectInput("transformType", "Transformation:",
                                            choices = c("None"            = "none",
                                                        "log (natural)"   = "log",
                                                        "log10"           = "log10",
                                                        "sqrt"            = "sqrt",
                                                        "Standardise (z)" = "scale",
                                                        "Centre (x - mean)" = "center",
                                                        "Reciprocal (1/x)" = "reciprocal",
                                                        "Box-Cox"         = "boxcox"),
                                            selected = "none")),
                      column(4, actionButton("applyTransform", "Apply Transformation",
                                             icon = icon("bolt"), class = "btn-warning",
                                             style = "margin-top:25px;"))
                    ),
                    helpText("Transformed variables are appended as new columns (e.g. <code>log_yield</code>).",
                             "Log and Box-Cox require positive values; square root allows zero.",
                             "Reset using the button below.")
                )
              ),
              fluidRow(
                box(title = "Processed Data", width = 12, status = "success", solidHeader = TRUE,
                    fluidRow(
                      column(3, verbatimTextOutput("prepSummary")),
                      column(3, actionButton("resetPrep", "Reset All Prep",
                                             icon = icon("undo"), class = "btn-default",
                                             style = "margin-top:25px;")),
                      column(3, downloadButton("downloadProcessed", "Download Processed CSV",
                                               class = "btn-success", style = "margin-top:25px;")),
                      column(3)
                    ),
                    br(),
                    DTOutput("processedPreview")
                )
              )
      ),
      
      # ── Descriptive Stats ────────────────────────────────────────────────────
      tabItem(tabName = "statistics",
              fluidRow(
                box(title = "Descriptive Statistics (all numeric variables)", width = 12,
                    status = "primary", solidHeader = TRUE,
                    DTOutput("summaryTable"),
                    br(),
                    downloadButton("downloadStats", "Download Summary CSV"))
              ),
              fluidRow(
                box(title = "Extended Statistics (selected variables)", width = 12,
                    status = "info", solidHeader = TRUE,
                    DTOutput("extendedStatsTable"),
                    br(),
                    downloadButton("downloadExt", "Download Extended CSV"))
              )
      ),
      
      # ── Distributions ────────────────────────────────────────────────────────
      tabItem(tabName = "distributions",
              fluidRow(
                box(title = "Plot Controls", width = 12, status = "primary", solidHeader = TRUE,
                    column(3, radioButtons("plotType", "Plot Type:",
                                           choices = c("Histogram" = "hist",
                                                       "Density"   = "density",
                                                       "Boxplot"   = "box",
                                                       "Violin"    = "violin",
                                                       "Q-Q Plot"  = "qq"),
                                           selected = "hist")),
                    column(3,
                           checkboxInput("showStats",    "Show Statistics",        TRUE),
                           checkboxInput("showCI",       "95% CI for the mean (density)", FALSE),
                           checkboxInput("showOutliers", "Highlight Outliers",     TRUE)),
                    column(3,
                           checkboxInput("facetPlots", "Facet All Variables", FALSE),
                           uiOutput("groupVarUI")),
                    column(3, uiOutput("facetByUI"))
                )
              ),
              fluidRow(uiOutput("distributionPlotsUI"))
      ),
      
      # ── Group Comparisons ────────────────────────────────────────────────────
      tabItem(tabName = "groups",
              fluidRow(
                box(title = "Group Comparison Setup", width = 12, status = "primary", solidHeader = TRUE,
                    column(3, uiOutput("gcYUI")),
                    column(3, uiOutput("gcGroupUI")),
                    column(3, selectInput("gcTest", "Test:",
                                          choices = c("Auto (Welch; robust default)" = "auto",
                                                      "Welch t-test / ANOVA" = "welch",
                                                      "Student t-test"       = "t",
                                                      "One-way ANOVA + Tukey" = "anova",
                                                      "Wilcoxon"             = "wilcox",
                                                      "Kruskal-Wallis + pairwise Wilcoxon (Holm)" = "kw"),
                                          selected = "auto")),
                    column(3, selectInput("gcPlot", "Plot Type:",
                                          choices = c("Boxplot" = "box",
                                                      "Violin + Boxplot" = "violinbox",
                                                      "Strip + Mean ± SE" = "meanse"),
                                          selected = "violinbox"))
                )
              ),
              fluidRow(
                box(title = "Test Result", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("gcTestOut")),
                box(title = "Group Summary", width = 6, status = "info", solidHeader = TRUE,
                    DTOutput("gcSummary"))
              ),
              fluidRow(
                box(title = "Pairwise / Post-hoc", width = 12, status = "warning", solidHeader = TRUE,
                    DTOutput("gcPostHoc"))
              ),
              fluidRow(
                box(title = "Comparison Plot", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("gcPlot", height = 520),
                    br(),
                    downloadButton("downloadGC", "Download Comparison Plot"))
              )
      ),
      
      # ── Correlations ─────────────────────────────────────────────────────────
      tabItem(tabName = "correlations",
              fluidRow(
                box(title = "Correlation Controls", width = 12, status = "primary", solidHeader = TRUE,
                    column(3, radioButtons("relationType", "Display:",
                                           choices = c("Correlation Matrix"  = "corr",
                                                       "Scatter Plot Matrix" = "scatter",
                                                       "Pairwise Scatter"    = "pair"),
                                           selected = "corr", inline = FALSE)),
                    column(3, selectInput("corrMethod", "Method:",
                                          choices = c("Pearson"  = "pearson",
                                                      "Spearman" = "spearman",
                                                      "Kendall"  = "kendall"),
                                          selected = "pearson")),
                    column(3, selectInput("corrOrder", "Ordering:",
                                          choices = c("Hierarchical cluster" = "hclust",
                                                      "Original"             = "original",
                                                      "Alphabetical"         = "alphabet",
                                                      "AOE (angular)"        = "AOE"),
                                          selected = "hclust")),
                    column(3,
                           checkboxInput("showSig", "Show significance stars", TRUE),
                           checkboxInput("holmAdj", "Holm-adjust p-values",    TRUE))
                )
              ),
              fluidRow(
                conditionalPanel("input.relationType == 'corr'",
                                 box(title = "Correlation Matrix", width = 12,
                                     status = "info", solidHeader = TRUE,
                                     plotOutput("corrPlot", height = 620),
                                     br(),
                                     downloadButton("downloadCorr", "Download Plot"),
                                     downloadButton("downloadCorrCSV", "Download r + p Tables"))),
                conditionalPanel("input.relationType == 'scatter'",
                                 box(title = "Scatter Plot Matrix", width = 12,
                                     status = "info", solidHeader = TRUE,
                                     plotOutput("scatterMatrix", height = 720),
                                     br(),
                                     downloadButton("downloadScatter", "Download Plot"))),
                conditionalPanel("input.relationType == 'pair'",
                                 box(title = "Pairwise Scatter", width = 12,
                                     status = "info", solidHeader = TRUE,
                                     fluidRow(
                                       column(3, uiOutput("xVarSelect")),
                                       column(3, uiOutput("yVarSelect")),
                                       column(3, uiOutput("colorVarSelect")),
                                       column(3,
                                              checkboxInput("showRegression", "Regression line", TRUE),
                                              checkboxInput("showEquation",   "Equation & R²",   TRUE))
                                     ),
                                     plotlyOutput("pairwisePlot", height = 580)))
              )
      ),
      
      # ── PCA (FactoMineR + factoextra) ─────────────────────────────────────────
      tabItem(tabName = "pca",
              fluidRow(
                box(title = "PCA Setup", width = 12, status = "primary", solidHeader = TRUE,
                    column(3, uiOutput("pcaVarsUI")),
                    column(3, uiOutput("pcaGroupUI")),
                    column(2, numericInput("pcaX", "PC on X:", value = 1, min = 1, max = 10)),
                    column(2, numericInput("pcaY", "PC on Y:", value = 2, min = 1, max = 10)),
                    column(2,
                           checkboxInput("pcaScale",   "Scale variables",    TRUE),
                           checkboxInput("pcaEllipse", "Confidence ellipses", TRUE),
                           checkboxInput("pcaRepel",   "Repel labels",        TRUE))
                )
              ),
              fluidRow(
                box(title = "Scree Plot (Eigenvalues)", width = 6, status = "info", solidHeader = TRUE,
                    plotOutput("pcaScree", height = 380),
                    downloadButton("downloadPCAScree", "Download Scree Plot")),
                box(title = "PCA Score Plot", width = 6, status = "info", solidHeader = TRUE,
                    plotOutput("pcaBiplot", height = 380),
                    downloadButton("downloadPCABiplot", "Download Score Plot"))
              ),
              fluidRow(
                box(title = "Variables — Correlation Circle", width = 6, status = "success", solidHeader = TRUE,
                    plotOutput("pcaVarCircle", height = 380),
                    downloadButton("downloadPCAVarCircle", "Download Variable Plot")),
                box(title = "Variable Contributions", width = 6, status = "success", solidHeader = TRUE,
                    uiOutput("pcaContribDimUI"),
                    plotOutput("pcaContrib", height = 340),
                    downloadButton("downloadPCAContrib", "Download Contribution Plot"))
              ),
              fluidRow(
                box(title = "Eigenvalue Table", width = 4, status = "warning", solidHeader = TRUE,
                    DTOutput("pcaEigen")),
                box(title = "Variable Loadings (Correlations)", width = 4, status = "warning", solidHeader = TRUE,
                    DTOutput("pcaLoadings"),
                    br(),
                    downloadButton("downloadPCALoadings", "Download Loadings CSV")),
                box(title = "Individual Scores", width = 4, status = "warning", solidHeader = TRUE,
                    DTOutput("pcaScores"),
                    br(),
                    downloadButton("downloadPCAScores", "Download Full Scores CSV"))
              )
      ),
      
      # ── Regression ───────────────────────────────────────────────────────────
      tabItem(tabName = "regression",
              fluidRow(
                box(title = "Model Specification", width = 12, status = "primary", solidHeader = TRUE,
                    column(3, uiOutput("regYUI")),
                    column(4, uiOutput("regXUI")),
                    column(3, checkboxInput("regStd", "Standardise predictors", FALSE)),
                    column(2, actionButton("fitReg", "Fit Model",
                                           icon = icon("play"),
                                           class = "btn-success",
                                           style = "margin-top:25px;"))
                )
              ),
              fluidRow(
                box(title = "Coefficients", width = 7, status = "info", solidHeader = TRUE,
                    DTOutput("regCoefs"),
                    br(),
                    downloadButton("downloadRegCoefs", "Download Coefficients CSV")),
                box(title = "Fit Statistics", width = 5, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("regFitStats"))
              ),
              fluidRow(
                box(title = "Diagnostic Plots", width = 12, status = "warning", solidHeader = TRUE,
                    plotOutput("regDiag", height = 620),
                    br(),
                    downloadButton("downloadRegDiag", "Download Diagnostics"))
              )
      ),
      
      # ── Data Quality ─────────────────────────────────────────────────────────
      tabItem(tabName = "quality",
              fluidRow(
                box(title = "Missing Values Summary", width = 6, status = "warning", solidHeader = TRUE,
                    DTOutput("missingTable")),
                box(title = "Outlier Detection (IQR)", width = 6, status = "danger", solidHeader = TRUE,
                    DTOutput("outlierTable"))
              ),
              fluidRow(
                box(title = "Normality Assessment", width = 12, status = "info", solidHeader = TRUE,
                    helpText("Shapiro-Wilk p-values are adjusted across variables using Holm's method. A non-significant result does not prove normality."),
                    DTOutput("normalityTable"))
              ),
              fluidRow(
                box(title = "Missing Values by Variable", width = 12, status = "warning", solidHeader = TRUE,
                    plotOutput("missingBarPlot", height = 420))
              )
      )
    )
  )
)

# =============================================================================
#  Server
# =============================================================================

server <- function(input, output, session) {
  
  # ── Shared ggplot styling ────────────────────────────────────────────────────
  get_colors <- reactive({ publication_palettes[[input$colorPalette]] })
  
  get_theme <- reactive({
    bs   <- input$baseFontSize
    bold <- if (input$useBoldText) "bold" else "plain"
    grid <- if (input$useGridLines) element_line(color = "gray90", linewidth = 0.3) else element_blank()
    
    base_theme <- switch(input$plotTheme,
                         "minimal"   = theme_minimal(base_size = bs),
                         "classic"   = theme_classic(base_size = bs),
                         "light"     = theme_light(base_size = bs),
                         "gray"      = theme_gray(base_size = bs),
                         "bw"        = theme_bw(base_size = bs),
                         "economist" = theme_economist(base_size = bs),
                         "stata"     = theme_stata(base_size = bs),
                         theme_minimal(base_size = bs))
    
    base_theme + theme(
      plot.title        = element_text(size = bs + 3, face = bold, hjust = 0.5, margin = margin(b = 12)),
      plot.subtitle     = element_text(size = bs, hjust = 0.5, margin = margin(b = 8)),
      axis.title.x      = element_text(size = bs + 1, face = bold, margin = margin(t = 8)),
      axis.title.y      = element_text(size = bs + 1, face = bold, margin = margin(r = 8)),
      axis.text         = element_text(size = bs - 1, color = "black"),
      legend.title      = element_text(size = bs,     face = bold),
      legend.text       = element_text(size = bs - 1),
      legend.position   = "right",
      legend.background = element_rect(fill = "white", color = NA),
      strip.text        = element_text(size = bs,     face = bold, color = "black",
                                       margin = margin(4, 4, 4, 4)),
      strip.background  = element_rect(fill = "gray95", color = "gray70", linewidth = 0.4),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_rect(fill = NA, color = "black", linewidth = 0.8),
      panel.grid.major  = grid,
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "white", color = NA),
      plot.margin       = margin(12, 12, 12, 12)
    )
  })
  
  # Save a ggplot using the current export settings. Chooses device by format.
  save_gg <- function(p, file) {
    validate(need(!is.null(p), "There is no plot to export."))
    fmt <- input$exportFormat %||% "png"
    device <- switch(fmt,
                     "png"  = "png",
                     "pdf"  = "pdf",
                     "tiff" = "tiff",
                     "svg"  = grDevices::svg)
    args <- list(
      filename = file, plot = p, device = device,
      width = input$exportWidth, height = input$exportHeight,
      dpi = input$plotDPI, units = "in", bg = "white"
    )
    if (identical(fmt, "tiff")) args$compression <- "lzw"
    do.call(ggsave, args)
  }
  
  # ── Data upload with format detection ────────────────────────────────────────
  # Excel sheet picker
  output$sheetPickerUI <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (tolower(ext) %in% c("xlsx", "xls") && has_readxl) {
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) NULL)
      if (!is.null(sheets))
        selectInput("xlsxSheet", "Excel Sheet:", choices = sheets, selected = sheets[1])
    }
  })
  
  raw_data <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    path <- input$file$datapath
    
    tryCatch({
      df <- if (ext %in% c("xlsx", "xls")) {
        if (!has_readxl) {
          showNotification("Install the 'readxl' package to open Excel files.", type = "error")
          return(NULL)
        }
        sheet <- input$xlsxSheet %||% 1
        as.data.frame(readxl::read_excel(path, sheet = sheet))
      } else {
        sep <- if (input$delim == "auto") detect_delimiter(path) else input$delim
        read.table(path, header = TRUE, sep = sep,
                   stringsAsFactors = FALSE, quote = '"',
                   check.names = FALSE, na.strings = c("", "NA", "N/A", "#N/A", "."),
                   comment.char = "", fill = TRUE)
      }
      if (nrow(df) == 0 || ncol(df) == 0) {
        showNotification("Uploaded file has no usable data.", type = "error"); return(NULL)
      }
      names(df) <- make.unique(trimws(names(df)), sep = "_")
      df
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error"); NULL
    })
  })
  
  # ── Data Prep: persistent state for row filter + transformations ────────────
  prep_state <- reactiveValues(keep_cols = NULL, filter_var = "None",
                               filter_vals = NULL, added_cols = list())

  augmented_data <- reactive({
    df <- raw_data()
    req(df)
    for (nm in names(prep_state$added_cols)) df[[nm]] <- prep_state$added_cols[[nm]]
    df
  })
  
  # Reset the prep state whenever a new dataset is uploaded
  observeEvent(raw_data(), {
    prep_state$keep_cols   <- names(raw_data())
    prep_state$filter_var  <- "None"
    prep_state$filter_vals <- NULL
    prep_state$added_cols  <- list()
  }, ignoreNULL = TRUE)
  
  observeEvent(input$resetPrep, {
    req(raw_data())
    prep_state$keep_cols   <- names(raw_data())
    prep_state$filter_var  <- "None"
    prep_state$filter_vals <- NULL
    prep_state$added_cols  <- list()
    updateSelectInput(session, "filterVar",      selected = "None")
    updateSelectInput(session, "transformVar",   selected = NULL)
    updateSelectInput(session, "transformType",  selected = "none")
    updateCheckboxInput(session, "filterKeepNA", value = FALSE)
    showNotification("Data prep reset.", type = "message")
  })
  
  # Column-keep UI
  output$colKeepUI <- renderUI({
    req(augmented_data())
    choices <- names(augmented_data())
    checkboxGroupInput("keepCols", "Keep columns:",
                       choices  = choices,
                       selected = intersect(prep_state$keep_cols %||% choices, choices),
                       inline = TRUE)
  })
  observeEvent(input$keepCols, {
    req(raw_data())
    if (!length(input$keepCols)) {
      showNotification("Keep at least one column in the processed dataset.", type = "warning")
      updateCheckboxGroupInput(session, "keepCols", selected = prep_state$keep_cols)
      return()
    }
    prep_state$keep_cols <- input$keepCols
  }, ignoreNULL = FALSE)
  
  # Row filter UI
  output$filterVarUI <- renderUI({
    req(augmented_data())
    selectInput("filterVar", "Filter variable:",
                choices  = c("None", names(augmented_data())),
                selected = prep_state$filter_var)
  })
  output$filterValuesUI <- renderUI({
    req(augmented_data(), input$filterVar)
    if (input$filterVar == "None") return(NULL)
    x <- augmented_data()[[input$filterVar]]
    if (is.numeric(x)) {
      finite_x <- x[is.finite(x)]
      if (!length(finite_x)) return(helpText("This variable has no finite values to filter."))
      rng <- range(finite_x)
      span <- diff(rng)
      slider_limits <- if (span == 0) rng + c(-0.5, 0.5) else rng
      stored_range <- prep_state$filter_vals
      stored_ok <- !is.null(stored_range) && length(stored_range) == 2 &&
        is.numeric(stored_range) && all(is.finite(stored_range)) &&
        stored_range[1] >= slider_limits[1] && stored_range[2] <= slider_limits[2]
      sliderInput("filterRange", "Keep rows where value is within:",
                  min = slider_limits[1], max = slider_limits[2],
                  value = if (stored_ok) stored_range else rng,
                  step = max(diff(slider_limits) / 100, .Machine$double.eps^0.5))
    } else {
      vals <- sort(unique(as.character(x[!is.na(x)])))
      selectInput("filterLevels", "Keep rows where value is one of:",
                  choices  = vals,
                  selected = intersect(prep_state$filter_vals %||% vals, vals),
                  multiple = TRUE)
    }
  })
  observeEvent(input$filterVar, {
    if (!identical(prep_state$filter_var, input$filterVar)) prep_state$filter_vals <- NULL
    prep_state$filter_var <- input$filterVar
  }, ignoreNULL = FALSE)
  observeEvent(input$filterRange, {
    req(input$filterVar, input$filterVar != "None")
    if (is.numeric(augmented_data()[[input$filterVar]]))
      prep_state$filter_vals <- input$filterRange
  })
  observeEvent(input$filterLevels, {
    req(input$filterVar, input$filterVar != "None")
    if (!is.numeric(augmented_data()[[input$filterVar]]))
      prep_state$filter_vals <- input$filterLevels
  })
  
  # Transform UI
  output$transformVarUI <- renderUI({
    req(augmented_data())
    nc <- names(augmented_data())[sapply(augmented_data(), is.numeric)]
    selectInput("transformVar", "Variable:", choices = nc)
  })
  
  # Apply transformation
  observeEvent(input$applyTransform, {
    req(augmented_data(), input$transformVar, input$transformType)
    if (input$transformType == "none") {
      showNotification("No transformation selected.", type = "warning"); return()
    }
    v   <- input$transformVar
    x   <- augmented_data()[[v]]
    new_name <- paste0(input$transformType, "_", v)
    
    new_x <- switch(input$transformType,
                    "log"        = ifelse(x > 0, log(x),      NA_real_),
                    "log10"      = ifelse(x > 0, log10(x),    NA_real_),
                    "sqrt"       = ifelse(x >= 0, sqrt(x),    NA_real_),
                    "scale"      = {
                      s <- sd(x, na.rm = TRUE)
                      if (is.finite(s) && s > 0) as.numeric(scale(x)) else rep(NA_real_, length(x))
                    },
                    "center"     = as.numeric(scale(x, center = TRUE, scale = FALSE)),
                    "reciprocal" = ifelse(x != 0, 1 / x,      NA_real_),
                    "boxcox"     = {
                      # Simple Box-Cox via MASS::boxcox-free implementation:
                      # find lambda that maximises log-likelihood on positive x.
                      xx <- x[x > 0 & !is.na(x)]
                      if (length(xx) < 5 || length(unique(xx)) < 2) {
                        showNotification(
                          "Box-Cox requires at least five positive, non-constant observations.",
                          type = "error"
                        )
                        rep(NA_real_, length(x))
                      } else {
                        lams <- seq(-2, 2, by = 0.05)
                        llk  <- sapply(lams, function(l) {
                          y <- if (abs(l) < 1e-8) log(xx) else (xx^l - 1) / l
                          -0.5 * length(y) * log(var(y)) + (l - 1) * sum(log(xx))
                        })
                        lambda <- lams[which.max(llk)]
                        showNotification(sprintf("Box-Cox lambda = %.2f", lambda),
                                         type = "message")
                        y <- ifelse(x > 0,
                                    if (abs(lambda) < 1e-8) log(x) else (x^lambda - 1) / lambda,
                                    NA_real_)
                        y
                      }
                    })
    
    prep_state$added_cols[[new_name]] <- new_x
    if (!(new_name %in% prep_state$keep_cols))
      prep_state$keep_cols <- c(prep_state$keep_cols, new_name)
    showNotification(paste("Added column:", new_name), type = "message")
  })
  
  # Assemble working (processed) dataset
  working_data <- reactive({
    df <- augmented_data()
    req(df)
    # Row filter
    if (!is.null(prep_state$filter_var) && prep_state$filter_var != "None") {
      fv <- prep_state$filter_var
      if (fv %in% names(df)) {
        x <- df[[fv]]
        if (is.numeric(x) && length(prep_state$filter_vals) == 2) {
          keep_rows <- !is.na(x) & x >= prep_state$filter_vals[1] & x <= prep_state$filter_vals[2]
        } else if (!is.null(prep_state$filter_vals)) {
          keep_rows <- !is.na(x) & as.character(x) %in% prep_state$filter_vals
        } else {
          keep_rows <- rep(TRUE, nrow(df))
        }
        if (isTRUE(input$filterKeepNA)) keep_rows <- keep_rows | is.na(x)
        df <- df[keep_rows, , drop = FALSE]
      }
    }
    # Column keep
    if (!is.null(prep_state$keep_cols)) {
      keep <- intersect(prep_state$keep_cols, names(df))
      df <- df[, keep, drop = FALSE]
    }
    df
  })
  
  # Convenience reactive for numeric-only
  numeric_data <- reactive({
    req(working_data())
    working_data() %>% select(where(is.numeric))
  })
  
  # ── Prep tab outputs ─────────────────────────────────────────────────────────
  output$prepSummary <- renderPrint({
    req(working_data())
    cat("Rows:", nrow(working_data()), "\n")
    cat("Cols:", ncol(working_data()), "\n")
    cat("Added:", length(prep_state$added_cols), "\n")
  })
  
  output$processedPreview <- renderDT({
    req(working_data())
    datatable(head(working_data(), 200),
              options = list(pageLength = 10, scrollX = TRUE, scrollY = "350px"),
              rownames = FALSE, class = "cell-border stripe")
  })
  
  output$downloadProcessed <- downloadHandler(
    filename = function() paste0("processed_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(working_data(), file, row.names = FALSE)
  )
  
  # ── Sidebar variable selector (drives Distributions, Stats, Correlations) ───
  output$varSelectSidebar <- renderUI({
    req(numeric_data())
    nc <- ncol(numeric_data())
    if (nc == 0) return(helpText("No numeric variables found"))
    previous <- isolate(input$variables)
    selected <- intersect(previous %||% character(), names(numeric_data()))
    if (!length(selected)) selected <- names(numeric_data())[seq_len(min(3, nc))]
    selectInput("variables", "Numeric Variables:",
                choices  = names(numeric_data()),
                multiple = TRUE,
                selected = selected)
  })
  
  # Categorical variable options from the working data
  cat_vars <- reactive({
    req(working_data())
    cls <- col_classes(working_data())
    cls$categorical
  })
  
  output$groupVarUI <- renderUI({
    opts <- c("None", cat_vars())
    selectInput("groupVar", "Group/color by (categorical):",
                choices = opts, selected = "None")
  })
  output$facetByUI <- renderUI({
    if (!isTRUE(input$facetPlots)) return(NULL)
    selectInput("facetBy", "Additional facet (categorical):", choices = c("None", cat_vars()),
                selected = "None")
  })
  
  # Relationships helpers
  output$xVarSelect <- renderUI({
    req(numeric_data()); selectInput("xVar", "X Variable:", choices = names(numeric_data()))
  })
  output$yVarSelect <- renderUI({
    req(numeric_data())
    selectInput("yVar", "Y Variable:", choices = names(numeric_data()),
                selected = names(numeric_data())[min(2, ncol(numeric_data()))])
  })
  output$colorVarSelect <- renderUI({
    req(working_data())
    selectInput("colorVar", "Color by:", choices = c("None", names(working_data())),
                selected = "None")
  })
  
  # ── Overview ─────────────────────────────────────────────────────────────────
  output$nRowsBox <- renderValueBox({
    req(working_data())
    valueBox(format(nrow(working_data()), big.mark = ","), "Rows",
             icon = icon("database"), color = "blue")
  })
  output$nColsBox <- renderValueBox({
    req(working_data())
    valueBox(ncol(working_data()), "Columns", icon = icon("columns"), color = "green")
  })
  output$numericColsBox <- renderValueBox({
    req(numeric_data())
    valueBox(ncol(numeric_data()), "Numeric Columns",
             icon = icon("hashtag"), color = "purple")
  })
  output$missingBox <- renderValueBox({
    req(working_data())
    pct <- round(sum(is.na(working_data())) / prod(dim(working_data())) * 100, 1)
    valueBox(paste0(pct, "%"), "Missing Values",
             icon = icon("exclamation-triangle"),
             color = if (pct > 10) "red" else if (pct > 5) "yellow" else "green")
  })
  output$dataStructure <- renderPrint({ req(working_data()); str(working_data()) })
  
  output$missingPlot <- renderPlot({
    req(working_data())
    df <- working_data()
    md <- df %>%
      summarise(across(everything(), ~ sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
      mutate(Missing_Pct = Missing_Count / nrow(df) * 100) %>%
      arrange(desc(Missing_Pct))
    if (sum(md$Missing_Count) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No missing values.",
                 size = 6, color = "darkgreen", fontface = "bold") +
        theme_void()
    } else {
      ggplot(md, aes(x = reorder(Variable, Missing_Pct), y = Missing_Pct)) +
        geom_col(fill = get_colors()[1], alpha = 0.85, width = 0.7) +
        geom_text(aes(label = sprintf("%.1f%%", Missing_Pct)),
                  hjust = -0.1, size = 4, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
        labs(x = NULL, y = "Missing (%)") +
        get_theme()
    }
  })
  
  output$varTypesPlot <- renderPlot({
    req(working_data())
    vt <- data.frame(Type = sapply(working_data(), function(x) class(x)[1])) %>%
      count(Type)
    ggplot(vt, aes(x = reorder(Type, n), y = n, fill = Type)) +
      geom_col(alpha = 0.85, show.legend = FALSE, width = 0.7) +
      geom_text(aes(label = n), hjust = -0.3, size = 5, fontface = "bold") +
      coord_flip() +
      scale_fill_manual(values = get_colors()) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Number of Variables", title = "Variable Types") +
      get_theme()
  })
  
  # ── Descriptive Statistics ───────────────────────────────────────────────────
  summary_stats <- reactive({
    req(numeric_data())
    validate(need(ncol(numeric_data()) > 0, "No numeric variables are available."))
    describe_numeric(numeric_data(), extended = FALSE)
  })
  
  extended_stats <- reactive({
    req(input$variables, numeric_data())
    selected_vars <- intersect(input$variables, names(numeric_data()))
    validate(need(length(selected_vars) > 0, "Select at least one numeric variable."))
    describe_numeric(numeric_data()[, selected_vars, drop = FALSE], extended = TRUE)
  })
  
  output$summaryTable <- renderDT({
    datatable(summary_stats(),
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE, class = "cell-border stripe")
  })
  output$extendedStatsTable <- renderDT({
    datatable(extended_stats(),
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE, class = "cell-border stripe") %>%
      formatStyle("Skewness",
                  backgroundColor = styleInterval(c(-1, -0.5, 0.5, 1),
                                                  c("#f4cccc","#fce5cd","#ccffcc","#fce5cd","#f4cccc")))
  })
  
  # ── Distributions ────────────────────────────────────────────────────────────
  build_dist_plot <- function(df_full, v) {
    vals_clean <- df_full[[v]]; vals_clean <- vals_clean[!is.na(vals_clean)]
    if (!length(vals_clean)) return(message_plot(paste(v, "has no non-missing values.")))
    if (input$plotType == "density" && length(unique(vals_clean)) < 2)
      return(message_plot(paste(v, "is constant; density cannot be estimated.")))
    colors     <- get_colors()
    
    mean_val   <- mean(vals_clean)
    median_val <- median(vals_clean)
    sd_val     <- sd(vals_clean)
    q1 <- quantile(vals_clean, 0.25); q3 <- quantile(vals_clean, 0.75)
    iqr_val <- q3 - q1
    n_out   <- sum(vals_clean < q1 - 1.5 * iqr_val | vals_clean > q3 + 1.5 * iqr_val)
    fs <- input$baseFontSize
    
    use_group <- !is.null(input$groupVar) && input$groupVar != "None" &&
      input$groupVar %in% names(df_full) &&
      any(!is.na(df_full[[input$groupVar]][!is.na(df_full[[v]])]))
    grp <- if (use_group) as.factor(df_full[[input$groupVar]][!is.na(df_full[[v]])]) else NULL
    group_colors <- if (use_group) expand_palette(colors, nlevels(droplevels(grp))) else colors
    
    p <- switch(input$plotType,
                
                "hist" = {
                  if (use_group) {
                    ggplot(data.frame(x = vals_clean, grp = grp),
                           aes(x = x, fill = grp)) +
                      geom_histogram(bins = 30, color = "white",
                                     alpha = 0.75, linewidth = 0.3, position = "identity") +
                      scale_fill_manual(values = group_colors, name = input$groupVar) +
                      labs(x = v, y = "Frequency", title = paste("Histogram:", v))
                  } else {
                    p0 <- ggplot(data.frame(x = vals_clean), aes(x = x)) +
                      geom_histogram(bins = 30, fill = colors[1], color = "white",
                                     alpha = 0.85, linewidth = 0.3) +
                      labs(x = v, y = "Frequency", title = paste("Histogram:", v))
                    if (input$showStats) {
                      lbl <- sprintf("Mean = %.2f\nMedian = %.2f\nSD = %.2f\nn = %d",
                                     mean_val, median_val, sd_val, length(vals_clean))
                      p0 <- p0 + annotate("label", x = Inf, y = Inf, label = lbl,
                                          hjust = 1.05, vjust = 1.05, size = fs / 3.4,
                                          fill = alpha("white", 0.85), label.size = 0.3,
                                          fontface = "bold")
                    }
                    p0
                  }
                },
                
                "density" = {
                  if (use_group) {
                    ggplot(data.frame(x = vals_clean, grp = grp),
                           aes(x = x, fill = grp, color = grp)) +
                      geom_density(alpha = 0.45, linewidth = 1) +
                      scale_fill_manual (values = group_colors, name = input$groupVar) +
                      scale_color_manual(values = group_colors, name = input$groupVar) +
                      labs(x = v, y = "Density", title = paste("Density:", v))
                  } else {
                    p0 <- ggplot(data.frame(x = vals_clean), aes(x = x)) +
                      geom_density(fill = colors[1], color = colors[1],
                                   alpha = 0.55, linewidth = 1.1) +
                      geom_vline(xintercept = mean_val,   color = colors[2], linetype = "dashed",  linewidth = 1) +
                      geom_vline(xintercept = median_val, color = colors[3], linetype = "dotted",  linewidth = 1) +
                      labs(x = v, y = "Density", title = paste("Density:", v))
                    if (input$showCI) {
                      ci_half <- if (length(vals_clean) > 1)
                        qt(0.975, df = length(vals_clean) - 1) * sd_val / sqrt(length(vals_clean))
                      else NA_real_
                      if (is.finite(ci_half)) {
                      p0 <- p0 + annotate("rect",
                                          xmin = mean_val - ci_half,
                                          xmax = mean_val + ci_half,
                                          ymin = 0, ymax = Inf, alpha = 0.12, fill = colors[4])
                      }
                    }
                    p0
                  }
                },
                
                "box" = {
                  if (use_group) {
                    ggplot(data.frame(y = vals_clean, grp = grp), aes(x = grp, y = y, fill = grp)) +
                      geom_boxplot(alpha = 0.8, outlier.size = 1.8, linewidth = 0.6,
                                   outlier.shape = if (isTRUE(input$showOutliers)) 19 else NA) +
                      scale_fill_manual(values = group_colors, name = input$groupVar) +
                      labs(x = input$groupVar, y = v, title = paste("Boxplot:", v))
                  } else {
                    p0 <- ggplot(data.frame(y = vals_clean, x = ""), aes(x = x, y = y)) +
                      geom_boxplot(fill = colors[1], alpha = 0.8, color = "black",
                                   outlier.color = colors[2], outlier.size = 2,
                                   outlier.shape = if (isTRUE(input$showOutliers)) 19 else NA,
                                   width = 0.45) +
                      stat_summary(fun = mean, geom = "point", shape = 23,
                                   size = 3, fill = colors[3], color = "black") +
                      labs(y = v, x = NULL, title = paste("Boxplot:", v)) +
                      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
                    if (input$showStats && n_out > 0) {
                      p0 <- p0 + annotate("text", x = 1.35, y = max(vals_clean),
                                          label = sprintf("%d outliers (%.1f%%)",
                                                          n_out, n_out / length(vals_clean) * 100),
                                          color = colors[2], fontface = "bold", size = fs / 3.5)
                    }
                    p0
                  }
                },
                
                "violin" = {
                  if (use_group) {
                    ggplot(data.frame(y = vals_clean, grp = grp), aes(x = grp, y = y, fill = grp)) +
                      geom_violin(alpha = 0.7, trim = FALSE, linewidth = 0.5) +
                      geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA, linewidth = 0.4) +
                      scale_fill_manual(values = group_colors, name = input$groupVar) +
                      labs(x = input$groupVar, y = v, title = paste("Violin:", v))
                  } else {
                    ggplot(data.frame(y = vals_clean, x = ""), aes(x = x, y = y)) +
                      geom_violin(fill = colors[1], alpha = 0.75, color = "black",
                                  trim = FALSE, linewidth = 0.6) +
                      geom_boxplot(width = 0.12, fill = "white", color = "black",
                                   outlier.color = colors[2], outlier.size = 1.5) +
                      labs(y = v, x = NULL, title = paste("Violin:", v)) +
                      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
                  }
                },
                
                "qq" = {
                  qq_df <- qqnorm(vals_clean, plot.it = FALSE)
                  ggplot(data.frame(theoretical = qq_df$x, sample = qq_df$y),
                         aes(x = theoretical, y = sample)) +
                    geom_point(color = colors[1], size = 1.8, alpha = 0.75) +
                    geom_qq_line(data = data.frame(x = vals_clean),
                                 aes(sample = x),
                                 color = colors[2], linewidth = 1, linetype = "dashed",
                                 inherit.aes = FALSE) +
                    labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                         title = paste("Q-Q Plot:", v))
                }
    )
    p + get_theme()
  }
  
  faceted_plot_obj <- reactive({
    req(input$variables, input$facetPlots)
    selected_vars <- intersect(input$variables, names(numeric_data()))
    validate(need(length(selected_vars) > 0, "Select at least one numeric variable."))
    use_facet_var <- !is.null(input$facetBy) && input$facetBy != "None" &&
      input$facetBy %in% names(working_data())
    keep_cols <- unique(c(selected_vars, if (use_facet_var) input$facetBy))
    df_long <- working_data()[, keep_cols, drop = FALSE] %>%
      pivot_longer(cols = all_of(selected_vars), names_to = "Variable", values_to = "Value")
    if (use_facet_var) df_long$FacetVar <- as.factor(df_long[[input$facetBy]])
    colors <- expand_palette(get_colors(), length(selected_vars))
    p <- switch(input$plotType,
                "hist"    = ggplot(df_long, aes(x = Value, fill = Variable)) +
                  geom_histogram(bins = 30, alpha = 0.8, color = "white", linewidth = 0.3) +
                  scale_fill_manual(values = colors),
                "density" = ggplot(df_long, aes(x = Value, fill = Variable, color = Variable)) +
                  geom_density(alpha = 0.5, linewidth = 1.1) +
                  scale_fill_manual(values = colors) + scale_color_manual(values = colors),
                "box"     = ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
                  geom_boxplot(alpha = 0.8, outlier.size = 1.5, linewidth = 0.5) +
                  scale_fill_manual(values = colors),
                "violin"  = ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
                  geom_violin(alpha = 0.8, trim = FALSE, linewidth = 0.5) +
                  geom_boxplot(width = 0.12, alpha = 0.6, outlier.shape = NA,
                               fill = "white", linewidth = 0.4) +
                  scale_fill_manual(values = colors),
                "qq"      = ggplot(df_long, aes(sample = Value, color = Variable)) +
                  stat_qq(alpha = 0.7, size = 1.4, na.rm = TRUE) +
                  stat_qq_line(linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
                  scale_color_manual(values = colors) +
                  labs(x = "Theoretical Quantiles", y = "Sample Quantiles"))
    facets <- if (use_facet_var) ggplot2::vars(Variable, FacetVar) else ggplot2::vars(Variable)
    p + facet_wrap(facets, scales = "free", ncol = 3) +
      get_theme() + labs(title = paste("Distributions:", toupper(input$plotType)))
  })
  
  output$distributionPlotsUI <- renderUI({
    req(input$variables)
    if (isTRUE(input$facetPlots)) {
      fluidRow(box(title = "Faceted Distribution Plots", width = 12,
                   status = "primary", solidHeader = TRUE,
                   plotOutput("facetedPlot", height = 620),
                   br(), downloadButton("downloadFaceted", "Download Plot")))
    } else {
      boxes <- lapply(seq_along(input$variables), function(i) {
        var <- input$variables[[i]]
        sn <- paste0("v", i)
        box(title = var, width = 4, status = "primary", solidHeader = TRUE,
            plotOutput(paste0("plot_", sn), height = 380),
            br(), downloadButton(paste0("dl_", sn), "Download",
                                 class = "btn-sm btn-default"))
      })
      do.call(fluidRow, boxes)
    }
  })
  
  output$facetedPlot <- renderPlot({ faceted_plot_obj() }, res = 110)
  
  observe({
    req(input$variables, !isTRUE(input$facetPlots))
    local_vars <- input$variables
    lapply(seq_along(local_vars), function(i) {
      local({
        var <- local_vars[[i]]
        sn  <- paste0("v", i)
        output[[paste0("plot_", sn)]] <- renderPlot({ build_dist_plot(working_data(), var) },
                                                    res = 110)
        output[[paste0("dl_", sn)]] <- downloadHandler(
          filename = function() paste0(safe_filename(var), "_", input$plotType, "_",
                                       input$colorPalette, ".", input$exportFormat),
          content  = function(file) save_gg(build_dist_plot(working_data(), var), file)
        )
      })
    })
  })
  
  output$downloadFaceted <- downloadHandler(
    filename = function() paste0("faceted_", input$plotType, "_",
                                 input$colorPalette, "_", Sys.Date(), ".",
                                 input$exportFormat),
    content  = function(file) save_gg(faceted_plot_obj(), file)
  )
  
  # ── Group Comparisons ────────────────────────────────────────────────────────
  output$gcYUI     <- renderUI({
    req(numeric_data()); selectInput("gcY", "Numeric response (Y):",
                                     choices = names(numeric_data()))
  })
  output$gcGroupUI <- renderUI({
    req(cat_vars())
    choices <- setdiff(cat_vars(), input$gcY %||% "")
    if (!length(choices)) return(helpText("No categorical grouping column is available."))
    selectInput("gcGroup", "Grouping variable:", choices = choices)
  })
  
  gc_data <- reactive({
    req(input$gcY, input$gcGroup, working_data())
    df <- working_data()
    data.frame(y = df[[input$gcY]],
               g = as.factor(df[[input$gcGroup]])) %>%
      filter(!is.na(y), !is.na(g)) %>%
      mutate(g = droplevels(g))
  })
  
  gc_summary_df <- reactive({
    req(gc_data())
    gc_data() %>% group_by(g) %>%
      summarise(N      = n(),
                Mean   = mean(y),
                SD     = sd(y),
                SE     = sd(y) / sqrt(n()),
                Median = median(y),
                Min    = min(y),
                Max    = max(y),
                .groups = "drop") %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  })
  
  output$gcSummary <- renderDT({
    datatable(gc_summary_df(), rownames = FALSE, options = list(pageLength = 10))
  })
  
  gc_test_obj <- reactive({
    req(gc_data())
    d <- gc_data()
    n_groups <- nlevels(d$g)
    if (n_groups < 2) return(list(text = "Need at least 2 groups.", posthoc = NULL))
    
    requested_test <- input$gcTest %||% "auto"
    test_type <- if (requested_test == "auto") "welch" else requested_test

    pairwise_df <- function(mat) {
      if (is.null(mat)) return(NULL)
      as.data.frame(as.table(mat), stringsAsFactors = FALSE) %>%
        filter(!is.na(Freq)) %>%
        transmute(Group1 = as.character(Var1), Group2 = as.character(Var2),
                  p_adj = round(Freq, 4), Sig = sig_stars(Freq))
    }

    test_result <- tryCatch({
      posthoc <- NULL
      test_text <- switch(
        test_type,
        "t" = {
          if (n_groups != 2) stop("Student t-test requires exactly two groups.")
          capture.output(print(t.test(y ~ g, data = d, var.equal = TRUE)))
        },
        "welch" = {
          if (n_groups == 2) {
            capture.output(print(t.test(y ~ g, data = d, var.equal = FALSE)))
          } else {
            test_out <- capture.output(print(oneway.test(y ~ g, data = d, var.equal = FALSE)))
            posthoc <- pairwise_df(pairwise.t.test(
              d$y, d$g, p.adjust.method = "holm", pool.sd = FALSE
            )$p.value)
            test_out
          }
        },
        "anova" = {
          fit <- aov(y ~ g, data = d)
          posthoc <- broom::tidy(TukeyHSD(fit)) %>%
            mutate(across(where(is.numeric), ~ round(.x, 4)),
                   Sig = sig_stars(adj.p.value))
          capture.output(print(summary(fit)))
        },
        "wilcox" = {
          if (n_groups != 2) stop("Wilcoxon rank-sum test requires exactly two groups.")
          capture.output(print(wilcox.test(y ~ g, data = d, exact = FALSE)))
        },
        "kw" = {
          kw <- kruskal.test(y ~ g, data = d)
          posthoc <- pairwise_df(pairwise.wilcox.test(
            d$y, d$g, p.adjust.method = "holm", exact = FALSE
          )$p.value)
          capture.output(print(kw))
        }
      )
      list(text = test_text, posthoc = posthoc)
    }, error = function(e) list(text = paste("Test could not be computed:", e$message), posthoc = NULL))

    # Brown-Forsythe test: a median-centred form of Levene's test.
    lev <- tryCatch({
      med <- tapply(d$y, d$g, median)
      zi <- abs(d$y - med[as.character(d$g)])
      anova(lm(zi ~ d$g))
    }, error = function(e) NULL)

    auto_note <- if (requested_test == "auto")
      "Automatic choice: Welch's test (robust to unequal variances)." else NULL
    if (is.null(lev)) {
      test_result$text <- c(auto_note, test_result$text, "",
                            "Brown-Forsythe variance test: not computed.")
    } else {
      pval <- lev$`Pr(>F)`[1]
      interpretation <- if (is.na(pval)) {
        "  → Variance equality could not be assessed."
      } else if (pval < 0.05) {
        "  → Evidence of unequal variances; prefer Welch or Kruskal-Wallis."
      } else {
        "  → No evidence of unequal variances."
      }
      test_result$text <- c(
        auto_note, test_result$text, "",
        sprintf("Brown-Forsythe variance test: F = %.3f, p = %.4f",
                lev$`F value`[1], pval),
        interpretation
      )
    }
    test_result
  })
  
  output$gcTestOut <- renderPrint({
    cat(paste(gc_test_obj()$text, collapse = "\n"))
  })
  output$gcPostHoc <- renderDT({
    ph <- gc_test_obj()$posthoc
    if (is.null(ph)) return(datatable(data.frame(Note = "No pairwise table for this test.")))
    datatable(ph, rownames = FALSE, options = list(pageLength = 15))
  })
  
  gc_plot_obj <- reactive({
    req(gc_data())
    d <- gc_data(); colors <- expand_palette(get_colors(), nlevels(d$g))
    p <- ggplot(d, aes(x = g, y = y, fill = g))
    p <- switch(input$gcPlot,
                "box"       = p + geom_boxplot(alpha = 0.8, outlier.size = 1.5, linewidth = 0.6),
                "violinbox" = p + geom_violin(alpha = 0.65, trim = FALSE, linewidth = 0.5) +
                  geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA, linewidth = 0.4),
                "meanse"    = p + geom_jitter(aes(color = g), width = 0.15, alpha = 0.4, size = 1.5) +
                  stat_summary(fun = mean, geom = "point", shape = 21, size = 4,
                               color = "black", fill = "white") +
                  stat_summary(fun.data = mean_se, geom = "errorbar",
                               width = 0.15, linewidth = 0.8, color = "black") +
                  scale_color_manual(values = colors, guide = "none"))
    p <- p + scale_fill_manual(values = colors, guide = "none") +
      labs(x = input$gcGroup, y = input$gcY,
           title = paste(input$gcY, "by", input$gcGroup)) +
      get_theme()
    p
  })
  
  output$gcPlot <- renderPlot({ gc_plot_obj() }, res = 110)
  output$downloadGC <- downloadHandler(
    filename = function() paste0("group_", safe_filename(input$gcY), "_by_",
                                 safe_filename(input$gcGroup), "_",
                                 Sys.Date(), ".", input$exportFormat),
    content  = function(file) save_gg(gc_plot_obj(), file)
  )
  
  # ── Correlations ─────────────────────────────────────────────────────────────
  corr_objs <- reactive({
    req(numeric_data(), input$variables)
    selected_vars <- intersect(input$variables, names(numeric_data()))
    validate(need(length(selected_vars) >= 2,
                  "Select at least two numeric variables in the sidebar."))
    mat <- numeric_data()[, selected_vars, drop = FALSE]
    # Drop columns with too few observations or no finite variance.
    keep <- vapply(mat, function(x) {
      z <- x[!is.na(x)]
      length(z) >= 3 && is.finite(var(z)) && var(z) > 0
    }, logical(1))
    mat <- mat[, keep, drop = FALSE]
    validate(need(ncol(mat) >= 2,
                  "At least two selected variables must have non-zero variance and three observations."))
    cp  <- cor_pmat(as.matrix(mat), method = input$corrMethod)
    if (isTRUE(input$holmAdj)) {
      p_vec <- cp$p[upper.tri(cp$p)]
      adj <- p.adjust(p_vec, method = "holm")
      cp$p[upper.tri(cp$p)] <- adj
      cp$p[lower.tri(cp$p)] <- t(cp$p)[lower.tri(cp$p)]
    }
    list(mat = cp$r, pmat = cp$p, colors = get_colors())
  })
  
  draw_corrplot <- function(obj) {
    corrplot::corrplot(obj$mat,
                       method      = "color", type = "upper", order = input$corrOrder,
                       addCoef.col = "black", tl.col = "black", tl.srt = 45,
                       tl.cex      = input$baseFontSize / 11, number.cex = input$baseFontSize / 13,
                       col         = colorRampPalette(c(obj$colors[1], "white", obj$colors[2]))(200),
                       mar         = c(0, 0, 2, 0),
                       title       = paste(toupper(input$corrMethod), "Correlation"),
                       cl.cex      = input$baseFontSize / 11, cl.pos = "r",
                       addgrid.col = "gray80",
                       p.mat       = if (input$showSig) obj$pmat else NULL,
                       sig.level   = if (input$showSig) c(0.001, 0.01, 0.05) else NULL,
                       insig       = if (input$showSig) "label_sig" else "pch",
                       pch.cex     = 0.9, pch.col = "grey30")
  }
  output$corrPlot <- renderPlot({ draw_corrplot(corr_objs()) })
  
  scatter_plot_obj <- reactive({
    req(input$variables, numeric_data())
    df     <- numeric_data() %>% select(any_of(input$variables))
    validate(need(ncol(df) >= 2, "Select at least two numeric variables in the sidebar."))
    colors <- get_colors()
    ggpairs(df,
            lower = list(continuous = wrap("points", alpha = 0.5, size = 1.5, color = colors[1])),
            diag  = list(continuous = wrap("densityDiag", fill = colors[1],
                                           alpha = 0.7, color = colors[1], linewidth = 0.8)),
            upper = list(continuous = wrap("cor", size = input$baseFontSize / 2.8, color = "black")),
            title = "Scatter Plot Matrix") +
      get_theme() +
      theme(plot.title = element_text(hjust = 0.5, size = input$baseFontSize + 3),
            strip.text = element_text(size = input$baseFontSize))
  })
  output$scatterMatrix <- renderPlot({ scatter_plot_obj() }, res = 100)
  
  output$pairwisePlot <- renderPlotly({
    req(input$xVar, input$yVar, numeric_data())
    df <- data.frame(x = numeric_data()[[input$xVar]],
                     y = numeric_data()[[input$yVar]])
    validate(need(sum(complete.cases(df[, c("x", "y")])) >= 2,
                  "The selected variables need at least two complete pairs."))
    colors <- get_colors()
    use_color <- !is.null(input$colorVar) && input$colorVar != "None" &&
      input$colorVar %in% names(working_data()) && any(!is.na(working_data()[[input$colorVar]]))
    if (use_color) {
      df$color_var <- working_data()[[input$colorVar]]
      if (is.numeric(df$color_var)) {
        p <- ggplot(df, aes(x = x, y = y, color = color_var)) +
          geom_point(alpha = 0.7, size = 2.5) +
          scale_color_viridis_c(option = "viridis", name = input$colorVar)
      } else {
        df$color_var <- as.factor(df$color_var)
        color_values <- expand_palette(colors, nlevels(droplevels(df$color_var)))
        p <- ggplot(df, aes(x = x, y = y, color = color_var)) +
          geom_point(alpha = 0.7, size = 2.5) +
          scale_color_manual(values = color_values, name = input$colorVar)
      }
    } else {
      p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(color = colors[1], alpha = 0.7, size = 2.5)
    }
    if (input$showRegression)
      p <- p + geom_smooth(data = df, aes(x = x, y = y, group = 1),
                           inherit.aes = FALSE, method = "lm", formula = y ~ x, se = TRUE,
                           color = colors[2], fill = colors[2],
                           linewidth = 1.1, alpha = 0.15)
    if (input$showEquation && input$showRegression) {
      cc <- complete.cases(df[, c("x", "y")])
      if (sum(cc) >= 3 && length(unique(df$x[cc])) > 1) {
        m  <- lm(y ~ x, data = df[cc, ])
        r2 <- summary(m)$r.squared; cf <- coef(m)
        eq <- sprintf("y = %.3fx + %.3f\nR² = %.3f", cf[2], cf[1], r2)
        xpos <- min(df$x, na.rm = TRUE) + 0.05 * diff(range(df$x, na.rm = TRUE))
        ypos <- max(df$y, na.rm = TRUE)
        p <- p + annotate("label", x = xpos, y = ypos, label = eq,
                          hjust = 0, vjust = 1, size = input$baseFontSize / 3.3,
                          fontface = "bold", fill = alpha("white", 0.85), label.size = 0.3)
      }
    }
    p <- p + labs(x = input$xVar, y = input$yVar,
                  title = paste(input$yVar, "vs.", input$xVar)) + get_theme()
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(font = list(size = input$baseFontSize))
  })
  
  # ── PCA (FactoMineR + factoextra) ────────────────────────────────────────────
  output$pcaVarsUI <- renderUI({
    req(numeric_data())
    previous <- isolate(input$pcaVars)
    selected <- intersect(previous %||% character(), names(numeric_data()))
    if (length(selected) < 2) selected <- names(numeric_data())
    selectInput("pcaVars", "PCA variables:",
                choices  = names(numeric_data()),
                selected = selected,
                multiple = TRUE)
  })
  output$pcaGroupUI <- renderUI({
    selectInput("pcaGroup", "Color / group by:",
                choices = c("None", cat_vars()), selected = "None")
  })
  output$pcaContribDimUI <- renderUI({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    npc <- ncol(obj$pca$ind$coord)
    selectInput("pcaContribDim", "Show contributions for:",
                choices  = setNames(seq_len(npc), paste0("Dim ", seq_len(npc))),
                selected = 1)
  })
  
  # Core PCA reactive: uses FactoMineR::PCA
  pca_obj <- reactive({
    req(input$pcaVars, numeric_data())
    pca_vars <- intersect(input$pcaVars, names(numeric_data()))
    validate(need(length(pca_vars) >= 2, "Select at least two numeric variables for PCA."))
    mat_all <- numeric_data()[, pca_vars, drop = FALSE]
    used_rows <- which(complete.cases(mat_all))
    mat <- mat_all[used_rows, , drop = FALSE]
    validate(need(nrow(mat) >= 3, "PCA requires at least three complete observations."))
    # Drop zero-variance columns
    kp <- vapply(mat, function(x) is.finite(var(x)) && var(x) > 0, logical(1))
    mat <- mat[, kp, drop = FALSE]
    validate(need(ncol(mat) >= 2, "PCA requires at least two non-constant variables."))
    
    pca_res <- tryCatch(
      FactoMineR::PCA(
        mat,
        scale.unit = isTRUE(input$pcaScale),
        graph = FALSE,
        ncp = min(ncol(mat), nrow(mat) - 1, 10)
      ),
      error = function(e) NULL
    )
    validate(need(!is.null(pca_res),
                  "PCA could not be fitted; check for collinearity or invalid values."))
    
    # Build grouping vector aligned to complete-case rows
    use_group <- !is.null(input$pcaGroup) && input$pcaGroup != "None"
    grp <- if (use_group) {
      grp_raw <- as.character(working_data()[[input$pcaGroup]][used_rows])
      grp_raw[is.na(grp_raw) | !nzchar(grp_raw)] <- "(Missing)"
      as.factor(grp_raw)
    } else NULL
    
    # Track per-group counts (needed to decide whether ellipses are safe)
    grp_counts <- if (!is.null(grp)) table(droplevels(grp)) else NULL
    
    list(pca = pca_res, used_rows = used_rows, grp = grp,
         grp_counts = grp_counts, mat = mat)
  })

  pca_axes <- function(obj) {
    npc <- ncol(obj$pca$ind$coord)
    validate(need(npc >= 2, "At least two principal components are required for this plot."))
    ax <- max(1L, min(as.integer(input$pcaX %||% 1), npc))
    ay <- max(1L, min(as.integer(input$pcaY %||% 2), npc))
    if (ax == ay) ay <- if (ax == 1L) 2L else 1L
    c(ax, ay)
  }
  
  # --- Scree plot ---
  pca_scree_obj <- reactive({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    p <- fviz_screeplot(obj$pca, addlabels = TRUE,
                        barfill  = get_colors()[1],
                        barcolor = get_colors()[1],
                        linecolor = get_colors()[2]) +
      labs(title = "Scree Plot — Variance Explained",
           x = "Principal Component", y = "% Variance Explained") +
      get_theme()
    p
  })
  output$pcaScree <- renderPlot({ pca_scree_obj() }, res = 110)
  
  # --- Individuals biplot ---
  pca_biplot_obj <- reactive({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    axes <- pca_axes(obj)
    
    has_grp <- !is.null(obj$grp)
    
    # Only draw ellipses when EVERY group has >= 3 observations;
    # otherwise ggplot2 floods the console with drop-warnings.
    ellipse_ok <- FALSE
    if (has_grp && isTRUE(input$pcaEllipse) && !is.null(obj$grp_counts)) {
      if (all(obj$grp_counts >= 3)) {
        ellipse_ok <- TRUE
      } else {
        n_small <- sum(obj$grp_counts < 3)
        showNotification(
          sprintf("Ellipses disabled: %d group(s) have < 3 observations.", n_small),
          type = "warning", duration = 5)
      }
    }
    
    grp_for_plot <- obj$grp
    
    p <- suppressWarnings(
      fviz_pca_ind(obj$pca, axes = axes,
                   geom.ind     = "point",
                   col.ind      = if (has_grp) grp_for_plot else get_colors()[1],
                   addEllipses  = ellipse_ok,
                   ellipse.type = "confidence",
                   ellipse.level = 0.95,
                   palette      = if (has_grp)
                     expand_palette(get_colors(), nlevels(grp_for_plot)) else get_colors(),
                   pointsize    = 2.2,
                   alpha.ind    = 0.7,
                   legend.title = if (has_grp) input$pcaGroup else "",
                   repel        = isTRUE(input$pcaRepel),
                   title        = "PCA Score Plot")
    )
    p + get_theme()
  })
  output$pcaBiplot <- renderPlot({ pca_biplot_obj() }, res = 110)
  
  # --- Variable correlation circle ---
  pca_var_circle_obj <- reactive({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    axes <- pca_axes(obj)
    
    p <- fviz_pca_var(obj$pca, axes = axes,
                      col.var    = "contrib",
                      gradient.cols = c(get_colors()[3], get_colors()[1], get_colors()[2]),
                      repel       = isTRUE(input$pcaRepel),
                      title       = "Variable Correlation Circle")
    p + get_theme()
  })
  output$pcaVarCircle <- renderPlot({ pca_var_circle_obj() }, res = 110)
  
  # --- Variable contributions bar plot ---
  pca_contrib_obj <- reactive({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    dim_idx <- as.integer(input$pcaContribDim %||% 1)
    p <- fviz_contrib(obj$pca, choice = "var", axes = dim_idx,
                      fill  = get_colors()[1],
                      color = get_colors()[1]) +
      labs(title = paste("Variable Contributions to Dim", dim_idx)) +
      get_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p
  })
  output$pcaContrib <- renderPlot({ pca_contrib_obj() }, res = 110)
  
  # --- Eigenvalue table ---
  output$pcaEigen <- renderDT({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    eig <- as.data.frame(obj$pca$eig)
    eig <- cbind(Component = paste0("Dim.", seq_len(nrow(eig))), round(eig, 3))
    names(eig) <- c("Component", "Eigenvalue", "Variance (%)", "Cumulative (%)")
    datatable(eig, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE, dom = "t")) %>%
      formatStyle("Cumulative (%)",
                  background = styleColorBar(c(0, 100), get_colors()[1]),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left center")
  })
  
  # --- Loadings table (variable correlations with PCs) ---
  output$pcaLoadings <- renderDT({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    ld <- as.data.frame(round(obj$pca$var$coord, 3))
    ld <- cbind(Variable = rownames(ld), ld)
    datatable(ld, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # --- Scores table ---
  output$pcaScores <- renderDT({
    obj <- pca_obj(); if (is.null(obj)) return(NULL)
    sc <- as.data.frame(round(obj$pca$ind$coord, 3))
    sc <- cbind(Row = obj$used_rows, sc)
    if (!is.null(obj$grp)) sc$Group <- obj$grp
    datatable(head(sc, 200), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- Downloads ---
  output$downloadPCAScree <- downloadHandler(
    filename = function() paste0("pca_scree_", Sys.Date(), ".", input$exportFormat),
    content  = function(file) save_gg(pca_scree_obj(), file)
  )
  output$downloadPCABiplot <- downloadHandler(
    filename = function() paste0("pca_score_plot_", Sys.Date(), ".", input$exportFormat),
    content  = function(file) save_gg(pca_biplot_obj(), file)
  )
  output$downloadPCAVarCircle <- downloadHandler(
    filename = function() paste0("pca_variable_circle_", Sys.Date(), ".", input$exportFormat),
    content  = function(file) save_gg(pca_var_circle_obj(), file)
  )
  output$downloadPCAContrib <- downloadHandler(
    filename = function() paste0("pca_contributions_dim", input$pcaContribDim %||% 1,
                                 "_", Sys.Date(), ".", input$exportFormat),
    content  = function(file) save_gg(pca_contrib_obj(), file)
  )
  output$downloadPCALoadings <- downloadHandler(
    filename = function() paste0("pca_loadings_", Sys.Date(), ".csv"),
    content  = function(file) {
      obj <- pca_obj(); if (is.null(obj)) return()
      ld <- as.data.frame(round(obj$pca$var$coord, 4))
      ld$cos2 <- round(rowSums(obj$pca$var$cos2), 4)
      ld <- cbind(Variable = rownames(ld), ld)
      write.csv(ld, file, row.names = FALSE)
    }
  )
  output$downloadPCAScores <- downloadHandler(
    filename = function() paste0("pca_scores_", Sys.Date(), ".csv"),
    content  = function(file) {
      obj <- pca_obj(); if (is.null(obj)) return()
      sc <- as.data.frame(round(obj$pca$ind$coord, 4))
      sc <- cbind(Row = obj$used_rows, sc)
      if (!is.null(obj$grp)) sc$Group <- obj$grp
      write.csv(sc, file, row.names = FALSE)
    }
  )
  
  # ── Regression ───────────────────────────────────────────────────────────────
  output$regYUI <- renderUI({
    req(numeric_data()); selectInput("regY", "Response (Y):",
                                     choices = names(numeric_data()))
  })
  output$regXUI <- renderUI({
    req(working_data())
    selectInput("regX", "Predictors (X):",
                choices  = setdiff(names(working_data()), input$regY %||% ""),
                multiple = TRUE)
  })
  
  reg_fit <- eventReactive(input$fitReg, {
    req(input$regY, input$regX)
    df <- working_data()[, c(input$regY, input$regX), drop = FALSE]
    complete <- complete.cases(df)
    numeric_cols <- vapply(df, is.numeric, logical(1))
    if (any(numeric_cols)) {
      complete <- complete & apply(df[, numeric_cols, drop = FALSE], 1, function(z) all(is.finite(z)))
    }
    df <- df[complete, , drop = FALSE]
    if (nrow(df) < 3)
      return(list(error = "At least three complete, finite observations are required."))
    if (input$regStd) {
      num_predictors <- vapply(df, is.numeric, logical(1)) & names(df) != input$regY
      scalable <- num_predictors & vapply(df, function(x) {
        s <- if (is.numeric(x)) sd(x) else NA_real_
        is.finite(s) && s > 0
      }, logical(1))
      if (any(scalable))
        df[, scalable] <- lapply(df[, scalable, drop = FALSE], function(x) as.numeric(scale(x)))
    }
    original_names <- names(df)
    internal_names <- make.names(original_names, unique = TRUE)
    name_map <- setNames(original_names, internal_names)
    names(df) <- internal_names
    y_internal <- internal_names[match(input$regY, original_names)]
    x_internal <- internal_names[match(input$regX, original_names)]
    fm <- reformulate(x_internal, response = y_internal)
    fit <- tryCatch(lm(fm, data = df), error = function(e) e)
    if (inherits(fit, "error"))
      return(list(error = paste("Model failed to fit:", fit$message)))
    if (df.residual(fit) < 1)
      return(list(error = "The model has no residual degrees of freedom; reduce the predictors or add observations."))
    list(fit = fit, data = df, name_map = name_map, aliased = any(is.na(coef(fit))))
  })

  tidy_regression <- function(rf, conf.int = TRUE) {
    tb <- broom::tidy(rf$fit, conf.int = conf.int)
    map_names <- names(rf$name_map)
    tb$term <- vapply(tb$term, function(term) {
      hits <- map_names[startsWith(term, map_names)]
      if (!length(hits)) return(term)
      hit <- hits[which.max(nchar(hits))]
      paste0(rf$name_map[[hit]], substring(term, nchar(hit) + 1))
    }, character(1))
    tb
  }
  
  output$regCoefs <- renderDT({
    rf <- reg_fit()
    if (!is.null(rf$error)) return(datatable(data.frame(Message = rf$error), rownames = FALSE))
    tb <- tidy_regression(rf, conf.int = TRUE) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4)),
             Sig = sig_stars(p.value))
    datatable(tb, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("p.value", backgroundColor = styleInterval(0.05, c("#d5f5e3", "#fadbd8")))
  })
  
  output$regFitStats <- renderPrint({
    rf <- reg_fit()
    if (!is.null(rf$error)) { cat(rf$error); return() }
    s <- summary(rf$fit); g <- broom::glance(rf$fit)
    cat(sprintf("n                 = %d\n",  nrow(rf$data)))
    cat(sprintf("R²                = %.4f\n", s$r.squared))
    cat(sprintf("Adjusted R²       = %.4f\n", s$adj.r.squared))
    cat(sprintf("Residual SE       = %.4f (df = %d)\n",
                s$sigma, s$df[2]))
    if (!is.null(s$fstatistic)) {
      cat(sprintf("F-statistic       = %.3f on %d and %d df\n",
                  s$fstatistic[1], s$fstatistic[2], s$fstatistic[3]))
      cat(sprintf("Overall p-value   = %.4g\n", g$p.value))
    }
    cat(sprintf("AIC / BIC         = %.1f / %.1f\n", g$AIC, g$BIC))
    if (isTRUE(rf$aliased))
      cat("\nWarning: one or more coefficients are not estimable (collinearity detected).\n")
  })
  
  reg_diag_plot <- reactive({
    rf <- reg_fit(); if (!is.null(rf$error)) return(NULL)
    fit  <- rf$fit
    res  <- residuals(fit); fit_v <- fitted(fit)
    std  <- rstandard(fit);  ck   <- cooks.distance(fit)
    colors <- get_colors()
    
    p1 <- ggplot(data.frame(fit_v, res), aes(x = fit_v, y = res)) +
      geom_point(color = colors[1], alpha = 0.7) +
      geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
      labs(x = "Fitted", y = "Residuals", title = "Residuals vs Fitted") + get_theme()
    if (length(res) >= 5 && length(unique(fit_v)) >= 4)
      p1 <- p1 + geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                             color = colors[2], linewidth = 0.9)
    
    p2 <- ggplot(data.frame(std = std), aes(sample = std)) +
      stat_qq(color = colors[1], alpha = 0.7) +
      stat_qq_line(color = colors[2], linetype = "dashed", linewidth = 0.8) +
      labs(x = "Theoretical Quantiles", y = "Standardised Residuals",
           title = "Normal Q-Q") + get_theme()
    
    p3 <- ggplot(data.frame(fit_v, srs = sqrt(abs(std))), aes(fit_v, srs)) +
      geom_point(color = colors[1], alpha = 0.7) +
      labs(x = "Fitted", y = expression(sqrt("|Std residuals|")),
           title = "Scale-Location") + get_theme()
    if (length(std) >= 5 && length(unique(fit_v)) >= 4)
      p3 <- p3 + geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                             color = colors[2], linewidth = 0.9)
    
    p4 <- ggplot(data.frame(idx = seq_along(ck), ck), aes(x = idx, y = ck)) +
      geom_col(fill = colors[1], width = 0.6) +
      geom_hline(yintercept = 4 / length(ck), color = colors[2], linetype = "dashed", linewidth = 0.8) +
      labs(x = "Observation", y = "Cook's distance",
           title = "Cook's distance") + get_theme()
    
    # Arrange: prefer patchwork (returns a ggplot object ggsave saves cleanly);
    # fall back to gridExtra::arrangeGrob (also saveable via ggsave's `plot` arg);
    # final fallback shows only the residuals plot with a note.
    if (has_patchwork) {
      patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
    } else if (has_gridextra) {
      gridExtra::arrangeGrob(p1, p2, p3, p4, ncol = 2)
    } else {
      p1 + labs(subtitle = "Install 'patchwork' or 'gridExtra' for all four panels")
    }
  })
  
  output$regDiag <- renderPlot({
    p <- reg_diag_plot()
    if (inherits(p, c("ggplot", "patchwork"))) print(p)
    else if (inherits(p, "gtable") || inherits(p, "grob")) grid::grid.draw(p)
    else print(p)
  }, res = 110)
  
  output$downloadRegCoefs <- downloadHandler(
    filename = function() paste0("regression_coefs_", Sys.Date(), ".csv"),
    content  = function(file) {
      rf <- reg_fit(); if (!is.null(rf$error)) return()
      write.csv(tidy_regression(rf, conf.int = TRUE), file, row.names = FALSE)
    }
  )
  output$downloadRegDiag <- downloadHandler(
    filename = function() paste0("regression_diag_", Sys.Date(), ".", input$exportFormat),
    content  = function(file) {
      p <- reg_diag_plot(); if (is.null(p)) return()
      save_gg(p, file)
    }
  )
  
  # ── Data Quality ─────────────────────────────────────────────────────────────
  output$missingTable <- renderDT({
    req(working_data())
    df <- working_data()
    ms <- data.frame(Variable = names(df),
                     Missing_Count   = sapply(df, function(x) sum(is.na(x))),
                     Missing_Percent = round(sapply(df, function(x) mean(is.na(x)) * 100), 2),
                     Complete_Count  = sapply(df, function(x) sum(!is.na(x)))) %>%
      arrange(desc(Missing_Count))
    datatable(ms, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("Missing_Percent",
                  backgroundColor = styleInterval(c(5, 10, 20),
                                                  c("lightgreen", "yellow", "orange", "red")))
  })
  output$outlierTable <- renderDT({
    req(numeric_data())
    validate(need(ncol(numeric_data()) > 0, "No numeric variables are available."))
    os <- bind_rows(lapply(names(numeric_data()), function(v) {
      z <- numeric_data()[[v]]
      z <- z[!is.na(z)]
      if (!length(z))
        return(data.frame(Variable = v, Complete_N = 0, Outlier_Count = 0,
                          Outlier_Percent = NA_real_))
      q <- quantile(z, c(0.25, 0.75), names = FALSE)
      iqr <- q[2] - q[1]
      n_out <- sum(z < q[1] - 1.5 * iqr | z > q[2] + 1.5 * iqr)
      data.frame(Variable = v, Complete_N = length(z), Outlier_Count = n_out,
                 Outlier_Percent = round(100 * n_out / length(z), 2))
    })) %>%
      arrange(desc(Outlier_Count))
    datatable(os, options = list(pageLength = 10), rownames = FALSE) %>%
      formatStyle("Outlier_Percent",
                  backgroundColor = styleInterval(c(1, 5, 10),
                                                  c("lightgreen", "yellow", "orange", "red")))
  })
  output$normalityTable <- renderDT({
    req(numeric_data())
    validate(need(ncol(numeric_data()) > 0, "No numeric variables are available."))
    nt <- bind_rows(lapply(names(numeric_data()), function(v) {
      x <- numeric_data()[[v]]
      x <- x[!is.na(x)]
      n <- length(x)
      pval <- if (n >= 3 && n <= 5000 && length(unique(x)) > 1) {
        tryCatch(shapiro.test(x)$p.value, error = function(e) NA_real_)
      } else NA_real_
      data.frame(
        Variable = v, N = n, Shapiro_p = pval,
        Skewness = if (n >= 3 && length(unique(x)) > 1) moments::skewness(x) else NA_real_,
        Kurtosis = if (n >= 4 && length(unique(x)) > 1) moments::kurtosis(x) else NA_real_
      )
    })) %>%
      mutate(
        Shapiro_p_Holm = p.adjust(Shapiro_p, method = "holm"),
        Interpretation = case_when(
          is.na(Shapiro_p) ~ "Not tested (requires 3–5000 non-constant values)",
          Shapiro_p_Holm < 0.05 ~ "Evidence against normality",
          TRUE ~ "No evidence against normality"
        ),
        across(c(Shapiro_p, Shapiro_p_Holm, Skewness, Kurtosis), ~ round(.x, 4))
      )
    datatable(nt, options = list(pageLength = 10), rownames = FALSE) %>%
      formatStyle(
        "Interpretation",
        backgroundColor = styleEqual(
          c("No evidence against normality", "Evidence against normality",
            "Not tested (requires 3–5000 non-constant values)"),
          c("lightgreen", "lightcoral", "lightyellow")
        )
      ) %>%
      formatStyle("Shapiro_p_Holm",
                  backgroundColor = styleInterval(0.05, c("lightcoral", "lightgreen")))
  })
  output$missingBarPlot <- renderPlot({
    req(working_data())
    md <- data.frame(Variable = names(working_data()),
                     Missing_Percent = sapply(working_data(), function(x) mean(is.na(x)) * 100)) %>%
      arrange(desc(Missing_Percent)) %>% filter(Missing_Percent > 0)
    if (nrow(md) == 0) {
      ggplot() + annotate("text", 0.5, 0.5, label = "No missing values.",
                          size = 8, color = "darkgreen", fontface = "bold") + theme_void()
    } else {
      ggplot(md, aes(reorder(Variable, Missing_Percent), Missing_Percent, fill = Missing_Percent)) +
        geom_col(alpha = 0.85, width = 0.7) +
        geom_text(aes(label = sprintf("%.1f%%", Missing_Percent)),
                  hjust = -0.1, size = input$baseFontSize / 3.2, fontface = "bold") +
        coord_flip() +
        scale_fill_gradient(low = get_colors()[3], high = get_colors()[1], name = "Missing (%)") +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
        labs(x = NULL, y = "Missing (%)", title = "Missing data by variable") +
        get_theme()
    }
  })
  
  # ── Data preview & downloads ────────────────────────────────────────────────
  output$dataPreviewTable <- renderDT({
    req(raw_data())
    datatable(raw_data(),
              options  = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"),
              rownames = TRUE, class = "cell-border stripe")
  })
  
  output$downloadStats <- downloadHandler(
    filename = function() paste0("statistics_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(summary_stats(), file, row.names = FALSE)
  )
  output$downloadExt <- downloadHandler(
    filename = function() paste0("extended_stats_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(extended_stats(), file, row.names = FALSE)
  )
  
  output$downloadCorr <- downloadHandler(
    filename = function() paste0("correlation_", input$colorPalette, "_",
                                 input$plotDPI, "dpi_", Sys.Date(), ".",
                                 input$exportFormat),
    content = function(file) {
      fmt <- input$exportFormat
      dev_fun <- switch(fmt,
                        "png"  = function() png(file, width = input$exportWidth,
                                                height = input$exportHeight,
                                                units = "in", res = input$plotDPI, bg = "white"),
                        "pdf"  = function() pdf(file, width = input$exportWidth,
                                                height = input$exportHeight, bg = "white"),
                        "tiff" = function() tiff(file, width = input$exportWidth,
                                                 height = input$exportHeight,
                                                 units = "in", res = input$plotDPI, bg = "white",
                                                 compression = "lzw"),
                        "svg"  = function() grDevices::svg(
                          file, width = input$exportWidth, height = input$exportHeight,
                          bg = "white"
                        ))
      dev_fun(); on.exit(dev.off()); draw_corrplot(corr_objs())
    }
  )
  output$downloadCorrCSV <- downloadHandler(
    filename = function() paste0("correlation_rp_", Sys.Date(), ".csv"),
    content  = function(file) {
      co <- corr_objs()
      r  <- as.data.frame(round(co$mat,  4)); r$`_` <- "r";   r$Variable <- rownames(r)
      p  <- as.data.frame(round(co$pmat, 4)); p$`_` <- "p";   p$Variable <- rownames(p)
      out <- rbind(r, p)
      out <- out[, c("Variable", "_", setdiff(names(out), c("Variable", "_")))]
      write.csv(out, file, row.names = FALSE)
    }
  )
  output$downloadScatter <- downloadHandler(
    filename = function() paste0("scatter_matrix_", input$colorPalette, "_",
                                 input$plotDPI, "dpi_", Sys.Date(), ".",
                                 input$exportFormat),
    content  = function(file) save_gg(scatter_plot_obj(), file)
  )
  
  # Analysis bundle: separate rectangular CSV files are safer than stacking
  # incompatible tables into a single malformed CSV.
  output$downloadReport <- downloadHandler(
    filename = function() paste0("analysis_bundle_", Sys.Date(), ".zip"),
    content  = function(file) {
      bundle_dir <- tempfile("analysis_bundle_")
      dir.create(bundle_dir)
      on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)

      write.csv(working_data(), file.path(bundle_dir, "processed_data.csv"), row.names = FALSE)
      write.csv(summary_stats(), file.path(bundle_dir, "descriptive_statistics.csv"), row.names = FALSE)
      if (!is.null(input$variables) && length(input$variables))
        write.csv(extended_stats(), file.path(bundle_dir, "extended_statistics.csv"), row.names = FALSE)
      missing_table <- data.frame(
        Variable        = names(working_data()),
        Missing_Count   = sapply(working_data(), function(x) sum(is.na(x))),
        Missing_Percent = round(sapply(working_data(), function(x) mean(is.na(x)) * 100), 2)
      )
      write.csv(missing_table, file.path(bundle_dir, "missing_values.csv"), row.names = FALSE)
      writeLines(
        c(
          "Publication Statistics Dashboard analysis bundle",
          paste("Created:", Sys.time()),
          paste("Rows in processed data:", nrow(working_data())),
          paste("Columns in processed data:", ncol(working_data())),
          "",
          "Statistical results should be interpreted alongside study design and model assumptions."
        ),
        file.path(bundle_dir, "README.txt")
      )
      old_dir <- setwd(bundle_dir)
      on.exit(setwd(old_dir), add = TRUE)
      utils::zip(file, files = list.files(bundle_dir), flags = "-j")
    }
  )
}

# =============================================================================
shinyApp(ui = ui, server = server)
