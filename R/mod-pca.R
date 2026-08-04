#' PCA tab module
#'
#' Scree plot, score plot, correlation circle, contribution bars, and the
#' eigenvalue / loadings / scores tables, all built on \pkg{FactoMineR} and
#' \pkg{factoextra}.
#'
#' @param id Module id.
#' @param working_data A reactive returning the processed data frame.
#' @param numeric_data A reactive returning its numeric columns.
#' @param cat_vars A reactive returning the names of categorical columns.
#' @param settings The list returned by [mod_settings_server()].
#' @return `mod_pca_ui()` returns a `tabItem`. `mod_pca_server()` is called for
#'   its side effects.
#' @name mod_pca
NULL

#' @rdname mod_pca
#' @export
mod_pca_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "pca",
    fluidRow(
      box(title = "PCA Setup", width = 12, status = "primary", solidHeader = TRUE,
          column(3, uiOutput(ns("pcaVarsUI"))),
          column(3, uiOutput(ns("pcaGroupUI"))),
          column(2, numericInput(ns("pcaX"), "PC on X:", value = 1, min = 1, max = 10)),
          column(2, numericInput(ns("pcaY"), "PC on Y:", value = 2, min = 1, max = 10)),
          column(2,
                 checkboxInput(ns("pcaScale"), "Scale variables", TRUE),
                 checkboxInput(ns("pcaEllipse"), "Confidence ellipses", TRUE),
                 checkboxInput(ns("pcaRepel"), "Repel labels", TRUE))
      )
    ),
    fluidRow(
      box(title = "Scree Plot (Eigenvalues)", width = 6, status = "info",
          solidHeader = TRUE,
          plotOutput(ns("pcaScree"), height = 380),
          downloadButton(ns("downloadPCAScree"), "Download Scree Plot")),
      box(title = "PCA Score Plot", width = 6, status = "info", solidHeader = TRUE,
          plotOutput(ns("pcaBiplot"), height = 380),
          downloadButton(ns("downloadPCABiplot"), "Download Score Plot"))
    ),
    fluidRow(
      box(title = "Variables - Correlation Circle", width = 6, status = "success",
          solidHeader = TRUE,
          plotOutput(ns("pcaVarCircle"), height = 380),
          downloadButton(ns("downloadPCAVarCircle"), "Download Variable Plot")),
      box(title = "Variable Contributions", width = 6, status = "success",
          solidHeader = TRUE,
          uiOutput(ns("pcaContribDimUI")),
          plotOutput(ns("pcaContrib"), height = 340),
          downloadButton(ns("downloadPCAContrib"), "Download Contribution Plot"))
    ),
    fluidRow(
      box(title = "Eigenvalue Table", width = 4, status = "warning",
          solidHeader = TRUE, DT::DTOutput(ns("pcaEigen"))),
      box(title = "Variable Loadings (Correlations)", width = 4, status = "warning",
          solidHeader = TRUE,
          DT::DTOutput(ns("pcaLoadings")),
          br(),
          downloadButton(ns("downloadPCALoadings"), "Download Loadings CSV")),
      box(title = "Individual Scores", width = 4, status = "warning",
          solidHeader = TRUE,
          DT::DTOutput(ns("pcaScores")),
          br(),
          downloadButton(ns("downloadPCAScores"), "Download Full Scores CSV"))
    )
  )
}

#' @rdname mod_pca
#' @export
mod_pca_server <- function(id, working_data, numeric_data, cat_vars, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$pcaVarsUI <- renderUI({
      req(numeric_data())
      nd <- numeric_data()
      previous <- isolate(input$pcaVars)
      selected <- intersect(previous %||% character(), names(nd))
      if (length(selected) < 2) selected <- names(nd)
      selectInput(ns("pcaVars"), "PCA variables:", choices = names(nd),
                  selected = selected, multiple = TRUE)
    })

    output$pcaGroupUI <- renderUI({
      selectInput(ns("pcaGroup"), "Color / group by:",
                  choices = c("None", cat_vars()), selected = "None")
    })

    pca_obj <- reactive({
      req(input$pcaVars, numeric_data())
      res <- fit_pca(numeric_data(), input$pcaVars, scale_unit = isTRUE(input$pcaScale))
      validate(need(is.null(res$error), res$error %||% ""))

      grp <- NULL
      if (!is.null(input$pcaGroup) && !identical(input$pcaGroup, "None")) {
        grp_raw <- as.character(working_data()[[input$pcaGroup]][res$used_rows])
        grp_raw[is.na(grp_raw) | !nzchar(grp_raw)] <- "(Missing)"
        grp <- droplevels(as.factor(grp_raw))
      }

      res$grp <- grp
      res$grp_counts <- if (!is.null(grp)) table(grp) else NULL
      res
    })

    axes <- reactive({
      obj <- pca_obj()
      npc <- ncol(obj$pca$ind$coord)
      validate(need(npc >= 2,
                    "At least two principal components are required for this plot."))
      pca_axes(npc, input$pcaX, input$pcaY)
    })

    output$pcaContribDimUI <- renderUI({
      obj <- pca_obj()
      npc <- ncol(obj$pca$ind$coord)
      selectInput(ns("pcaContribDim"), "Show contributions for:",
                  choices = stats::setNames(seq_len(npc), paste0("Dim ", seq_len(npc))),
                  selected = 1)
    })

    pca_scree_obj <- reactive({
      factoextra::fviz_screeplot(
        pca_obj()$pca, addlabels = TRUE,
        barfill   = settings$colors()[1],
        barcolor  = settings$colors()[1],
        linecolor = settings$colors()[2]
      ) +
        labs(title = "Scree Plot - Variance Explained",
             x = "Principal Component", y = "% Variance Explained") +
        settings$gg_theme()
    })

    pca_biplot_obj <- reactive({
      obj <- pca_obj()
      has_grp <- !is.null(obj$grp)

      # Ellipses need at least three points per group; below that ggplot2
      # floods the console with drop-warnings and draws nothing useful.
      ellipse_ok <- FALSE
      if (has_grp && isTRUE(input$pcaEllipse) && !is.null(obj$grp_counts)) {
        if (all(obj$grp_counts >= 3)) {
          ellipse_ok <- TRUE
        } else {
          showNotification(
            sprintf("Ellipses disabled: %d group(s) have < 3 observations.",
                    sum(obj$grp_counts < 3)),
            type = "warning", duration = 5
          )
        }
      }

      p <- suppressWarnings(factoextra::fviz_pca_ind(
        obj$pca, axes = axes(),
        geom.ind      = "point",
        col.ind       = if (has_grp) obj$grp else settings$colors()[1],
        addEllipses   = ellipse_ok,
        ellipse.type  = "confidence",
        ellipse.level = 0.95,
        palette       = if (has_grp) {
          expand_palette(settings$colors(), nlevels(obj$grp))
        } else {
          settings$colors()
        },
        pointsize    = 2.2,
        alpha.ind    = 0.7,
        legend.title = if (has_grp) input$pcaGroup else "",
        repel        = isTRUE(input$pcaRepel),
        title        = "PCA Score Plot"
      ))
      p + settings$gg_theme()
    })

    pca_var_circle_obj <- reactive({
      factoextra::fviz_pca_var(
        pca_obj()$pca, axes = axes(),
        col.var       = "contrib",
        gradient.cols = c(settings$colors()[3], settings$colors()[1],
                          settings$colors()[2]),
        repel = isTRUE(input$pcaRepel),
        title = "Variable Correlation Circle"
      ) + settings$gg_theme()
    })

    pca_contrib_obj <- reactive({
      dim_idx <- as.integer(input$pcaContribDim %||% 1)
      factoextra::fviz_contrib(
        pca_obj()$pca, choice = "var", axes = dim_idx,
        fill = settings$colors()[1], color = settings$colors()[1]
      ) +
        labs(title = paste("Variable Contributions to Dim", dim_idx)) +
        settings$gg_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$pcaScree     <- renderPlot({ pca_scree_obj() }, res = 110)
    output$pcaBiplot    <- renderPlot({ pca_biplot_obj() }, res = 110)
    output$pcaVarCircle <- renderPlot({ pca_var_circle_obj() }, res = 110)
    output$pcaContrib   <- renderPlot({ pca_contrib_obj() }, res = 110)

    output$pcaEigen <- DT::renderDT({
      DT::formatStyle(
        DT::datatable(pca_eigen_table(pca_obj()$pca), rownames = FALSE,
                      options = list(pageLength = 10, scrollX = TRUE, dom = "t")),
        "Cumulative (%)",
        background = DT::styleColorBar(c(0, 100), settings$colors()[1]),
        backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
        backgroundPosition = "left center"
      )
    })

    loadings_table <- reactive({
      ld <- as.data.frame(round(pca_obj()$pca$var$coord, 4))
      ld$cos2 <- round(rowSums(pca_obj()$pca$var$cos2), 4)
      cbind(Variable = rownames(ld), ld)
    })

    scores_table <- reactive({
      obj <- pca_obj()
      sc <- as.data.frame(round(obj$pca$ind$coord, 4))
      sc <- cbind(Row = obj$used_rows, sc)
      if (!is.null(obj$grp)) sc$Group <- obj$grp
      sc
    })

    output$pcaLoadings <- DT::renderDT({
      DT::datatable(loadings_table(), rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    output$pcaScores <- DT::renderDT({
      DT::datatable(utils::head(scores_table(), 200), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # ---- Downloads -----------------------------------------------------------
    plot_download <- function(stem, plot_reactive) {
      downloadHandler(
        filename = function() {
          paste0(stem, "_", Sys.Date(), ".", settings$export()$format)
        },
        content = function(file) settings$save(plot_reactive(), file)
      )
    }

    output$downloadPCAScree     <- plot_download("pca_scree", pca_scree_obj)
    output$downloadPCABiplot    <- plot_download("pca_score_plot", pca_biplot_obj)
    output$downloadPCAVarCircle <- plot_download("pca_variable_circle", pca_var_circle_obj)

    output$downloadPCAContrib <- downloadHandler(
      filename = function() {
        paste0("pca_contributions_dim", input$pcaContribDim %||% 1, "_",
               Sys.Date(), ".", settings$export()$format)
      },
      content = function(file) settings$save(pca_contrib_obj(), file)
    )

    output$downloadPCALoadings <- downloadHandler(
      filename = function() paste0("pca_loadings_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(loadings_table(), file, row.names = FALSE)
    )

    output$downloadPCAScores <- downloadHandler(
      filename = function() paste0("pca_scores_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(scores_table(), file, row.names = FALSE)
    )
  })
}
