#' Correlation methods offered on the Correlations tab
#'
#' @return Named character vector for `selectInput(choices = )`.
#' @export
corr_method_choices <- function() {
  c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")
}

#' Correlation matrix orderings offered on the Correlations tab
#'
#' @return Named character vector for `selectInput(choices = )`.
#' @export
corr_order_choices <- function() {
  c("Hierarchical cluster" = "hclust",
    "Original"             = "original",
    "Alphabetical"         = "alphabet",
    "AOE (angular)"        = "AOE")
}

#' Drop columns that cannot be correlated
#'
#' Columns with fewer than three observations or with zero variance produce
#' `NA` correlations and can break the clustering used for matrix ordering.
#'
#' @param df A data frame of numeric columns.
#' @return `df` restricted to usable columns.
#' @export
drop_unusable_for_cor <- function(df) {
  keep <- vapply(df, function(x) {
    z <- x[!is.na(x)]
    length(z) >= 3 && is.finite(stats::var(z)) && stats::var(z) > 0
  }, logical(1))
  df[, keep, drop = FALSE]
}

#' Build the correlation and p-value matrices for the Correlations tab
#'
#' @param df A data frame of numeric columns.
#' @param method One of [corr_method_choices()].
#' @param holm Holm-adjust the p-values across all unique pairs?
#' @return A list with `mat` (correlations) and `pmat` (p-values).
#' @export
corr_objects <- function(df, method = "pearson", holm = TRUE) {
  cp <- cor_pmat(as.matrix(df), method = method)
  if (isTRUE(holm)) cp$p <- holm_adjust_pmat(cp$p)
  list(mat = cp$r, pmat = cp$p)
}

#' Draw the correlation matrix
#'
#' Base graphics, so this is called for its side effect rather than returning
#' an object; exporting it goes through [open_export_device()].
#'
#' @param obj A list as produced by [corr_objects()].
#' @param method Correlation method, used only in the title.
#' @param order One of [corr_order_choices()].
#' @param colors A colour palette, see [get_palette()].
#' @param font_size Base font size, scaled down for labels and coefficients.
#' @param show_sig Mark non-significant cells?
#' @return Invisibly `NULL`.
#' @export
draw_corrplot <- function(obj, method = "pearson", order = "hclust",
                          colors = get_palette("Nature"), font_size = 13,
                          show_sig = TRUE) {
  corrplot::corrplot(
    obj$mat,
    method      = "color",
    type        = "upper",
    order       = order,
    addCoef.col = "black",
    tl.col      = "black",
    tl.srt      = 45,
    tl.cex      = font_size / 11,
    number.cex  = font_size / 13,
    col         = grDevices::colorRampPalette(c(colors[1], "white", colors[2]))(200),
    mar         = c(0, 0, 2, 0),
    title       = paste(toupper(method), "Correlation"),
    cl.cex      = font_size / 11,
    cl.pos      = "r",
    addgrid.col = "gray80",
    p.mat       = if (isTRUE(show_sig)) obj$pmat else NULL,
    sig.level   = if (isTRUE(show_sig)) c(0.001, 0.01, 0.05) else NULL,
    insig       = if (isTRUE(show_sig)) "label_sig" else "pch",
    pch.cex     = 0.9,
    pch.col     = "grey30"
  )
  invisible(NULL)
}

#' Flatten the correlation and p-value matrices into one exportable table
#'
#' @param obj A list as produced by [corr_objects()].
#' @return A long data frame with a `Statistic` column marking `r` and `p`
#'   blocks.
#' @export
corr_export_table <- function(obj) {
  r <- as.data.frame(round(obj$mat, 4))
  p <- as.data.frame(round(obj$pmat, 4))
  r <- cbind(Variable = rownames(r), Statistic = "r", r)
  p <- cbind(Variable = rownames(p), Statistic = "p", p)
  out <- rbind(r, p)
  rownames(out) <- NULL
  out
}

#' Scatter plot matrix
#'
#' @param df A data frame of numeric columns.
#' @param colors A colour palette, see [get_palette()].
#' @param gg_theme A ggplot2 theme, see [build_plot_theme()].
#' @param font_size Base font size.
#' @return A `ggmatrix` object.
#' @export
scatter_matrix_plot <- function(df, colors = get_palette("Nature"),
                                gg_theme = build_plot_theme(), font_size = 13) {
  GGally::ggpairs(
    df,
    lower = list(continuous = GGally::wrap("points", alpha = 0.5, size = 1.5,
                                           color = colors[1])),
    diag  = list(continuous = GGally::wrap("densityDiag", fill = colors[1],
                                           alpha = 0.7, color = colors[1],
                                           linewidth = 0.8)),
    upper = list(continuous = GGally::wrap("cor", size = font_size / 2.8,
                                           color = "black")),
    title = "Scatter Plot Matrix"
  ) +
    gg_theme +
    theme(plot.title = element_text(hjust = 0.5, size = font_size + 3),
          strip.text = element_text(size = font_size))
}

#' Pairwise scatter plot with optional regression line and equation
#'
#' @param df The working data frame.
#' @param x_var,y_var Numeric column names for the axes.
#' @param color_var Optional column to colour by; numeric columns get a
#'   continuous viridis scale, others a discrete palette.
#' @param show_regression Add an OLS line with a confidence band?
#' @param show_equation Annotate the slope, intercept and R-squared?
#' @param colors A colour palette, see [get_palette()].
#' @param gg_theme A ggplot2 theme, see [build_plot_theme()].
#' @param font_size Base font size.
#' @return A ggplot object.
#' @export
pairwise_scatter_plot <- function(df, x_var, y_var, color_var = NULL,
                                  show_regression = TRUE, show_equation = TRUE,
                                  colors = get_palette("Nature"),
                                  gg_theme = build_plot_theme(),
                                  font_size = 13) {
  d <- data.frame(x = df[[x_var]], y = df[[y_var]])

  use_color <- !is.null(color_var) && !identical(color_var, "None") &&
    color_var %in% names(df) && any(!is.na(df[[color_var]]))

  if (use_color) {
    d$color_var <- df[[color_var]]
    if (is.numeric(d$color_var)) {
      p <- ggplot(d, aes(x = .data$x, y = .data$y, color = .data$color_var)) +
        geom_point(alpha = 0.7, size = 2.5) +
        scale_color_viridis_c(option = "viridis", name = color_var)
    } else {
      d$color_var <- droplevels(as.factor(d$color_var))
      p <- ggplot(d, aes(x = .data$x, y = .data$y, color = .data$color_var)) +
        geom_point(alpha = 0.7, size = 2.5) +
        scale_color_manual(values = expand_palette(colors, nlevels(d$color_var)),
                           name = color_var)
    }
  } else {
    p <- ggplot(d, aes(x = .data$x, y = .data$y)) +
      geom_point(color = colors[1], alpha = 0.7, size = 2.5)
  }

  if (isTRUE(show_regression)) {
    p <- p + geom_smooth(data = d, aes(x = .data$x, y = .data$y, group = 1),
                         inherit.aes = FALSE, method = "lm", formula = y ~ x,
                         se = TRUE, color = colors[2], fill = colors[2],
                         linewidth = 1.1, alpha = 0.15)
  }

  if (isTRUE(show_equation) && isTRUE(show_regression)) {
    cc <- stats::complete.cases(d[, c("x", "y")])
    if (sum(cc) >= 3 && length(unique(d$x[cc])) > 1) {
      m  <- stats::lm(y ~ x, data = d[cc, ])
      r2 <- summary(m)$r.squared
      cf <- stats::coef(m)
      eq <- sprintf("y = %.3fx + %.3f\nR2 = %.3f", cf[2], cf[1], r2)
      xpos <- min(d$x, na.rm = TRUE) + 0.05 * diff(range(d$x, na.rm = TRUE))
      ypos <- max(d$y, na.rm = TRUE)
      p <- p + annotate("label", x = xpos, y = ypos, label = eq,
                        hjust = 0, vjust = 1, size = font_size / 3.3,
                        fontface = "bold", fill = scales::alpha("white", 0.85),
                        linewidth = 0.3)
    }
  }

  p +
    labs(x = x_var, y = y_var, title = paste(y_var, "vs.", x_var)) +
    gg_theme
}
