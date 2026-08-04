#' Distribution plot types
#'
#' @return Named character vector for `radioButtons(choices = )`.
#' @export
dist_plot_choices <- function() {
  c("Histogram" = "hist",
    "Density"   = "density",
    "Boxplot"   = "box",
    "Violin"    = "violin",
    "Q-Q Plot"  = "qq")
}

#' Default options for the distribution plots
#'
#' @param ... Values overriding the defaults.
#' @return A list of plot options.
#' @export
dist_options <- function(...) {
  defaults <- list(
    plot_type     = "hist",
    show_stats    = TRUE,
    show_ci       = FALSE,
    show_outliers = TRUE,
    group_var     = NULL,
    colors        = get_palette("Nature"),
    gg_theme      = build_plot_theme(),
    font_size     = 13
  )
  utils::modifyList(defaults, list(...))
}

#' Single-variable distribution plot
#'
#' @param df The working data frame.
#' @param var Name of the numeric column to plot.
#' @param opts Options as built by [dist_options()].
#' @return A ggplot object; a [message_plot()] when the variable cannot be
#'   plotted.
#' @export
#' @examples
#' build_dist_plot(mtcars, "mpg", dist_options(plot_type = "density"))
build_dist_plot <- function(df, var, opts = dist_options()) {
  vals <- df[[var]]
  vals_clean <- vals[!is.na(vals)]

  if (!length(vals_clean)) {
    return(message_plot(paste(var, "has no non-missing values.")))
  }
  if (identical(opts$plot_type, "density") && length(unique(vals_clean)) < 2) {
    return(message_plot(paste(var, "is constant; density cannot be estimated.")))
  }

  colors <- opts$colors
  fs     <- opts$font_size

  mean_val   <- mean(vals_clean)
  median_val <- stats::median(vals_clean)
  sd_val     <- stats::sd(vals_clean)
  q1 <- stats::quantile(vals_clean, 0.25, names = FALSE)
  q3 <- stats::quantile(vals_clean, 0.75, names = FALSE)
  iqr_val <- q3 - q1
  n_out <- sum(vals_clean < q1 - 1.5 * iqr_val | vals_clean > q3 + 1.5 * iqr_val)

  gvar <- opts$group_var
  use_group <- !is.null(gvar) && !identical(gvar, "None") && gvar %in% names(df) &&
    any(!is.na(df[[gvar]][!is.na(vals)]))
  grp <- if (use_group) droplevels(as.factor(df[[gvar]][!is.na(vals)])) else NULL
  group_colors <- if (use_group) expand_palette(colors, nlevels(grp)) else colors

  outlier_shape <- if (isTRUE(opts$show_outliers)) 19 else NA

  p <- switch(
    opts$plot_type,

    "hist" = {
      if (use_group) {
        ggplot(data.frame(x = vals_clean, grp = grp), aes(x = .data$x, fill = .data$grp)) +
          geom_histogram(bins = 30, color = "white", alpha = 0.75,
                         linewidth = 0.3, position = "identity") +
          scale_fill_manual(values = group_colors, name = gvar) +
          labs(x = var, y = "Frequency", title = paste("Histogram:", var))
      } else {
        p0 <- ggplot(data.frame(x = vals_clean), aes(x = .data$x)) +
          geom_histogram(bins = 30, fill = colors[1], color = "white",
                         alpha = 0.85, linewidth = 0.3) +
          labs(x = var, y = "Frequency", title = paste("Histogram:", var))
        if (isTRUE(opts$show_stats)) {
          lbl <- sprintf("Mean = %.2f\nMedian = %.2f\nSD = %.2f\nn = %d",
                         mean_val, median_val, sd_val, length(vals_clean))
          p0 <- p0 + annotate("label", x = Inf, y = Inf, label = lbl,
                              hjust = 1.05, vjust = 1.05, size = fs / 3.4,
                              fill = scales::alpha("white", 0.85),
                              linewidth = 0.3, fontface = "bold")
        }
        p0
      }
    },

    "density" = {
      if (use_group) {
        ggplot(data.frame(x = vals_clean, grp = grp),
               aes(x = .data$x, fill = .data$grp, color = .data$grp)) +
          geom_density(alpha = 0.45, linewidth = 1) +
          scale_fill_manual(values = group_colors, name = gvar) +
          scale_color_manual(values = group_colors, name = gvar) +
          labs(x = var, y = "Density", title = paste("Density:", var))
      } else {
        p0 <- ggplot(data.frame(x = vals_clean), aes(x = .data$x)) +
          geom_density(fill = colors[1], color = colors[1],
                       alpha = 0.55, linewidth = 1.1) +
          geom_vline(xintercept = mean_val, color = colors[2],
                     linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = median_val, color = colors[3],
                     linetype = "dotted", linewidth = 1) +
          labs(x = var, y = "Density", title = paste("Density:", var))
        if (isTRUE(opts$show_ci)) {
          n <- length(vals_clean)
          ci_half <- if (n > 1) stats::qt(0.975, df = n - 1) * sd_val / sqrt(n) else NA_real_
          if (is.finite(ci_half)) {
            p0 <- p0 + annotate("rect",
                                xmin = mean_val - ci_half, xmax = mean_val + ci_half,
                                ymin = 0, ymax = Inf, alpha = 0.12, fill = colors[4])
          }
        }
        p0
      }
    },

    "box" = {
      if (use_group) {
        ggplot(data.frame(y = vals_clean, grp = grp),
               aes(x = .data$grp, y = .data$y, fill = .data$grp)) +
          geom_boxplot(alpha = 0.8, outlier.size = 1.8, linewidth = 0.6,
                       outlier.shape = outlier_shape) +
          scale_fill_manual(values = group_colors, name = gvar) +
          labs(x = gvar, y = var, title = paste("Boxplot:", var))
      } else {
        p0 <- ggplot(data.frame(y = vals_clean, x = ""), aes(x = .data$x, y = .data$y)) +
          geom_boxplot(fill = colors[1], alpha = 0.8, color = "black",
                       outlier.color = colors[2], outlier.size = 2,
                       outlier.shape = outlier_shape, width = 0.45) +
          stat_summary(fun = mean, geom = "point", shape = 23,
                       size = 3, fill = colors[3], color = "black") +
          labs(y = var, x = NULL, title = paste("Boxplot:", var)) +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        if (isTRUE(opts$show_stats) && n_out > 0) {
          p0 <- p0 + annotate(
            "text", x = 1.35, y = max(vals_clean),
            label = sprintf("%d outliers (%.1f%%)",
                            n_out, n_out / length(vals_clean) * 100),
            color = colors[2], fontface = "bold", size = fs / 3.5
          )
        }
        p0
      }
    },

    "violin" = {
      if (use_group) {
        ggplot(data.frame(y = vals_clean, grp = grp),
               aes(x = .data$grp, y = .data$y, fill = .data$grp)) +
          geom_violin(alpha = 0.7, trim = FALSE, linewidth = 0.5) +
          geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA,
                       linewidth = 0.4) +
          scale_fill_manual(values = group_colors, name = gvar) +
          labs(x = gvar, y = var, title = paste("Violin:", var))
      } else {
        ggplot(data.frame(y = vals_clean, x = ""), aes(x = .data$x, y = .data$y)) +
          geom_violin(fill = colors[1], alpha = 0.75, color = "black",
                      trim = FALSE, linewidth = 0.6) +
          geom_boxplot(width = 0.12, fill = "white", color = "black",
                       outlier.color = colors[2], outlier.size = 1.5) +
          labs(y = var, x = NULL, title = paste("Violin:", var)) +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
    },

    "qq" = {
      qq_df <- stats::qqnorm(vals_clean, plot.it = FALSE)
      ggplot(data.frame(theoretical = qq_df$x, sample = qq_df$y),
             aes(x = .data$theoretical, y = .data$sample)) +
        geom_point(color = colors[1], size = 1.8, alpha = 0.75) +
        geom_qq_line(data = data.frame(x = vals_clean), aes(sample = .data$x),
                     color = colors[2], linewidth = 1, linetype = "dashed",
                     inherit.aes = FALSE) +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
             title = paste("Q-Q Plot:", var))
    },

    message_plot(paste("Unknown plot type:", opts$plot_type))
  )

  p + opts$gg_theme
}

#' Faceted distribution plot across several variables
#'
#' @param df The working data frame.
#' @param variables Numeric columns to include.
#' @param facet_by Optional categorical column adding a second facet
#'   dimension.
#' @param opts Options as built by [dist_options()].
#' @return A ggplot object.
#' @export
build_faceted_plot <- function(df, variables, facet_by = NULL,
                               opts = dist_options()) {
  use_facet_var <- !is.null(facet_by) && !identical(facet_by, "None") &&
    facet_by %in% names(df)

  keep_cols <- unique(c(variables, if (use_facet_var) facet_by))
  df_long <- tidyr::pivot_longer(
    df[, keep_cols, drop = FALSE],
    cols = tidyselect::all_of(variables),
    names_to = "Variable", values_to = "Value"
  )
  if (use_facet_var) df_long$FacetVar <- as.factor(df_long[[facet_by]])

  colors <- expand_palette(opts$colors, length(variables))

  p <- switch(
    opts$plot_type,
    "hist" = ggplot(df_long, aes(x = .data$Value, fill = .data$Variable)) +
      geom_histogram(bins = 30, alpha = 0.8, color = "white", linewidth = 0.3) +
      scale_fill_manual(values = colors),
    "density" = ggplot(df_long, aes(x = .data$Value, fill = .data$Variable,
                                    color = .data$Variable)) +
      geom_density(alpha = 0.5, linewidth = 1.1) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors),
    "box" = ggplot(df_long, aes(x = .data$Variable, y = .data$Value,
                                fill = .data$Variable)) +
      geom_boxplot(alpha = 0.8, outlier.size = 1.5, linewidth = 0.5) +
      scale_fill_manual(values = colors),
    "violin" = ggplot(df_long, aes(x = .data$Variable, y = .data$Value,
                                   fill = .data$Variable)) +
      geom_violin(alpha = 0.8, trim = FALSE, linewidth = 0.5) +
      geom_boxplot(width = 0.12, alpha = 0.6, outlier.shape = NA,
                   fill = "white", linewidth = 0.4) +
      scale_fill_manual(values = colors),
    "qq" = ggplot(df_long, aes(sample = .data$Value, color = .data$Variable)) +
      stat_qq(alpha = 0.7, size = 1.4, na.rm = TRUE) +
      stat_qq_line(linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
      scale_color_manual(values = colors) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles"),
    return(message_plot(paste("Unknown plot type:", opts$plot_type)))
  )

  facets <- if (use_facet_var) vars(.data$Variable, .data$FacetVar) else vars(.data$Variable)
  p +
    facet_wrap(facets, scales = "free", ncol = 3) +
    opts$gg_theme +
    labs(title = paste("Distributions:", toupper(opts$plot_type)))
}
