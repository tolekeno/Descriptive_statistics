#' Fit a linear model from user-selected columns
#'
#' Column names chosen in the UI are frequently non-syntactic (`"Grain yield
#' (t/ha)"`), so they are mapped to syntactic names before building the
#' formula and mapped back when the coefficients are tidied.
#'
#' @param df A data frame containing the response and predictors.
#' @param y_var Name of the response column.
#' @param x_vars Names of the predictor columns.
#' @param standardise Standardise numeric predictors (not the response) to
#'   mean 0, SD 1?
#' @return Either `list(error = <message>)`, or a list with:
#'   \describe{
#'     \item{fit}{The `lm` object.}
#'     \item{data}{The complete-case model frame actually used.}
#'     \item{name_map}{Named character vector mapping syntactic names back to
#'       the originals.}
#'     \item{aliased}{`TRUE` when some coefficients were not estimable.}
#'   }
#' @export
#' @examples
#' fit_regression(mtcars, "mpg", c("wt", "hp"))$fit
fit_regression <- function(df, y_var, x_vars, standardise = FALSE) {
  df <- df[, c(y_var, x_vars), drop = FALSE]

  complete <- stats::complete.cases(df)
  numeric_cols <- vapply(df, is.numeric, logical(1))
  if (any(numeric_cols)) {
    finite_rows <- apply(df[, numeric_cols, drop = FALSE], 1,
                         function(z) all(is.finite(z)))
    complete <- complete & finite_rows
  }
  df <- df[complete, , drop = FALSE]

  if (nrow(df) < 3) {
    return(list(error = "At least three complete, finite observations are required."))
  }

  if (isTRUE(standardise)) {
    num_predictors <- vapply(df, is.numeric, logical(1)) & names(df) != y_var
    scalable <- num_predictors & vapply(df, function(x) {
      s <- if (is.numeric(x)) stats::sd(x) else NA_real_
      is.finite(s) && s > 0
    }, logical(1))
    if (any(scalable)) {
      df[, scalable] <- lapply(df[, scalable, drop = FALSE],
                               function(x) as.numeric(scale(x)))
    }
  }

  original_names <- names(df)
  internal_names <- make.names(original_names, unique = TRUE)
  name_map <- stats::setNames(original_names, internal_names)
  names(df) <- internal_names

  y_internal <- internal_names[match(y_var, original_names)]
  x_internal <- internal_names[match(x_vars, original_names)]

  fm <- stats::reformulate(x_internal, response = y_internal)
  fit <- tryCatch(stats::lm(fm, data = df), error = function(e) e)
  if (inherits(fit, "error")) {
    return(list(error = paste("Model failed to fit:", conditionMessage(fit))))
  }
  if (stats::df.residual(fit) < 1) {
    return(list(error = paste("The model has no residual degrees of freedom;",
                              "reduce the predictors or add observations.")))
  }

  list(fit = fit, data = df, name_map = name_map,
       aliased = any(is.na(stats::coef(fit))))
}

#' Tidy a fitted model, restoring the original column names
#'
#' Factor predictors produce terms like `treatment_codeB`, so the longest
#' matching syntactic name is stripped off and the remainder (the level) is
#' re-appended.
#'
#' @param rf A successful result from [fit_regression()].
#' @param conf.int Include confidence intervals?
#' @return A data frame of coefficients.
#' @export
tidy_regression <- function(rf, conf.int = TRUE) {
  tb <- as.data.frame(broom::tidy(rf$fit, conf.int = conf.int))
  map_names <- names(rf$name_map)
  tb$term <- vapply(tb$term, function(term) {
    hits <- map_names[startsWith(term, map_names)]
    if (!length(hits)) return(term)
    hit <- hits[which.max(nchar(hits))]
    paste0(rf$name_map[[hit]], substring(term, nchar(hit) + 1))
  }, character(1))
  tb
}

#' Printable fit statistics for a linear model
#'
#' @param rf A successful result from [fit_regression()].
#' @return A character vector, one line per statistic.
#' @export
regression_fit_stats <- function(rf) {
  s <- summary(rf$fit)
  g <- broom::glance(rf$fit)

  lines <- c(
    sprintf("n                 = %d",  nrow(rf$data)),
    sprintf("R2                = %.4f", s$r.squared),
    sprintf("Adjusted R2       = %.4f", s$adj.r.squared),
    sprintf("Residual SE       = %.4f (df = %d)", s$sigma, s$df[2])
  )
  if (!is.null(s$fstatistic)) {
    lines <- c(lines,
               sprintf("F-statistic       = %.3f on %d and %d df",
                       s$fstatistic[1], s$fstatistic[2], s$fstatistic[3]),
               sprintf("Overall p-value   = %.4g", g$p.value))
  }
  lines <- c(lines, sprintf("AIC / BIC         = %.1f / %.1f", g$AIC, g$BIC))
  if (isTRUE(rf$aliased)) {
    lines <- c(lines, "",
               "Warning: one or more coefficients are not estimable (collinearity detected).")
  }
  lines
}

#' Four-panel regression diagnostic figure
#'
#' Residuals vs fitted, normal Q-Q, scale-location and Cook's distance.
#' Loess smooths are added only when there are enough distinct fitted values
#' for the fit to be meaningful.
#'
#' @param rf A successful result from [fit_regression()].
#' @param colors A colour palette, see [get_palette()].
#' @param gg_theme A ggplot2 theme, see [build_plot_theme()].
#' @return A patchwork object, which behaves as a ggplot for printing and
#'   saving.
#' @export
regression_diagnostics <- function(rf, colors = get_palette("Nature"),
                                   gg_theme = build_plot_theme()) {
  fit   <- rf$fit
  res   <- stats::residuals(fit)
  fit_v <- stats::fitted(fit)
  std   <- stats::rstandard(fit)
  ck    <- stats::cooks.distance(fit)

  smooth_ok <- length(res) >= 5 && length(unique(fit_v)) >= 4

  p1 <- ggplot(data.frame(fit_v = fit_v, res = res), aes(x = .data$fit_v, y = .data$res)) +
    geom_point(color = colors[1], alpha = 0.7) +
    geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
    labs(x = "Fitted", y = "Residuals", title = "Residuals vs Fitted") +
    gg_theme
  if (smooth_ok) {
    p1 <- p1 + geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                           color = colors[2], linewidth = 0.9)
  }

  p2 <- ggplot(data.frame(std = std), aes(sample = .data$std)) +
    stat_qq(color = colors[1], alpha = 0.7) +
    stat_qq_line(color = colors[2], linetype = "dashed", linewidth = 0.8) +
    labs(x = "Theoretical Quantiles", y = "Standardised Residuals",
         title = "Normal Q-Q") +
    gg_theme

  p3 <- ggplot(data.frame(fit_v = fit_v, srs = sqrt(abs(std))),
               aes(x = .data$fit_v, y = .data$srs)) +
    geom_point(color = colors[1], alpha = 0.7) +
    labs(x = "Fitted", y = expression(sqrt("|Std residuals|")),
         title = "Scale-Location") +
    gg_theme
  if (smooth_ok) {
    p3 <- p3 + geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                           color = colors[2], linewidth = 0.9)
  }

  p4 <- ggplot(data.frame(idx = seq_along(ck), ck = ck),
               aes(x = .data$idx, y = .data$ck)) +
    geom_col(fill = colors[1], width = 0.6) +
    geom_hline(yintercept = 4 / length(ck), color = colors[2],
               linetype = "dashed", linewidth = 0.8) +
    labs(x = "Observation", y = "Cook's distance", title = "Cook's distance") +
    gg_theme

  patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
}
