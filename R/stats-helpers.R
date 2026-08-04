#' Sample skewness
#'
#' The moment (biased) estimator, matching `moments::skewness()`. Implemented
#' here so the package does not depend on \pkg{moments} for two one-line
#' formulas.
#'
#' @param x Numeric vector; missing values are removed.
#' @return A single number, or `NA_real_` when undefined.
#' @export
#' @examples
#' skewness(c(1, 2, 2, 3, 9))
skewness <- function(x) {
  z <- x[!is.na(x)]
  n <- length(z)
  if (n < 3 || length(unique(z)) < 2) return(NA_real_)
  m <- mean(z)
  m2 <- mean((z - m)^2)
  if (!is.finite(m2) || m2 <= 0) return(NA_real_)
  mean((z - m)^3) / m2^(3 / 2)
}

#' Sample kurtosis
#'
#' The moment (non-excess) estimator, matching `moments::kurtosis()`; a normal
#' distribution gives approximately 3.
#'
#' @param x Numeric vector; missing values are removed.
#' @return A single number, or `NA_real_` when undefined.
#' @export
#' @examples
#' kurtosis(rnorm(100))
kurtosis <- function(x) {
  z <- x[!is.na(x)]
  n <- length(z)
  if (n < 4 || length(unique(z)) < 2) return(NA_real_)
  m <- mean(z)
  m2 <- mean((z - m)^2)
  if (!is.finite(m2) || m2 <= 0) return(NA_real_)
  mean((z - m)^4) / m2^2
}

#' Significance stars
#'
#' @param p Numeric vector of p-values.
#' @return Character vector of stars: `***` < 0.001, `**` < 0.01, `*` < 0.05,
#'   `.` < 0.1, otherwise empty.
#' @export
#' @examples
#' sig_stars(c(0.0001, 0.02, 0.4, NA))
sig_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*",
                              ifelse(p < 0.1, ".", "")))))
}

#' Correlation and p-value matrices
#'
#' Computes pairwise correlations with [stats::cor.test()], using complete
#' pairs for each cell. Pairs with fewer than three complete observations, and
#' tests that error (for example zero variance), are left as `NA` rather than
#' aborting the whole matrix.
#'
#' @param mat A numeric matrix or data frame.
#' @param method One of `"pearson"`, `"spearman"`, `"kendall"`.
#' @return A list with `r` (correlations) and `p` (p-values), both square
#'   matrices with the column names of `mat`.
#' @export
#' @examples
#' cor_pmat(as.matrix(mtcars[, c("mpg", "wt", "hp")]))
cor_pmat <- function(mat, method = "pearson") {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p <- matrix(NA_real_, n, n)
  r <- matrix(NA_real_, n, n)
  rownames(p) <- colnames(p) <- rownames(r) <- colnames(r) <- colnames(mat)
  diag(r) <- 1
  diag(p) <- 0
  if (n < 2) return(list(r = r, p = p))

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      ok <- stats::complete.cases(mat[, c(i, j), drop = FALSE])
      if (sum(ok) < 3) next
      ct <- tryCatch(
        suppressWarnings(stats::cor.test(mat[ok, i], mat[ok, j], method = method)),
        error = function(e) NULL
      )
      if (is.null(ct)) next
      r[i, j] <- r[j, i] <- unname(ct$estimate)
      p[i, j] <- p[j, i] <- ct$p.value
    }
  }
  list(r = r, p = p)
}

#' Holm-adjust the off-diagonal cells of a p-value matrix
#'
#' The upper triangle holds each unique pair exactly once, so it is adjusted
#' and then mirrored into the lower triangle.
#'
#' @param pmat A square, symmetric p-value matrix.
#' @return `pmat` with adjusted off-diagonal values.
#' @export
holm_adjust_pmat <- function(pmat) {
  upper <- upper.tri(pmat)
  pmat[upper] <- stats::p.adjust(pmat[upper], method = "holm")
  pmat[lower.tri(pmat)] <- t(pmat)[lower.tri(pmat)]
  pmat
}

#' Descriptive statistics for every numeric column
#'
#' @param df A data frame of numeric columns.
#' @param extended When `FALSE`, returns location and spread (N, missing,
#'   mean, median, SD, min, quartiles, max, IQR). When `TRUE`, returns the
#'   shape-oriented set instead (SE, coefficient of variation, skewness,
#'   kurtosis, range).
#' @return A data frame with one row per column of `df`, rounded to three
#'   decimals.
#' @export
#' @examples
#' describe_numeric(mtcars[, c("mpg", "wt")])
#' describe_numeric(mtcars[, c("mpg", "wt")], extended = TRUE)
describe_numeric <- function(df, extended = FALSE) {
  rows <- lapply(names(df), function(v) {
    x <- df[[v]]
    z <- x[!is.na(x)]
    n <- length(z)
    avg  <- if (n) mean(z) else NA_real_
    sdev <- if (n > 1) stats::sd(z) else NA_real_
    q <- if (n) stats::quantile(z, c(0.25, 0.75), names = FALSE) else c(NA_real_, NA_real_)

    if (!extended) {
      return(data.frame(
        Variable = v,
        N        = n,
        Missing  = sum(is.na(x)),
        Mean     = avg,
        Median   = if (n) stats::median(z) else NA_real_,
        SD       = sdev,
        Min      = if (n) min(z) else NA_real_,
        Q1       = q[1],
        Q3       = q[2],
        Max      = if (n) max(z) else NA_real_,
        IQR      = if (n) q[2] - q[1] else NA_real_,
        check.names = FALSE
      ))
    }

    data.frame(
      Variable = v,
      N        = n,
      Missing  = sum(is.na(x)),
      Mean     = avg,
      Median   = if (n) stats::median(z) else NA_real_,
      SD       = sdev,
      SE       = if (n > 1) sdev / sqrt(n) else NA_real_,
      `CV (%)` = if (n > 1 && is.finite(avg) && abs(avg) > sqrt(.Machine$double.eps)) {
        100 * sdev / avg
      } else {
        NA_real_
      },
      Skewness = skewness(z),
      Kurtosis = kurtosis(z),
      Range    = if (n) diff(range(z)) else NA_real_,
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  round_numeric(out, 3)
}

#' Missing-value summary
#'
#' @param df A data frame.
#' @return A data frame with counts and percentages, ordered by most missing.
#' @export
missing_summary <- function(df) {
  out <- data.frame(
    Variable        = names(df),
    Missing_Count   = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    Missing_Percent = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
    Complete_Count  = vapply(df, function(x) sum(!is.na(x)), numeric(1)),
    row.names       = NULL,
    stringsAsFactors = FALSE
  )
  out[order(-out$Missing_Count), , drop = FALSE]
}

#' Outlier counts using the 1.5 x IQR rule
#'
#' @param df A data frame of numeric columns.
#' @return A data frame with one row per column, ordered by outlier count.
#' @export
outlier_summary <- function(df) {
  rows <- lapply(names(df), function(v) {
    z <- df[[v]]
    z <- z[!is.na(z)]
    if (!length(z)) {
      return(data.frame(Variable = v, Complete_N = 0, Outlier_Count = 0,
                        Outlier_Percent = NA_real_))
    }
    q <- stats::quantile(z, c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    n_out <- sum(z < q[1] - 1.5 * iqr | z > q[2] + 1.5 * iqr)
    data.frame(Variable = v, Complete_N = length(z), Outlier_Count = n_out,
               Outlier_Percent = round(100 * n_out / length(z), 2))
  })
  out <- do.call(rbind, rows)
  out[order(-out$Outlier_Count), , drop = FALSE]
}

#' Normality assessment for every numeric column
#'
#' Runs the Shapiro-Wilk test where it is defined (3 to 5000 non-constant
#' values) and Holm-adjusts the p-values across variables, since testing every
#' column is itself a multiple-comparison problem.
#'
#' @param df A data frame of numeric columns.
#' @return A data frame with N, raw and adjusted p-values, skewness, kurtosis
#'   and a plain-language interpretation.
#' @export
normality_table <- function(df) {
  rows <- lapply(names(df), function(v) {
    x <- df[[v]]
    x <- x[!is.na(x)]
    n <- length(x)
    pval <- if (n >= 3 && n <= 5000 && length(unique(x)) > 1) {
      tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    } else {
      NA_real_
    }
    data.frame(Variable = v, N = n, Shapiro_p = pval,
               Skewness = skewness(x), Kurtosis = kurtosis(x))
  })
  out <- do.call(rbind, rows)

  out$Shapiro_p_Holm <- stats::p.adjust(out$Shapiro_p, method = "holm")
  out$Interpretation <- ifelse(
    is.na(out$Shapiro_p), "Not tested (requires 3-5000 non-constant values)",
    ifelse(out$Shapiro_p_Holm < 0.05,
           "Evidence against normality",
           "No evidence against normality")
  )
  num_cols <- c("Shapiro_p", "Shapiro_p_Holm", "Skewness", "Kurtosis")
  out[num_cols] <- lapply(out[num_cols], round, digits = 4)
  out[, c("Variable", "N", "Shapiro_p", "Shapiro_p_Holm",
          "Skewness", "Kurtosis", "Interpretation"), drop = FALSE]
}
