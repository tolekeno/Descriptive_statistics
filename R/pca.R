#' Fit a principal component analysis
#'
#' Restricts to complete cases across the selected variables, drops constant
#' columns, and caps the number of retained components at what the data can
#' support.
#'
#' @param df A data frame.
#' @param variables Numeric column names to include.
#' @param scale_unit Scale variables to unit variance?
#' @param max_ncp Maximum number of components to retain.
#' @return Either `list(error = <message>)` or a list with `pca` (the
#'   `FactoMineR::PCA` object), `used_rows` (row indices of the complete cases)
#'   and `mat` (the matrix actually analysed).
#' @export
#' @examples
#' fit_pca(mtcars, c("mpg", "wt", "hp", "disp"))$pca$eig
fit_pca <- function(df, variables, scale_unit = TRUE, max_ncp = 10) {
  variables <- intersect(variables, names(df))
  if (length(variables) < 2) {
    return(list(error = "Select at least two numeric variables for PCA."))
  }

  mat_all <- df[, variables, drop = FALSE]
  used_rows <- which(stats::complete.cases(mat_all))
  mat <- mat_all[used_rows, , drop = FALSE]
  if (nrow(mat) < 3) {
    return(list(error = "PCA requires at least three complete observations."))
  }

  keep <- vapply(mat, function(x) {
    v <- stats::var(x)
    is.finite(v) && v > 0
  }, logical(1))
  mat <- mat[, keep, drop = FALSE]
  if (ncol(mat) < 2) {
    return(list(error = "PCA requires at least two non-constant variables."))
  }

  pca_res <- tryCatch(
    FactoMineR::PCA(mat, scale.unit = isTRUE(scale_unit), graph = FALSE,
                    ncp = min(ncol(mat), nrow(mat) - 1, max_ncp)),
    error = function(e) NULL
  )
  if (is.null(pca_res)) {
    return(list(error = "PCA could not be fitted; check for collinearity or invalid values."))
  }

  list(pca = pca_res, used_rows = used_rows, mat = mat)
}

#' Resolve the requested PCA axes against the components available
#'
#' Clamps both axes into range and separates them if the user picked the same
#' component twice.
#'
#' @param n_components Number of components retained.
#' @param x,y Requested axis numbers.
#' @return An integer vector of length two.
#' @export
#' @examples
#' pca_axes(3, 1, 1)
#' pca_axes(2, 5, 9)
pca_axes <- function(n_components, x = 1, y = 2) {
  ax <- max(1L, min(as.integer(x %||% 1), n_components))
  ay <- max(1L, min(as.integer(y %||% 2), n_components))
  if (ax == ay) ay <- if (ax == 1L) min(2L, n_components) else 1L
  c(ax, ay)
}

#' Eigenvalue table for a fitted PCA
#'
#' @param pca A `FactoMineR::PCA` object.
#' @return A data frame with one row per component.
#' @export
pca_eigen_table <- function(pca) {
  eig <- as.data.frame(pca$eig)
  out <- cbind(Component = paste0("Dim.", seq_len(nrow(eig))), round(eig, 3))
  names(out) <- c("Component", "Eigenvalue", "Variance (%)", "Cumulative (%)")
  rownames(out) <- NULL
  out
}
