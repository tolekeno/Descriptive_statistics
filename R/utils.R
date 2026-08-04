#' Null-coalescing operator
#'
#' Returns `a` unless it is `NULL` or zero-length, in which case `b` is
#' returned. Shiny inputs are frequently `NULL` before their UI has rendered,
#' and multi-select inputs go to `character(0)` when cleared, so this guards
#' nearly every input read in the app.
#'
#' Deliberately not exported: base R gained its own `%||%` in 4.4, which does
#' not treat zero-length values as missing. Exporting this one would mask it.
#'
#' @param a,b Values to choose between.
#' @return `a` if it is non-`NULL` and has positive length, otherwise `b`.
#' @noRd
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

#' Is an optional package available?
#'
#' @param pkg Package name.
#' @return `TRUE` when the package can be loaded.
#' @export
has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

#' Make a string safe to use as a file name
#'
#' Replaces every character outside `[A-Za-z0-9._-]` with an underscore and
#' collapses runs of underscores. Used to build download file names from
#' user-supplied column names.
#'
#' @param x Character vector.
#' @return Character vector of the same length; empty results become
#'   `"variable"`.
#' @export
#' @examples
#' safe_filename("Grain yield (t/ha)")
safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "variable")
}

#' Expand a colour palette to an arbitrary number of levels
#'
#' Grouping variables often have more levels than a journal palette has
#' colours; this interpolates rather than recycling, which would produce
#' duplicate group colours.
#'
#' @param colors Character vector of colours.
#' @param n Number of colours required.
#' @return Character vector of length `n`.
#' @export
#' @examples
#' expand_palette(c("#E64B35", "#4DBBD5"), 5)
expand_palette <- function(colors, n) {
  if (n <= 0) return(character())
  if (n <= length(colors)) return(colors[seq_len(n)])
  grDevices::colorRampPalette(colors)(n)
}

#' A ggplot carrying an explanatory message
#'
#' Rendered instead of an opaque plotting error when a variable cannot be
#' plotted (all missing, constant, and so on).
#'
#' @param text Message to display.
#' @return A ggplot object.
#' @export
message_plot <- function(text) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = text, size = 5, color = "grey30") +
    xlim(0, 1) + ylim(0, 1) + theme_void()
}

#' Round every numeric column of a data frame
#'
#' A base-R replacement for `dplyr::mutate(across(where(is.numeric), round))`
#' that avoids depending on tidyselect verbs inside package code.
#'
#' @param df A data frame.
#' @param digits Number of decimal places.
#' @return `df` with numeric columns rounded; column names are preserved
#'   exactly, including non-syntactic ones.
#' @export
#' @examples
#' round_numeric(data.frame(a = 1.23456, b = "x"), digits = 2)
round_numeric <- function(df, digits = 3) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits = digits)
  df
}

#' Select the numeric columns of a data frame
#'
#' @param df A data frame.
#' @return `df` restricted to its numeric columns, as a data frame.
#' @export
numeric_columns <- function(df) {
  df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
}
