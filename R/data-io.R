#' Guess the field delimiter of a delimited text file
#'
#' Counts candidate delimiters across the first non-empty records and picks the
#' most frequent one. This detects semicolon- and pipe-separated exports, which
#' many locales produce from Excel.
#'
#' @param path Path to a text file.
#' @return A single-character delimiter; `","` when nothing can be determined.
#' @export
detect_delimiter <- function(path) {
  lines <- readLines(path, n = 10, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(",")
  candidates <- c(",", "\t", ";", "|")
  counts <- vapply(candidates, function(z) {
    mean(vapply(
      lines,
      function(x) lengths(regmatches(x, gregexpr(z, x, fixed = TRUE))),
      numeric(1)
    ))
  }, numeric(1))
  if (max(counts) == 0) "," else candidates[which.max(counts)]
}

#' Delimiter choices offered in the sidebar
#'
#' @return Named character vector for `selectInput(choices = )`.
#' @export
delimiter_choices <- function() {
  c("Auto" = "auto", "Comma" = ",", "Tab" = "\t",
    "Semicolon" = ";", "Pipe" = "|")
}

#' Read a tabular data file
#'
#' Dispatches on the file extension: Excel workbooks are read with
#' \pkg{readxl} (an optional dependency), everything else with
#' [utils::read.table()] using either an explicit or a detected delimiter.
#'
#' @param path Path to the file on disk.
#' @param file_name Original file name, used only to read the extension
#'   (upload temp files have no extension).
#' @param delim One of [delimiter_choices()]; `"auto"` triggers
#'   [detect_delimiter()].
#' @param sheet Excel sheet name or index.
#' @return A data frame with trimmed, unique column names.
#' @export
read_tabular <- function(path, file_name = path, delim = "auto", sheet = 1) {
  ext <- tolower(tools::file_ext(file_name))

  df <- if (ext %in% c("xlsx", "xls")) {
    if (!has_pkg("readxl")) {
      stop("Install the 'readxl' package to open Excel files.", call. = FALSE)
    }
    as.data.frame(readxl::read_excel(path, sheet = sheet %||% 1))
  } else {
    sep <- if (identical(delim, "auto")) detect_delimiter(path) else delim
    utils::read.table(
      path, header = TRUE, sep = sep,
      stringsAsFactors = FALSE, quote = "\"",
      check.names = FALSE,
      na.strings = c("", "NA", "N/A", "#N/A", "."),
      comment.char = "", fill = TRUE
    )
  }

  if (nrow(df) == 0 || ncol(df) == 0) {
    stop("The file contains no usable data.", call. = FALSE)
  }
  names(df) <- make.unique(trimws(names(df)), sep = "_")
  df
}

#' Classify columns as numeric or categorical
#'
#' Integer-like columns with few distinct values (treatment codes, block
#' numbers, replicate ids) are reported as categorical, because grouping by
#' them is almost always what the user wants.
#'
#' @param df A data frame.
#' @param cat_unique_max Maximum number of distinct values for an integer-like
#'   numeric column to count as categorical.
#' @return A list with elements `numeric`, `categorical` and `all`.
#' @export
#' @examples
#' col_classes(data.frame(yield = rnorm(50), block = rep(1:4, length.out = 50)))
col_classes <- function(df, cat_unique_max = 20) {
  types <- vapply(df, function(x) {
    if (is.numeric(x)) {
      z <- unique(x[!is.na(x)])
      is_whole <- length(z) > 0 &&
        all(abs(z - round(z)) < sqrt(.Machine$double.eps))
      low_cardinality <- length(z) >= 2 && length(z) <= cat_unique_max &&
        length(z) <= max(5, nrow(df) / 4)
      if (is_whole && low_cardinality) "categorical" else "numeric"
    } else if (is.logical(x)) {
      "categorical"
    } else if (is.factor(x) || is.character(x)) {
      "categorical"
    } else {
      "other"
    }
  }, character(1))

  list(
    numeric     = names(types)[types == "numeric"],
    categorical = names(types)[types == "categorical"],
    all         = names(df)
  )
}
