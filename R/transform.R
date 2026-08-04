#' Transformations offered on the Data Prep tab
#'
#' @return Named character vector for `selectInput(choices = )`.
#' @export
transform_choices <- function() {
  c("None"              = "none",
    "log (natural)"     = "log",
    "log10"             = "log10",
    "sqrt"              = "sqrt",
    "Standardise (z)"   = "scale",
    "Centre (x - mean)" = "center",
    "Reciprocal (1/x)"  = "reciprocal",
    "Box-Cox"           = "boxcox")
}

#' Maximum-likelihood Box-Cox lambda
#'
#' Profile log-likelihood over a grid of lambda from -2 to 2. Only positive
#' values contribute, as the Box-Cox family is undefined at or below zero.
#'
#' @param x Numeric vector.
#' @param lambdas Grid of candidate lambda values.
#' @return The maximising lambda, or `NA_real_` when there are fewer than five
#'   positive, non-constant values.
#' @export
#' @examples
#' boxcox_lambda(rlnorm(100))
boxcox_lambda <- function(x, lambdas = seq(-2, 2, by = 0.05)) {
  xx <- x[x > 0 & !is.na(x)]
  if (length(xx) < 5 || length(unique(xx)) < 2) return(NA_real_)
  llk <- vapply(lambdas, function(l) {
    y <- if (abs(l) < 1e-8) log(xx) else (xx^l - 1) / l
    v <- stats::var(y)
    if (!is.finite(v) || v <= 0) return(-Inf)
    -0.5 * length(y) * log(v) + (l - 1) * sum(log(xx))
  }, numeric(1))
  if (all(!is.finite(llk))) return(NA_real_)
  lambdas[which.max(llk)]
}

#' Apply a numeric transformation
#'
#' Values outside a transformation's domain become `NA` rather than `NaN` or
#' `-Inf`, so that downstream plots and summaries treat them as missing.
#'
#' @param x Numeric vector.
#' @param type One of the values in [transform_choices()].
#' @return A list with:
#'   \describe{
#'     \item{values}{The transformed vector, same length as `x`.}
#'     \item{ok}{`TRUE` when the transformation produced usable values.}
#'     \item{message}{A note for the user, or `NULL`.}
#'   }
#' @export
#' @examples
#' apply_transform(c(1, 10, 100), "log10")$values
#' apply_transform(c(-1, 0, 4), "sqrt")$values
apply_transform <- function(x, type) {
  na_result <- function(msg) {
    list(values = rep(NA_real_, length(x)), ok = FALSE, message = msg)
  }

  # Apply `f` only where `in_domain` holds, leaving everything else NA. Using
  # ifelse() here would evaluate f() over the whole vector and emit spurious
  # NaN warnings for values the transformation was never going to accept.
  on_domain <- function(in_domain, f) {
    out <- rep(NA_real_, length(x))
    ok <- in_domain & !is.na(x)
    out[ok] <- f(x[ok])
    out
  }

  switch(
    type,
    "none" = list(values = x, ok = FALSE, message = "No transformation selected."),
    "log" = list(values = on_domain(x > 0, log), ok = TRUE, message = NULL),
    "log10" = list(values = on_domain(x > 0, log10), ok = TRUE, message = NULL),
    "sqrt" = list(values = on_domain(x >= 0, sqrt), ok = TRUE, message = NULL),
    "scale" = {
      s <- stats::sd(x, na.rm = TRUE)
      if (is.finite(s) && s > 0) {
        list(values = as.numeric(scale(x)), ok = TRUE, message = NULL)
      } else {
        na_result("Cannot standardise a constant variable.")
      }
    },
    "center" = list(values = as.numeric(scale(x, center = TRUE, scale = FALSE)),
                    ok = TRUE, message = NULL),
    "reciprocal" = list(values = on_domain(x != 0, function(z) 1 / z),
                        ok = TRUE, message = NULL),
    "boxcox" = {
      lambda <- boxcox_lambda(x)
      if (is.na(lambda)) {
        na_result("Box-Cox requires at least five positive, non-constant observations.")
      } else {
        power <- if (abs(lambda) < 1e-8) {
          log
        } else {
          function(z) (z^lambda - 1) / lambda
        }
        list(values = on_domain(x > 0, power), ok = TRUE,
             message = sprintf("Box-Cox lambda = %.2f", lambda))
      }
    },
    na_result(paste("Unknown transformation:", type))
  )
}
