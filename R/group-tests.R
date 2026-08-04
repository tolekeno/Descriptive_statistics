#' Tests offered on the Group Comparisons tab
#'
#' @return Named character vector for `selectInput(choices = )`.
#' @export
group_test_choices <- function() {
  c("Auto (Welch; robust default)"              = "auto",
    "Welch t-test / ANOVA"                      = "welch",
    "Student t-test"                            = "t",
    "One-way ANOVA + Tukey"                     = "anova",
    "Wilcoxon"                                  = "wilcox",
    "Kruskal-Wallis + pairwise Wilcoxon (Holm)" = "kw")
}

#' Per-group summary statistics
#'
#' @param d A data frame with a numeric column `y` and a factor column `g`.
#' @return A data frame with one row per group, rounded to three decimals.
#' @export
group_summary <- function(d) {
  parts <- split(d$y, d$g)
  rows <- lapply(names(parts), function(g) {
    y <- parts[[g]]
    n <- length(y)
    data.frame(
      Group  = g,
      N      = n,
      Mean   = mean(y),
      SD     = stats::sd(y),
      SE     = if (n > 1) stats::sd(y) / sqrt(n) else NA_real_,
      Median = stats::median(y),
      Min    = min(y),
      Max    = max(y),
      stringsAsFactors = FALSE
    )
  })
  round_numeric(do.call(rbind, rows), 3)
}

#' Reshape a pairwise p-value matrix into a tidy table
#'
#' `pairwise.t.test()` and friends return a lower-triangular matrix with `NA`
#' in the unused cells; those are dropped here.
#'
#' @param mat A matrix of adjusted p-values, or `NULL`.
#' @return A data frame with `Group1`, `Group2`, `p_adj` and `Sig`, or `NULL`.
#' @export
pairwise_matrix_to_df <- function(mat) {
  if (is.null(mat)) return(NULL)
  tab <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  tab <- tab[!is.na(tab$Freq), , drop = FALSE]
  if (!nrow(tab)) return(NULL)
  data.frame(
    Group1 = as.character(tab$Var1),
    Group2 = as.character(tab$Var2),
    p_adj  = round(tab$Freq, 4),
    Sig    = sig_stars(tab$Freq),
    stringsAsFactors = FALSE
  )
}

#' Brown-Forsythe test of equal variances
#'
#' A median-centred form of Levene's test, which is far less sensitive to
#' non-normality than the classical mean-centred version.
#'
#' @param d A data frame with a numeric `y` and a factor `g`.
#' @return A list with `F` and `p`, or `NULL` when the test cannot be computed.
#' @export
brown_forsythe <- function(d) {
  tryCatch({
    med <- tapply(d$y, d$g, stats::median)
    zi <- abs(d$y - med[as.character(d$g)])
    a <- stats::anova(stats::lm(zi ~ d$g))
    list(F = a$`F value`[1], p = a$`Pr(>F)`[1])
  }, error = function(e) NULL)
}

#' Run a group comparison test
#'
#' @param d A data frame with a numeric `y` and a factor `g` (already
#'   filtered for missing values and dropped of empty levels).
#' @param test_type One of `"t"`, `"welch"`, `"anova"`, `"wilcox"`, `"kw"`.
#' @return A list with `text` (the printed test output, as a character vector)
#'   and `posthoc` (a data frame, or `NULL` when the test has no pairwise
#'   companion).
#' @export
run_group_test <- function(d, test_type) {
  tryCatch({
    posthoc <- NULL
    n_groups <- nlevels(d$g)

    test_text <- switch(
      test_type,
      "t" = {
        if (n_groups != 2) stop("Student t-test requires exactly two groups.")
        utils::capture.output(print(stats::t.test(y ~ g, data = d, var.equal = TRUE)))
      },
      "welch" = {
        if (n_groups == 2) {
          utils::capture.output(print(stats::t.test(y ~ g, data = d, var.equal = FALSE)))
        } else {
          out <- utils::capture.output(
            print(stats::oneway.test(y ~ g, data = d, var.equal = FALSE))
          )
          posthoc <- pairwise_matrix_to_df(stats::pairwise.t.test(
            d$y, d$g, p.adjust.method = "holm", pool.sd = FALSE
          )$p.value)
          out
        }
      },
      "anova" = {
        fit <- stats::aov(y ~ g, data = d)
        tuk <- broom::tidy(stats::TukeyHSD(fit))
        tuk <- round_numeric(as.data.frame(tuk), 4)
        tuk$Sig <- sig_stars(tuk$adj.p.value)
        posthoc <- tuk
        utils::capture.output(print(summary(fit)))
      },
      "wilcox" = {
        if (n_groups != 2) stop("Wilcoxon rank-sum test requires exactly two groups.")
        utils::capture.output(print(stats::wilcox.test(y ~ g, data = d, exact = FALSE)))
      },
      "kw" = {
        kw <- stats::kruskal.test(y ~ g, data = d)
        posthoc <- pairwise_matrix_to_df(stats::pairwise.wilcox.test(
          d$y, d$g, p.adjust.method = "holm", exact = FALSE
        )$p.value)
        utils::capture.output(print(kw))
      },
      stop("Unknown test: ", test_type)
    )

    list(text = test_text, posthoc = posthoc)
  }, error = function(e) {
    list(text = paste("Test could not be computed:", conditionMessage(e)),
         posthoc = NULL)
  })
}

#' Full group comparison report
#'
#' Runs the requested test and appends a Brown-Forsythe variance check with a
#' plain-language reading, because the choice between Welch, ANOVA and
#' Kruskal-Wallis hinges on that assumption.
#'
#' @param d A data frame with numeric `y` and factor `g`.
#' @param requested_test One of [group_test_choices()]; `"auto"` resolves to
#'   Welch.
#' @return A list with `text` and `posthoc`, as in [run_group_test()].
#' @export
group_test_report <- function(d, requested_test = "auto") {
  if (nlevels(d$g) < 2) {
    return(list(text = "Need at least 2 groups.", posthoc = NULL))
  }
  test_type <- if (identical(requested_test, "auto")) "welch" else requested_test
  result <- run_group_test(d, test_type)

  auto_note <- if (identical(requested_test, "auto")) {
    "Automatic choice: Welch's test (robust to unequal variances)."
  } else {
    NULL
  }

  lev <- brown_forsythe(d)
  if (is.null(lev)) {
    result$text <- c(auto_note, result$text, "",
                     "Brown-Forsythe variance test: not computed.")
    return(result)
  }

  interpretation <- if (is.na(lev$p)) {
    "  -> Variance equality could not be assessed."
  } else if (lev$p < 0.05) {
    "  -> Evidence of unequal variances; prefer Welch or Kruskal-Wallis."
  } else {
    "  -> No evidence of unequal variances."
  }

  result$text <- c(
    auto_note, result$text, "",
    sprintf("Brown-Forsythe variance test: F = %.3f, p = %.4f", lev$F, lev$p),
    interpretation
  )
  result
}
