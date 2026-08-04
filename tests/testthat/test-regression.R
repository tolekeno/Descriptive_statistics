test_that("fit_regression fits a plain model", {
  rf <- fit_regression(mtcars, "mpg", c("wt", "hp"))

  expect_null(rf$error)
  expect_s3_class(rf$fit, "lm")
  expect_equal(nrow(rf$data), nrow(mtcars))
  expect_false(rf$aliased)
})

test_that("fit_regression survives non-syntactic column names", {
  df <- mtcars[, c("mpg", "wt")]
  names(df) <- c("Grain yield (t/ha)", "Plant height, cm")

  rf <- fit_regression(df, "Grain yield (t/ha)", "Plant height, cm")
  expect_null(rf$error)

  tb <- tidy_regression(rf)
  expect_true("Plant height, cm" %in% tb$term)
  expect_true("(Intercept)" %in% tb$term)
})

test_that("tidy_regression restores original names on factor levels", {
  df <- data.frame(y = rnorm(30),
                   `treatment group` = rep(c("ctrl", "trtA", "trtB"), 10),
                   check.names = FALSE)
  rf <- fit_regression(df, "y", "treatment group")
  tb <- tidy_regression(rf)

  expect_true(any(grepl("^treatment group", tb$term)))
  expect_true("treatment grouptrtA" %in% tb$term)
})

test_that("standardising rescales predictors but not the response", {
  rf <- fit_regression(mtcars, "mpg", c("wt", "hp"), standardise = TRUE)

  expect_null(rf$error)
  expect_equal(mean(rf$data$wt), 0)
  expect_equal(sd(rf$data$wt), 1)
  expect_equal(rf$data$mpg, mtcars$mpg)   # response untouched
})

test_that("standardising skips constant predictors instead of dividing by zero", {
  df <- data.frame(y = rnorm(20), x = rnorm(20), flat = rep(4, 20))
  rf <- fit_regression(df, "y", c("x", "flat"), standardise = TRUE)

  expect_null(rf$error)
  expect_equal(rf$data$flat, rep(4, 20))
  expect_false(any(is.nan(rf$data$flat)))
})

test_that("fit_regression rejects data it cannot model", {
  too_few <- data.frame(y = c(1, 2), x = c(1, 2))
  expect_match(fit_regression(too_few, "y", "x")$error, "three complete")

  no_df <- data.frame(y = c(1, 2, 3), x = c(1, 2, 3), z = c(1, 4, 9))
  expect_match(fit_regression(no_df, "y", c("x", "z"))$error,
               "residual degrees of freedom")
})

test_that("fit_regression drops non-finite rows", {
  df <- data.frame(y = c(1, 2, 3, 4, Inf), x = c(1, 2, 3, 4, 5))
  rf <- fit_regression(df, "y", "x")

  expect_null(rf$error)
  expect_equal(nrow(rf$data), 4)
})

test_that("collinear predictors are flagged as aliased rather than failing", {
  df <- data.frame(y = rnorm(20), x = 1:20)
  df$x2 <- df$x * 2
  rf <- fit_regression(df, "y", c("x", "x2"))

  expect_null(rf$error)
  expect_true(rf$aliased)
})

test_that("regression_fit_stats reports the headline numbers", {
  rf <- fit_regression(mtcars, "mpg", c("wt", "hp"))
  lines <- regression_fit_stats(rf)
  joined <- paste(lines, collapse = "\n")

  expect_match(joined, "R2")
  expect_match(joined, "Adjusted R2")
  expect_match(joined, "AIC / BIC")
  expect_match(joined, "n                 = 32")
})

test_that("regression_diagnostics returns a printable four-panel figure", {
  rf <- fit_regression(mtcars, "mpg", c("wt", "hp"))
  p <- regression_diagnostics(rf)

  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 3)   # plus the base plot = 4 panels
})
