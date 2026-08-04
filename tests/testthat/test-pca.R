test_that("fit_pca fits and reports the rows it used", {
  res <- fit_pca(mtcars, c("mpg", "wt", "hp", "disp"))

  expect_null(res$error)
  expect_equal(length(res$used_rows), nrow(mtcars))
  expect_equal(ncol(res$mat), 4)
  expect_true(nrow(res$pca$eig) >= 2)
})

test_that("fit_pca restricts to complete cases", {
  df <- mtcars[, c("mpg", "wt", "hp")]
  df$mpg[1:3] <- NA

  res <- fit_pca(df, names(df))
  expect_null(res$error)
  expect_equal(length(res$used_rows), nrow(mtcars) - 3)
  expect_false(1 %in% res$used_rows)
})

test_that("fit_pca drops constant columns", {
  df <- mtcars[, c("mpg", "wt", "hp")]
  df$flat <- 1

  res <- fit_pca(df, names(df))
  expect_null(res$error)
  expect_false("flat" %in% colnames(res$mat))
})

test_that("fit_pca refuses inputs it cannot analyse", {
  expect_match(fit_pca(mtcars, "mpg")$error, "at least two numeric")

  two_rows <- mtcars[1:2, c("mpg", "wt")]
  expect_match(fit_pca(two_rows, c("mpg", "wt"))$error, "three complete")

  flat <- data.frame(a = rep(1, 10), b = rep(2, 10))
  expect_match(fit_pca(flat, c("a", "b"))$error, "non-constant")
})

test_that("pca_axes clamps out-of-range requests", {
  expect_equal(pca_axes(5, 1, 2), c(1, 2))
  expect_equal(pca_axes(3, 9, 9), c(3, 1))
  expect_equal(pca_axes(4, 0, -3), c(1, 2))   # both clamp to 1, then separate
  expect_equal(pca_axes(2, 5, 9), c(2, 1))
})

test_that("pca_axes separates a duplicated axis", {
  expect_equal(pca_axes(4, 1, 1), c(1, 2))
  expect_equal(pca_axes(4, 3, 3), c(3, 1))
})

test_that("pca_eigen_table is labelled for display", {
  res <- fit_pca(mtcars, c("mpg", "wt", "hp", "disp"))
  tb <- pca_eigen_table(res$pca)

  expect_named(tb, c("Component", "Eigenvalue", "Variance (%)", "Cumulative (%)"))
  expect_equal(tb$Component[1], "Dim.1")
  expect_equal(tb[["Cumulative (%)"]][nrow(tb)], 100)
})
