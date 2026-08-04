test_that("skewness and kurtosis match the moment definitions", {
  x <- c(2, 4, 4, 4, 5, 5, 7, 9)
  m <- mean(x)
  expected_skew <- mean((x - m)^3) / mean((x - m)^2)^(3 / 2)
  expected_kurt <- mean((x - m)^4) / mean((x - m)^2)^2

  expect_equal(skewness(x), expected_skew)
  expect_equal(kurtosis(x), expected_kurt)
})

test_that("skewness and kurtosis return NA where undefined", {
  expect_true(is.na(skewness(c(1, 2))))          # fewer than 3 values
  expect_true(is.na(skewness(rep(5, 10))))       # constant
  expect_true(is.na(kurtosis(c(1, 2, 3))))       # fewer than 4 values
  expect_true(is.na(kurtosis(rep(5, 10))))
  expect_true(is.na(skewness(c(1, 2, NA))))
})

test_that("a symmetric distribution has near-zero skewness", {
  expect_lt(abs(skewness(c(-3, -2, -1, 0, 1, 2, 3))), 1e-12)
})

test_that("sig_stars maps p-values to the conventional thresholds", {
  expect_equal(
    sig_stars(c(0.0005, 0.005, 0.03, 0.08, 0.5, NA)),
    c("***", "**", "*", ".", "", "")
  )
})

test_that("cor_pmat returns symmetric matrices with a unit diagonal", {
  set.seed(1)
  m <- cbind(a = rnorm(30), b = rnorm(30), c = rnorm(30))
  res <- cor_pmat(m)

  expect_equal(dim(res$r), c(3, 3))
  expect_equal(diag(res$r), c(a = 1, b = 1, c = 1))
  expect_equal(diag(res$p), c(a = 0, b = 0, c = 0))
  expect_equal(res$r, t(res$r))
  expect_equal(res$p, t(res$p))
  expect_equal(res$r["a", "b"], unname(cor(m[, "a"], m[, "b"])))
})

test_that("cor_pmat leaves impossible pairs as NA instead of failing", {
  m <- cbind(a = c(1, 2, 3, 4), b = c(1, 1, 1, 1))   # b is constant
  res <- cor_pmat(m)
  expect_true(is.na(res$r["a", "b"]))
  expect_true(is.na(res$p["a", "b"]))
})

test_that("cor_pmat handles a single column", {
  res <- cor_pmat(cbind(a = 1:5))
  expect_equal(dim(res$r), c(1, 1))
  expect_equal(res$r[1, 1], 1)
})

test_that("holm_adjust_pmat adjusts once per unique pair and stays symmetric", {
  p <- matrix(c(0, 0.01, 0.02,
                0.01, 0, 0.03,
                0.02, 0.03, 0), nrow = 3)
  adj <- holm_adjust_pmat(p)

  expect_equal(adj, t(adj))
  expect_equal(diag(adj), c(0, 0, 0))
  expect_equal(sort(adj[upper.tri(adj)]),
               sort(p.adjust(c(0.01, 0.02, 0.03), method = "holm")))
})

test_that("describe_numeric reports the basic summary set", {
  df <- data.frame(x = c(1, 2, 3, 4, NA), y = c(10, 20, 30, 40, 50))
  out <- describe_numeric(df)

  expect_equal(out$Variable, c("x", "y"))
  expect_equal(out$N, c(4, 5))
  expect_equal(out$Missing, c(1, 0))
  expect_equal(out$Mean, c(2.5, 30))
  expect_equal(out$Min, c(1, 10))
  expect_equal(out$Max, c(4, 50))
  expect_true(all(c("Q1", "Q3", "IQR", "SD", "Median") %in% names(out)))
})

test_that("describe_numeric extended mode reports shape statistics", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8))
  out <- describe_numeric(df, extended = TRUE)

  expect_true(all(c("SE", "CV (%)", "Skewness", "Kurtosis", "Range") %in% names(out)))
  expect_equal(out$Range, 7)
  expect_equal(out$SE, round(sd(df$x) / sqrt(8), 3))
})

test_that("describe_numeric copes with all-missing and constant columns", {
  df <- data.frame(empty = c(NA_real_, NA_real_), flat = c(2, 2))
  out <- describe_numeric(df, extended = TRUE)

  expect_equal(out$N, c(0, 2))
  expect_true(is.na(out$Mean[1]))
  expect_true(is.na(out$Skewness[2]))   # constant column has no skewness
  expect_equal(out$Range[2], 0)
})

test_that("describe_numeric guards against a divide-by-zero CV", {
  out <- describe_numeric(data.frame(x = c(-1, 0, 1)), extended = TRUE)
  expect_true(is.na(out[["CV (%)"]]))
})

test_that("missing_summary counts and orders by missingness", {
  df <- data.frame(a = c(1, NA, NA), b = c(1, 2, 3), c = c(NA, 2, 3))
  out <- missing_summary(df)

  expect_equal(out$Variable, c("a", "c", "b"))
  expect_equal(out$Missing_Count, c(2, 1, 0))
  expect_equal(out$Complete_Count, c(1, 2, 3))
  expect_equal(out$Missing_Percent[1], round(2 / 3 * 100, 2))
})

test_that("outlier_summary applies the 1.5 x IQR rule", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 100))
  out <- outlier_summary(df)

  expect_equal(out$Outlier_Count, 1)
  expect_equal(out$Complete_N, 9)
})

test_that("outlier_summary handles an all-missing column", {
  out <- outlier_summary(data.frame(x = c(NA_real_, NA_real_)))
  expect_equal(out$Complete_N, 0)
  expect_equal(out$Outlier_Count, 0)
})

test_that("normality_table adjusts across variables and explains the result", {
  set.seed(42)
  df <- data.frame(normal = rnorm(50), skewed = rexp(50), flat = rep(1, 50))
  out <- normality_table(df)

  expect_equal(nrow(out), 3)
  expect_true(is.na(out$Shapiro_p[out$Variable == "flat"]))
  expect_equal(out$Interpretation[out$Variable == "flat"],
               "Not tested (requires 3-5000 non-constant values)")
  expect_true(all(out$Shapiro_p_Holm >= out$Shapiro_p, na.rm = TRUE))
})
