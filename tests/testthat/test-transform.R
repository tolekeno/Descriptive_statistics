test_that("log-family transforms send out-of-domain values to NA", {
  x <- c(-1, 0, 1, 10)

  expect_equal(apply_transform(x, "log")$values, c(NA, NA, log(1), log(10)))
  expect_equal(apply_transform(x, "log10")$values, c(NA, NA, 0, 1))
  expect_equal(apply_transform(x, "sqrt")$values, c(NA, 0, 1, sqrt(10)))
  expect_equal(apply_transform(c(-2, 0, 4), "reciprocal")$values,
               c(-0.5, NA, 0.25))
})

test_that("scale and center behave like their base-R counterparts", {
  x <- c(2, 4, 6, 8)

  scaled <- apply_transform(x, "scale")
  expect_true(scaled$ok)
  expect_equal(mean(scaled$values), 0)
  expect_equal(sd(scaled$values), 1)

  centred <- apply_transform(x, "center")
  expect_equal(centred$values, x - mean(x))
})

test_that("standardising a constant column is refused rather than producing NaN", {
  res <- apply_transform(rep(3, 10), "scale")
  expect_false(res$ok)
  expect_true(all(is.na(res$values)))
  expect_match(res$message, "constant")
})

test_that("'none' is reported as a no-op", {
  res <- apply_transform(1:5, "none")
  expect_false(res$ok)
  expect_equal(res$values, 1:5)
})

test_that("an unknown transformation fails safely", {
  res <- apply_transform(1:5, "nonsense")
  expect_false(res$ok)
  expect_match(res$message, "Unknown transformation")
})

test_that("boxcox_lambda finds a log-like lambda for lognormal data", {
  set.seed(7)
  x <- rlnorm(300, meanlog = 1, sdlog = 0.5)
  expect_lt(abs(boxcox_lambda(x)), 0.35)
})

test_that("boxcox_lambda finds a near-identity lambda for normal data", {
  set.seed(7)
  x <- rnorm(300, mean = 50, sd = 5)
  expect_gt(boxcox_lambda(x), 0.5)
})

test_that("boxcox_lambda refuses inputs it cannot fit", {
  expect_true(is.na(boxcox_lambda(c(1, 2, 3))))       # fewer than 5 positive
  expect_true(is.na(boxcox_lambda(rep(2, 20))))       # constant
  expect_true(is.na(boxcox_lambda(c(-1, -2, -3))))    # nothing positive
})

test_that("the boxcox transform reports its lambda and preserves length", {
  set.seed(7)
  x <- c(rlnorm(50), -1, 0)
  res <- apply_transform(x, "boxcox")

  expect_true(res$ok)
  expect_length(res$values, length(x))
  expect_true(all(is.na(res$values[51:52])))
  expect_match(res$message, "lambda")
})

test_that("transform_choices covers every branch of apply_transform", {
  x <- c(1, 2, 3, 4, 5, 6)
  for (type in unname(transform_choices())) {
    res <- apply_transform(x, type)
    expect_length(res$values, length(x))
    expect_type(res$ok, "logical")
  }
})
