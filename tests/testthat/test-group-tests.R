make_groups <- function(n = 20, shift = 0, k = 2, seed = 99) {
  set.seed(seed)
  g <- factor(rep(letters[seq_len(k)], each = n))
  y <- rnorm(n * k) + rep(seq(0, by = shift, length.out = k), each = n)
  data.frame(y = y, g = g)
}

test_that("group_summary reports one row per group with matching statistics", {
  d <- make_groups(k = 3)
  out <- group_summary(d)

  expect_equal(nrow(out), 3)
  expect_equal(out$Group, c("a", "b", "c"))
  expect_equal(out$N, c(20, 20, 20))
  expect_equal(out$Mean[1], round(mean(d$y[d$g == "a"]), 3))
  expect_equal(out$SE[1], round(sd(d$y[d$g == "a"]) / sqrt(20), 3))
})

test_that("pairwise_matrix_to_df drops the empty half of the matrix", {
  m <- matrix(c(0.01, 0.20, NA, 0.30), nrow = 2,
              dimnames = list(c("b", "c"), c("a", "b")))
  out <- pairwise_matrix_to_df(m)

  expect_equal(nrow(out), 3)
  expect_named(out, c("Group1", "Group2", "p_adj", "Sig"))
  expect_equal(out$Sig[out$p_adj == 0.01], "*")
  expect_null(pairwise_matrix_to_df(NULL))
})

test_that("brown_forsythe detects a genuine variance difference", {
  set.seed(3)
  d <- data.frame(
    y = c(rnorm(40, sd = 1), rnorm(40, sd = 6)),
    g = factor(rep(c("a", "b"), each = 40))
  )
  bf <- brown_forsythe(d)

  expect_type(bf, "list")
  expect_lt(bf$p, 0.05)
})

test_that("brown_forsythe finds no difference for equal variances", {
  bf <- brown_forsythe(make_groups(n = 40))
  expect_gt(bf$p, 0.05)
})

test_that("two-group tests run and produce no post-hoc table", {
  d <- make_groups(shift = 3)

  for (test in c("t", "welch", "wilcox")) {
    res <- run_group_test(d, test)
    expect_type(res$text, "character")
    expect_null(res$posthoc)
    expect_true(any(grepl("p-value", res$text)))
  }
})

test_that("two-group-only tests refuse three groups instead of erroring out", {
  d <- make_groups(k = 3)

  for (test in c("t", "wilcox")) {
    res <- run_group_test(d, test)
    expect_match(paste(res$text, collapse = " "), "Test could not be computed")
    expect_null(res$posthoc)
  }
})

test_that("multi-group tests return a pairwise table", {
  d <- make_groups(k = 3, shift = 4)

  for (test in c("welch", "anova", "kw")) {
    res <- run_group_test(d, test)
    expect_s3_class(res$posthoc, "data.frame")
    expect_gt(nrow(res$posthoc), 0)
    expect_true("Sig" %in% names(res$posthoc))
  }
})

test_that("group_test_report appends the Brown-Forsythe check", {
  res <- group_test_report(make_groups(shift = 2), "welch")
  joined <- paste(res$text, collapse = "\n")

  expect_match(joined, "Brown-Forsythe variance test")
  expect_match(joined, "unequal variances|No evidence of unequal variances")
})

test_that("'auto' resolves to Welch and says so", {
  res <- group_test_report(make_groups(shift = 2), "auto")
  expect_match(res$text[1], "Automatic choice: Welch")
})

test_that("group_test_report needs at least two groups", {
  d <- data.frame(y = rnorm(10), g = factor(rep("only", 10)))
  res <- group_test_report(d, "auto")

  expect_equal(res$text, "Need at least 2 groups.")
  expect_null(res$posthoc)
})
