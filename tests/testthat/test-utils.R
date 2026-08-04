test_that("%||% falls back on NULL and zero-length values", {
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal(character() %||% "fallback", "fallback")
  expect_equal("value" %||% "fallback", "value")
  expect_equal(0 %||% "fallback", 0)
  expect_false(FALSE %||% TRUE)
})

test_that("safe_filename strips characters that are illegal in file names", {
  expect_equal(safe_filename("Grain yield (t/ha)"), "Grain_yield_t_ha")
  expect_equal(safe_filename("a//b"), "a_b")
  expect_equal(safe_filename("___"), "variable")
  expect_equal(safe_filename("plain-name_1.csv"), "plain-name_1.csv")
})

test_that("expand_palette interpolates rather than recycling", {
  pal <- c("#000000", "#FFFFFF")
  expect_length(expand_palette(pal, 2), 2)
  expect_equal(expand_palette(pal, 1), "#000000")
  expect_length(expand_palette(pal, 7), 7)
  expect_false(anyDuplicated(expand_palette(pal, 7)) > 0)
  expect_length(expand_palette(pal, 0), 0)
})

test_that("round_numeric leaves non-numeric columns and odd names alone", {
  df <- data.frame(a = 1.23456, b = "x", check.names = FALSE)
  names(df)[1] <- "CV (%)"
  out <- round_numeric(df, 2)
  expect_equal(out[["CV (%)"]], 1.23)
  expect_equal(out$b, "x")
  expect_equal(names(out), c("CV (%)", "b"))
})

test_that("numeric_columns keeps only numeric columns as a data frame", {
  df <- data.frame(a = 1:3, b = letters[1:3], c = c(1.5, 2.5, 3.5))
  out <- numeric_columns(df)
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("a", "c"))
})

test_that("numeric_columns returns a zero-column frame when nothing is numeric", {
  out <- numeric_columns(data.frame(a = letters[1:3]))
  expect_equal(ncol(out), 0)
  expect_equal(nrow(out), 3)
})
