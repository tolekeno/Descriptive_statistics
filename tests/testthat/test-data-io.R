test_that("detect_delimiter recognises the common separators", {
  make_file <- function(lines) {
    path <- tempfile(fileext = ".txt")
    writeLines(lines, path)
    path
  }

  expect_equal(detect_delimiter(make_file(c("a,b,c", "1,2,3"))), ",")
  expect_equal(detect_delimiter(make_file(c("a;b;c", "1;2;3"))), ";")
  expect_equal(detect_delimiter(make_file(c("a\tb\tc", "1\t2\t3"))), "\t")
  expect_equal(detect_delimiter(make_file(c("a|b|c", "1|2|3"))), "|")
})

test_that("detect_delimiter skips blank lines and defaults to comma", {
  path <- tempfile(fileext = ".txt")
  writeLines(c("", "   ", "a;b", "1;2"), path)
  expect_equal(detect_delimiter(path), ";")

  single <- tempfile(fileext = ".txt")
  writeLines(c("onlyonecolumn", "value"), single)
  expect_equal(detect_delimiter(single), ",")

  empty <- tempfile(fileext = ".txt")
  writeLines(character(), empty)
  expect_equal(detect_delimiter(empty), ",")
})

test_that("read_tabular round-trips a comma file and de-duplicates names", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(" x , x , y ", "1,2,3", "4,5,6"), path)

  df <- read_tabular(path, path, delim = "auto")
  expect_equal(nrow(df), 2)
  expect_equal(names(df), c("x", "x_1", "y"))
  expect_equal(df$y, c(3, 6))
})

test_that("read_tabular treats the documented placeholders as missing", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,NA", "2,#N/A", "3,."), path)

  df <- read_tabular(path, path)
  expect_true(all(is.na(df$b)))
})

test_that("read_tabular rejects a file with no data rows", {
  path <- tempfile(fileext = ".csv")
  writeLines("a,b", path)
  expect_error(read_tabular(path, path), "no usable data")
})

test_that("col_classes treats low-cardinality integers as categorical", {
  df <- data.frame(
    yield   = rnorm(60),
    block   = rep(1:4, length.out = 60),
    variety = rep(c("A", "B"), length.out = 60),
    flag    = rep(c(TRUE, FALSE), length.out = 60),
    stringsAsFactors = FALSE
  )
  cls <- col_classes(df)

  expect_equal(cls$numeric, "yield")
  expect_setequal(cls$categorical, c("block", "variety", "flag"))
  expect_equal(cls$all, names(df))
})

test_that("col_classes keeps high-cardinality integers numeric", {
  df <- data.frame(id = 1:100)
  expect_equal(col_classes(df)$numeric, "id")
})

test_that("col_classes keeps continuous columns numeric even when few distinct", {
  df <- data.frame(x = rep(c(1.5, 2.5), 30))
  expect_equal(col_classes(df)$numeric, "x")
})
test_that("auto delimiter detection handles quoted text and decimal commas", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c('label;value', '"a,b,c,d";1,2', '"e,f,g,h";3,4'), path)
  expect_identical(detect_delimiter(path), ";")
  expect_identical(read_tabular(path)$label, c("a,b,c,d", "e,f,g,h"))
})

test_that("extensionless Shiny uploads can read the selected Excel sheet", {
  skip_if_not_installed("readxl")
  path <- tempfile()
  on.exit(unlink(path))
  example <- readxl::readxl_example("datasets.xlsx")
  file.copy(example, path)
  sheet <- readxl::excel_sheets(example)[2L]
  expected <- as.data.frame(readxl::read_xlsx(example, sheet = sheet))
  expect_identical(read_tabular(path, "datasets.xlsx", sheet = sheet), expected)
})
