test_that("every palette has usable colours", {
  pals <- publication_palettes()

  expect_gt(length(pals), 5)
  for (nm in names(pals)) {
    expect_length(pals[[nm]], 8)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}", pals[[nm]])), info = nm)
  }
})

test_that("get_palette falls back for an unknown name", {
  expect_equal(get_palette("Nature"), publication_palettes()[["Nature"]])
  expect_equal(get_palette("does not exist"), publication_palettes()[["Nature"]])
  expect_equal(get_palette(NULL), publication_palettes()[["Nature"]])
})

test_that("build_plot_theme returns a theme for every choice", {
  for (nm in unname(plot_theme_choices())) {
    expect_s3_class(build_plot_theme(nm, 12), "theme")
  }
  expect_s3_class(build_plot_theme("not a theme"), "theme")
})

test_that("build_dist_plot produces a ggplot for every plot type", {
  for (type in unname(dist_plot_choices())) {
    p <- build_dist_plot(mtcars, "mpg", dist_options(plot_type = type))
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("build_dist_plot honours a grouping variable", {
  df <- mtcars
  df$cyl <- factor(df$cyl)
  p <- build_dist_plot(df, "mpg", dist_options(plot_type = "box", group_var = "cyl"))

  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("build_dist_plot explains rather than errors on unplottable data", {
  df <- data.frame(x = rep(NA_real_, 5))
  p <- build_dist_plot(df, "x", dist_options())
  expect_s3_class(p, "ggplot")

  flat <- data.frame(x = rep(2, 5))
  p2 <- build_dist_plot(flat, "x", dist_options(plot_type = "density"))
  expect_s3_class(p2, "ggplot")
})

test_that("build_faceted_plot builds for every plot type", {
  for (type in unname(dist_plot_choices())) {
    p <- build_faceted_plot(mtcars, c("mpg", "wt", "hp"),
                            opts = dist_options(plot_type = type))
    expect_s3_class(p, "ggplot")
  }
})

test_that("build_faceted_plot accepts a second facet dimension", {
  df <- mtcars
  df$cyl <- factor(df$cyl)
  p <- build_faceted_plot(df, c("mpg", "wt"), facet_by = "cyl")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("drop_unusable_for_cor removes constant and near-empty columns", {
  df <- data.frame(
    good = rnorm(20),
    flat = rep(1, 20),
    sparse = c(1, 2, rep(NA, 18))
  )
  out <- drop_unusable_for_cor(df)
  expect_equal(names(out), "good")
})

test_that("corr_objects returns matched r and p matrices", {
  obj <- corr_objects(mtcars[, c("mpg", "wt", "hp")], holm = FALSE)

  expect_equal(dim(obj$mat), c(3, 3))
  expect_equal(dim(obj$pmat), c(3, 3))
  expect_equal(diag(obj$mat), c(mpg = 1, wt = 1, hp = 1))
})

test_that("Holm adjustment only raises p-values", {
  raw <- corr_objects(mtcars[, c("mpg", "wt", "hp", "disp")], holm = FALSE)
  adj <- corr_objects(mtcars[, c("mpg", "wt", "hp", "disp")], holm = TRUE)

  expect_true(all(adj$pmat >= raw$pmat, na.rm = TRUE))
})

test_that("corr_export_table stacks the r and p blocks", {
  obj <- corr_objects(mtcars[, c("mpg", "wt")])
  tb <- corr_export_table(obj)

  expect_equal(nrow(tb), 4)
  expect_equal(unique(tb$Statistic), c("r", "p"))
  expect_true(all(c("Variable", "Statistic", "mpg", "wt") %in% names(tb)))
})

test_that("pairwise_scatter_plot builds with and without colouring", {
  df <- mtcars
  df$cyl_f <- factor(df$cyl)

  expect_s3_class(pairwise_scatter_plot(df, "wt", "mpg"), "ggplot")
  expect_s3_class(pairwise_scatter_plot(df, "wt", "mpg", color_var = "cyl_f"), "ggplot")
  expect_s3_class(pairwise_scatter_plot(df, "wt", "mpg", color_var = "hp"), "ggplot")
  expect_s3_class(
    pairwise_scatter_plot(df, "wt", "mpg", show_regression = FALSE),
    "ggplot"
  )
})

test_that("save_plot writes a file in each supported format", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  for (fmt in unname(export_format_choices())) {
    file <- tempfile(fileext = paste0(".", fmt))
    save_plot(p, file, list(format = fmt, width = 5, height = 4, dpi = 150))
    expect_true(file.exists(file), info = fmt)
    expect_gt(file.size(file), 0)
  }
})

test_that("save_plot refuses a NULL plot", {
  expect_error(save_plot(NULL, tempfile()), "no plot to export")
})

test_that("open_export_device opens a writable device", {
  file <- tempfile(fileext = ".png")
  open_export_device(file, list(format = "png", width = 4, height = 3, dpi = 100))
  plot(1:10)
  grDevices::dev.off()

  expect_true(file.exists(file))
  expect_gt(file.size(file), 0)
})
