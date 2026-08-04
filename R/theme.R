#' Available plot themes
#'
#' @return Named character vector suitable for `selectInput(choices = )`.
#' @export
plot_theme_choices <- function() {
  c("Minimal"   = "minimal",
    "Classic"   = "classic",
    "Light"     = "light",
    "Gray"      = "gray",
    "BW"        = "bw",
    "Economist" = "economist",
    "Stata"     = "stata")
}

#' Build the shared publication ggplot theme
#'
#' Every figure in the dashboard is finished with this theme so that exported
#' panels are visually consistent.
#'
#' @param theme_name One of [plot_theme_choices()].
#' @param base_size Base font size in points.
#' @param grid_lines Draw major grid lines?
#' @param bold_titles Render titles and axis labels bold?
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + build_plot_theme("bw", 12)
build_plot_theme <- function(theme_name = "minimal",
                             base_size = 13,
                             grid_lines = TRUE,
                             bold_titles = TRUE) {
  bs   <- base_size
  bold <- if (isTRUE(bold_titles)) "bold" else "plain"
  grid <- if (isTRUE(grid_lines)) {
    element_line(color = "gray90", linewidth = 0.3)
  } else {
    element_blank()
  }

  base_theme <- switch(
    theme_name %||% "minimal",
    "minimal"   = theme_minimal(base_size = bs),
    "classic"   = theme_classic(base_size = bs),
    "light"     = theme_light(base_size = bs),
    "gray"      = theme_gray(base_size = bs),
    "bw"        = theme_bw(base_size = bs),
    "economist" = ggthemes::theme_economist(base_size = bs),
    "stata"     = ggthemes::theme_stata(base_size = bs),
    theme_minimal(base_size = bs)
  )

  base_theme + theme(
    plot.title        = element_text(size = bs + 3, face = bold, hjust = 0.5,
                                     margin = margin(b = 12)),
    plot.subtitle     = element_text(size = bs, hjust = 0.5, margin = margin(b = 8)),
    axis.title.x      = element_text(size = bs + 1, face = bold, margin = margin(t = 8)),
    axis.title.y      = element_text(size = bs + 1, face = bold, margin = margin(r = 8)),
    axis.text         = element_text(size = bs - 1, color = "black"),
    legend.title      = element_text(size = bs, face = bold),
    legend.text       = element_text(size = bs - 1),
    legend.position   = "right",
    legend.background = element_rect(fill = "white", color = NA),
    strip.text        = element_text(size = bs, face = bold, color = "black",
                                     margin = margin(4, 4, 4, 4)),
    strip.background  = element_rect(fill = "gray95", color = "gray70", linewidth = 0.4),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.border      = element_rect(fill = NA, color = "black", linewidth = 0.8),
    panel.grid.major  = grid,
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "white", color = NA),
    plot.margin       = margin(12, 12, 12, 12)
  )
}

#' Export formats offered in the sidebar
#'
#' @return Named character vector for `selectInput(choices = )`.
#' @export
export_format_choices <- function() {
  c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff", "SVG" = "svg")
}

#' Default export settings
#'
#' @return A list with `format`, `width`, `height` and `dpi`.
#' @export
default_export <- function() {
  list(format = "png", width = 9, height = 6, dpi = 300)
}

#' Save a ggplot using the current export settings
#'
#' @param plot A ggplot (or patchwork / grob) object.
#' @param file Destination path.
#' @param export A list as produced by [default_export()].
#' @return Invisibly, `file`.
#' @export
save_plot <- function(plot, file, export = default_export()) {
  if (is.null(plot)) stop("There is no plot to export.", call. = FALSE)
  fmt <- export$format %||% "png"
  device <- switch(fmt,
                   "png"  = "png",
                   "pdf"  = "pdf",
                   "tiff" = "tiff",
                   "svg"  = grDevices::svg,
                   "png")
  args <- list(
    filename = file,
    plot     = plot,
    device   = device,
    width    = export$width  %||% 9,
    height   = export$height %||% 6,
    dpi      = export$dpi    %||% 300,
    units    = "in",
    bg       = "white"
  )
  if (identical(fmt, "tiff")) args$compression <- "lzw"
  do.call(ggsave, args)
  invisible(file)
}

#' Open a graphics device matching the current export settings
#'
#' Base-graphics figures (the corrplot matrix) cannot go through
#' [save_plot()], so they open a device directly. The caller is responsible
#' for calling [grDevices::dev.off()].
#'
#' @param file Destination path.
#' @param export A list as produced by [default_export()].
#' @return Invisibly `NULL`; called for the side effect of opening a device.
#' @export
open_export_device <- function(file, export = default_export()) {
  fmt <- export$format %||% "png"
  w   <- export$width  %||% 9
  h   <- export$height %||% 6
  dpi <- export$dpi    %||% 300
  switch(
    fmt,
    "png"  = grDevices::png(file, width = w, height = h, units = "in",
                            res = dpi, bg = "white"),
    "pdf"  = grDevices::pdf(file, width = w, height = h, bg = "white"),
    "tiff" = grDevices::tiff(file, width = w, height = h, units = "in",
                             res = dpi, bg = "white", compression = "lzw"),
    "svg"  = grDevices::svg(file, width = w, height = h, bg = "white"),
    grDevices::png(file, width = w, height = h, units = "in",
                   res = dpi, bg = "white")
  )
  invisible(NULL)
}
