#' DescriptiveStats: Publication-Quality Descriptive Statistics Dashboard
#'
#' A modular Shiny application for exploratory data analysis and
#' publication-ready reporting of tabular data. Launch it with
#' [run_app()].
#'
#' The package is organised in three layers:
#' \describe{
#'   \item{Analysis helpers}{Plain functions with no Shiny dependency, such as
#'     [describe_numeric()], [cor_pmat()] and [run_group_test()]. These are
#'     unit tested and can be used outside the app.}
#'   \item{Shiny modules}{One module per dashboard tab, e.g.
#'     \code{mod_pca_ui()} / \code{mod_pca_server()}.}
#'   \item{Application}{[app_ui()], [app_server()] and [run_app()].}
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @importFrom stats aov anova coef complete.cases cooks.distance cor.test
#' @importFrom stats df.residual fitted kruskal.test lm median oneway.test
#' @importFrom stats p.adjust pairwise.t.test pairwise.wilcox.test qqnorm qt
#' @importFrom stats quantile reformulate reorder residuals rstandard sd
#' @importFrom stats setNames shapiro.test t.test TukeyHSD var wilcox.test
#' @importFrom utils capture.output head modifyList str write.csv
#' @importFrom grDevices colorRampPalette dev.off pdf png svg tiff
## usethis namespace: end
NULL
