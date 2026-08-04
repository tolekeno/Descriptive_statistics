# DescriptiveStats

A publication-quality descriptive statistics dashboard for tabular data, built
with Shiny and packaged as an installable R package.

Upload a CSV, TSV or Excel file and get exploratory data analysis, statistical
testing and journal-ready figures across ten tabs — no code required.

## Features

| Tab | What it does |
| --- | --- |
| **Data Upload** | CSV / TSV / semicolon / pipe with automatic delimiter detection; Excel with a sheet picker |
| **Overview** | Row, column, numeric and missingness headline counts; structure dump; variable-type chart |
| **Data Prep** | Column selection, row filtering, and eight numeric transformations (log, log10, sqrt, z-score, centre, reciprocal, Box-Cox) |
| **Descriptive Stats** | N, missing, mean, median, SD, quartiles, IQR — plus SE, CV, skewness, kurtosis and range for selected variables |
| **Distributions** | Histogram, density, boxplot, violin and Q-Q plots, with grouping, faceting and per-variable export |
| **Group Comparisons** | Welch / Student t-test, one-way ANOVA + Tukey, Wilcoxon, Kruskal-Wallis + pairwise Wilcoxon, each with a Brown-Forsythe variance check |
| **Correlations** | Pearson / Spearman / Kendall matrix with significance stars and Holm adjustment, scatter plot matrix, and an interactive pairwise scatter |
| **PCA** | Scree plot, score plot with confidence ellipses, correlation circle, contribution bars, and eigenvalue / loadings / scores tables |
| **Regression** | Multiple linear regression with a coefficient table, fit statistics and four diagnostic panels |
| **Data Quality** | Missingness, IQR outlier counts, and Holm-adjusted Shapiro-Wilk normality assessment |

Every figure uses one of eleven journal-style palettes (Nature, Science,
Lancet, JAMA, NEJM, Economist, Tableau10, Okabe-Ito, and three viridis ramps)
and exports to PNG, PDF, TIFF or SVG at a chosen DPI and page size.

The dashboard accepts CSV, TSV, semicolon-delimited, pipe-delimited and Excel
files. The maximum upload size is 100 MB per session; a hosting provider may
apply a smaller platform-level limit. Uploaded datasets are processed only for
the active session and are never written to this repository.

## Installation

```r
# install.packages("remotes")
remotes::install_github("tolekeno/Descriptive_statistics")
```

Installing the package pulls in every dependency automatically. Excel support
is optional; install `readxl` to enable it:

```r
install.packages("readxl")
```

## Usage

```r
library(DescriptiveStats)
run_app()
```

`run_app()` accepts `max_upload_mb` (default 100) and `launch.browser`.

## Deploying to shinyapps.io or Posit Connect

GitHub stores and versions the source but does not run an R Shiny server. To
make the app publicly accessible, deploy the app directory shipped inside the
installed package:

```r
install.packages("rsconnect")

rsconnect::setAccountInfo(
  name   = "YOUR_ACCOUNT_NAME",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

rsconnect::deployApp(
  appDir   = system.file("app", package = "DescriptiveStats"),
  appName  = "publication-statistics-dashboard",
  appTitle = "Publication Statistics Dashboard"
)
```

Never commit an account token or secret to the repository.

## Using the analysis functions without the app

The statistical layer is plain R with no Shiny dependency, so it can be used
directly in a script or R Markdown report:

```r
library(DescriptiveStats)

describe_numeric(mtcars[, c("mpg", "wt", "hp")], extended = TRUE)

d <- data.frame(y = mtcars$mpg, g = factor(mtcars$cyl))
cat(paste(group_test_report(d, "auto")$text, collapse = "\n"))

rf <- fit_regression(mtcars, "mpg", c("wt", "hp"))
regression_fit_stats(rf)
regression_diagnostics(rf)

fit_pca(mtcars, c("mpg", "wt", "hp", "disp"))$pca$eig
```

## Project structure

```
R/
  utils.R, palettes.R, theme.R        # helpers, colour palettes, ggplot theme, export
  data-io.R                           # delimiter detection, file reading, column typing
  stats-helpers.R                     # descriptives, correlations, outliers, normality
  transform.R                         # numeric transformations and Box-Cox
  group-tests.R                       # group comparison tests and Brown-Forsythe
  regression.R, pca.R                 # model fitting and diagnostics
  plots-distribution.R                # distribution and faceted plots
  plots-correlation.R                 # correlation matrix and scatter plots
  mod-*.R                             # one Shiny module per tab, plus sidebar modules
  app-ui.R, app-server.R, run-app.R   # application assembly
inst/app/app.R                        # deployment entry point
tests/testthat/                       # unit tests for the analysis layer
legacy/                               # the original single-file script, for reference
```

The three layers are deliberately separate: the analysis functions know nothing
about Shiny, the modules know nothing about each other, and the sidebar
`settings` module hands every tab the same styling and export configuration.

Before version 1.0.0 the app was a single 2,132-line script run with
`shiny::runApp()`. That script is preserved unchanged under `legacy/`; see
[NEWS.md](NEWS.md) for what changed.

## Development

```r
devtools::load_all()
devtools::test()
devtools::document()
```

## License

MIT — see [LICENSE.md](LICENSE.md).
