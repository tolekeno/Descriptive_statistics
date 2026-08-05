# DescriptiveStats 1.0.0

First packaged release. The dashboard was previously a single
2,132-line script (`Publication_Statistics_Dashboard.R`); it is now an
installable R package. The original script remains in the git history and can
be recovered with `git show 5cb3cbc:Publication_Statistics_Dashboard.R`.

## Structure

* Split into an analysis layer with no Shiny dependency, one Shiny module per
  tab, and a thin application assembly layer (`app_ui()`, `app_server()`,
  `run_app()`).
* Shared plot styling and export settings moved into a `settings` module that
  hands every tab the same configuration, replacing direct reads of sidebar
  inputs from across the server.
* Added unit tests covering the analysis layer.

## Fixes

* `brown_forsythe()` (the variance check on the Group Comparisons tab) called
  `tapply()` from the wrong namespace and returned `NULL`, so the variance test
  silently reported "not computed".
* Transformations no longer emit spurious `NaN` warnings; `log`, `log10`,
  `sqrt`, `reciprocal` and Box-Cox are now evaluated only on in-domain values.
* Label annotations use `linewidth` rather than the `label.size` argument
  removed in ggplot2 4.0, which was being ignored with a warning.
* Group colours are computed from dropped factor levels, so a grouping column
  carrying unused levels no longer shifts the palette.

## Changes

* Dropped the `moments` dependency; `skewness()` and `kurtosis()` are
  implemented directly and exported.
* Dropped the `viridis` dependency in favour of the lighter `viridisLite`.
* Regression diagnostics always use `patchwork`, which is now a hard
  dependency; the `gridExtra` fallback has been removed.
* The analysis bundle is written with `zip::zipr()` instead of `utils::zip()`,
  which needs an external zip binary on Windows.
