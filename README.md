# Publication Statistics Dashboard

An interactive R Shiny application for data preparation, descriptive statistics,
publication-quality graphics, group comparisons, correlations, principal
component analysis, regression, and data-quality assessment.

## Repository contents

- `app.R` — standard Shiny entry point used by deployment services.
- `Publication_Statistics_Dashboard.R` — complete user interface, server logic,
  statistical methods, visualizations, and export functions.
- `install_dependencies.R` — installs the packages needed by the app.
- `.gitignore` — prevents local R files, deployment metadata, and uploaded data
  from being committed accidentally.

## Run locally

Install a current version of R, download or clone this repository, and open a
terminal in the repository folder. Install the dependencies once:

```r
source("install_dependencies.R")
```

Then run the app:

```r
shiny::runApp()
```

The app opens in the default web browser. Uploaded datasets are processed only
for the active Shiny session and are not included in this repository.

## Upload the project to GitHub

1. Create an empty repository on GitHub.
2. Extract the supplied ZIP file.
3. Upload all extracted files to the repository root, preserving their names.
4. Commit the files to the `main` branch.

The repository can also be cloned and run directly from R:

```r
shiny::runGitHub("YOUR_REPOSITORY", "YOUR_GITHUB_USERNAME")
```

Replace the two placeholders with the repository name and GitHub username.

## Deploy to shinyapps.io

GitHub stores and versions the source code but does not itself execute an R
Shiny server. To make the app publicly accessible, deploy the cloned repository
to shinyapps.io or Posit Connect.

Create a shinyapps.io account, obtain the account name, token, and secret from
the shinyapps.io dashboard, and configure them locally without adding them to
GitHub:

```r
rsconnect::setAccountInfo(
  name = "YOUR_ACCOUNT_NAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

rsconnect::deployApp(
  appDir = ".",
  appName = "publication-statistics-dashboard",
  appTitle = "Publication Statistics Dashboard"
)
```

Never commit an account token or secret to the repository.

## Supported inputs and outputs

The dashboard accepts CSV, TSV, semicolon-delimited, pipe-delimited, and Excel
files. Excel support is supplied by `readxl`. It exports prepared data,
statistical tables, analysis bundles, and figures in PNG, TIFF, PDF, or SVG
formats where supported by the selected analysis.

The maximum upload size is 100 MB per Shiny session. A hosting provider may
apply a smaller platform-level limit.
