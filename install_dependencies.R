# Install packages needed to run the Publication Statistics Dashboard.

cran_repository <- "https://cloud.r-project.org"

app_packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "ggplot2",
  "dplyr",
  "tidyr",
  "viridis",
  "corrplot",
  "GGally",
  "moments",
  "scales",
  "plotly",
  "ggthemes",
  "broom",
  "FactoMineR",
  "factoextra",
  "readxl",
  "patchwork",
  "gridExtra"
)

missing_packages <- app_packages[
  !vapply(app_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  install.packages(missing_packages, repos = cran_repository, dependencies = TRUE)
} else {
  message("All application dependencies are already installed.")
}

# rsconnect is used from the developer's computer to publish to shinyapps.io
# or Posit Connect. It is not required by the running application.
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = cran_repository)
}

message("Dependency setup is complete.")
