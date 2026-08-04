# GitHub and deployment entry point for the Shiny application.
# The complete application is kept in a separate, descriptively named source file.

app <- source(
  "Publication_Statistics_Dashboard.R",
  local = TRUE,
  chdir = TRUE
)$value

app
