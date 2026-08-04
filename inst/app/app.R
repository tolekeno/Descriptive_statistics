# Entry point for shiny::runApp() and for deployment to shinyapps.io,
# Posit Connect or Shiny Server. The package must be installed first:
#   remotes::install_github("tolekeno/Descriptive_statistics")
library(DescriptiveStats)

options(shiny.maxRequestSize = 100 * 1024^2)

shinyApp(ui = app_ui(), server = app_server)
