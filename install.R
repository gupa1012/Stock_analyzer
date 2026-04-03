# install.R
# Run this script once to install all required R packages.
# Usage:  Rscript install.R

pkgs <- c(
  # Core Shiny
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "shinycssloaders",
  # Data & scraping
  "RSelenium",
  "rvest",
  "httr",
  "jsonlite",
  "xml2",
  # Market data
  "quantmod",
  "alphavantager",
  "zoo",
  # Visualisation
  "plotly",
  "DT",
  "reactable",
  # Data manipulation
  "dplyr",
  "tidyr",
  "stringr",
  # HTML
  "htmltools"
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages are already installed.")
}

message("Done.")
