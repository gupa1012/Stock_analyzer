# install.R
# Run this script once to install all required R packages.
# Usage:  Rscript install.R

pkgs <- c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "shinycssloaders",
  "RSelenium",
  "rvest",
  "plotly",
  "DT",
  "dplyr",
  "tidyr",
  "stringr",
  "jsonlite",
  "htmltools",
  "httr"
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages are already installed.")
}

message("Done.")
