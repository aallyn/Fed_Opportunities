# Load necessary libraries
library(here)
library(httr)
library(rmarkdown)
library(blastula)
library(glue)
library(lubridate)

# Set directories
project_dir <- here::here()
data_dir <- file.path(project_dir, "Data")
report_file <- file.path(project_dir, "Opportunity_Report.qmd")
today <- format(Sys.Date(), "%Y-%m-%d")
output_html <- file.path(project_dir, glue("docs/Opportunity_Report_{today}.html"))
# output_pdf <- file.path(project_dir, glue("Opportunity_Report_{today}.pdf"))

# Ensure data directory exists
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Step 1: Download HTML source pages
urls <- c(
  "https://www.fisheries.noaa.gov/funding-opportunities/open-opportunities",
  "https://www.mafmc.org/"
)

paths <- c(
  file.path(data_dir, "NOAA_Fisheries.html"),
  file.path(data_dir, "MAFMC.html")
)

for (i in seq_along(urls)) {
  page <- httr::GET(urls[i])
  writeBin(httr::content(page, "raw"), paths[i])
}

# Step 2: Render HTML and PDF versions of the report
# system("quarto render Opportunity_Report.qmd --output-dir docs")
# rmarkdown::render(report_file, output_format = "pdf", output_file = output_pdf)
