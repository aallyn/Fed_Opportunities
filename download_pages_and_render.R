library(httr)

# Define absolute paths
project_dir <- "/Users/aallyn/GitHub/Fed_Opportunities"
data_dir <- file.path(project_dir, "Data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Define URLs and their corresponding output paths (using full paths)
urls <- c(
  "https://www.fisheries.noaa.gov/funding-opportunities/open-opportunities",
  "https://www.mafmc.org/"
)

paths <- c(
  file.path(data_dir, "NOAA_Fisheries.html"),
  file.path(data_dir, "MAFMC.html")
)

# Download each page
for (i in seq_along(urls)) {
  page <- httr::GET(urls[i])
  writeBin(httr::content(page, "raw"), paths[i])
}

# Render the Quarto report (which quarto)
# Sys.setenv(QUARTO_PATH = "/Applications/quarto/bin/quarto")

# quarto::quarto_render(
#   input = file.path(project_dir, "Opportunity_Report.qmd"),
#   output_file = file.path(project_dir, "index.html")
# )
