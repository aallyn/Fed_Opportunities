library(httr)

urls <- c(
  "https://www.fisheries.noaa.gov/funding-opportunities/open-opportunities",
  "https://www.mafmc.org/"
)

paths <- c(
  "Data/NOAA_Fisheries.html",
  "Data/MAFMC.html"
)

for (i in seq_along(urls)) {
  page <- GET(urls[i])
  writeBin(content(page, "raw"), paths[i])
}

quarto::quarto_render("grants_summary.qmd", output_file = "index.html")