#####
# Libraries and Preliminaries
#####
library(xml2)
library(tidyverse)
library(lubridate)
library(rvest)
library(blastula)

today_str <- format(Sys.Date(), "%Y%m%d")
today_date <- today()  

#####
# Grants.gov 
#####
# Step 1: Create today's filename and download URL
today_str <- format(Sys.Date(), "%Y%m%d")
today_date <- today()  # today's date
filename <- paste0("GrantsDBExtract", today_str, "v2.zip")
base_url <- "https://prod-grants-gov-chatbot.s3.amazonaws.com/extracts/"
url <- paste0(base_url, filename)

# Step 2: Download ZIP file
zip_path <- tempfile(fileext = ".zip")
tryCatch({
  download.file(url, zip_path, mode = "wb")
}, error = function(e) {
  stop("Download failed. Check if the file exists at: ", url)
})

# Step 3: Unzip XML file
unzip_dir <- tempdir()
unzip(zip_path, exdir = unzip_dir)

# Step 4: Find and read the XML
xml_file <- list.files(unzip_dir, pattern = "\\.xml$", full.names = TRUE)[1]
# Load XML file and register namespaces
doc <- read_xml(xml_file)
ns <- xml_ns_rename(xml_ns(doc), d1 = "ns")

# Find all opportunity nodes
opps <- xml_find_all(doc, ".//ns:OpportunitySynopsisDetail_1_0", ns)

# Extract relevant fields per opportunity node
df <- tibble(
    OpportunityID = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:OpportunityID", ns))),
    Agency = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:AgencyName", ns))),
    Title = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:OpportunityTitle", ns))),
    Deadline = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:CloseDate", ns))),
    Posted = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:PostDate", ns))),
    AdditionalInfoURL = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:AdditionalInformationURL", ns))),
)

# Assuming df already has Deadline parsed as Date or POSIXct
df <- df |>
    mutate(
        Deadline = na_if(Deadline, ""),
        Posted = na_if(Posted, ""),
        # Try parsing ISO 8601 datetime first, fallback to date only if needed
        Deadline = parse_date_time(Deadline, orders = c("ymd", "ymd_HMS", "mdy")),
        Posted = parse_date_time(Posted, orders = c("ymd", "ymd_HMS", "mdy"))
    ) |>
    filter(!is.na(Deadline)) |>      # exclude missing deadlines
    filter(Deadline >= today_date)      # only deadlines today or later

# Step 6: Filter relevant, recent opportunities
keywords <- c("artificial intelligence", "statistics", "geoscience", "climate", "ocean", "oceanography", "ecosystem", "fisheries", "biology", "STEM", "education", "resilience", "modeling")

df_filtered <- df %>%
    filter(
        Posted >= today() - days(7),
        str_detect(str_to_lower(Title), str_c(keywords, collapse = "|"))
    )


#####
# NOAA Fisheries
#####
noaa_page <- read_html("Data/Grants _ NOAA Fisheries.html")

# Find all opportunity cards
noaa_cards <- html_elements(noaa_page, ".vertical-list__item")

# Extract and align with Grants.gov structure
noaa_df <- map_dfr(noaa_cards, function(card) {
    title_node <- html_element(card, "h4.funding-item__title a")
    title <- html_text(title_node, trim = TRUE)
    url <- html_attr(title_node, "href")
    if (!is.na(url) && !str_starts(url, "http")) {
        url <- paste0("https://www.fisheries.noaa.gov", url)
    }

    summary <- html_text(html_element(card, ".funding-item__summary"), trim = TRUE)

    deadline_text <- html_text(html_element(card, ".funding-item__close-date"), trim = TRUE)
    deadline <- parse_date_time(deadline_text, orders = c("mdy", "Ymd"))

    posted_text <- html_text(html_element(card, ".funding-item__open-date"), trim = TRUE)
    posted <- parse_date_time(posted_text, orders = c("mdy", "Ymd"))

    tibble(
        OpportunityID = NA_character_,
        Agency = "NOAA Fisheries",
        Title = title,
        Deadline = deadline,
        Posted = posted,
        AdditionalInfoURL = url
    )
}) %>%
    filter(is.na(Deadline) | Deadline >= today())


#####
# MAFMC Latest News
#####
# Load the homepage
# Read the saved MAFMC HTML page
mafmc_page <- read_html("Data/Mid-Atlantic Fishery Management Council.html")

# Extract each summary-item (each news card)
news_cards <- html_elements(mafmc_page, ".summary-item")

# Parse each card
mafmc_df <- map_dfr(news_cards, function(card) {
  title <- html_element(card, ".summary-title") |>
    html_text(trim = TRUE)

  url <- html_element(card, ".summary-title a") |>
    html_attr("href")

  # Fix relative URL if needed
  if (!is.na(url) && !str_starts(url, "http")) {
    url <- paste0("https://www.mafmc.org", url)
  }

  # Optional: parse excerpt
  excerpt <- html_element(card, ".summary-excerpt") |>
      html_text(trim = TRUE)
  
  date_node <- html_element(card, "time.summary-metadata-item--date")
  date <- html_attr(date_node, "datetime")
  if (!is.na(date)) {
    date <- ymd(date)
  }

  tibble(
    OpportunityID = NA_character_,
    Agency = "MAFMC",
    Title = title,
    Deadline = as.Date(date),
    Posted = as.Date(date),
    AdditionalInfoURL = url
  )
})

mafmc_df<- mafmc_df |>
  filter(str_detect(str_to_lower(Title), "funding|contractor|proposals"))

#####
# Saving and rendering
#####
csv_file <- file.path(here::here(), paste0("GMRI_Grants.csv"))

# Read previous data if exists
prev_file <- csv_file
if (file.exists(prev_file)) {
  prev_df <- read_csv(prev_file)
} else {
  prev_df <- tibble()  # empty if no previous data
}

# Mark new rows in current dataset
out <- bind_rows(df_filtered, noaa_df, mafmc_df) %>%
  mutate(
    Title = replace_na(Title, ""),  # replace NA with empty string to avoid issues
    IsNew = if (nrow(prev_df) > 0) {
      # also replace NA in prev_df$Title
      !Title %in% replace_na(prev_df$Title, "")
    } else {
      TRUE  # all new if no previous data
    }
  ) %>%
  arrange(desc(IsNew), Deadline, Agency)

write_csv(out, csv_file)

# Render Markdown (optional)
quarto::quarto_render("grants_summary.qmd", output_file = "index.html")


# Compose the email
# email <- compose_email(
#   body = md(glue::glue("
# Hey Felipe,

# Here's our latest grants report generated on {Sys.Date()}.

# All the best,

# Andrew
# "))
# )

# Send the email
# smtp_send(
#   email,
#   from = "aallyn@gmri.org",
#   to = c("fbenavides@gmri.org", "aallyn@gmri.org"),
#   subject = paste("Grants Report", Sys.Date()),
#   attachments = paste0("grants_summary_", Sys.Date(), ".html"),
#   credentials = creds_key("outlook")
# )

# smtp_send(
#   email,
#   from = "andrew.allyn@gmail.com",
#   to = c("fbenavides@gmri.org", "aallyn@gmri.org"),
#   subject = paste("Grants Report", Sys.Date()),
#   credentials = creds_key("gmail")
# )
