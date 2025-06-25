#####
# Libraries and Preliminaries
#####
library(tibble)
library(purrr)
library(stringr)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(xml2)
library(rvest)
library(here)
library(janitor)

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
keywords <- c("accelerator", "artificial intelligence", "statistics", "prediction", "forecast", "geoscience", "climate", "ocean", "oceanography", "ecosystem", "fisheries", "biology", "STEM", "education", "resilience", "modeling", "habitat")

df_filtered <- df %>%
  filter(str_detect(Title, regex(str_c(keywords, collapse = "|"), ignore_case = TRUE))) |>
  filter(!str_detect(Agency, regex("U\\.S\\. Mission", ignore_case = TRUE))) |>
  filter(!str_detect(Agency, "National Institutes of Health"))


#####
# NOAA Fisheries
#####
noaa_page <- read_html("Data/NOAA_Fisheries.html")

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
}) |>
    filter(is.na(Deadline) | Deadline >= today())


#####
# MAFMC Latest News
#####
# Load the homepage
# Read the saved MAFMC HTML page
mafmc_page <- read_html("Data/MAFMC.html")

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

mafmc_df <- mafmc_df |>
  filter(str_detect(str_to_lower(Title), "funding|contractor|proposals")) |>
  filter(is.na(Deadline) | Deadline >= today())


#####
# Maine RFAs
#####

# Load Maine grants page
me_page <- read_html("https://www.maine.gov/dafs/bbm/procurementservices/vendors/grants")

# Parse rows in the RFA table
# Extract the grant opportunity rows (adjust the selector as needed)
me_table <- me_page |>
  html_element("table") |>
  html_table() |>
  clean_names() |>
  filter(rfa_status == "Open")

me_table <- me_table |>
  mutate(application_due_date = na_if(application_due_date, ""), application_due_date = if_else(is.na(application_due_date), format(Sys.Date(), "%m/%d/%Y"), application_due_date))

# Some formatting
me_df_rfa<- tibble(
    OpportunityID = as.character(me_table$rfa_number),
    Agency = "State of Maine - RFA",
    Title = me_table$rfa_title,
    Deadline = as.Date(me_table$application_due_date, "%m/%d/%Y"),
    Posted = as.Date(me_table$date_posted, "%m/%d/%Y"),
    AdditionalInfoURL = "https://www.maine.gov/dafs/bbm/procurementservices/vendors/grants"
  )

#####
# Maine RFAs
#####

# Load Maine grants page
me_page <- read_html("https://www.maine.gov/dafs/bbm/procurementservices/vendors/rfps")

# Parse rows in the RFA table
# Extract the grant opportunity rows (adjust the selector as needed)
me_table <- me_page |>
  html_element("table") |>
  html_table() |>
  clean_names() |>
  filter(rfp_status == "Open")

me_table <- me_table |>
  mutate(proposal_due_date = na_if(proposal_due_date, ""), proposal_due_date = if_else(is.na(proposal_due_date), format(Sys.Date(), "%m/%d/%Y"), proposal_due_date))

# Some formatting
me_df_rfp<- tibble(
    OpportunityID = as.character(me_table$rfp_number),
    Agency = "State of Maine - RFP",
    Title = me_table$title,
    Deadline = as.Date(me_table$proposal_due_date, "%m/%d/%Y"),
    Posted = as.Date(me_table$date_posted, "%m/%d/%Y"),
    AdditionalInfoURL = "https://www.maine.gov/dafs/bbm/procurementservices/vendors/rfps"
  )

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

# Combine and filter
out <- bind_rows(df_filtered, noaa_df, mafmc_df, me_df_rfa, me_df_rfp) |>
  mutate(
    Title = replace_na(Title, ""),
    Deadline = as.Date(Deadline),
    IsNew = if (nrow(prev_df) > 0) {
      !Title %in% replace_na(prev_df$Title, "")
    } else {
      TRUE
    }
  ) %>%
  filter(!is.na(Deadline)) %>%
  filter(Deadline >= today_date)

# Add "No new updates" row if nothing is new
if (!any(out$IsNew)) {
  no_new_row <- tibble(
    OpportunityID = NA_character_,
    Agency = "",
    Title = "📭 No new funding opportunities since the last update.",
    Deadline = NA_Date_,
    Posted = NA_Date_,
    AdditionalInfoURL = "",
    IsNew = FALSE
  )
  out <- bind_rows(no_new_row, out)
}

out <- bind_rows(df_filtered, noaa_df, mafmc_df, me_df_rfa, me_df_rfp) %>%
  mutate(
    Title = replace_na(Title, ""),
    Deadline = as.Date(Deadline),
    Posted = as.Date(Posted),
    IsNew = if (nrow(prev_df) > 0) {
      !Title %in% replace_na(prev_df$Title, "")
    } else {
      TRUE
    }
  ) %>%
  filter(!is.na(Deadline)) %>%
  filter(Deadline >= Sys.Date()) %>%  # <-- THIS LINE ENSURES ONLY FUTURE DEADLINES
  arrange(desc(IsNew), Deadline, Agency)


write_csv(out, csv_file)
