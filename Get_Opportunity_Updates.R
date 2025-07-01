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

Sys.setenv(TZ = "America/New_York")
today_str <- format(Sys.Date(), "%Y%m%d")
today_date <- today()

#####
# Grants.gov
#####
# Step 1: Create today's filename and download URL
today_str <- format(Sys.Date(), "%Y%m%d")
today_date <- today() # today's date
filename <- paste0("GrantsDBExtract", today_str, "v2.zip")
base_url <- "https://prod-grants-gov-chatbot.s3.amazonaws.com/extracts/"
url <- paste0(base_url, filename)

# Step 2: Download ZIP file
zip_path <- tempfile(fileext = ".zip")
tryCatch(
  {
    download.file(url, zip_path, mode = "wb")
  },
  error = function(e) {
    stop("Download failed. Check if the file exists at: ", url)
  }
)

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
  AdditionalInfoURL = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:AdditionalInformationURL", ns)))
)

# Parse and filter dates
df <- df |>
  mutate(
    Deadline = na_if(Deadline, ""),
    Posted = na_if(Posted, ""),
    Deadline = parse_date_time(Deadline, orders = c("ymd", "ymd_HMS", "mdy")),
    Posted = parse_date_time(Posted, orders = c("ymd", "ymd_HMS", "mdy"))
  ) |>
  filter(!is.na(Deadline), Deadline >= today_date)

# Expanded keywords for GMRI relevance
keywords <- c(
  "climate", "resilience", "adaptation", "mitigation", "carbon", "sea level", "acidification", "greenhouse",
  "ocean", "marine", "ecosystem", "habitat", "estuarine", "biodiversity", "restoration",
  "fisheries", "stock assessment", "bycatch", "aquaculture", "shellfish", "seafood",
  "modeling", "forecast", "prediction", "statistics", "remote sensing", "geospatial", "machine learning", "artificial intelligence", "AI", "data science",
  "offshore wind", "renewable", "energy", "blue economy", "marine spatial planning",
  "coastal", "community", "equity", "stakeholder", "engagement", "participation", "governance",
  "STEM", "education", "workforce", "training", "teacher", "student", "capacity building", "internship"
)

filter_relevant <- function(df) {
  df %>% filter(
    str_detect(Title, regex(str_c(keywords, collapse = "|"), ignore_case = TRUE)),
    !str_detect(Title, regex("Warfare", ignore_case = TRUE)),
    !str_detect(Agency, regex("U\\.S\\. Mission", ignore_case = TRUE)),
    !str_detect(Agency, regex("National Institutes of Health", ignore_case = TRUE)),
    !str_detect(Agency, regex("Indian Health Service", ignore_case = TRUE)),
    !str_detect(Agency, regex("National Institute of Corrections", ignore_case = TRUE)),
    !str_detect(Agency, regex("Fort Worth District", ignore_case = TRUE)),
    !str_detect(Agency, regex("Alaska District", ignore_case = TRUE)),
    !str_detect(Agency, regex("Administration for Community Living", ignore_case = TRUE)),
    !str_detect(Agency, regex("Air Force", ignore_case = TRUE)),
    !str_detect(Agency, regex("Administration for Community Living", ignore_case = TRUE)),
    !str_detect(Agency, regex("Agency for Health Care Research and Quality", ignore_case = TRUE)),
    !str_detect(Agency, regex("Dept of the Army -- Materiel Command", ignore_case = TRUE)),
    !str_detect(Agency, regex("Department of Health and Human Services", ignore_case = TRUE)),
    !str_detect(Agency, regex("Office on Violence Against Women", ignore_case = TRUE)),
    !str_detect(Agency, regex("DOT - Federal Railroad Administration", ignore_case = TRUE))
  )
}

# Apply to Grants.gov
df_filtered <- filter_relevant(df)

#####
# NOAA Fisheries
#####
noaa_page <- read_html("Data/NOAA_Fisheries.html")
noaa_cards <- html_elements(noaa_page, ".vertical-list__item")

noaa_df <- map_dfr(noaa_cards, function(card) {
  title_node <- html_element(card, "h4.funding-item__title a")
  title <- html_text(title_node, trim = TRUE)
  url <- html_attr(title_node, "href")
  if (!is.na(url) && !str_starts(url, "http")) {
    url <- paste0("https://www.fisheries.noaa.gov", url)
  }
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
  filter(is.na(Deadline) | Deadline >= today()) |>
  filter_relevant()

#####
# MAFMC
#####
mafmc_page <- read_html("Data/MAFMC.html")
news_cards <- html_elements(mafmc_page, ".summary-item")

mafmc_df <- map_dfr(news_cards, function(card) {
  title <- html_element(card, ".summary-title") |> html_text(trim = TRUE)
  url <- html_element(card, ".summary-title a") |> html_attr("href")
  if (!is.na(url) && !str_starts(url, "http")) {
    url <- paste0("https://www.mafmc.org", url)
  }
  date_node <- html_element(card, "time.summary-metadata-item--date")
  date <- html_attr(date_node, "datetime")
  if (!is.na(date)) date <- ymd(date)
  tibble(
    OpportunityID = NA_character_,
    Agency = "MAFMC",
    Title = title,
    Deadline = as.Date(date),
    Posted = as.Date(date),
    AdditionalInfoURL = url
  )
}) %>%
  filter(str_detect(str_to_lower(Title), "funding|contractor|proposals")) %>%
  filter(is.na(Deadline) | Deadline >= today()) %>%
  filter_relevant()

#####
# Maine RFAs
#####
me_page <- read_html("https://www.maine.gov/dafs/bbm/procurementservices/vendors/grants")
me_table <- me_page |>
  html_element("table") |>
  html_table() |>
  clean_names() |>
  filter(rfa_status == "Open")
me_table <- me_table |> mutate(application_due_date = na_if(application_due_date, ""), application_due_date = if_else(is.na(application_due_date), format(Sys.Date(), "%m/%d/%Y"), application_due_date))

me_df_rfa <- tibble(
  OpportunityID = as.character(me_table$rfa_number),
  Agency = "State of Maine - RFA",
  Title = me_table$rfa_title,
  Deadline = as.Date(me_table$application_due_date, "%m/%d/%Y"),
  Posted = as.Date(me_table$date_posted, "%m/%d/%Y"),
  AdditionalInfoURL = "https://www.maine.gov/dafs/bbm/procurementservices/vendors/grants"
) |> filter_relevant()

me_page <- read_html("https://www.maine.gov/dafs/bbm/procurementservices/vendors/rfps")
me_table <- me_page |>
  html_element("table") |>
  html_table() |>
  clean_names() |>
  filter(rfp_status == "Open")
me_table <- me_table |> mutate(proposal_due_date = na_if(proposal_due_date, ""), proposal_due_date = if_else(is.na(proposal_due_date), format(Sys.Date(), "%m/%d/%Y"), proposal_due_date))

me_df_rfp <- tibble(
  OpportunityID = as.character(me_table$rfp_number),
  Agency = "State of Maine - RFP",
  Title = me_table$title,
  Deadline = as.Date(me_table$proposal_due_date, "%m/%d/%Y"),
  Posted = as.Date(me_table$date_posted, "%m/%d/%Y"),
  AdditionalInfoURL = "https://www.maine.gov/dafs/bbm/procurementservices/vendors/rfps"
) |> filter_relevant()

#####
# Saving and rendering
#####
csv_file <- file.path(here::here(), paste0("GMRI_Grants.csv"))
prev_file <- csv_file
prev_df <- if (file.exists(prev_file)) read_csv(prev_file) else tibble()

out <- bind_rows(df_filtered, noaa_df, mafmc_df, me_df_rfa, me_df_rfp) |>
  mutate(
    Title = replace_na(Title, ""),
    Deadline = as.Date(Deadline),
    Posted = as.Date(Posted),
    IsNew = if (nrow(prev_df) > 0) {
      !Title %in% replace_na(prev_df$Title, "")
    } else {
      TRUE
    }
  ) |>
  filter(!is.na(Deadline), Deadline >= Sys.Date()) |>
  arrange(desc(IsNew), Deadline, Agency)

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

write_csv(out, csv_file)
