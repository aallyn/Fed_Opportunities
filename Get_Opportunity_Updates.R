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
# Grants.gov (includes NOAA and USFWS — no separate scrapers needed)
#####

# Step 1: Build download URL from today's date
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

# Step 3: Unzip
unzip_dir <- tempdir()
unzip(zip_path, exdir = unzip_dir)

# Step 4: Find and read the XML
xml_file <- list.files(unzip_dir, pattern = "\\.xml$", full.names = TRUE)[1]
doc <- read_xml(xml_file)
ns <- xml_ns_rename(xml_ns(doc), d1 = "ns")
opps <- xml_find_all(doc, ".//ns:OpportunitySynopsisDetail_1_0", ns)

df <- tibble(
  OpportunityID = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:OpportunityID", ns))),
  Agency        = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:AgencyName", ns))),
  Title         = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:OpportunityTitle", ns))),
  Deadline      = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:CloseDate", ns))),
  Posted        = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:PostDate", ns))),
  AdditionalInfoURL = map_chr(opps, ~ xml_text(xml_find_first(.x, ".//ns:AdditionalInformationURL", ns)))
) |>
  mutate(
    AdditionalInfoURL = if_else(
      is.na(AdditionalInfoURL) | AdditionalInfoURL == "",
      paste0("https://www.grants.gov/search-results-detail/", OpportunityID),
      AdditionalInfoURL
    )
  )

# Parse and filter to open opportunities
df <- df |>
  mutate(
    Deadline = na_if(Deadline, ""),
    Posted   = na_if(Posted, ""),
    Deadline = parse_date_time(Deadline, orders = c("ymd", "ymd_HMS", "mdy")),
    Posted   = parse_date_time(Posted,   orders = c("ymd", "ymd_HMS", "mdy"))
  ) |>
  filter(!is.na(Deadline), Deadline >= today_date)

# ── Agency blocklist ────────────────────────────────────────────────────────
# Blocked by department/category (not individual offices) to reduce maintenance.
# DoD is intentionally KEPT — GMRI has connections there.
# Keeping USFWS — it files through Grants.gov and will be caught by keyword filter.

blocked_agencies <- c(
  # Health & Human Services (broad)
  "Department of Health and Human Services",
  "National Institutes of Health",
  "Indian Health Service",
  "Agency for Health Care Research and Quality",
  "Health Resources and Services Administration",
  "Centers for Disease Control",
  "Substance Abuse and Mental Health",
  # Justice
  "Department of Justice",
  "Office for Victims",
  "Office on Violence Against Women",
  "National Institute of Corrections",
  # Transportation (non-relevant sub-agencies)
  "DOT - Federal Railroad Administration",
  "Federal Transit Administration",
  "Federal Highway Administration",
  "Federal Aviation Administration",
  # Labor
  "Employment and Training Administration",
  "Department of Labor",
  # Other irrelevant
  "U\\.S\\. Mission",
  "National Endowment for the Humanities",
  "Administration for Community Living",
  "Bureau of Polotical-Military Affairs",   # sic — matches original typo in source data
  "Agricultural Marketing Service",
  "Washington Headquarters Services",       # DoD admin, not research
  # Army Corps district offices (not relevant; national-level USACE is fine)
  "Fort Worth District",
  "Alaska District",
  "USACE Portland District",
  "Naval Facilities",
  "Munitions Directorate",
  "Missile Defense"
)

blocked_pattern <- str_c(blocked_agencies, collapse = "|")

filter_relevant <- function(df) {
  df |>
    filter(
      !str_detect(Title,  regex("Warfare",       ignore_case = TRUE)),
      !str_detect(Agency, regex(blocked_pattern, ignore_case = TRUE))
    )
}

# ── Keyword lists ────────────────────────────────────────────────────────────
# Core GMRI science keywords — always included regardless of agency
core_keywords <- c(
  "climate", "resilience", "adaptation", "mitigation", "carbon", "sea level",
  "acidification", "greenhouse",
  "ocean", "marine", "ecosystem", "habitat", "estuarine", "biodiversity",
  "restoration", "coastal",
  "fisheries", "stock assessment", "bycatch", "aquaculture", "shellfish",
  "seafood", "blue economy", "marine spatial planning",
  "modeling", "forecast", "prediction", "statistics", "remote sensing",
  "geospatial", "machine learning", "artificial intelligence", "data science",
  "offshore wind", "renewable energy",
  "spatial", "species distribution", "movement ecology",
  "community", "equity", "stakeholder", "engagement", "governance",
  "ROSES", "small business", "technology transfer",
  "agriculture and food research initiative"
)

# Education/STEM keywords — included ONLY when paired with a marine/coastal/
# environment anchor term (prevents pulling in army STEM internships, etc.)
education_anchor_keywords <- c(
  "ocean", "marine", "coastal", "fisheries", "environment", "climate",
  "ecosystem", "sea", "watershed", "estuar"
)

education_only_keywords <- c(
  "STEM", "education", "workforce", "training", "teacher", "student",
  "capacity building", "internship", "outreach", "informal education",
  "science education", "environmental education"
)

# Build filter: core match OR (education term AND anchor term)
core_pattern    <- str_c(core_keywords, collapse = "|")
edu_pattern     <- str_c(education_only_keywords, collapse = "|")
anchor_pattern  <- str_c(education_anchor_keywords, collapse = "|")

df_filtered <- filter_relevant(df) |>
  filter(
    str_detect(Title, regex(core_pattern, ignore_case = TRUE)) |
    (
      str_detect(Title, regex(edu_pattern,    ignore_case = TRUE)) &
      str_detect(Title, regex(anchor_pattern, ignore_case = TRUE))
    )
  )

#####
# EPA Research Grants — live scrape
#####

epa_url  <- "https://www.epa.gov/research-grants/research-funding-opportunities"
epa_page <- tryCatch(read_html(epa_url), error = function(e) NULL)

epa_df <- tibble()
if (!is.null(epa_page)) {
  epa_list <- epa_page |>
    html_element(xpath = "//h4[contains(text(),'Open Funding Opportunities')]/following-sibling::ul[1]")

  if (!is.null(epa_list) && length(html_elements(epa_list, "li")) > 0) {
    epa_df <- map_dfr(html_elements(epa_list, "li"), function(item) {
      title_node <- html_element(item, "a")
      title <- html_text(title_node, trim = TRUE)
      href  <- html_attr(title_node, "href")
      if (!is.na(href) && !startsWith(href, "http")) {
        href <- paste0("https://www.epa.gov", href)
      }
      date_node   <- html_element(item, "div.datetime")
      posted_text <- html_text(date_node, trim = TRUE)
      posted_date <- parse_date_time(posted_text, orders = c("b d, Y", "B d, Y", "m/d/Y"))
      tibble(
        OpportunityID     = NA_character_,
        Agency            = "EPA",
        Title             = title,
        Deadline          = as.Date(NA),
        Posted            = as.Date(posted_date),
        AdditionalInfoURL = href
      )
    }) |>
    filter(
      str_detect(str_to_lower(Title), "funding|grants|rfp|rfa|proposal|program|research"),
      is.na(Deadline) | Deadline >= Sys.Date()
    )
  }
}

#####
# MAFMC — live scrape
# NOTE: MAFMC uses Squarespace; the .summary-item selector has been stable.
# If it breaks, check for a new theme rollout.
#####

mafmc_url  <- "https://www.mafmc.org/news"
mafmc_page <- tryCatch(read_html(mafmc_url), error = function(e) NULL)

mafmc_df <- tibble()
if (!is.null(mafmc_page)) {
  news_cards <- html_elements(mafmc_page, ".summary-item")
  if (length(news_cards) > 0) {
    mafmc_df <- map_dfr(news_cards, function(card) {
      title <- html_element(card, ".summary-title")  |> html_text(trim = TRUE)
      href  <- html_element(card, ".summary-title a") |> html_attr("href")
      if (!is.na(href) && !str_starts(href, "http")) {
        href <- paste0("https://www.mafmc.org", href)
      }
      date_node <- html_element(card, "time.summary-metadata-item--date")
      date_str  <- html_attr(date_node, "datetime")
      date_val  <- if (!is.na(date_str)) ymd(date_str) else as.Date(NA)
      tibble(
        OpportunityID     = NA_character_,
        Agency            = "MAFMC",
        Title             = title,
        Deadline          = as.Date(NA),
        Posted            = as.Date(date_val),
        AdditionalInfoURL = href
      )
    }) |>
    filter(str_detect(str_to_lower(Title), "funding|contractor|proposal"))

    # Approximate deadline: 60 days from post date
    mafmc_df <- mafmc_df |> mutate(Deadline = Posted + 60)
  }
}

#####
# NEFMC — live scrape
#####

nefmc_url  <- "https://www.nefmc.org/news"
nefmc_page <- tryCatch(read_html(nefmc_url), error = function(e) NULL)

nefmc_df <- tibble()
if (!is.null(nefmc_page)) {
  news_cards <- html_elements(nefmc_page, "article.news")
  if (length(news_cards) > 0) {
    nefmc_df <- map_dfr(news_cards, function(card) {
      title_node <- html_element(card, "h3 a")
      title <- html_text(title_node, trim = TRUE)
      href  <- html_attr(title_node, "href")
      if (!is.na(href) && !str_starts(href, "http")) {
        href <- paste0("https://www.nefmc.org", href)
      }
      date_text   <- html_text(html_element(card, "p.date"), trim = TRUE)
      posted_str  <- str_extract(date_text, "\\w+ \\d{1,2}, \\d{4}")
      posted_date <- parse_date_time(posted_str, orders = "B d, Y")
      tibble(
        OpportunityID     = NA_character_,
        Agency            = "NEFMC",
        Title             = title,
        Deadline          = as.Date(NA),
        Posted            = as.Date(posted_date),
        AdditionalInfoURL = href
      )
    }) |>
    filter(str_detect(str_to_lower(Title), "funding|contractor|proposal|grants|rfa|rfp"))

    nefmc_df <- nefmc_df |> mutate(Deadline = Posted + 60)
  }
}

#####
# Maine RFAs and RFPs — live scrape (unchanged; these work fine)
#####

me_rfa_page <- tryCatch(
  read_html("https://www.maine.gov/dafs/bbm/procurementservices/vendors/grants"),
  error = function(e) NULL
)

me_df_rfa <- tibble()
if (!is.null(me_rfa_page)) {
  me_table <- me_rfa_page |>
    html_element("table") |>
    html_table() |>
    clean_names() |>
    filter(rfa_status == "Open") |>
    mutate(
      application_due_date = na_if(application_due_date, ""),
      application_due_date = if_else(
        is.na(application_due_date),
        format(Sys.Date(), "%m/%d/%Y"),
        application_due_date
      )
    )

  me_df_rfa <- tibble(
    OpportunityID     = as.character(me_table$rfa_number),
    Agency            = "State of Maine - RFA",
    Title             = me_table$rfa_title,
    Deadline          = as.Date(me_table$application_due_date, "%m/%d/%Y"),
    Posted            = as.Date(me_table$date_posted,          "%m/%d/%Y"),
    AdditionalInfoURL = "https://www.maine.gov/dafs/bbm/procurementservices/vendors/grants"
  ) |> filter_relevant()
}

me_rfp_page <- tryCatch(
  read_html("https://www.maine.gov/dafs/bbm/procurementservices/vendors/rfps"),
  error = function(e) NULL
)

me_df_rfp <- tibble()
if (!is.null(me_rfp_page)) {
  me_table <- me_rfp_page |>
    html_element("table") |>
    html_table() |>
    clean_names() |>
    filter(rfp_status == "Open") |>
    mutate(
      proposal_due_date = na_if(proposal_due_date, ""),
      proposal_due_date = if_else(
        is.na(proposal_due_date),
        format(Sys.Date(), "%m/%d/%Y"),
        proposal_due_date
      )
    )

  me_df_rfp <- tibble(
    OpportunityID     = as.character(me_table$rfp_number),
    Agency            = "State of Maine - RFP",
    Title             = me_table$title,
    Deadline          = as.Date(me_table$proposal_due_date, "%m/%d/%Y"),
    Posted            = as.Date(me_table$date_posted,       "%m/%d/%Y"),
    AdditionalInfoURL = "https://www.maine.gov/dafs/bbm/procurementservices/vendors/rfps"
  ) |> filter_relevant()
}

#####
# NOAA Standing Programs
# These are recurring NOAA programs that don't appear on Grants.gov (formula
# grants, standing program pages, regional announcements). Maintained manually
# in GMRI_Standing_Programs.csv — update when programs change or new ones added.
#####

standing_programs_file <- file.path(here::here(), "GMRI_Standing_Programs.csv")

standing_df <- tibble()
if (file.exists(standing_programs_file)) {
  standing_df <- read_csv(standing_programs_file, show_col_types = FALSE) |>
    transmute(
      OpportunityID     = NA_character_,
      Agency            = Agency,
      Title             = paste0(Title, " [", TypicalWindow, "]"),
      Deadline          = as.Date(NA),   # No fixed deadline; user checks page
      Posted            = as.Date(NA),
      AdditionalInfoURL = URL,
      IsNew             = FALSE          # Standing programs are never "new"
    )
}

#####
# Combine, deduplicate, and save
#####

csv_file <- file.path(here::here(), "GMRI_Grants.csv")
prev_df  <- if (file.exists(csv_file)) read_csv(csv_file, show_col_types = FALSE) else tibble()

out <- bind_rows(df_filtered, epa_df, nefmc_df, mafmc_df, me_df_rfa, me_df_rfp, standing_df) |>
  mutate(
    Title    = replace_na(Title, ""),
    Deadline = as.Date(Deadline),
    Posted   = as.Date(Posted),
    IsNew    = if (nrow(prev_df) > 0) {
      # Standing programs are always FALSE; for everything else check against prev
      case_when(
        !is.na(IsNew) & IsNew == FALSE ~ FALSE,   # standing programs
        TRUE ~ !Title %in% replace_na(prev_df$Title, "")
      )
    } else {
      if_else(!is.na(IsNew) & IsNew == FALSE, FALSE, TRUE)
    }
  ) |>
  filter(is.na(Deadline) | Deadline >= Sys.Date()) |>  # NA deadline = standing program, keep it
  arrange(desc(IsNew), Deadline, Agency)

write_csv(out, csv_file)
message("Done. ", nrow(out), " opportunities written to ", csv_file)
