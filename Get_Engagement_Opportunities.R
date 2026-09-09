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
library(httr)
library(jsonlite)
library(xml2)
library(rvest)
library(here)

Sys.setenv(TZ = "America/New_York")
today_date <- today()

`%||%` <- function(x, y) if (is.null(x)) y else x

# jsonlite drops a column entirely if every object in a batch is missing it
# (e.g. no notice in a page has a deadline) -- pad it back in as NA so
# downstream transmute()/select() calls don't fail on a column that happens
# not to exist in a given response.
ensure_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  for (col in missing_cols) df[[col]] <- NA
  df
}

REQUEST_TIMEOUT <- 30
REQUEST_PAUSE_SECONDS <- 0.5

# ── Lookback window ──────────────────────────────────────────────────────────
# Weekly cadence with a couple days of buffer, same reasoning as the Python
# prototype's config.yaml lookback_days.
lookback_days <- 8
since_date <- today_date - lookback_days

#####
# Target agencies (Federal Register + SAM.gov)
# Same GMRI focus areas as Get_Opportunity_Updates.R's Grants.gov pipeline.
# Tune this list once real output is visible.
#####
target_agencies <- list(
  list(label = "NOAA",
       fr_match  = c("National Oceanic and Atmospheric Administration"),
       sam_match = c("National Oceanic and Atmospheric Administration", "NOAA")),
  list(label = "EPA",
       fr_match  = c("Environmental Protection Agency"),
       sam_match = c("Environmental Protection Agency", "EPA")),
  list(label = "National Science Foundation",
       fr_match  = c("National Science Foundation"),
       sam_match = c("National Science Foundation", "NSF")),
  list(label = "Navy / Office of Naval Research",
       fr_match  = c("Navy Department"),
       sam_match = c("Department of the Navy", "Office of Naval Research", "ONR")),
  list(label = "Fish and Wildlife Service",
       fr_match  = c("Fish and Wildlife Service"),
       sam_match = c("Fish and Wildlife Service", "USFWS")),
  list(label = "USDA / NIFA",
       fr_match  = c("Agriculture Department", "National Institute of Food and Agriculture"),
       sam_match = c("Department of Agriculture", "National Institute of Food and Agriculture", "NIFA"))
)

# ── Category patterns ────────────────────────────────────────────────────────
# Document *type* classification -- separate from the agency scoping above.
category_patterns <- list(
  "Request for Information" = c(
    "request for information", "\\bRFI\\b",
    "information is being solicited", "seeking (public )?comments?"
  ),
  "Notice of Intent" = c("notice of intent"),
  "FACA / Nomination" = c(
    "federal advisory committee", "call for nominations",
    "nominations? (are|is) (being |now )?(sought|solicited|requested)"
  ),
  "Review Panel" = c("review panel", "peer reviewers?", "serve as a reviewer", "panelists?"),
  "Sources Sought" = c("sources sought"),
  "Public Meeting / Workshop" = c("public meeting", "public workshop", "listening session")
)

classify_text <- function(text, categories) {
  hits <- character(0)
  for (cat_label in names(categories)) {
    pattern <- str_c(categories[[cat_label]], collapse = "|")
    if (str_detect(text, regex(pattern, ignore_case = TRUE))) {
      hits <- c(hits, cat_label)
    }
  }
  hits
}

# ── NSF DCL topical relevance filter ────────────────────────────────────────
# NSF publishes DCLs across every field it funds, not just ocean/marine/climate
# topics, so (unlike the agency-scoped FR/SAM sources above) DCLs need a topic
# filter. Keep this in sync with Get_Opportunity_Updates.R's core_keywords.
core_keywords <- c(
  "climate", "resilience", "adaptation", "mitigation", "carbon", "sea level",
  "acidification", "greenhouse",
  "ocean", "marine", "ecosystem", "habitat", "estuarine", "biodiversity",
  "restoration", "coastal",
  "fisheries", "stock assessment", "bycatch", "aquaculture", "shellfish",
  "seafood", "blue economy", "marine spatial planning",
  "offshore wind", "renewable energy",
  "spatial", "species distribution", "movement ecology"
)
core_pattern <- str_c(core_keywords, collapse = "|")

#####
# Federal Register
#####

FR_AGENCIES_URL  <- "https://www.federalregister.gov/api/v1/agencies.json"
FR_DOCUMENTS_URL <- "https://www.federalregister.gov/api/v1/documents.json"

resolve_fr_slugs <- function(target_agencies) {
  resp <- GET(FR_AGENCIES_URL, timeout(REQUEST_TIMEOUT))
  stop_for_status(resp)
  all_agencies <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  agency_names_lower <- str_to_lower(all_agencies$name)

  map(target_agencies, function(target) {
    wanted <- str_to_lower(target$fr_match)
    is_match <- map_lgl(agency_names_lower, function(nm) any(str_detect(nm, fixed(wanted))))
    sort(unique(all_agencies$slug[is_match]))
  }) |> set_names(map_chr(target_agencies, "label"))
}

fetch_fr_notices_for_slug <- function(slug, since_date) {
  documents <- list()
  page <- 1
  repeat {
    resp <- GET(FR_DOCUMENTS_URL, query = list(
      `conditions[agencies][]`             = slug,
      `conditions[type][]`                 = "NOTICE",
      `conditions[publication_date][gte]`  = format(since_date, "%Y-%m-%d"),
      per_page = 100,
      page     = page,
      order    = "newest"
    ), timeout(REQUEST_TIMEOUT))
    stop_for_status(resp)
    data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    if (!is.null(data$results) && nrow(data$results) > 0) {
      documents[[length(documents) + 1]] <- data$results
    }
    total_pages <- data$total_pages %||% 1
    if (page >= total_pages) break
    page <- page + 1
    Sys.sleep(REQUEST_PAUSE_SECONDS)
  }
  if (length(documents) == 0) return(tibble())
  bind_rows(documents)
}

gather_federal_register_items <- function(target_agencies, since_date) {
  message("Federal Register: resolving agency slugs...")
  resolved_slugs <- resolve_fr_slugs(target_agencies)

  map_dfr(target_agencies, function(target) {
    label <- target$label
    slugs <- resolved_slugs[[label]]
    if (length(slugs) == 0) {
      message("  [!] No Federal Register agency matched for '", label, "'. Skipping.")
      return(tibble())
    }
    message("  ", label, ": querying slug(s) ", str_c(slugs, collapse = ", "))

    map_dfr(slugs, function(slug) {
      docs <- tryCatch(fetch_fr_notices_for_slug(slug, since_date), error = function(e) {
        message("      [!] Error fetching ", slug, ": ", conditionMessage(e))
        tibble()
      })
      Sys.sleep(REQUEST_PAUSE_SECONDS)
      if (nrow(docs) == 0) return(tibble())

      docs |>
        ensure_cols(c("title", "abstract", "html_url", "publication_date",
                       "comments_close_on", "document_number")) |>
        mutate(text = str_c(replace_na(title, ""), " ", replace_na(abstract, ""))) |>
        rowwise() |>
        mutate(cat_hits = list(classify_text(text, category_patterns))) |>
        ungroup() |>
        filter(map_int(cat_hits, length) > 0) |>
        transmute(
          Agency            = label,
          Title             = title,
          Deadline          = as.Date(comments_close_on),
          Posted            = as.Date(publication_date),
          AdditionalInfoURL = html_url,
          Categories        = map_chr(cat_hits, str_c, collapse = "; "),
          OpportunityID     = document_number
        )
    })
  }) |>
    distinct(OpportunityID, .keep_all = TRUE)
}

#####
# SAM.gov
#####

SAM_OPPORTUNITIES_URL <- "https://api.sam.gov/opportunities/v2/search"
SAM_NOTICE_TYPES <- "r,s"  # r = Sources Sought, s = Special Notice

fetch_sam_notices <- function(api_key, since_date) {
  notices <- list()
  limit <- 1000
  offset <- 0
  posted_from <- format(since_date, "%m/%d/%Y")
  posted_to   <- format(today_date, "%m/%d/%Y")

  repeat {
    resp <- GET(SAM_OPPORTUNITIES_URL, query = list(
      api_key    = api_key,
      postedFrom = posted_from,
      postedTo   = posted_to,
      ptype      = SAM_NOTICE_TYPES,
      limit      = limit,
      offset     = offset
    ), timeout(REQUEST_TIMEOUT))
    if (status_code(resp) == 401) {
      stop("SAM.gov API rejected the key (401 Unauthorized). Check that SAM_API_KEY is current and active.")
    }
    stop_for_status(resp)
    data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    batch <- data$opportunitiesData
    if (!is.null(batch) && length(batch) > 0 && nrow(batch) > 0) {
      notices[[length(notices) + 1]] <- batch
    }
    total_records <- data$totalRecords %||% 0
    offset <- offset + limit
    if (offset >= total_records || is.null(batch) || nrow(batch) == 0) break
    Sys.sleep(REQUEST_PAUSE_SECONDS)
  }
  if (length(notices) == 0) return(tibble())
  bind_rows(notices)
}

gather_sam_items <- function(target_agencies, since_date) {
  api_key <- Sys.getenv("SAM_API_KEY")
  if (api_key == "") {
    message("SAM.gov: SAM_API_KEY not set -- skipping SAM.gov search. ",
            "Get a free key at https://sam.gov/data-services")
    return(tibble())
  }

  message("SAM.gov: fetching Sources Sought / Special Notice opportunities...")
  notices <- tryCatch(fetch_sam_notices(api_key, since_date), error = function(e) {
    message("  [!] Error fetching from SAM.gov: ", conditionMessage(e), ". Skipping this source for this run.")
    NULL
  })
  if (is.null(notices) || nrow(notices) == 0) return(tibble())
  message("  Retrieved ", nrow(notices), " notice(s) government-wide; matching to target agencies...")

  notices <- notices |>
    ensure_cols(c("fullParentPathName", "department", "subTier", "office", "title",
                  "type", "responseDeadLine", "postedDate", "uiLink", "noticeId")) |>
    mutate(across(c(fullParentPathName, department, subTier, office, title, type),
                  ~ replace_na(as.character(.x), ""))) |>
    mutate(
      haystack = str_to_lower(str_c(fullParentPathName, " ", department, " ", subTier, " ", office)),
      notice_type_lower = str_to_lower(type)
    ) |>
    rowwise() |>
    mutate(cat_hits = list({
      hits <- classify_text(title, category_patterns)
      if (str_detect(notice_type_lower, "sources sought") && !("Sources Sought" %in% hits)) {
        hits <- c(hits, "Sources Sought")
      }
      hits
    })) |>
    ungroup() |>
    filter(map_int(cat_hits, length) > 0)

  map_dfr(target_agencies, function(target) {
    wanted <- str_to_lower(target$sam_match)
    matched <- notices |>
      filter(map_lgl(haystack, function(h) any(str_detect(h, fixed(wanted)))))
    if (nrow(matched) == 0) return(tibble())

    matched |>
      transmute(
        Agency            = target$label,
        Title             = if_else(is.na(title) | title == "", "(untitled)", title),
        Deadline          = as.Date(responseDeadLine),
        Posted            = as.Date(postedDate),
        AdditionalInfoURL = if_else(
          !is.na(uiLink) & uiLink != "", uiLink,
          str_c("https://sam.gov/opp/", noticeId, "/view")
        ),
        Categories        = map_chr(cat_hits, str_c, collapse = "; "),
        OpportunityID     = noticeId
      )
  })
}

#####
# NSF Dear Colleague Letters
# NSF's funding-opportunities search page (and its CSV export) sit behind an
# AWS WAF bot challenge and can't be fetched with a plain GET, so this crawls
# the site's public sitemap instead -- not gated, and it's the canonical
# Sitemap: entry in nsf.gov/robots.txt. DCL pages live at
# /funding/information/dcl-... and are themselves plain server-rendered HTML.
#####

NSF_SITEMAP_INDEX_URL <- "https://www.nsf.gov/sitemap.xml"

fetch_nsf_dcl_items <- function(since_date) {
  index_resp <- tryCatch(GET(NSF_SITEMAP_INDEX_URL, timeout(REQUEST_TIMEOUT)), error = function(e) NULL)
  if (is.null(index_resp)) {
    message("  [!] Could not reach NSF sitemap index. Skipping NSF DCLs for this run.")
    return(tibble())
  }
  index_xml <- read_xml(content(index_resp, as = "text", encoding = "UTF-8"))
  ns <- xml_ns_rename(xml_ns(index_xml), d1 = "sm")
  sub_sitemap_urls <- xml_text(xml_find_all(index_xml, ".//sm:sitemap/sm:loc", ns))

  message("NSF: scanning ", length(sub_sitemap_urls), " sitemap page(s) for recent Dear Colleague Letters...")

  dcl_urls <- map_dfr(sub_sitemap_urls, function(sm_url) {
    resp <- tryCatch(GET(sm_url, timeout(REQUEST_TIMEOUT)), error = function(e) NULL)
    if (is.null(resp)) return(tibble())
    doc <- tryCatch(read_xml(content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
    if (is.null(doc)) return(tibble())
    ns2 <- xml_ns_rename(xml_ns(doc), d1 = "sm")
    url_nodes <- xml_find_all(doc, ".//sm:url", ns2)
    # <lastmod> is optional per the sitemap spec, so pull it per-<url> node
    # rather than as a separate xpath query -- otherwise a missing lastmod on
    # any entry misaligns the loc/lastmod vectors.
    tibble(
      url     = map_chr(url_nodes, ~ xml_text(xml_find_first(.x, "./sm:loc", ns2))),
      lastmod = map_chr(url_nodes, ~ {
        node <- xml_find_first(.x, "./sm:lastmod", ns2)
        if (is.na(node)) NA_character_ else xml_text(node)
      })
    )
  }) |>
    filter(
      str_detect(url, fixed("/funding/information/dcl-")),
      as.Date(lastmod) >= since_date
    )

  if (nrow(dcl_urls) == 0) return(tibble())
  message("  ", nrow(dcl_urls), " recently-updated DCL page(s) found; checking relevance...")

  map_dfr(seq_len(nrow(dcl_urls)), function(i) {
    page_url <- dcl_urls$url[i]
    page <- tryCatch(read_html(page_url), error = function(e) NULL)
    if (is.null(page)) return(tibble())

    title <- html_text(html_element(page, "title"), trim = TRUE) |>
      str_remove(" \\| NSF.*$")
    if (is.na(title) || !str_detect(str_to_lower(title), regex(core_pattern, ignore_case = TRUE))) {
      return(tibble())
    }

    tibble(
      Agency            = "NSF",
      Title             = title,
      Deadline          = as.Date(NA),
      Posted            = as.Date(dcl_urls$lastmod[i]),
      AdditionalInfoURL = page_url,
      Categories        = "Request for Information",
      OpportunityID     = NA_character_
    )
  })
}

#####
# Combine, deduplicate, and save
#####

message("=== GMRI Engagement Opportunities (since ", since_date, ") ===")

fr_items  <- tryCatch(gather_federal_register_items(target_agencies, since_date),
                       error = function(e) { message("  [!] Federal Register fetch failed: ", conditionMessage(e)); tibble() })
sam_items <- gather_sam_items(target_agencies, since_date)
nsf_items <- tryCatch(fetch_nsf_dcl_items(since_date),
                       error = function(e) { message("  [!] NSF DCL fetch failed: ", conditionMessage(e)); tibble() })

csv_file <- file.path(here::here(), "GMRI_Engagement_Opportunities.csv")
prev_df  <- if (file.exists(csv_file)) read_csv(csv_file, show_col_types = FALSE) else tibble()

out <- bind_rows(fr_items, sam_items, nsf_items) |>
  mutate(
    Title    = replace_na(Title, ""),
    Deadline = as.Date(Deadline),
    Posted   = as.Date(Posted),
    IsNew    = if (nrow(prev_df) > 0) {
      !Title %in% replace_na(prev_df$Title, "")
    } else {
      TRUE
    }
  ) |>
  filter(is.na(Deadline) | Deadline >= Sys.Date()) |>
  arrange(desc(IsNew), Deadline, Agency)

write_csv(out, csv_file)
message("Done. ", nrow(out), " engagement opportunities written to ", csv_file)
