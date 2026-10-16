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
       sam_match = c("Department of Agriculture", "National Institute of Food and Agriculture", "NIFA")),
  list(label = "NIH",
       fr_match  = c("National Institutes of Health"),
       sam_match = c("National Institutes of Health", "NIH"))
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
# National Academies of Sciences, Engineering, and Medicine (NASEM)
# NASEM isn't a federal agency, so it never shows up in the Federal Register
# or SAM.gov above. Its public meetings, workshops, and webinars are the
# engagement surface. The /events listing only shows 15 at a time and its
# pagination is query-string based (disallowed by robots.txt), so this uses
# the public events sitemap instead. Every <lastmod> in that sitemap is
# rewritten nightly, so unlike the NSF block it can't be filtered by date --
# instead, event URLs carry the owning board's code (e.g. DELS-OSB) and event
# IDs increase over time, so this takes the newest few events from relevant
# boards and reads each page. robots.txt asks for a 10 second crawl delay.
#####

NAS_EVENTS_SITEMAP_URL <- "https://www.nationalacademies.org/sitemap_events.xml"
NAS_CRAWL_DELAY_SECONDS <- 10
NAS_MAX_EVENT_PAGES <- 30

# Board-code prefixes as they appear in event URLs, e.g.
# /projects/DELS-OSB-24-04/event/47396. require_keywords = TRUE means the
# board is broad enough that events must also match core_keywords (on
# title + NASEM's own topic tags) to be kept.
nas_target_boards <- list(
  list(code = "DELS-OSB-",    label = "Ocean Studies Board",                         require_keywords = FALSE),
  list(code = "DELS-BASCPR-", label = "Board on Atmospheric Sciences and Climate",   require_keywords = FALSE),
  list(code = "DELS-PRB-",    label = "Polar Research Board",                        require_keywords = FALSE),
  list(code = "GULF-",        label = "Gulf Research Program",                       require_keywords = TRUE),
  list(code = "DELS-WSTB-",   label = "Water Science and Technology Board",          require_keywords = TRUE),
  list(code = "DELS-BESR-",   label = "Board on Earth Sciences and Resources",       require_keywords = TRUE),
  list(code = "DELS-BANR-",   label = "Board on Agriculture and Natural Resources",  require_keywords = TRUE)
)

# Event status chips that mean the public can't take part.
nas_excluded_statuses <- c("closed", "canceled", "cancelled", "postponed")

# "October 9, 2026", "October 19 - 20, 2026", or
# "October 30 - November 2, 2026" -> start date
parse_nas_event_date <- function(date_text) {
  if (is.na(date_text)) return(as.Date(NA))
  year  <- str_extract(date_text, "\\d{4}\\s*$")
  start <- str_trim(str_split_i(date_text, " - ", 1))
  if (!str_detect(start, "\\d{4}$")) start <- str_c(start, ", ", year)
  suppressWarnings(as.Date(mdy(start)))
}

fetch_nas_event <- function(event_url) {
  event_id <- str_extract(event_url, "\\d+$")
  resp <- tryCatch(GET(event_url, timeout(REQUEST_TIMEOUT)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  raw  <- content(resp, as = "text", encoding = "UTF-8")
  page <- read_html(raw)

  # The event detail is rendered as a modal on top of its parent project page.
  # Its id is "event-modal-<id>_title" or "event-modal-permalink-<id>_title"
  # depending on the page template, so match on the suffix.
  modal <- html_element(page, sprintf("[aria-labelledby$='-%s_title']", event_id))
  if (is.na(modal)) return(NULL)
  overlay <- html_element(modal, "[data-component='heroes:event-overlay']")

  # Each modal also carries a small JSON blob with sub_type/topics/created_at.
  meta_json <- str_match(raw, sprintf('(\\{"id":"%s",.*?\\});', event_id))[, 2]
  meta <- if (is.na(meta_json)) list() else tryCatch(fromJSON(meta_json), error = function(e) list())

  list(
    title      = html_text(html_element(overlay, ".f-heading-02"), trim = TRUE),
    statuses   = str_to_lower(html_text(html_elements(overlay, "div.gap-spacing-set-2 span span"), trim = TRUE)),
    date_text  = html_text(html_element(overlay, ".f-body-01 p"), trim = TRUE),
    sub_type   = meta$sub_type %||% NA_character_,
    topics     = meta$topics %||% character(0),
    created_at = meta$created_at %||% NA_character_
  )
}

fetch_nas_event_items <- function() {
  resp <- tryCatch(GET(NAS_EVENTS_SITEMAP_URL, timeout(REQUEST_TIMEOUT)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) {
    message("  [!] Could not reach NASEM events sitemap. Skipping NASEM events for this run.")
    return(tibble())
  }
  doc <- read_xml(content(resp, as = "text", encoding = "UTF-8"))
  ns  <- xml_ns_rename(xml_ns(doc), d1 = "sm")
  urls <- xml_text(xml_find_all(doc, ".//sm:url/sm:loc", ns))

  board_codes <- map_chr(nas_target_boards, "code")
  candidates <- tibble(url = urls) |>
    mutate(
      event_id   = as.integer(str_extract(url, "(?<=/event/)\\d+$")),
      board_code = str_match(url, "/(?:projects|units|programs)/([A-Z]+-[A-Z]*)")[, 2]
    ) |>
    filter(!is.na(event_id), !is.na(board_code)) |>
    mutate(board_idx = map_int(board_code, function(code) {
      hit <- which(str_starts(str_c(code, "-"), fixed(board_codes)))
      if (length(hit) == 0) NA_integer_ else hit[1]
    })) |>
    filter(!is.na(board_idx)) |>
    arrange(desc(event_id)) |>
    slice_head(n = NAS_MAX_EVENT_PAGES)

  if (nrow(candidates) == 0) return(tibble())
  message("NASEM: checking the ", nrow(candidates), " newest event(s) from target boards ",
          "(", NAS_CRAWL_DELAY_SECONDS, "s crawl delay, ~",
          ceiling(nrow(candidates) * NAS_CRAWL_DELAY_SECONDS / 60), " min)...")

  map_dfr(seq_len(nrow(candidates)), function(i) {
    if (i > 1) Sys.sleep(NAS_CRAWL_DELAY_SECONDS)
    board <- nas_target_boards[[candidates$board_idx[i]]]
    ev <- fetch_nas_event(candidates$url[i])
    if (is.null(ev) || is.na(ev$title)) return(tibble())
    if (any(ev$statuses %in% nas_excluded_statuses)) return(tibble())

    event_date <- parse_nas_event_date(ev$date_text)
    if (is.na(event_date) || event_date < today_date) return(tibble())

    if (board$require_keywords) {
      haystack <- str_c(ev$title, " ", str_c(ev$topics, collapse = " "))
      if (!str_detect(haystack, regex(core_pattern, ignore_case = TRUE))) return(tibble())
    }

    cats <- unique(c("Public Meeting / Workshop", classify_text(ev$title, category_patterns)))
    tibble(
      Agency            = "National Academies (NASEM)",
      Title             = if (is.na(ev$sub_type)) ev$title else str_c(ev$title, " [", ev$sub_type, "]"),
      Deadline          = event_date,
      Posted            = as.Date(ev$created_at),
      AdditionalInfoURL = candidates$url[i],
      Categories        = str_c(cats, collapse = "; "),
      OpportunityID     = str_c("NASEM-", candidates$event_id[i])
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
nas_items <- tryCatch(fetch_nas_event_items(),
                       error = function(e) { message("  [!] NASEM events fetch failed: ", conditionMessage(e)); tibble() })

csv_file <- file.path(here::here(), "GMRI_Engagement_Opportunities.csv")
prev_df  <- if (file.exists(csv_file)) read_csv(csv_file, show_col_types = FALSE) else tibble()

out <- bind_rows(fr_items, sam_items, nsf_items, nas_items) |>
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
