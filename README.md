# Fed_Opportunities

A weekly-refreshed GitHub Pages site tracking federal (and a few state/regional) opportunities relevant to GMRI, plus a Slack ping when it updates. The site has two tabs:

- **Grants Summary** — funding opportunities: things to apply for money through.
- **Engagement Opportunities** — Requests for Information, Notices of Intent, FACA/advisory-committee nomination calls, review-panel solicitations, Sources Sought/Special Notices, and public meetings/workshops: things to notice and respond to (or nominate someone for), not funding.

## Sources

**Funding side** (`Get_Opportunity_Updates.R` → `GMRI_Grants.csv`):
- Grants.gov weekly XML extract (covers NOAA, USFWS, DoD, and most other federal grantmakers), filtered by an agency blocklist plus a marine/climate/ocean keyword filter.
- Live scrapes: EPA research grants, MAFMC news, NEFMC news, Maine state RFAs/RFPs.
- `GMRI_Standing_Programs.csv` — manually maintained list of recurring NOAA programs that don't appear on Grants.gov (formula grants, standing solicitations). Update this file directly when a program changes.

**Engagement side** (`Get_Engagement_Opportunities.R` → `GMRI_Engagement_Opportunities.csv`):
- Federal Register API — NOTICE-type documents for target agencies, classified into categories (RFI, NOI, FACA/Nomination, Review Panel, Sources Sought, Public Meeting/Workshop) via regex.
- SAM.gov Opportunities API — Sources Sought and Special Notices, government-wide, matched down to target agencies client-side. Requires a `SAM_API_KEY` (skips gracefully with a warning if unset).
- NSF Dear Colleague Letters — crawled via nsf.gov's public sitemap (`sitemap.xml`), not NSF's funding-opportunities search page. That search page (and its CSV export) sit behind an AWS WAF bot challenge and can't be fetched with a plain HTTP request; the sitemap route is unauthenticated, `robots.txt`-sanctioned, and finds DCL pages (`/funding/information/dcl-...`) by URL pattern and recent `<lastmod>`. Filtered to ocean/marine/climate-relevant titles, since NSF's DCLs span every field it funds.

## Pipeline

Each `Get_*.R` script fetches its sources, classifies/filters them, diffs titles against the previous run's CSV to flag `IsNew`, and writes a CSV. `Opportunity_Report.qmd` and `Engagement_Report.qmd` each read their CSV and render an HTML table (new items flagged with a red dot) via Quarto. `_quarto.yml` (`project: type: website`) ties both pages together with a two-tab navbar and renders them into `docs/`, which is published via GitHub Pages.

This runs weekly via `.github/workflows/update-report.yml` (Mondays, also triggerable manually via `workflow_dispatch`): download pages → run both `Get_*.R` scripts → `quarto render` the whole site → commit `docs/` and both CSVs → push → post a short Slack message linking to the report.

`run_all.sh` / `download_pages_and_render.R` is a local macOS-cron equivalent, kept in parity with the workflow above but not what actually runs in production — GitHub Actions is the source of truth (that's where the weekly "Auto-update report" commits come from).

## Running locally

```sh
# Refresh one CSV in isolation
Rscript Get_Opportunity_Updates.R
SAM_API_KEY=... Rscript Get_Engagement_Opportunities.R   # SAM_API_KEY optional; SAM.gov is skipped without it

# Preview the rendered site (both tabs)
quarto render
open docs/Opportunity_Report.html docs/Engagement_Report.html
```

`local_testing.R` is a minimal example of sourcing `Get_Opportunity_Updates.R` directly and dumping a quick table without going through Quarto — useful for a fast sanity check on the fetch logic alone.

Get a free SAM.gov API key at <https://sam.gov/data-services>. In GitHub Actions it's read from the `SAM_API_KEY` repo secret; locally, export it in your shell before running `Get_Engagement_Opportunities.R`.

## Adding a new source

Follow the pattern already in `Get_Opportunity_Updates.R` / `Get_Engagement_Opportunities.R`: a new block that fetches from the source, filters it (through `category_patterns`/`core_keywords` on the engagement side, or the keyword/agency filters on the funding side), normalizes the result to the same column shape as everything else in that script, and gets `bind_rows()`'d into the combined output. Wrap the fetch in `tryCatch()` so one broken source degrades gracefully (an empty result for that source) instead of failing the whole weekly run.

If the source doesn't expose a clean API or static page — the NSF Dear Colleague Letters block in `Get_Engagement_Opportunities.R` is the most recent example of that (its listing page is bot-gated; the fix was crawling the site's sitemap instead) — expect to spend real time finding a fetchable entry point before writing the R.

## Tuning

- **Agency scope**: `blocked_agencies` in `Get_Opportunity_Updates.R` (funding side, blocklist) and `target_agencies` in `Get_Engagement_Opportunities.R` (engagement side, allowlist) both determine what shows up and are worth revisiting periodically as GMRI's priorities shift.
- **Keywords/categories**: `core_keywords`/`education_only_keywords` (funding relevance) and `category_patterns` (engagement document-type classification) are plain regex vectors at the top of each script — edit them directly.
