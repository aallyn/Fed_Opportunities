# Fed_Opportunities

A weekly-refreshed GitHub Pages site tracking federal (and a few state/regional) opportunities relevant to GMRI, plus a Slack ping when it updates. The site has two tabs:

- **Grants Summary** — funding opportunities: things to apply for money through.
- **Engagement Opportunities** — Requests for Information, Notices of Intent, FACA/advisory-committee nomination calls, review-panel solicitations, Sources Sought/Special Notices, and public meetings/workshops: things to notice and respond to (or nominate someone for), not funding.

Live site: <https://aallyn.github.io/Fed_Opportunities/>

## Interacting with the Reporter

You don't need to edit any code to change what the reporter tracks. Describe the change you want to Claude Code and review what it does:

1. **Open Claude Code.** In the Claude desktop app, switch to the **Code** tab and start a new session.
2. **Give it the repo and the change you want.** Paste `https://github.com/aallyn/Fed_Opportunities` and describe the change in plain language. For example:
   - "Add engagement opportunities from the National Academies of Sciences."
   - "Add 'kelp' and 'seaweed' to the keyword filter."
   - "Stop showing USDA items on the Engagement tab."
   - "The MAFMC scraper returned nothing this week. Can you find out why?"

   Claude clones the repo, reads the relevant script, and proposes how to make the change.
3. **Talk it through.** Ask questions and push back on scope (which agencies, keywords, or categories), the same way you would with a colleague. For a new or changed source, have Claude run that fetch on its own and show you what it returns.
4. **Render locally and review before you commit.** Ask Claude to re-run the affected `Get_*.R` script(s) and `quarto render` the site. Then open `docs/Opportunity_Report.html` and `docs/Engagement_Report.html` and check that the tables look right. **Don't skip this step:** every push to `main` triggers the GitHub Actions workflow, which re-runs the pipeline and **republishes the live site**, so a broken change goes straight to the page everyone uses. Pushes don't post to Slack (only the weekly Monday run and manual runs do), so a bad push would sit on the site until someone noticed it.
   - SAM.gov results need a `SAM_API_KEY`. Without one, a local run of `Get_Engagement_Opportunities.R` leaves out SAM.gov items. That's fine for previewing.
5. **Commit and push.** Once the preview looks right, ask Claude to commit and push to `main`. Commit only the code and documentation changes, and ask Claude to discard the locally regenerated CSVs and `docs/`. The workflow regenerates both with the full set of API keys. Put related changes in a single push so the site only rebuilds once.
6. **Confirm the run.** The workflow takes about 10–15 minutes. Check the repo's **Actions** tab and then the live site.

**Setup (one time):** you need push access to this repo, plus R and [Quarto](https://quarto.org/docs/get-started/) installed locally for step 4. If either is missing, Claude can tell you what's needed.

## Sources

**Funding side** (`Get_Opportunity_Updates.R` → `GMRI_Grants.csv`):
- Grants.gov weekly XML extract (covers NOAA, USFWS, DoD, and most other federal grantmakers), filtered by an agency blocklist plus a marine/climate/ocean keyword filter.
- Live scrapes: EPA research grants, MAFMC news, NEFMC news, Maine state RFAs/RFPs.
- `GMRI_Standing_Programs.csv` — manually maintained list of recurring NOAA programs that don't appear on Grants.gov (formula grants, standing solicitations). Update this file directly when a program changes.

**Engagement side** (`Get_Engagement_Opportunities.R` → `GMRI_Engagement_Opportunities.csv`):
- Federal Register API — NOTICE-type documents for target agencies, classified into categories (RFI, NOI, FACA/Nomination, Review Panel, Sources Sought, Public Meeting/Workshop) via regex.
- SAM.gov Opportunities API — Sources Sought and Special Notices, government-wide, matched down to target agencies client-side. Requires a `SAM_API_KEY` (skips gracefully with a warning if unset).
- NSF Dear Colleague Letters — crawled via nsf.gov's public sitemap (`sitemap.xml`), not NSF's funding-opportunities search page. That search page (and its CSV export) sit behind an AWS WAF bot challenge and can't be fetched with a plain HTTP request; the sitemap route is unauthenticated, `robots.txt`-sanctioned, and finds DCL pages (`/funding/information/dcl-...`) by URL pattern and recent `<lastmod>`. Filtered to ocean/marine/climate-relevant titles, since NSF's DCLs span every field it funds.
- National Academies of Sciences, Engineering, and Medicine (NASEM) public events — meetings, workshops, and webinars open to the public. NASEM isn't a federal agency, so it doesn't appear in the Federal Register or SAM.gov. The `/events` listing paginates via query strings, which nationalacademies.org's `robots.txt` disallows, so this reads `sitemap_events.xml` instead. Every `<lastmod>` in that sitemap is rewritten nightly, so it can't be date-filtered like NSF's. Instead, event URLs carry the owning board's code (e.g. `DELS-OSB` = Ocean Studies Board), so the script takes the newest `NAS_MAX_EVENT_PAGES` events from `nas_target_boards` and reads each page. Closed/canceled sessions and past events are dropped, and broader boards also have to match `core_keywords`. Requests are spaced 10s apart per `robots.txt`'s `Crawl-delay`, which adds ~5 minutes to the run.

## Pipeline

Each `Get_*.R` script fetches its sources, classifies/filters them, diffs titles against the previous run's CSV to flag `IsNew`, and writes a CSV. `Opportunity_Report.qmd` and `Engagement_Report.qmd` each read their CSV and render an HTML table (new items flagged with a red dot) via Quarto. `_quarto.yml` (`project: type: website`) ties both pages together with a two-tab navbar and renders them into `docs/`, which is published via GitHub Pages.

This runs weekly via `.github/workflows/update-report.yml` (Mondays, also triggerable manually via `workflow_dispatch`): download pages → run both `Get_*.R` scripts → `quarto render` the whole site → commit `docs/` and both CSVs → push → post a short Slack message linking to both tabs. The Slack post is skipped when the run was triggered by a push to `main` (code/README changes), so only the weekly scheduled run and manual runs notify the channel.

## Running locally

```sh
# Refresh one CSV in isolation
Rscript Get_Opportunity_Updates.R
SAM_API_KEY=... Rscript Get_Engagement_Opportunities.R   # SAM_API_KEY optional; SAM.gov is skipped without it

# Preview the rendered site (both tabs)
quarto render
open docs/Opportunity_Report.html docs/Engagement_Report.html
```

Get a free SAM.gov API key at <https://sam.gov/data-services>. In GitHub Actions it's read from the `SAM_API_KEY` repo secret; locally, export it in your shell before running `Get_Engagement_Opportunities.R`.

## Adding a new source

Follow the pattern already in `Get_Opportunity_Updates.R` / `Get_Engagement_Opportunities.R`: a new block that fetches from the source, filters it (through `category_patterns`/`core_keywords` on the engagement side, or the keyword/agency filters on the funding side), normalizes the result to the same column shape as everything else in that script, and gets `bind_rows()`'d into the combined output. Wrap the fetch in `tryCatch()` so one broken source degrades gracefully (an empty result for that source) instead of failing the whole weekly run.

If the source doesn't expose a clean API or static page — the NSF Dear Colleague Letters block in `Get_Engagement_Opportunities.R` is the most recent example of that (its listing page is bot-gated; the fix was crawling the site's sitemap instead) — expect to spend real time finding a fetchable entry point before writing the R.

## Tuning

- **Agency scope**: `blocked_agencies` in `Get_Opportunity_Updates.R` (funding side, blocklist) and `target_agencies` in `Get_Engagement_Opportunities.R` (engagement side, allowlist) both determine what shows up and are worth revisiting periodically as GMRI's priorities shift.
- **Keywords/categories**: `core_keywords`/`education_only_keywords` (funding relevance) and `category_patterns` (engagement document-type classification) are plain regex vectors at the top of each script — edit them directly.
