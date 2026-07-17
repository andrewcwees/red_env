# Redlining & Transportation-Sector Environmental Exposure — Shiny work sample

An interactive R Shiny dashboard built from the **redenv** research project
(Weesner, Higashidani, … Lim; University of Arizona). For a selected U.S. city
it maps **present-day transportation-sector environmental exposure** across
neighborhoods graded by the 1930s **HOLC ("redlining")** maps, and shows how
that exposure tracks HOLC grade.

Purpose: a job-search work sample demonstrating R + Shiny + `leaflet` + spatial
data (HOLC polygons, 8,836 graded zones across 200 cities) + reproducible
dashboarding.

**Live app:** _(add your shinyapps.io URL after deploying — see below)_
**Code:** this repository.

## What it does
Pick a **city** (searchable, 200 cities), a **transportation sector**, and a
**pollutant / measure**:

- **Map** (`leaflet` choropleth) — the selected city's HOLC zones colored by
  exposure on a **log scale** (darker = higher), outlined by HOLC grade
  (green A → red D). Hover a zone for its grade, exposure, and demographics.
- **Equity chart** (`ggplot2`) — the national exposure distribution grouped by
  **HOLC grade** (default) or, via a toggle, by **demographic quintile**
  (median income, % Hispanic, % Black), with the selected city's zones
  overlaid as points. This is the disparity story: exposure rises from grade A
  to grade D, and tracks lower income / higher minority share.
- **Value boxes** — national **Grade D-vs-A exposure ratio** for the current
  sector × pollutant, **zones mapped** in the city, and the anchor finding:
  **rail is the most stratified transportation sector** (ranks #1 in 61% of
  sector × measure comparisons).

## Scope (deliberately hedged)
This is a descriptive work sample, not a published or causal result, and it
carries the project's documented data-quality limits by design:

- **Rail + Road only.** Aviation is excluded — 63–69% of HOLC zones have zero
  aviation exposure, so an aviation choropleth is mostly empty and misleading.
- **No combined exposure index (CEI)** — the combined index is structurally
  unreliable and is omitted entirely.
- **Per-city exploration + a durable national finding**, not a single national
  causal claim. No causal language.
- **Cities with < 5 graded zones are dropped** (21 cities / 84 zones) — they
  render sparse, unreadable maps and unstable per-city summaries.

## Files
- `app.R` — the Shiny app. Self-contained; reads only `redenv_data.rds`.
- `redenv_data.rds` — baked bundle (HOLC zone polygons + grade + demographics;
  zone × sector × pollutant exposure; city selector table; sector-ranking
  summary). ~2.1 MB; **no `D:\` data or API key needed at runtime.**
- `prep_data.R` — one-time build script that regenerates the bundle from the
  canonical frame (`D:/research/redenv/data/output/xp_out.csv` + the HOLC
  crosswalk `cw.gpkg`). Only needed to refresh the data.

## Run locally
```r
# from this folder
shiny::runApp()
```
Requires: `shiny`, `bslib`, `leaflet`, `dplyr`, `sf`, `ggplot2`, `scales`.

## Deploy a live link
Run `deploy.R` (it wraps `rsconnect::deployApp()`): create a free
shinyapps.io account, paste your token from Account > Tokens, and run the
script. Redeploys reuse the same URL. Then paste that URL into the **Live app**
line above.

## Provenance / data note
Derived from **unpublished HUM Lab data**. External sharing of this derived,
aggregated subset was **signed off by Chris Lim (2026-07-17)** (the same gate
the phxmob work sample cleared 2026-07-08). **Keep the deployed app private
until it is finished, then make it public.** Underlying data: NEMO
transportation-sector emissions (EPA NEI 2017); BTS National Transportation
Noise Map; HOLC grades via the University of Richmond Mapping Inequality
project; ACS 2019 demographics.
