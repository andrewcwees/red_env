# Redlining & Transportation-Sector Environmental Exposure — Shiny work sample

An interactive R Shiny dashboard built from the **redenv** research project
(Weesner, Higashidani, … Lim; University of Arizona). For a selected U.S. city
it maps **present-day transportation-sector environmental exposure** across
neighborhoods graded by the 1930s **HOLC ("redlining")** maps, and shows how
that exposure tracks HOLC grade.

Purpose: a job-search work sample demonstrating R + Shiny + `leaflet` + spatial
data (HOLC polygons, 2,826 graded zones across 14 cities in 11 states) +
reproducible dashboarding. The underlying study analyzes the full 8,920-zone
frame; the map's smaller set is a display filter only (see **Scope** below).

**Live app:** _(add your shinyapps.io URL after deploying — see below)_
**Code:** this repository.

## What it does
Pick a **city** (searchable, 14 cities), an **emission source** (rail, road,
aviation), and an **emission type** (noise, NO, NO₂, PM2.5):

- **Map** (`leaflet` choropleth) — two mutually exclusive fill views, switched
  with the layer control: **Exposure** (log scale, darker = higher) and **HOLC
  grade** (green A → red D). Each carries its own legend. Zones with **no
  measurable exposure** are drawn in a separate grey rather than at the bottom
  of the ramp, so an absence never reads as a low value. Hover a zone for its
  grade, exposure, and demographics. The exposure ramp is **fitted to the
  selected city**, so it reads the within-city spread; colors are deliberately
  not comparable between cities, and cross-city context comes from the equity
  chart instead. If no zone in the current view has any measurable exposure,
  the header says so instead of leaving an unexplained grey map. A **basemap**
  toggle switches between a light cartographic base
  (default) and **satellite imagery** (Esri World Imagery, keyless), and a
  **fill opacity** slider dims the choropleth so the infrastructure underneath
  it — the rail yard, the interchange, the airport — is legible.
- **Equity chart** (`ggplot2`) — the pooled exposure distribution across all 14
  mapped cities, grouped by **HOLC grade** (default) or, via the **Group by**
  control in the chart's own header, by **demographic quintile** (median
  income, % Hispanic, % Black), with the selected city's zones overlaid as
  points. Because the map's color ramp is city-fitted, this chart is where
  cross-city comparison lives.

  The chart reports **two disparities separately**, because they compound and
  pooling them is misleading. The boxes show **intensity** among zones that
  have measurable exposure; the row beneath shows each group's **share with
  none**. Both track HOLC grade: for rail noise, 34% of grade-A zones have no
  measurable exposure against 12% of grade-D zones, and among zones that do
  have it, grade D carries **2.0×** the exposure of grade A.
- **Value boxes**, between the map and the chart — the **Grade D-vs-A intensity
  ratio** across the mapped cities for the current source × type (computed over
  zones with exposure, so it matches the chart); **zones mapped in the selected
  city**, broken out per HOLC grade; and **sector stratification**, the share of
  first-place finishes each source takes across 16 pollutant × disparity-measure
  comparisons on the full 8,920-zone study frame — rail 59%, aviation 24%, road
  18%. First place means the steepest gradient of the three sources, so the
  measure ranks the sources against each other rather than sizing any one
  source's disparity. CEI is excluded from that tally as it is everywhere else.

## Scope (deliberately hedged)
This is a descriptive work sample, not a published or causal result, and it
carries the project's documented data-quality limits by design:

- **All three sources are shown — rail, road, and aviation.** Aviation is
  sparse: 54–78% of mapped zones have no measurable aviation exposure (against
  10–21% for rail and 0% for road). No city in the mapped set is empty for any
  source × type, but that is a consequence of the 100-zone display cutoff
  retaining only large cities; at a cutoff of 20, 33 of 155 cities had no
  measurable aviation noise anywhere. Aviation is
  included rather than
  dropped, because excluding a sector for having a weaker or messier gradient
  would make "rail is the most stratified sector" partly an artifact of which
  sectors were displayed. Aviation's own profile is genuinely mixed and the
  presence/intensity split is what makes it readable: **presence** is graded
  (80% of grade-A zones have no aviation NO exposure against 66% of grade-D),
  while **intensity** among zones that have it is uneven (D:A 1.1× for noise
  but 1.8–2.2× for the air pollutants, against rail's 2.0–2.3× and road's
  1.1–1.5×). Its demographic gradients
  also point the opposite way from the other sectors — aviation noise carries
  the strongest %Hispanic gradient of any sector × pollutant in the project,
  but its income gradient runs positive where rail's and road's run negative.
- **No combined exposure index (CEI)** — the combined index is structurally
  unreliable and is omitted entirely.
- **Per-city exploration + a durable national finding**, not a single national
  causal claim. No causal language.
- **Cities with < 100 graded zones are dropped from the map** (207 cities /
  6,094 zones, from 221 cities / 8,920 zones down to 14 / 2,826) — they render
  sparse, unreadable maps and unstable per-city summaries. Zones per city in
  the retained set: min 111, median 138, max 677. **This is a display filter
  only.** The study's statistical analysis runs on the full 8,920-zone frame,
  so figures quoted here (computed on the 2,826-zone display bundle) differ
  slightly from the paper's. State which basis when quoting either.

## Files
- `app.R` — the Shiny app. Self-contained; reads only `redenv_data.rds`.
- `redenv_data.rds` — baked bundle (HOLC zone polygons + grade + demographics;
  zone × sector × pollutant exposure; city selector table; sector-ranking
  summary). ~0.65 MB; **no `D:\` data or API key needed at runtime.**
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
