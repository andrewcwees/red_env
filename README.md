# Redlining & Transportation-Sector Environmental Exposure — Shiny work sample

An interactive R Shiny dashboard built from the **redenv** research project
(Weesner, Higashidani, … Lim; University of Arizona). For a selected U.S. city
it maps **present-day transportation-sector environmental exposure** across
neighborhoods graded by the 1930s **HOLC ("redlining")** maps, and shows how
that exposure tracks HOLC grade.

Purpose: a job-search work sample demonstrating R + Shiny + `leaflet` + spatial
data (HOLC polygons, 8,289 graded zones across 155 cities in 38 states) +
reproducible dashboarding.

**Live app:** _(add your shinyapps.io URL after deploying — see below)_
**Code:** this repository.

## What it does
Pick a **city** (searchable, 155 cities), a **transportation sector**, and a
**pollutant / measure**:

- **Map** (`leaflet` choropleth) — two mutually exclusive fill views, switched
  with the layer control: **Exposure** (log scale, darker = higher) and **HOLC
  grade** (green A → red D). Each carries its own legend. Zones with **no
  measurable exposure** are drawn in a separate grey rather than at the bottom
  of the ramp, so an absence never reads as a low value. Hover a zone for its
  grade, exposure, and demographics. The exposure ramp is **fitted to the
  selected city**, so it reads the within-city spread; colors are deliberately
  not comparable between cities, and cross-city context comes from the equity
  chart instead. If no zone in the current view has any measurable exposure
  (common for aviation), the header says so instead of leaving an unexplained
  grey map. A **basemap** toggle switches between a light cartographic base
  (default) and **satellite imagery** (Esri World Imagery, keyless), and a
  **fill opacity** slider dims the choropleth so the infrastructure underneath
  it — the rail yard, the interchange, the airport — is legible.
- **Equity chart** (`ggplot2`) — the national exposure distribution (all 155
  mapped cities pooled) grouped by **HOLC grade** (default) or, via a toggle,
  by **demographic quintile** (median income, % Hispanic, % Black), with the
  selected city's zones overlaid as points. Because the map's color ramp is
  city-fitted, this chart is where cross-city comparison lives.

  The chart reports **two disparities separately**, because they compound and
  pooling them is misleading. The boxes show **intensity** among zones that
  have measurable exposure; the row beneath shows each group's **share with
  none**. Both track HOLC grade: for rail noise, 36% of grade-A zones have no
  measurable exposure against 14% of grade-D zones, and among zones that do
  have it, grade D carries **3.1×** the exposure of grade A.
- **Value boxes** — the national **Grade D-vs-A intensity ratio** for the
  current sector × pollutant (computed over zones with exposure, so it matches
  the chart), **zones mapped**, and the anchor finding: **rail is the most
  stratified transportation sector** (ranks #1 in 61% of sector × measure
  comparisons).

## Scope (deliberately hedged)
This is a descriptive work sample, not a published or causal result, and it
carries the project's documented data-quality limits by design:

- **All three sectors are shown — rail, road, and aviation.** Aviation is
  sparse: 63–70% of HOLC zones have no measurable aviation exposure (against
  8–22% for rail and 0% for road), and whole cities come up empty: 33 of the
  155 have no measurable aviation noise anywhere, and 9–10 have none for its
  air pollutants, so their map is legitimately blank and says so. It is
  included rather than
  dropped, because excluding a sector for having a weaker or messier gradient
  would make "rail is the most stratified sector" partly an artifact of which
  sectors were displayed. Aviation's own profile is genuinely mixed and the
  presence/intensity split is what makes it readable: **presence** is graded
  (71% of grade-A zones have no aviation NO exposure against 60% of grade-D),
  while **intensity** among zones that have it is close to flat (D:A 0.9–1.4×,
  comparable to road, well below rail's 2.6–3.2×). Its demographic gradients
  also point the opposite way from the other sectors — aviation noise carries
  the strongest %Hispanic gradient of any sector × pollutant in the project,
  but its income gradient runs positive where rail's and road's run negative.
- **No combined exposure index (CEI)** — the combined index is structurally
  unreliable and is omitted entirely.
- **Per-city exploration + a durable national finding**, not a single national
  causal claim. No causal language.
- **Cities with < 20 graded zones are dropped** (66 cities / 631 zones, from
  221 cities / 8,920 zones down to 155 / 8,289) — they render sparse,
  unreadable maps and unstable per-city summaries. Zones per city in the
  retained set: min 20, median 35, max 677.

## Files
- `app.R` — the Shiny app. Self-contained; reads only `redenv_data.rds`.
- `redenv_data.rds` — baked bundle (HOLC zone polygons + grade + demographics;
  zone × sector × pollutant exposure; city selector table; sector-ranking
  summary). ~2.0 MB; **no `D:\` data or API key needed at runtime.**
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
