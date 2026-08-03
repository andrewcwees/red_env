# Redlining & Transportation-Sector Environmental Exposure: Shiny work sample

An interactive R Shiny dashboard built from the **redenv** research project
(Weesner, Higashidani, … Lim; University of Arizona). For a selected U.S. city
it maps **present-day transportation-sector environmental exposure** across
neighborhoods graded by the 1930s **HOLC ("redlining")** maps, and shows how
that exposure tracks HOLC grade.

Purpose: a job-search work sample demonstrating R + Shiny + `leaflet` + spatial
data (8,920 HOLC zones and 21,840 census tracts across 221 cities in 40 states)
+ reproducible dashboarding.

**Live app:** <https://019fc8f3-a697-e0c2-756f-5c406ccc871e.share.connect.posit.cloud/>
**Code:** this repository.

## Two views

A switch in the banner moves between **Map** (one city) and **Data** (the whole
study frame). Both describe the same 8,920 graded zones, so a figure on one tab
and a polygon on the other are the same population.

### Map

Pick a **city** (searchable, all 221), a **Source** (rail, road, aviation), and
an **Exposure** (noise, NO, NO₂, PM2.5). The two selectors sit side by side as
a T-chart, so reading down a column compares within one axis.

Three mutually exclusive fills in the layer control, because two choropleths
stacked cannot both be read:

- **HOLC (grade)**: graded neighborhoods filled green A → red D.
- **HOLC (exposure)**: those same neighborhoods on a log scale, darker = higher.
- **Tracts (exposure)**: the same quantity across the whole city view.
  Graded zones are a minority of any city, so this is what lets a reader ask
  where the redlined neighborhoods sit in their city's own exposure surface
  rather than only how they compare with each other. A zone's exposure is the
  area-weighted mean of the tracts intersecting it, so the two layers are one
  quantity at two resolutions, and they share **one per-city color ramp**, so
  switching between them keeps a color meaning the same thing.

Areas with **no measurable exposure** are drawn in a separate grey rather than
at the bottom of the ramp, so an absence never reads as a low value; the key
carries that swatch beneath the ramp, in the same box, under a rule marking it
as an absence rather than the bottom step of the scale. Hover any
polygon for its exposure and demographics. If no graded zone in the current view
has measurable exposure, the header says so, and says whether the tract layer
does.

An **Exposure Scaling** switch picks what the shading is relative to, and the
two settings answer different questions rather than one being better:
**This City** (default) fits the ramp to the selected city's own range and shows
the spread inside it, at the cost of colors meaning nothing between cities;
**Full Sample** fits it to the national range for the current Source × Exposure,
so two cities can be compared directly, at the cost of a low-exposure city
reading as almost uniformly pale, which is the honest answer at that scale. The
national domain is trimmed to the 2nd–98th percentile and out-of-range values
are clamped to the end colors, because exposure is tail-heavy enough that an
untrimmed ramp spends most of the palette on almost nothing. The key says which
scale is in force.

A **Background** toggle switches between a light cartographic base (default) and
satellite imagery (Esri World Imagery, keyless), and a **fill opacity** slider
dims the choropleth so the infrastructure underneath it is legible. Three
toggleable **infrastructure overlays** (interstates and freeways, rail track,
airports) clip to the city view; the one matching the selected source is drawn
by default. The overlay is a locator, not a second exposure estimate.

Three per-city figures sit in the sidebar beside the selectors that drive them:
**grade counts** (five stacked bars, scaled to the city total), **zones with
none** (the city's zero share for the current source × type), and **city rank**
(the city's median exposure as a percentile of the cities where any zone
registers).

### Data

This view is national throughout and carries no city selector, so no pooled
figure can be mistaken for the mapped city's.

- **Grade staircase**: four rows, A→D, in grade colors, each carrying that
  grade's median as a **multiple of grade A** plus its n, because grade A is the
  small-N constraint throughout this project. Bars sit on an axis **fixed across
  every Source × Exposure**, so a flat selection looks flat beside a steep one
  rather than being rescaled to fill the same track; a parity tick marks 1.0,
  where grade A's own bar ends. There is no headline ratio above it: the D row
  already prints that number, so a large copy of it added size and no
  information. Where the peak is not grade D (aviation peaks at B or C on all
  four pollutants) a footnote says so, since the endpoint is not always the
  maximum.
- **Equity chart** (`ggplot2`): one panel for the selected Source, grouped by
  **HOLC grade** (default) or by demographic quintile (median income,
  % Hispanic, % Black). Exposure is **indexed to the median of the leftmost
  group**, so the baseline sits at 1.0 and the last group's height is the
  disparity ratio. The raw units are arbitrary model output spanning five orders
  of magnitude between sectors (median road NO is 2.6e5 against aviation's 3.0),
  so an axis in them would read as nothing. Lines are group medians, bars the
  IQR; the log axis is clipped to the IQR envelope, but every median and
  quartile is computed on all data.

  The chart reports **two disparities separately**, because they compound and
  pooling them is misleading. The lines show **intensity** among zones that have
  measurable exposure; the grey row beneath shows each group's **share with
  none**. Both track HOLC grade: for rail noise, 36% of grade-A zones have no
  measurable exposure against 14% of grade-D zones, and among zones that do have
  it, grade D carries **3.0×** the exposure of grade A.

  Staircase and chart read the same slice, so they agree by construction rather
  than by coincidence.

## What the data shows

Rail is the only orderly sector. Indexed medians A→D: rail noise
1.00/1.53/2.27/**2.96** and rail NO 1.00/1.61/2.34/**2.98**, both monotone;
road is shallow and only noise is monotone (1.00/1.15/1.26/**1.40**); aviation
is non-monotone on all four pollutants, peaking at B or C and ending near parity
(NO₂ 1.00/1.09/0.87/**0.99**). "Rail is most stratified across every pollutant"
overstates it: the ordering rests on medians, not the regression panel.

The chart draws one sector at a time, so this comparison is something a reader
assembles by clicking through Rail, Road and Aviation rather than something the
app puts on one screen.

## Scope (deliberately hedged)
This is a descriptive work sample, not a published or causal result, and it
carries the project's documented data-quality limits by design:

- **All three sources are shown: rail, road, and aviation.** Aviation is
  sparse: 63–69% of zones have no measurable aviation exposure (against 8–22%
  for rail and 0% for road). It is included rather than dropped, because
  excluding a sector for having a weaker or messier gradient would make "rail is
  the most stratified sector" partly an artifact of which sectors were
  displayed. Aviation is discordant, not weak: **presence** is graded (70% of
  grade-A zones have no aviation NO exposure against 60% of grade-D), while
  **intensity** among zones that have it is nearly flat and non-monotone. Its
  demographic gradients also point the opposite way from the other sectors.
- **No combined exposure index (CEI)**: the combined index is structurally
  unreliable and is omitted entirely. It is also the only quantity in the source
  data that goes negative, being a standardized index rather than a
  concentration.
- **Per-city exploration + a durable national finding**, not a single national
  causal claim. No causal language.
- **Every city in the study frame is mapped** (221 cities, 4 to 677 graded zones
  each, median 26). An earlier build dropped cities with fewer than 100 graded
  zones because they rendered as a few polygons in empty space; the tract layer
  answers that objection directly, so the cutoff is gone and the map and the
  statistics no longer describe different populations.

## Files
- `app.R`: the Shiny app. Self-contained; reads only `redenv_data.rds`.
- `redenv_data.rds`: baked bundle (HOLC zone polygons + grade + demographics;
  census tract polygons + exposure; zone × sector × pollutant exposure; city
  selector table; infrastructure overlay geometry). ~12.5 MB; **no `D:\` data or
  API key needed at runtime.** Polygons ship unsimplified: `st_simplify` works
  one feature at a time, so it desynchronises the two copies of every shared
  border and leaves gaps between neighbouring tracts.
- `prep_data.R`: one-time build script that regenerates the bundle from the
  canonical frame (`D:/research/redenv/data/output/`). Only needed to refresh
  the data. Takes several minutes, mostly clipping national NTAD road and rail
  layers to 221 city bounding boxes.
- `manifest.json`: the pinned dependency list Posit Connect Cloud builds the
  app against. Regenerate it with `rsconnect::writeManifest()` whenever a
  package version changes, and commit it, or the deployed app keeps building
  against the old versions.

## Run locally
```r
# from this folder
shiny::runApp()
```
Requires: `shiny`, `bslib`, `leaflet`, `dplyr`, `sf`, `ggplot2`, `scales`.

## Deploy a live link
Published to **Posit Connect Cloud** from this repository: `main` branch,
`manifest.json` at the repo root. Connect Cloud resolves dependencies from that
manifest rather than by scanning `app.R`, so a package upgrade needs
`rsconnect::writeManifest()` rerun and the result committed. Redeploys reuse
the same URL.

## Provenance / data note
Derived from **unpublished HUM Lab data**. External sharing of this derived,
aggregated subset was **signed off by Chris Lim (2026-07-17)** (the same gate
the phxmob work sample cleared 2026-07-08). The app and this repository,
including the derived bundle, are public. Underlying data: NEMO
transportation-sector emissions (EPA NEI 2017); BTS National Transportation
Noise Map; HOLC grades via the University of Richmond Mapping Inequality
project; ACS 2019 demographics; BTS National Transportation Atlas Database for
the infrastructure overlay.
