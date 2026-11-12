# Redlining & Transportation-Sector Environmental Exposure

An interactive R Shiny dashboard built from the **redenv** research project
(Weesner et al., HUM.Lab, University of Arizona). It maps present-day
transportation-sector environmental exposure across neighborhoods graded by the
1930s **HOLC ("redlining")** maps, and shows how that exposure tracks HOLC
grade: 8,920 graded zones and 21,840 census tracts across 221 cities in 40
states.

**Live app:** <https://019fc8f3-a697-e0c2-756f-5c406ccc871e.share.connect.posit.cloud/>

Built with R, `shiny`, `bslib`, `leaflet`, `sf`, and `ggplot2`.

## Two views

A switch in the banner moves between **Map** (one city) and **Data** (the whole
study frame). Both describe the same 8,920 graded zones.

### Map: where is a city's exposure?

Pick a **city**, a **source** (rail, road, aviation), and an **exposure**
(noise, NO, NO₂, PM2.5).

- **Three fills**, one at a time: HOLC zones by grade, HOLC zones by exposure,
  or census tracts by exposure. The two exposure fills share one color ramp, so
  a reader can see where graded neighborhoods sit within their city's overall
  exposure surface.
- **No measurable exposure** is drawn in its own grey, so an absence never
  reads as a low value.
- **Exposure scaling**: *This City* fits the ramp to the selected city's range;
  *Full Sample* uses the national range (trimmed to the 2nd–98th percentile) so
  cities can be compared directly.
- **Overlays** for interstates and freeways, rail track, and airports, plus a
  satellite basemap and an opacity slider.
- **Per-city figures** in the sidebar: zone counts by grade, share of zones with
  no exposure, and the city's rank among all cities.

### Data: how does exposure vary by grade nationally?

- **Grade staircase**: each grade's median exposure as a multiple of grade A,
  on an axis fixed across every source and pollutant so flat and steep
  gradients are visibly different.
- **Equity chart**: exposure by HOLC grade or by demographic quintile (median
  income, % Hispanic, % Black), indexed to the first group. It reports two
  disparities separately: **intensity** among zones with measurable exposure,
  and each group's **share with none**. For rail noise, 36% of grade-A zones
  have no measurable exposure against 14% of grade-D zones, and among exposed
  zones grade D carries 3.0× the exposure of grade A.

## What the data shows

Rail is the only sector with a consistent gradient. Median exposure indexed to
grade A, A→D: rail noise 1.00 / 1.53 / 2.27 / **2.96**, rail NO 1.00 / 1.61 /
2.34 / **2.98**. Road is shallow (noise 1.00 / 1.15 / 1.26 / **1.40**).
Aviation is non-monotone on all four pollutants, peaking at grade B or C. These
comparisons rest on medians, not a regression model.

## Scope

A descriptive work sample, not a published or causal result.

- **Aviation is included** although 63–69% of zones have no measurable aviation
  exposure. Presence is graded (70% of grade-A zones have none against 60% of
  grade-D), while intensity among exposed zones is nearly flat.
- **No combined exposure index.** The source index is structurally unreliable
  and is omitted.
- **Every city in the study frame is mapped**, from 4 to 677 graded zones each.

## Files

- `app.R`: the Shiny app. Self-contained; reads only `redenv_data.rds`.
- `redenv_data.rds`: prebuilt data bundle (~12.5 MB): zone and tract polygons,
  exposure by zone and tract, demographics, and overlay geometry. No external
  data or API key needed at runtime.
- `prep_data.R`: rebuilds the bundle from the project's analytic frame. Set
  `REDENV_DATA` to the data root before running.
- `manifest.json`: pinned dependencies for Posit Connect Cloud. Regenerate with
  `rsconnect::writeManifest()` after any package update and commit it.

## Run locally

```r
# from this folder
shiny::runApp()
```

Requires `shiny`, `bslib`, `leaflet`, `dplyr`, `sf`, `ggplot2`, `scales`.

## Data and provenance

Derived from unpublished HUM.Lab data; this aggregated subset is shared with
the PI's permission. Sources: NEMO transportation-sector emissions (EPA NEI
2017); BTS National Transportation Noise Map; HOLC grades from the University
of Richmond's Mapping Inequality project; ACS 2019 demographics; BTS National
Transportation Atlas Database for the infrastructure overlays.
