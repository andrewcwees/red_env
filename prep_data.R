# prep_data.R ---------------------------------------------------------------
# One-time build: bake a slim, self-contained dataset for the redenv Shiny app.
#
# Reads the canonical redenv analytic frame on D:/ and writes redenv_data.rds
# next to app.R. After this runs, the app needs no D:\ access and no Census key.
#
# Geometry source: the HOLC/2010-tract crosswalk (cw.gpkg). Its zone key,
# HOLCID = label_state_city_grade, is built identically to the zone ID in
# xp_out.csv (see scripts/compile/clean.R), so the join is exact -- all 8,920
# graded zones match. (Native holc_maps.gpkg polygons exist but key on a
# different label field, so the crosswalk is the clean join.)
#
# Bundle contents (list):
#   zones  - one sf POLYGON row per HOLC zone: HOLCID, place, city, state,
#            grade + zone demographics; simplified, EPSG:4326
#   exp    - HOLCID x sector x pollutant -> xp, logxp
#            (Rail + Road only; NO CEI, NO Aviation, NO Total -- known defects)
#   cities - searchable city selector table (place, city, state, n_zones)
#   rank   - sector first-place ranking summary (the anchor finding)
#
# Hard constraints baked in here (see interactive_display_plan_2026-07-14.md):
#   * Rail + Road sectors only; Aviation excluded (63-69% zero zones).
#   * No CEI in any form (structurally broken combined index).
#   * logxp is the coloring scale for every pollutant (tail concentration).
#
# Source data are unpublished HUM Lab work; externalizing this derived subset
# requires sign-off from Chris Lim (same gate phxmob cleared 2026-07-08).

suppressWarnings(suppressMessages({
  library(sf); library(data.table); library(dplyr); library(stringr)
}))
sf_use_s2(FALSE)  # crosswalk polygons trip s2 on a few degenerate edges

app_dir <- "C:/Users/andre/Desktop/box/projects/life/job-search/portfolio-shiny-redenv"
d_out   <- "D:/research/redenv/data/output"

keep_sectors    <- c("Rail", "Road")
drop_pollutants <- c("CEI")

# 1. exposure + demographics from xp_out.csv --------------------------------
xp <- fread(file.path(d_out, "xp_out.csv"))
xp <- xp[group %in% c("Grade A", "Grade B", "Grade C", "Grade D")]  # graded zones only

# 1a. exposure slice: Rail + Road, no CEI, logxp carries the coloring
exp <- xp[sector %in% keep_sectors & !(pollutant %in% drop_pollutants),
          .(HOLCID = ID, sector, pollutant, xp, logxp)]
setkey(exp, HOLCID, sector, pollutant)
cat(sprintf("exposure rows: %d  (%d zones x %d sectors x %d pollutants)\n",
            nrow(exp), uniqueN(exp$HOLCID),
            uniqueN(exp$sector), uniqueN(exp$pollutant)))

# 1b. zone-level demographics (constant across sector/pollutant -> one row/zone)
demo <- unique(xp[, .(HOLCID = ID, region, division, state, age, inc, pdens,
                      pct_wht, pct_blk, pct_asn, pct_hisp, pct_other, pct_rent)])
stopifnot(!anyDuplicated(demo$HOLCID))

# 2. zone geometry from the crosswalk ---------------------------------------
zone_ids <- unique(exp$HOLCID)
cw <- st_read(file.path(d_out, "cw.gpkg"), quiet = TRUE) |> st_make_valid()

zones <- cw |>
  filter(HOLCID %in% zone_ids) |>
  group_by(HOLCID, city, state, grade) |>
  summarise(.groups = "drop") |>
  # simplify in an equal-area CRS, then back to WGS84 for leaflet
  st_transform(5070) |>
  st_simplify(dTolerance = 50, preserveTopology = TRUE) |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(place = paste0(city, ", ", state)) |>
  left_join(as.data.frame(demo)[, setdiff(names(demo), c("state"))], by = "HOLCID") |>
  select(HOLCID, place, city, state, grade, region, division,
         age, inc, pdens, pct_wht, pct_blk, pct_asn, pct_hisp, pct_other, pct_rent)

cat(sprintf("zone polygons: %d / %d exposure zones matched\n",
            nrow(zones), length(zone_ids)))

# 3. searchable city selector table -----------------------------------------
cities <- zones |>
  st_drop_geometry() |>
  count(place, city, state, name = "n_zones") |>
  arrange(state, city)
cat(sprintf("cities: %d  (zones/city: min %d, median %g, max %d; %d with <5 zones)\n",
            nrow(cities), min(cities$n_zones), median(cities$n_zones),
            max(cities$n_zones), sum(cities$n_zones < 5)))

# 4. anchor finding: sector first-place ranking -----------------------------
rank <- fread(file.path(d_out, "sector_strat/sector_rank_summary.csv"))

# 5. save bundle ------------------------------------------------------------
saveRDS(list(zones = zones, exp = exp, cities = cities, rank = rank),
        file.path(app_dir, "redenv_data.rds"))
sz <- file.info(file.path(app_dir, "redenv_data.rds"))$size
cat(sprintf("wrote redenv_data.rds (%.2f MB)\n", sz / 1024^2))
