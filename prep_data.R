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
# different label field, so the crosswalk is the clean join.) The thin-city
# filter below then takes those 8,920 zones / 221 cities down to the 2,826
# zones / 14 cities that reach the app.
#
# Bundle contents (list):
#   zones  - one sf POLYGON row per HOLC zone: HOLCID, place, city, state,
#            grade + zone demographics; simplified, EPSG:4326
#   exp    - HOLCID x sector x pollutant -> xp, logxp
#            (Rail + Road + Aviation; NO CEI, NO Total -- CEI is structurally
#            broken, Total is a sum of the three and carries no separate story)
#   cities - searchable city selector table (place, city, state, n_zones)
#   rank   - sector first-place ranking summary (the anchor finding), re-tallied
#            with the CEI comparison dropped
#   rank_cmp - how many pollutant x measure comparisons that tally covers
#   frame  - ALL 8,920 graded zones, no geometry: HOLCID, place, grade + the
#            three variables the equity chart groups by. The statistics run on
#            this; the map runs on `zones`, which the thin-city filter narrows
#            to 14 cities. Two populations, deliberately.
#   exp_frame - HOLCID x sector x pollutant -> xp for all 8,920 zones
#   infra  - list(road, rail, avi): NTAD transportation infrastructure clipped
#            to each mapped city, for the map's overlay layers
#
# Hard constraints baked in here (see interactive_display_plan_2026-07-14.md):
#   * No CEI in any form (structurally broken combined index).
#   * logxp is the coloring scale for every pollutant (tail concentration).
#   * Aviation is INCLUDED. It is zero-inflated (63-70% of zones have none, vs
#     8-22% for rail and 0% for road), which is why it was held back, but the
#     app now reports presence and intensity separately, so the zeros are shown
#     as an absence rather than folded into the ramp. Dropping a sector because
#     its gradient is weaker or messier would make the "rail is the most
#     stratified sector" claim partly an artifact of which sectors were shown --
#     and aviation is not in fact weak: it out-ranks road on the sector
#     stratification tally (22% vs 17% of first-place finishes), it just
#     disagrees in direction (its income and race gradients flip sign).
#
# Source data are unpublished HUM Lab work; externalizing this derived subset
# was signed off by Chris Lim (2026-07-17; same gate phxmob cleared 2026-07-08).

suppressWarnings(suppressMessages({
  library(sf); library(data.table); library(dplyr); library(stringr)
}))
sf_use_s2(FALSE)  # crosswalk polygons trip s2 on a few degenerate edges

app_dir <- "C:/Users/andre/Desktop/box/projects/life/job-search/portfolio-shiny-redenv"
d_out   <- "D:/research/redenv/data/output"
d_ntad  <- "D:/research/redenv/data/ntnm"   # NTAD infrastructure geojsons

keep_sectors    <- c("Rail", "Road", "Aviation")
drop_pollutants <- c("CEI")
min_zones       <- 100  # drop thin-coverage cities (< min_zones graded zones)

# infrastructure overlay (section 5)
road_fclass <- c(1, 2)  # HPMS functional class: 1 interstate, 2 other freeway
infra_pad   <- 0.12     # city bbox padding (fraction of extent) for the clip
infra_tol   <- 30       # line simplification tolerance, metres (EPSG:5070)

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

# 2a. drop thin-coverage cities ---------------------------------------------
# Cities with very few graded zones render sparse, unreadable maps and unstable
# per-city summaries. Filtering them (and their zones) also keeps the national
# equity reference restricted to adequately-covered cities.
zone_counts <- zones |> st_drop_geometry() |> count(place, name = "n_zones")
keep_places <- zone_counts$place[zone_counts$n_zones >= min_zones]
n_dropped_cities <- nrow(zone_counts) - length(keep_places)
n_dropped_zones  <- sum(zone_counts$n_zones[zone_counts$n_zones < min_zones])

# 2b. the full study frame, captured BEFORE the thin-city filter -------------
# The map and the statistics answer to different populations and now say so.
# min_zones exists to keep the choropleth legible, which is a display concern
# and no reason to narrow the statistics: the frame is every graded zone the
# study analyses, and the survivors of the filter are all large cities, so the
# two bases differ materially (rail noise D:A is 3.0x on the frame and 2.0x on
# the 14-city bundle). Geometry is dropped here -- the frame drives plots only,
# never the map, so 8,920 polygons would be dead weight in the bundle.
frame <- zones |>
  st_drop_geometry() |>
  select(HOLCID, place, grade, inc, pct_hisp, pct_blk)
# logxp is the map's coloring scale and the map never draws frame zones, so
# only xp is carried; dropping it keeps this table roughly a third smaller.
exp_frame <- exp[, .(HOLCID, sector, pollutant, xp)]
cat(sprintf("study frame: %d zones / %d cities, %d exposure rows\n",
            nrow(frame), length(unique(frame$place)), nrow(exp_frame)))

zones <- zones |> filter(place %in% keep_places)
exp   <- exp[HOLCID %in% zones$HOLCID]
cat(sprintf("thin-city filter (< %d zones): dropped %d cities / %d zones -> %d zones kept\n",
            min_zones, n_dropped_cities, n_dropped_zones, nrow(zones)))

# 3. searchable city selector table -----------------------------------------
cities <- zones |>
  st_drop_geometry() |>
  count(place, city, state, name = "n_zones") |>
  arrange(state, city)
cat(sprintf("cities: %d  (zones/city: min %d, median %g, max %d)\n",
            nrow(cities), min(cities$n_zones), median(cities$n_zones),
            max(cities$n_zones)))

# 4. anchor finding: sector first-place ranking -----------------------------
# Re-tallied here from sector_rank_long.csv rather than read from the baked
# sector_rank_summary.csv, because that summary includes the CEI comparison
# (M1_holc_pct_sig for CEI is one of its 17 pollutant x measure cells) and the
# app reports no CEI in any form. Dropping it leaves 16 comparisons: 4
# pollutants (NO, NO2, PM25, Noise) x 4 measures (HOLC pct-significant, and the
# |coef| on pct_blk / pct_hisp / ln_inc). Ranks are per comparison and already
# computed upstream, so dropping a comparison never changes the surviving ones.
# ties.method = "min" upstream means a tied comparison awards two first places,
# so the finishes total can exceed the comparison count; n_cmp is carried into
# the bundle so the app can state both without hardcoding either.
rank_long <- read.csv(file.path(d_out, "sector_strat/sector_rank_long.csv"),
                      stringsAsFactors = FALSE)
rank_long <- rank_long[rank_long$pollutant != "CEI", ]
n_cmp <- nrow(unique(rank_long[, c("pollutant", "measure")]))
rank  <- rank_long |>
  filter(rank == 1) |>
  count(sector, name = "n_first_place") |>
  mutate(pct_first = round(100 * n_first_place / sum(n_first_place), 1)) |>
  arrange(desc(n_first_place))
cat(sprintf("sector ranking: %d comparisons (CEI dropped), %d first places\n",
            n_cmp, sum(rank$n_first_place)))
print(as.data.frame(rank))

# 5. infrastructure overlay geometry ----------------------------------------
# The map answers "which zones are exposed"; the overlay answers "to what". The
# three NTAD layers on D:/ are national and far too large to ship (571/361/40
# MB), so each is filtered at the OGR level to the bundle's states, clipped to
# the mapped cities' padded bounding boxes, and simplified.
#
# ROADS are limited-access highways only -- HPMS functional class 1 (interstate)
# and 2 (other freeway/expressway). Class 3 (other principal arterial) is 137k
# segments across these states and blankets a city map so thoroughly that it
# stops explaining anything; classes 1-2 are also the roads that dominate the
# sector's emissions and noise, and urban interstates are the concrete case in
# the redlining-and-infrastructure literature. Attribute filtering happens in
# the OGR query so the full file is never materialised in R.
#
# RAIL is kept whole (no class filter): yard and spur track is exactly what
# generates rail exposure, so thinning to mainlines would misrepresent it.
#
# AIRPORTS are public-use, operational airports (SITE_TYPE_CODE "A"), which
# drops heliports, seaplane bases and private strips. This is a proximity cue,
# not the aviation emissions inventory -- the exposure surface is modeled from
# operations, so a small GA field on the map carries far less exposure than a
# hub. The towered flag sizes the marker to keep that visible.
#
# The clip box is the city's zone bbox padded by infra_pad, which is what the
# map's fitBounds shows, so lines run to the edge of the default view. Zoom out
# far enough and they stop; that is a bundle-size tradeoff, stated in the app.
cat("-- infrastructure overlay --\n")
bundle_states <- sort(unique(zones$state))
places        <- sort(unique(zones$place))

city_box <- function(pl, pad = infra_pad) {
  bb <- st_bbox(zones[zones$place == pl, ])
  dx <- (bb[["xmax"]] - bb[["xmin"]]) * pad
  dy <- (bb[["ymax"]] - bb[["ymin"]]) * pad
  st_as_sfc(st_bbox(c(xmin = bb[["xmin"]] - dx, xmax = bb[["xmax"]] + dx,
                      ymin = bb[["ymin"]] - dy, ymax = bb[["ymax"]] + dy),
                    crs = 4326))
}
boxes <- setNames(lapply(places, city_box), places)

# clip one national layer to every city, tagging rows with the city they serve.
# A feature spanning two mapped cities is emitted once per city, which is what
# a per-city lookup needs.
clip_to_cities <- function(x, simplify = TRUE) {
  parts <- lapply(places, function(pl) {
    # st_crop clips in lon/lat and says so on every call; the boxes are city-
    # sized, where the planar assumption costs nothing
    y <- suppressMessages(suppressWarnings(st_crop(x, boxes[[pl]])))
    if (!nrow(y)) return(NULL)
    if (simplify) {
      y <- y |> st_transform(5070) |> st_simplify(dTolerance = infra_tol) |>
        st_transform(4326)
      y <- y[!st_is_empty(y), ]
      if (!nrow(y)) return(NULL)
    }
    y$place <- pl
    y
  })
  do.call(rbind, Filter(Negate(is.null), parts))
}

quote_in <- function(v) paste0("'", v, "'", collapse = ",")
# The road layer keys on numeric state FIPS (rail and aviation carry the
# postal abbreviation), so the bundle's states are translated here rather than
# pulled from tigris -- a literal lookup keeps prep_data.R free of a Census
# dependency it needs for nothing else.
fips_lookup <- c(
  AL = 1, AK = 2, AZ = 4, AR = 5, CA = 6, CO = 8, CT = 9, DE = 10, DC = 11,
  FL = 12, GA = 13, HI = 15, ID = 16, IL = 17, IN = 18, IA = 19, KS = 20,
  KY = 21, LA = 22, ME = 23, MD = 24, MA = 25, MI = 26, MN = 27, MS = 28,
  MO = 29, MT = 30, NE = 31, NV = 32, NH = 33, NJ = 34, NM = 35, NY = 36,
  NC = 37, ND = 38, OH = 39, OK = 40, OR = 41, PA = 42, RI = 44, SC = 45,
  SD = 46, TN = 47, TX = 48, UT = 49, VT = 50, VA = 51, WA = 53, WV = 54,
  WI = 55, WY = 56)
state_fips <- unname(fips_lookup[bundle_states])
stopifnot(!anyNA(state_fips))

# -- roads: interstates + other freeways/expressways --
road <- st_read(
  file.path(d_ntad, "NTAD_road.geojson"), quiet = TRUE,
  query = sprintf(paste("SELECT SIGNT1, SIGNN1, LNAME, FCLASS, AADT",
                        "FROM \"NTAD_road\" WHERE STFIPS IN (%s) AND FCLASS IN (%s)"),
                  paste(state_fips, collapse = ","),
                  paste(road_fclass, collapse = ",")))
# route shield where the segment carries one ("I-90"), street name otherwise
route <- with(road, ifelse(SIGNT1 == "I" & nzchar(SIGNN1), paste0("I-", SIGNN1),
                    ifelse(SIGNT1 == "U" & nzchar(SIGNN1), paste0("US-", SIGNN1),
                    ifelse(SIGNT1 == "S" & nzchar(SIGNN1), paste0("SR-", SIGNN1),
                           NA_character_))))
road$name  <- ifelse(is.na(route), trimws(road$LNAME), route)
road$aadt  <- road$AADT
road$fclass <- road$FCLASS
road <- clip_to_cities(road[, c("name", "aadt", "fclass")])
cat(sprintf("roads: %d segments across %d cities\n", nrow(road),
            length(unique(road$place))))

# -- rail: all track in the bundle's states --
rail <- st_read(
  file.path(d_ntad, "NTAD_rail.geojson"), quiet = TRUE,
  query = sprintf("SELECT RROWNER1 FROM \"NTAD_rail\" WHERE STATEAB IN (%s)",
                  quote_in(bundle_states)))
rail$owner <- rail$RROWNER1
rail <- clip_to_cities(rail[, "owner"])
cat(sprintf("rail: %d segments across %d cities\n", nrow(rail),
            length(unique(rail$place))))

# -- airports: public-use, operational, fixed-wing --
avi <- st_read(
  file.path(d_ntad, "NTAD_aviation.geojson"), quiet = TRUE,
  query = paste("SELECT ARPT_NAME, ARPT_ID, TWR_TYPE_CODE FROM \"NTAD_aviation\"",
                "WHERE SITE_TYPE_CODE = 'A' AND FACILITY_USE_CODE = 'PU'",
                "AND ARPT_STATUS = 'O'"))
avi$name    <- tools::toTitleCase(tolower(avi$ARPT_NAME))
avi$id      <- avi$ARPT_ID
avi$towered <- avi$TWR_TYPE_CODE != "NON-ATCT"
avi <- clip_to_cities(avi[, c("name", "id", "towered")], simplify = FALSE)
cat(sprintf("airports: %d (%d towered) across %d cities\n", nrow(avi),
            sum(avi$towered), length(unique(avi$place))))

infra <- list(road = road, rail = rail, avi = avi)

# 6. save bundle ------------------------------------------------------------
saveRDS(list(zones = zones, exp = exp, cities = cities, rank = rank,
             rank_cmp = n_cmp, frame = frame, exp_frame = exp_frame,
             infra = infra),
        file.path(app_dir, "redenv_data.rds"))
sz <- file.info(file.path(app_dir, "redenv_data.rds"))$size
cat(sprintf("wrote redenv_data.rds (%.2f MB)\n", sz / 1024^2))
