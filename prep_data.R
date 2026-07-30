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
# EVERY city in the study frame is mapped: min_zones is 0, so the selector
# carries all 221 cities and the map and the statistics answer to the same
# 8,920 zones. The thin-city cutoff that used to narrow the map to 14 large
# cities is gone, and with it the two-number-bases problem it created. What
# made removing it viable is the tract layer below: a city with nine graded
# zones no longer renders as nine polygons floating in white space, because
# the tract layer covers the whole city view underneath them.
#
# Bundle contents (list):
#   zones  - one sf POLYGON row per HOLC zone: HOLCID, place, city, state,
#            grade + zone demographics; simplified, EPSG:4326
#   exp    - HOLCID x sector x pollutant -> xp, logxp
#            (Rail + Road + Aviation; NO CEI, NO Total -- CEI is structurally
#            broken. Total is dropped, but NOT because it is always a sum:
#            that holds for the air pollutants only. See the note at the
#            keep_sectors filter below before restoring it.)
#   cities - searchable city selector table (place, city, state, n_zones)
#   rank   - sector first-place ranking summary (the anchor finding), re-tallied
#            with the CEI comparison dropped
#   rank_cmp - how many pollutant x measure comparisons that tally covers
#   frame  - all 8,920 graded zones, no geometry: HOLCID, place, grade + the
#            three variables the equity chart groups by. Same population as
#            `zones` now that min_zones is 0; kept as its own table because the
#            plots need no geometry and the cutoff may be raised again.
#   exp_frame - HOLCID x sector x pollutant -> xp for all 8,920 zones
#   tracts - one sf POLYGON row per census tract intersecting any mapped city:
#            GEOID, inc, pop_total, pdens. NOT clipped to the city box (see
#            section 6), and carried once however many cities it serves.
#   tract_city - place x GEOID membership, the per-city lookup for `tracts`
#   exp_tract  - GEOID x sector x pollutant -> xp for those tracts
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
# 0 = map every city in the frame. Kept as a constant rather than deleted: the
# filter is one line and the app still tolerates a narrower map, so raising this
# stays a one-number change.
min_zones       <- 0

# infrastructure overlay (section 5)
road_fclass <- c(1, 2)  # HPMS functional class: 1 interstate, 2 other freeway
infra_pad   <- 0.12     # city bbox padding (fraction of extent) for the clip
infra_tol   <- 30       # line simplification tolerance, metres (EPSG:5070)
# FRA NET codes kept in the rail overlay: M main sub network, O other track
# (minor industrial leads), I major industrial lead, Y yard, S passing siding,
# F rail ferry connection. Dropped: A abandoned, R abandoned and physically
# removed, T trail on former right-of-way, X out of service. Definitions are the
# NTAD data dictionary's own (doi 10.21949/1528950).
rail_net    <- c("M", "O", "I", "Y", "S", "F")

# No polygon simplification tolerance: zones and tracts both ship at full
# resolution because independent per-feature simplification breaks shared
# borders (see sections 2 and 6). infra_tol still applies to the overlay, where
# lines share no borders and the flaw cannot arise.

# 1. exposure + demographics from xp_out.csv --------------------------------
# xp_all keeps every group because section 6 needs the all_tract rows; xp is the
# graded-zone slice everything else works from.
xp_all <- fread(file.path(d_out, "xp_out.csv"))
xp <- xp_all[group %in% c("Grade A", "Grade B", "Grade C", "Grade D")]  # graded zones only

# logxp is just log1p(xp) rounded to 3 dp (comb.R), so the tract table in
# section 6 carries xp alone and the app recomputes. Checked here rather than
# assumed, and checked early, because the tract table is ~300k rows and the
# saving is real.
#
# Scoped to the rows that actually get carried. CEI goes negative (down to
# -1.14, it is a standardized index rather than a concentration), so log1p is
# undefined on it and a check across the whole file fails on a column no part
# of this app ever reads. Total is excluded here too.
#
# Why Total is dropped, stated properly because the short version is wrong.
# For the AIR pollutants it really is Rail + Road + Aviation (every zone
# within 1%), and Road is 98.4-99.0% of that mass, so a Total view would
# duplicate the Road view exactly: indexed A->D, Total NO is 1.00/1.14/1.20/
# 1.19 against Road NO's 1.00/1.14/1.20/1.19.
# For NOISE it is NOT a sum and not derived from the three at all. comp.R
# reads BTS's own `rail_road_and_aviation` layer as a fourth sector; it is not
# the arithmetic sum (median 1.48 off), not the max (matches 1% of zones), and
# not the energetic dB sum (median 3.5 dB off). Its gradient is its own:
# 1.00/1.42/1.73/2.08, monotone, between road (1.40) and rail (2.96), with 0%
# zeros. Left out of the app deliberately, since a Source valid for one of
# four Types costs more than it explains -- but it is a real measure, not a
# redundant one, and is carried in context.md as an analysis lead.
# Tolerance is 2e-3 because logxp is stored rounded.
chk <- xp_all[sector %in% keep_sectors & !(pollutant %in% drop_pollutants)]
stopifnot(max(abs(chk$logxp - log1p(chk$xp)), na.rm = TRUE) < 2e-3)
rm(chk)

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

# NOT simplified. st_simplify works on one feature at a time, so two polygons
# sharing a border each get their own simplification of it and the two copies
# stop matching: the result is slivers and gaps along every shared edge. At a
# 50m tolerance this cost LA 1.01 km2 of overlap across 546 border-sharing zone
# pairs. preserveTopology = TRUE does not help -- it preserves each feature's
# own topology, never the topology between features. Any tolerance has the
# defect; a smaller one only makes the slivers smaller. See the tract note in
# section 6, where the same flaw was far worse.
zones <- cw |>
  filter(HOLCID %in% zone_ids) |>
  group_by(HOLCID, city, state, grade) |>
  summarise(.groups = "drop") |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(place = paste0(city, ", ", state)) |>
  left_join(as.data.frame(demo)[, setdiff(names(demo), c("state"))], by = "HOLCID") |>
  select(HOLCID, place, city, state, grade, region, division,
         age, inc, pdens, pct_wht, pct_blk, pct_asn, pct_hisp, pct_other, pct_rent)

cat(sprintf("zone polygons: %d / %d exposure zones matched\n",
            nrow(zones), length(zone_ids)))

# 2a. thin-coverage city filter (inert at min_zones = 0) ---------------------
# The objection this answered was that a city with a handful of graded zones
# renders as a few polygons in an empty frame. The tract layer answers it
# better: the whole city view is covered, and the graded zones sit inside a
# surface that shows what the rest of the city is exposed to. So every city is
# mapped, and this stays only as the knob to turn if that judgement reverses.
zone_counts <- zones |> st_drop_geometry() |> count(place, name = "n_zones")
keep_places <- zone_counts$place[zone_counts$n_zones >= min_zones]
n_dropped_cities <- nrow(zone_counts) - length(keep_places)
n_dropped_zones  <- sum(zone_counts$n_zones[zone_counts$n_zones < min_zones])

# 2b. the full study frame, captured BEFORE the thin-city filter -------------
# At min_zones = 0 this is the same population the map draws, so the app's
# figures and its choropleth finally describe one thing. The table is still
# built separately because it drops geometry: the plots never draw a polygon,
# and carrying 8,920 of them a second time would be dead weight.
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
# RAIL keeps every class of track that carries traffic -- yard and spur is
# exactly what generates rail exposure, so thinning to mainlines would
# misrepresent it -- but drops the four NET codes that are not track a train
# runs on: abandoned, physically removed, out of service, and trail on former
# right-of-way. A rails-to-trails corridor is a bike path, and drawing it as
# rail puts a non-source on a map whose whole subject is proximity to sources.
# The class itself is carried through as `net` so the hover label can name it.
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
# The boxes as one sf, so membership is a single indexed spatial predicate
# rather than 221 full passes over a national layer.
boxes_sf <- st_sf(place = places, geometry = do.call(c, boxes))

# clip one national layer to every city, tagging rows with the city they serve.
# A feature spanning two mapped cities is emitted once per city, which is what
# a per-city lookup needs.
#
# st_intersects first, st_crop second. At 14 cities cropping the whole layer per
# city was merely wasteful; at 221 it is quadratic enough to matter, because
# every call scans all 500k+ segments to keep a few hundred. The predicate is
# indexed, so one pass assigns candidates to boxes and each crop then runs on
# the handful of features that can possibly survive it.
clip_to_cities <- function(x, simplify = TRUE) {
  hits <- st_intersects(boxes_sf, x)
  parts <- lapply(seq_along(places), function(i) {
    pl  <- places[i]
    idx <- hits[[i]]
    if (!length(idx)) return(NULL)
    # st_crop clips in lon/lat and says so on every call; the boxes are city-
    # sized, where the planar assumption costs nothing
    y <- suppressMessages(suppressWarnings(st_crop(x[idx, ], boxes[[pl]])))
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

# -- rail: operational track in the bundle's states --
rail <- st_read(
  file.path(d_ntad, "NTAD_rail.geojson"), quiet = TRUE,
  query = sprintf(paste("SELECT RROWNER1, NET FROM \"NTAD_rail\"",
                        "WHERE STATEAB IN (%s) AND NET IN (%s)"),
                  quote_in(bundle_states), quote_in(rail_net)))
rail$owner <- rail$RROWNER1
rail$net   <- rail$NET
rail <- clip_to_cities(rail[, c("owner", "net")])
cat(sprintf("rail: %d segments across %d cities\n", nrow(rail),
            length(unique(rail$place))))
print(table(rail$net))

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

# 6. census tract exposure layer --------------------------------------------
# The HOLC layer can only ever answer for graded neighborhoods, which is a
# minority of any city and, in the thin cities now back in the selector, a very
# small minority. The tract layer draws the same modeled exposure across the
# whole city view, so the question the choropleth answers changes from "how does
# exposure vary among graded neighborhoods" to "where do the graded
# neighborhoods sit in the city's own exposure surface".
#
# Units are directly comparable: comb.R builds a zone's xp as the area-weighted
# mean of the tracts intersecting it, so a zone value and a tract value are the
# same quantity at two resolutions. That is what lets the app fit ONE per-city
# color ramp across both layers, which is the entire point -- a reader flipping
# between them sees the same color mean the same thing.
#
# Geometry is NOT clipped to the city box, unlike the infrastructure lines. A
# clipped tract gets a straight artificial edge that reads as a real boundary,
# and tracts are areal units where that misleads; lines have no such problem.
# The cost is that edge tracts run past the default view, which is harmless.
# Each tract is carried once however many city boxes it falls in, with
# membership in a side table, so the overlapping metros do not duplicate
# polygons.
cat("-- census tracts --\n")
tracts <- st_read(file.path(d_out, "noise_tract.gpkg"), layer = "noise_tract",
                  quiet = TRUE)
tracts <- tracts[, c("TRACTID", "inc", "pop_total", "pdens")]
names(tracts)[names(tracts) == "TRACTID"] <- "GEOID"
# NOT simplified, and this layer is why the rule matters. Tracts tessellate:
# every interior edge is shared with a neighbour, so simplifying each polygon
# independently desynchronises both copies of every one of them. At 150m the LA
# tract layer came out with 29 km2 of overlap, 59 km2 of territory lost, and
# 1,459 holes punched between neighbours against 20 in the source, which is
# what a reader sees as polygons that do not fit together.
#
# The proper fix is topology-aware simplification (rmapshaper::ms_simplify
# builds a shared-arc topology and simplifies each arc once, so both sides of a
# border move together). That would cut this layer back to roughly a third of
# its size and is worth doing if the bundle needs to shrink. It is not used here
# only because installing it needs an Rcpp upgrade that cannot complete while
# another R process holds the DLL.
tracts <- tracts |>
  st_transform(4326) |>
  st_make_valid()
tracts <- tracts[!st_is_empty(tracts), ]

tract_hits <- st_intersects(boxes_sf, tracts)
tract_city <- data.table(place = rep(places, lengths(tract_hits)),
                         GEOID = tracts$GEOID[unlist(tract_hits)])
tracts <- tracts[tracts$GEOID %in% tract_city$GEOID, ]
cat(sprintf("tracts: %d kept, %d place-tract pairs across %d cities\n",
            nrow(tracts), nrow(tract_city), uniqueN(tract_city$place)))

# xp only -- logxp is log1p(xp), checked in section 1, and at ~300k rows the
# second column is not worth its weight in the bundle.
exp_tract <- xp_all[group == "all_tract" & sector %in% keep_sectors &
                    !(pollutant %in% drop_pollutants),
                    .(GEOID = ID, sector, pollutant, xp)]
exp_tract <- exp_tract[GEOID %in% tracts$GEOID]
setkey(exp_tract, GEOID, sector, pollutant)
cat(sprintf("tract exposure rows: %d (%d tracts matched of %d)\n",
            nrow(exp_tract), uniqueN(exp_tract$GEOID), nrow(tracts)))

# The shared ramp only works if the two layers really are on one scale; a gross
# mismatch would mean comb.R's weighting is not what section 6 assumes.
zr <- range(exp$xp[exp$xp > 0]); tr_r <- range(exp_tract$xp[exp_tract$xp > 0])
cat(sprintf("xp range  zones: %.3g to %.3g   tracts: %.3g to %.3g\n",
            zr[1], zr[2], tr_r[1], tr_r[2]))

# 7. save bundle ------------------------------------------------------------
saveRDS(list(zones = zones, exp = exp, cities = cities, rank = rank,
             rank_cmp = n_cmp, frame = frame, exp_frame = exp_frame,
             tracts = tracts, tract_city = tract_city, exp_tract = exp_tract,
             infra = infra),
        file.path(app_dir, "redenv_data.rds"))
sz <- file.info(file.path(app_dir, "redenv_data.rds"))$size
cat(sprintf("wrote redenv_data.rds (%.2f MB)\n", sz / 1024^2))
