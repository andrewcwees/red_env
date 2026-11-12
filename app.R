# app.R ----------------------------------------------------------------------
# Redlining & Transportation-Sector Environmental Exposure -- interactive map
#
# Work sample built from the redenv research project (Weesner et al., HUM.Lab,
# University of Arizona). For a selected city it maps present-day
# transportation-sector environmental exposure across neighborhoods graded by
# the 1930s HOLC ("redlining") maps, and shows how exposure tracks HOLC grade.
#
#   * Map   -> one city: HOLC zones by grade, HOLC zones by exposure, or census
#              tracts by exposure (the two exposure fills share one ramp), with
#              road, rail, and airport overlays and per-city summary figures.
#   * Data  -> national view: an A-to-D staircase indexed to grade A and an
#              equity chart by HOLC grade or demographic quintile, reporting
#              presence and intensity of exposure separately.
#
# Scope: rail, road, and aviation; no combined exposure index; descriptive
# only, no causal claims. Not a published result.
#
# Self-contained: reads only redenv_data.rds (built by prep_data.R).
# Run with: shiny::runApp()
#
# Derived from unpublished HUM.Lab data; shared with the PI's permission.

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)

d      <- readRDS("redenv_data.rds")
zones  <- d$zones
exp    <- as.data.frame(d$exp)
cities <- d$cities
rank   <- d$rank
rank_cmp <- d$rank_cmp   # pollutant x measure comparisons behind the tally
infra  <- d$infra        # road / rail / avi geometry, clipped per city

# Census tract layer. The HOLC layer only ever covers graded neighborhoods --
# a minority of any city, and a very small one in the thin cities that are now
# back in the selector. Tracts cover the whole city view, so the map can show
# where the graded zones sit inside their city's exposure surface rather than
# only how graded zones compare with each other. Carried once per tract with
# city membership in a side table, because metros overlap.
tracts     <- d$tracts
tract_city <- as.data.frame(d$tract_city)
exp_tract  <- as.data.frame(d$exp_tract)
# logxp is log1p(xp) upstream (checked in prep_data.R), so the ~300k-row tract
# table ships without it and the one column it needs is derived here, once.
exp_tract$logxp <- log1p(exp_tract$xp)

# `frame` is every graded zone the study analyses and drives every plot and
# stat; `zones` is the same population carrying geometry, and drives the map.
# They were different populations while a thin-city cutoff narrowed the map to
# 14 large cities, which forced every figure to state which basis it used
# (rail noise D:A read 2.96x on one and 2.0x on the other). The cutoff is gone,
# so that distinction is retired -- the two tables now differ only in whether
# they carry polygons, which is why both still exist.
frame     <- as.data.frame(d$frame)
exp_frame <- as.data.frame(d$exp_frame)
frame_cities <- length(unique(frame$place))
frame_states <- length(unique(sub(".*, ", "", frame$place)))

# Ceiling for the D-vs-A staircase bars: the largest grade index anywhere in the
# frame (rail NO at grade D, 2.98x). The bars are lengths on a FIXED axis rather
# than rescaled to whichever selection is on screen, because a flat sector has to
# look flat beside a steep one -- rescaling per selection makes aviation NO2
# (1.09x at its peak) draw the same picture as rail NO (2.98x), which is the
# opposite of what the figure is for. Computed rather than hardcoded so it
# follows the data if prep_data.R changes.
grade_idx_max <- local({
  e <- merge(exp_frame, frame[, c("HOLCID", "grade")], by = "HOLCID")
  e <- e[!is.na(e$xp) & e$xp > 0, ]
  m <- tapply(e$xp, list(paste(e$sector, e$pollutant), e$grade), median)
  max(m / m[, "A"], na.rm = TRUE)   # matrix / vector divides down each column
})

# National color-ramp domain, one per sector x pollutant, for the map's
# National scale. Pooled over zones AND tracts for the same reason the
# per-city domain is: comb.R makes a zone's xp the area-weighted mean of the
# tracts under it, so they are one quantity at two resolutions.
#
# TRIMMED TO THE 2nd-98th PERCENTILE, and this is the whole reason the national
# scale is usable. Exposure is enormously tail-heavy (aviation NO tops out near
# 26M; the top 1% of tracts carries 92-99% of aviation mass), so an untrimmed
# min-to-max domain spends nearly the entire palette on a handful of places and
# renders everywhere else as one color -- the median city's middle 50% of
# polygons occupies about 8% of the ramp for the road air pollutants. Trimming
# roughly triples that. Values outside the domain are CLAMPED to the end colors
# by fill_exposure rather than dropped, so a trimmed tail still reads as "at or
# past the top of the scale" instead of falling through to the no-data fill.
#
# Computed here rather than baked in prep_data.R: it is one grouped pass over
# ~370k rows at startup and needs no rebuild to change the trim.
nat_trim   <- c(0.02, 0.98)
nat_domain <- local({
  v <- rbind(exp_frame[, c("sector", "pollutant", "xp")],
             exp_tract[, c("sector", "pollutant", "xp")])
  v <- v[!is.na(v$xp) & v$xp > 0, ]
  q <- tapply(log1p(v$xp), paste(v$sector, v$pollutant),
              function(x) unname(quantile(x, nat_trim)))
  q
})

# Estimated zone population. The pipeline's pop_total is an area-weighted MEAN
# of the intersecting tracts' totals (comb.R multiplies every numeric column by
# a weight that sums to 1 within a zone), so it is not a headcount and is not
# carried into the app bundle. pdens is people per km2 per tract (clean.R), an
# intensive quantity that the same weighting averages correctly, so density x
# zone area recovers a population estimate. It inherits the uniform-density-
# within-tract assumption that the whole crosswalk rests on.
zones$area_km2 <- as.numeric(st_area(st_transform(zones, 5070))) / 1e6
zones$pop_est  <- zones$pdens * zones$area_km2

zattr  <- st_drop_geometry(zones)   # zone grade + demographics, no geometry

# ---- lookups ----------------------------------------------------------------
sectors    <- c("Rail" = "Rail", "Road" = "Road", "Aviation" = "Aviation")
# NB: names are UI labels, values must match the data's pollutant levels
# exactly -- the data spells it "PM25", so a "PM2.5" value selects nothing.
# The subscript is the Unicode character U+2082, not markup: selectInput does
# not render HTML in its choice labels, and a <sub> tag would show literally.
pollutants <- c("Noise (LAeq,24)"         = "Noise",
                "Nitrogen Oxide (NO)"     = "NO",
                "Nitrogen Dioxide (NO₂)" = "NO2",
                "Fine Particulate Matter (PM2.5)" = "PM25")
poll_lab   <- c(Noise = "noise", NO = "NO", NO2 = "NO₂", PM25 = "PM2.5")
# Segment labels for Exposure. Source and Exposure sit side by side as a
# T-chart, so each column is about half the sidebar and there is no room for
# the spelled-out "Fine
# Particulate Matter (PM2.5)". The segments carry the short forms the map and
# chart headers already use, and the spelled-out names from `pollutants` ride
# along as title tooltips, so the abbreviation is not the only place a reader
# can learn what NO2 is. Same name-is-label, value-is-data convention as
# `pollutants`: the data spells it PM25, so the value must too.
poll_seg   <- c("Noise" = "Noise", "NO" = "NO",
                "NO₂"  = "NO2",   "PM2.5" = "PM25")

grade_levels <- c("A", "B", "C", "D")
# canonical HOLC grade colors (green/blue/yellow/red)
grade_cols <- c(A = "#76a865", B = "#7cb5bd", C = "#ffff00", D = "#d9838d")

# A zero is "no measurable exposure", not "low exposure", and it is common:
# 63-70% of zones for aviation, 22% for rail noise, 8-10% for rail
# NO/NO2/PM2.5. Since logxp is log1p(xp), a zero maps to logxp = 0 and would sit
# at the bottom of a continuous ramp -- visually identical to a genuinely low
# nonzero value. So zeros get their own color and the ramp is fitted to nonzero
# values only. For aviation this is the difference between a readable map and a
# misleading one: most of the country simply has no airport near it.
zero_col <- "#b8b8b8"
na_col   <- "#f0f0f0"

# equity chart: group exposure by HOLC grade (default) or a demographic quintile
equity_by <- c("HOLC grade"     = "grade",
               "Median income"  = "inc",
               "% Hispanic"     = "pct_hisp",
               "% Black"        = "pct_blk")
# demographic quintile ramp (low -> high value, yellow -> dark red)
q_ramp <- colorRampPalette(c("#ffffcc", "#800026"))

# Basemap. Positron stays the default: a near-white background is what lets a
# choropleth carry the message, and it is the right base for reading exposure.
# Satellite is offered for orientation -- it answers "what is actually there?"
# (the rail yard, the interchange, the airport) in a way an abstract base
# cannot, which matters for a map whose subject is proximity to infrastructure.
#
# Esri.WorldImagery is used because it needs no API key and no account. OSM
# itself has no satellite tiles -- OSM is vector street data, and every raster
# "OSM satellite" is a third-party imagery layer, so the choice is really which
# imagery host to point at. USGS.USImagery (public domain, NAIP) is the other
# keyless option and is US-only, which would suffice here; Esri's is sharper at
# city zoom and is the more conventional pick.
#
# Imagery carries no place names, so satellite pairs it with a labels-only
# overlay. Both are tile layers, so the labels sit above the imagery and below
# the polygons without any pane juggling.
basemaps <- c("Light" = "light", "Satellite" = "sat")
tiles_for <- function(map, which) {
  if (identical(which, "sat"))
    map |>
      addProviderTiles(providers$Esri.WorldImagery, group = "basemap") |>
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "basemap")
  else
    map |> addProviderTiles(providers$CartoDB.Positron, group = "basemap")
}

# The basemap switch lives on the map, beside the layer selectors, rather than
# in the sidebar: it changes what the map looks like, so it belongs where the
# reader is looking when they want it.
#
# It cannot be a second `addLayersControl`. Leaflet's binding allows exactly
# one ("Only allow one layers control at a time" -- it calls removeLayersControl
# before adding), and that one is already spent on the three mutually exclusive
# fills, which cannot become checkboxes without letting two choropleths stack.
# So this is hand-rolled and pushed through `addControl`, the same route
# `exposure_key()` takes for the same class of reason.
#
# It writes `input$basemap` directly, so every existing reader of that input
# keeps working untouched: the tile-swap observer, the polygon border colors,
# and the infrastructure line styling.
#
# Re-added on every draw rather than once at init, because the draw observer
# calls clearControls() and would otherwise wipe it. That is also what keeps
# the active button in step with the input.
# One builder for every on-map segmented control, so a second control matches
# the first by construction rather than by copied CSS that can drift. The input
# id rides on the wrapper as a data attribute and the JS reads it from there, so
# adding a control is this call plus an addControl -- no new JS.
# Every control in the top-right stack carries a header, including leaflet's own
# layers control, which gets one through CSS since it has no slot for it. Three
# boxes with no captions read as an undifferentiated pile of buttons.
map_seg_control <- function(input_id, choices, sel, default, titles = NULL,
                            header = NULL) {
  if (is.null(sel)) sel <- default
  tags$div(
    class = "mapseg", `data-input` = input_id,
    if (!is.null(header)) tags$div(class = "mapseg-head", header),
    tags$div(
      class = "mapseg-btns",
      lapply(seq_along(choices), function(i) {
        v <- unname(choices)[i]
        tags$button(type = "button", `data-val` = v,
                    title = if (!is.null(titles)) titles[i],
                    class = paste("mapseg-btn", if (identical(v, sel)) "active"),
                    names(choices)[i])
      })
    )
  )
}

basemap_control <- function(sel) map_seg_control("basemap", basemaps, sel, "light",
                                                 header = "Background")

# Color scale. On the map rather than in the sidebar because it changes what the
# colors on the map MEAN, and the same argument that put the basemap switch here
# applies with more force: a reader comparing two cities is looking at the map,
# not the sidebar. Same builder as the basemap switch, so the two read as one
# family of map controls. The header is what names the quantity, since "This
# City" and "Full Sample" name only the population; the buttons carry titles
# because the header cannot hold the whole tradeoff.
scale_choices <- c("This City" = "city", "Full Sample" = "national")
scale_control <- function(sel) map_seg_control(
  "scale", scale_choices, sel, "city", header = "Exposure Scaling",
  titles = c("Fit the color ramp to this city's own range: shows the spread inside the city, colors not comparable between cities",
             "Fit the color ramp to the national range for this source and exposure: colors comparable between cities"))

# Infrastructure overlay. One group per sector, named for what is drawn rather
# than for the sector, because that is what the reader sees on the map. The
# group shown by default follows the Emission Source selector: picking Rail and
# seeing rail track is the whole point of the layer, and stacking all three at
# once over a choropleth is unreadable.
infra_groups <- c(Road = "Highways", Rail = "Rail lines", Aviation = "Airports")

# What each drawn thing IS, for the hover labels.
#
# Highway class: the overlay already draws class 1 heavier than class 2, and the
# app treats that distinction as meaningful everywhere else, so the label says
# which one rather than leaving the reader to infer it from line weight.
road_class <- c("1" = "Interstate", "2" = "Freeway or expressway")

# Rail class from the FRA NET code, in a non-rail reader's vocabulary. The FRA
# splits industrial lead into major (I) and minor (O); both collapse to
# "Industrial track" here, because this is a portfolio piece and the major/minor
# distinction is jargon that buys a general reader nothing -- together they are
# 39% of segments, so it is the label most readers hit after yard track. What
# the label has to carry is that this is not mainline, which it does. The FRA
# definitions themselves are in prep_data.R and the data dictionary
# (doi 10.21949/1528950); this table is the display wording, not the source
# semantics. prep_data.R drops the four non-operational codes, so only these six
# can reach the map -- the fallback exists so an unmatched code renders as plain
# "Rail track" rather than NA at a reader.
rail_class <- c(M = "Mainline track",
                O = "Industrial track",
                I = "Industrial track",
                Y = "Yard track",
                S = "Passing siding",
                F = "Rail ferry connection")

# Owner reporting marks, expanded only where the mark could be verified against
# the AAR marks list AND the segments' own cities corroborate it. Anything not
# listed shows as the bare mark, which is what the source carries. Deliberately
# absent: PVTX (a private-owner placeholder, not a railroad), SIRR (the marks
# list reads Southern Industrial, but every segment here is Staten Island and
# north Jersey), EJR and NIRC (unverified; Metra's own mark is METX).
rail_owner <- c(
  UP   = "Union Pacific",             CSXT = "CSX Transportation",
  NS   = "Norfolk Southern",          BNSF = "BNSF Railway",
  CSAO = "Conrail Shared Assets",     CN   = "Canadian National",
  AMTK = "Amtrak",                    PHL  = "Pacific Harbor Line",
  CPRS = "Canadian Pacific",          IHB  = "Indiana Harbor Belt",
  MBTA = "Massachusetts Bay Transportation Authority",
  TMBL = "Tacoma Municipal Belt Line", URR = "Union Railroad",
  SCAX = "Southern California Regional Rail Authority",
  KCS  = "Kansas City Southern",      NJT  = "New Jersey Transit",
  PW   = "Providence & Worcester",    TRRA = "Terminal Railroad Association",
  NYSW = "New York, Susquehanna & Western",
  LI   = "Long Island Rail Road",     MNCW = "Metro-North Commuter Railroad")

# One hover-label format for the whole overlay, matching the zone and tract
# labels: a bold identity line, then "Label: <b>value</b>" rows. Before this the
# three groups each had their own run-on middot construction and only airports
# were bolded, so they read as three formats rather than one set. A row whose
# value is missing drops out entirely rather than showing a blank.
lab_row <- function(label, value, suffix = "") {
  v <- as.character(value)
  ifelse(!is.na(v) & nzchar(v),
         sprintf("<br/>%s: <b>%s</b>%s", label, v, suffix), "")
}

# Assemble one group's labels. The nrow guard is load-bearing: paste0 treats a
# zero-length vector as "" and returns a length-1 string, so a city with nothing
# in a group would get a single label assigned to a zero-row frame and the draw
# observer would die. Ten of the 221 cities carry no highways and two carry no
# rail, so this is a live path rather than a defensive one.
infra_lab <- function(x, ...) if (nrow(x)) lapply(paste0(...), htmltools::HTML) else list()

# The three mutually exclusive fill layers, named once. Every clearGroup,
# hideGroup and the layers control read from here, so a relabel is one edit and
# cannot leave a stale group name behind that silently stops clearing a layer.
# Labels are unit-first ("HOLC", "Tracts") with the variable in parentheses, so
# the control groups the two HOLC fills together and the reader picks a unit
# before picking what colors it.
fill_groups <- c(grade     = "HOLC (grade)",
                 zone_exp  = "HOLC (exposure)",
                 tract_exp = "Tracts (exposure)")

# Segmented control for Source and Exposure, the same visual language as the view
# switch: a sunken track with the active option filled blue and its label
# reversed to white. Three and four options is few enough that a dropdown only
# hides the choice set behind a click.
#
# Laid out as a T-chart: Source and Exposure side by side as two columns, each
# with its options stacked under its own heading. Reading down a column is
# what compares the options within one axis, and the two axes sit level with
# each other rather than one above the other.
#
# radioButtons rather than a hand-rolled control: it ships the keyboard
# behavior a radio group is supposed to have (arrow keys move within the
# group, the whole group is one tab stop) and Shiny already has an input
# binding for it. The styling hides the native circle and paints the text
# instead, so the real input is still there to be focused and read out.
#
# The served markup is the LEGACY form, not BS5's .form-check, because this
# `ui` is a static object built before any theme is in scope. So it is
# div.radio > label > (input + span), the label WRAPPING the input rather than
# following it. The CSS keys on `input + span` accordingly. Check the served
# markup with an HTTP fetch before touching those rules; what renderTags gives
# outside a session is not what the browser receives.
seg_radio <- function(id, label, choices, selected, titles = NULL) {
  labs <- if (is.null(titles)) as.list(names(choices))
          else mapply(function(n, t) span(title = t, n), names(choices), titles,
                      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(
    class = "seg-input",
    radioButtons(id, label, choiceNames = labs,
                 choiceValues = unname(choices), selected = selected)
  )
}

# Colors sit off both the exposure ramp (YlOrRd) and the HOLC grade palette
# (green/blue/yellow/red) so a line is never mistaken for a fill: near-black for
# road, violet for rail, cyan for airports (the last two matching the project's
# static city figures in scripts/analyze/maps.R).
#
# Every line is drawn twice, a wider halo beneath a thinner stroke. A single
# dark line disappears into satellite imagery and a single light one disappears
# into the near-white basemap; the halo makes one style legible on both, which
# matters because the basemap is a control the reader is invited to flip.
infra_style <- list(
  light = list(road = c(main = "#1a1a1a", halo = "#ffffff"),
               rail = c(main = "#6a3d9a", halo = "#ffffff")),
  sat   = list(road = c(main = "#ffffff", halo = "#000000"),
               rail = c(main = "#d0a3ff", halo = "#000000"))
)
avi_col <- "#00e5ff"

# city selector choices: "Birmingham, AL" -> "Birmingham, AL". The map is
# always a single city; there is no all-cities view.
city_choices <- setNames(cities$place, cities$place)

# The exposure key: a continuous ramp with the "no measurable exposure" swatch
# nested underneath it in the same box, above a rule that marks it as a separate
# reading rather than another step on the scale.
#
# The bar is a CSS gradient sampled from the palette, drawn left-to-right with
# the ticks under it. Horizontal rather than leaflet's usual vertical form
# because vertical made the box tall and mostly empty; see the .lg CSS. Ticks
# are positioned as percentages of the domain rather than spaced evenly, so a
# domain whose pretty() breaks are not evenly spread still labels honestly.
#
# p is NULL when the city has no nonzero values at all, in which case there is
# no ramp to draw and the key is the zero swatch alone -- which is the whole
# story for that view, and still needs saying.
exposure_key <- function(p, rng, has_zero, national = FALSE) {
  zero_row <- if (has_zero)
    div(class = "lg-zero",
        span(class = "lg-sw", style = sprintf("background:%s;", zero_col)),
        "No measurable exposure")

  if (is.null(p) || is.null(rng) || !all(is.finite(rng)))
    return(tags$div(class = "lg", div(class = "lg-title", "Exposure"),
                    zero_row))
  stops <- if (rng[1] == rng[2]) rep(rng[1], 2) else seq(rng[1], rng[2], length.out = 12)
  grad  <- paste(p(stops), collapse = ", ")
  if (rng[1] == rng[2]) {
    # One distinct value in the whole city (Fall River rail noise). A scale of
    # repeated identical labels is not a scale; one centred tick is the truth.
    ticks <- rng[1]
  } else {
    ticks <- pretty(rng, n = 3)
    ticks <- ticks[ticks >= rng[1] & ticks <= rng[2]]
    if (length(ticks) < 2) ticks <- rng
    # pretty() can return six on a narrow domain, which crowds a box this wide.
    if (length(ticks) > 4)
      ticks <- ticks[unique(round(seq(1, length(ticks), length.out = 4)))]
  }
  pct <- if (rng[2] > rng[1]) 100 * (ticks - rng[1]) / (rng[2] - rng[1])
         else rep(50, length(ticks))
  # Enough decimals that no two labels read the same. A city whose zones all sit
  # within a fraction of a log unit otherwise prints "1.0" three times over, and
  # the ticks stop being a scale while still taking the space of one.
  dig <- 1
  while (dig < 4 && anyDuplicated(sprintf(paste0("%.", dig, "f"), ticks))) dig <- dig + 1
  tick_lab <- sprintf(paste0("%.", dig, "f"), ticks)
  # A label centred on a tick at 0% or 100% hangs half of itself off the box, so
  # the two ends are aligned inward instead of centred.
  shift <- ifelse(pct <= 2, "0", ifelse(pct >= 98, "-100%", "-50%"))

  tags$div(
    class = "lg",
    div(class = "lg-title", "Exposure"),
    # The basis line is the whole reason the two scales can coexist: the same
    # ramp means two different things depending on it, so the key has to say
    # which. Under the national scale it also flags the clipped ends, because
    # the top color is "at or past the 98th percentile", not "the maximum".
    # One line of three clauses rather than three stacked lines -- it was the
    # tallest part of the key and it is a caption, not a paragraph.
    div(class = "lg-sub",
        sprintf("log(xp+1) · %s · zones + tracts",
                if (national) "national, 2nd-98th pct" else "within city")),
    div(class = "lg-bar",
        style = sprintf("background:linear-gradient(to right, %s);", grad)),
    div(class = "lg-ticks",
        lapply(seq_along(ticks), function(i)
          span(class = "lg-tick",
               style = sprintf("left:%.1f%%; transform:translateX(%s);",
                               pct[i], shift[i]),
               tick_lab[i]))),
    zero_row
  )
}

# 78 -> "78th", for the city's percentile in the national spread. The 11-13
# exception is why this is not a switch on the last digit alone.
ordinal <- function(p) {
  n   <- round(p)
  sfx <- if (n %% 100 %in% 11:13) "th"
         else switch(as.character(n %% 10),
                     "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, sfx)
}

# An external link as a string, for lines that continue past it. A tag list puts
# every child on its own line and the newline renders as a space, so an a() with
# ", publisher." as a sibling child shows a floating space before the comma; the
# whole line has to be one HTML string for the punctuation to sit tight.
ext_link <- function(href, label) {
  sprintf("<a href='%s' target='_blank' rel='noopener'>%s</a>", href, label)
}

# Anchor finding: how often each sector ranks first for steepness of gradient,
# across the pollutant x disparity-measure comparisons in
# sector_stratification.R. Sorted here so the value box shows the full standing
# rather than only the winner -- the gap is the point (rail well clear, and
# aviation, not road, in second). rank_n exceeds rank_cmp because the upstream
# rank() uses ties.method = "min", so a tied comparison awards two first places.
rank_tab <- as.data.frame(rank)
rank_tab <- rank_tab[order(-rank_tab$pct_first), ]
rank_n   <- sum(rank_tab$n_first_place)

# ---- UI ---------------------------------------------------------------------
ui <- page_sidebar(
  # A bare string title renders as one unframed line that says nothing about
  # what is on screen. This is a full banner instead: what the project asks,
  # then how to read the map. bslib drops the title tag straight into the
  # navbar's container-fluid (there is no .navbar-brand wrapper when the title
  # is a tag), so the banner is styled through .app-brand below. The city count
  # reads from the bundle so the min_zones filter can move without stranding a
  # hardcoded number.
  title = div(
    class = "app-brand",
    # Title and scope line stay put; only the explanatory paragraphs collapse,
    # so a minimized banner still says what the app is. Bootstrap 5's collapse
    # is used directly rather than an accordion: bslib ships the JS already, and
    # an accordion would impose its own frame and border on a navbar block.
    div(class = "brand-head",
        # Names the two subjects with no verb between them, which is what lets
        # the title pair them without claiming a mechanism; the descriptive-only
        # hedge stays in the method block. "Emissions" covers noise because
        # noise is one of the choices under the Exposure selector, alongside the
        # three air pollutants; "Exposure" is the other selector's label, so the
        # title's last word is the app's own vocabulary too.
        div(class = "app-brand-title",
            "Redlining & Disparities in Transportation Emissions Exposure"),
        # Right-aligned controls: the view switch, then About. The switch is not
        # authored here -- #brand-views is an empty slot, and the script further
        # down moves navset_underline's own nav list into it once Shiny has
        # bound it. Relocating the real element keeps the tab markup, the
        # shown.bs.tab resize fix, and the conditionalPanels reading input.view
        # all working untouched.
        div(class = "brand-actions",
            div(id = "brand-views"),
            tags$button(
              class = "btn btn-sm collapse-toggle", type = "button",
              `data-bs-toggle` = "collapse", `data-bs-target` = "#brand-body",
              `aria-expanded` = "true", `aria-controls` = "brand-body",
              span(class = "ct-label", "About"), span(class = "ct-chev", "▲")))),
    # The banner holds only what is true of the whole app: what the grades are,
    # what the app is computed on, and where the data came from. Anything that
    # explains one view's figures or controls belongs in that view's sidebar,
    # which is where a reader is already looking when they need it.
    div(
      id = "brand-body", class = "collapse show",
      # Written for a reader who has never heard of redlining, so the first
      # paragraph is the history in plain terms: what the grades were, that the
      # scale runs best to hazardous, and where the word comes from. The
      # Mapping Inequality link is not here -- the sourcing rows below carry it,
      # and citing it twice in one banner is what this rewrite removed.
      p(class = "app-brand-lede",
        "Home foreclosures surged during the Great Depression, prompting the",
        "federal government to establish the temporary Home Owners' Loan",
        "Corporation (HOLC) to buy defaulted mortgages and reissue them at",
        "more affordable rates. For more than 200 cities where HOLC",
        "bought assets, the agency rated neighborhoods on a four-letter grading",
        "system of A (best) through D (hazardous) to describe mortgage lending",
        "risk. The grades tracked who lived there: neighborhoods with Black,",
        "immigrant, and low-income residents were routinely marked D, colored",
        "red on the map, and largely shut out of federally backed mortgage",
        "lending. This is the origin of the term ‘redlining’."),
      # Second paragraph answers the question a newcomer actually has, which is
      # why a mortgage map would predict a highway, and then carries it through
      # to the present day. Both directions of the historical link are stated
      # because both are documented and they run opposite ways: appraisers
      # logged rail and industry already on the ground as detrimental
      # influences, and the interstates came after. Two claims deliberately not
      # made -- that rail shaped the grades (what is documented is the appraisal
      # practice, not the counterfactual) and that highway planners consulted
      # the HOLC maps (no source for that). Barajas 2021 is the citation on the
      # highway clause because it makes exactly the supportable claim: redlining
      # depressed land values, which compounded racially motivated decisions to
      # site freeways in Black neighborhoods. The citation sits mid-sentence, so
      # the paragraph is one HTML string for the reason ext_link() exists.
      #
      # The present-day findings were their own paragraph and are folded in
      # here: they are the same thread (how transportation enters, and what that
      # left behind), and splitting them left the banner with two short
      # paragraphs where one continuous one reads better.
      p(class = "app-brand-lede", HTML(paste0(
        "Transportation is part of this story. Appraisers marked down ",
        "neighborhoods located near emission sources like rail yards and ",
        "factories, so the placement of existing infrastructure fed into the ",
        "assignment of grades. In the 1950s, the construction of interstate ",
        "highways ran disproportionately through areas that had been graded ",
        "down (",
        ext_link("https://doi.org/10.1016/bs.atpp.2021.06.007",
                 "Barajas 2021"),
        "). Ninety years later the grades still track environmental burden: ",
        "formerly D-graded neighborhoods have more air pollution (",
        ext_link("https://doi.org/10.1021/acs.estlett.1c01012",
                 "Lane et al. 2022"),
        "), more transportation noise (",
        ext_link("https://doi.org/10.1038/s41370-024-00682-x",
                 "Collins and Grineski 2025"),
        "), fewer trees (",
        ext_link("https://doi.org/10.1016/j.socscimed.2019.112758",
                 "Namin et al. 2020"),
        "), and more urban heat (",
        ext_link("https://doi.org/10.3390/su13031511",
                 "Saverino et al. 2021"),
        "), a pattern found nearly everywhere it has been tested (",
        ext_link("https://doi.org/10.1007/s11524-022-00665-z",
                 "Swope et al. 2022"),
        ")."))),
      # Third paragraph is now what the dashboard itself is: the sample it runs
      # on and what it does not claim. The sample line is the app's own frame,
      # read from the bundle rather than typed, so a rebuild cannot strand it --
      # this is also the only place the population is stated now that the Data
      # sidebar's basis paragraph and the chart's method block are gone.
      #
      # The hedge is two sentences. Descriptive, which is the whole claim about
      # the figures: the app fits nothing. And the weaker reading of the grades
      # themselves -- a marker of where mid-century disinvestment concentrated
      # rather than a demonstrated cause of what a place is like now, which is
      # what the literature supports (the HOLC did not use these maps in its own
      # refinancing, the appraisal logic spread through FHA underwriting, and
      # the one boundary-discontinuity design in refs/ reports small effects).
      #
      # The close hands the direction of the gradient to the panels rather than
      # promising one: aviation noise is flat across grades and aviation NO
      # peaks at B, so a banner asserting a direction would be contradicted on
      # screen by the reader's second click.
      p(class = "app-brand-lede", HTML(sprintf(paste0(
        "This dashboard maps present-day transportation emissions onto those ",
        "grades: %s HOLC-graded neighborhoods across %d ",
        "cities in %d states, with three emission sources and four exposure ",
        "measures. The figures are descriptive. The grading system is best read ",
        "as a marker of where mid-century disinvestment concentrated rather ",
        "than as a demonstrated cause of what neighborhoods are like today."),
        format(nrow(frame), big.mark = ","), frame_cities, frame_states))),
      # Sourcing only, one source per line. Each names the quantity the app
      # computes, then its publisher, and links out. Every input is public, so
      # a reader who wants the unit, the vintage detail, or the methodology is
      # better served by the source's own page than by a longer line here.
      div(class = "app-brand-fine",
        span(class = "fine-h", "Data"),
        tags$ul(
          class = "fine-src",
          tags$li(HTML(paste0(
            "<b>Air pollutants.</b> NEMO, from the US EPA ",
            ext_link(paste0("https://www.epa.gov/air-emissions-inventories/",
                            "national-emissions-inventory-nei"),
                     "National Emissions Inventory"), " 2017."))),
          tags$li(HTML(paste0(
            "<b>Noise.</b> ",
            ext_link(paste0("https://www.bts.gov/geospatial/",
                            "national-transportation-noise-map"),
                     "National Transportation Noise Map"),
            ", US Bureau of Transportation Statistics."))),
          tags$li(HTML(paste0(
            "<b>Redlining maps.</b> ",
            ext_link("https://dsl.richmond.edu/panorama/redlining/",
                     "Mapping Inequality"),
            ", University of Richmond."))),
          tags$li(HTML(paste0(
            "<b>Sociodemographics.</b> ",
            ext_link("https://www.census.gov/programs-surveys/acs",
                     "American Community Survey"),
            " 2019, US Census Bureau."))),
          tags$li(HTML(paste0(
            "<b>Infrastructure overlay.</b> ",
            ext_link("https://www.bts.gov/ntad",
                     "National Transportation Atlas Database"),
            ", US Bureau of Transportation Statistics."))))),
      # Attribution and status, under the sourcing rows and dimmer than them.
      # Says who the work belongs to and that it is not a published result;
      # matches the same line in the phxmob app.
      div(class = "app-brand-source",
          HTML(paste0("HUM.Lab, University of Arizona.<br/>",
                      "Work sample, not a published result.")))
    )
  ),
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 330,
    # City is a Map control and nothing else. The Data view is about the
    # national frame, and a city selector sitting above a national figure
    # invites the reader to take that figure for the selected city's. Hidden
    # rather than removed: it stays in the DOM, so the map's observers keep
    # reading it while the reader is on the other tab.
    conditionalPanel(
      condition = "input.view === 'map' || input.view === undefined",
      selectizeInput(
        "city", "City", choices = NULL,  # populated server-side (searchable)
        # count comes from the bundle so the filter in prep_data.R can change
        # without stranding a hardcoded number here
        options = list(placeholder = sprintf("Type to search %d cities...",
                                             nrow(cities)),
                       maxOptions = 300)
      )
    ),
    div(class = "seg-pair",
        seg_radio("sector", "Source", sectors, "Rail"),
        seg_radio("poll", "Exposure", poll_seg, "Noise",
                  titles = names(pollutants))),
    # Source and Exposure stay visible in both views: the first tints the chart's
    # matching panel without driving it, and the second sets which pollutant
    # the whole national view is about. Everything below this point belongs to
    # one view or the other.
    #
    # The undefined arm matters throughout: conditionalPanel is evaluated
    # client-side, and on the very first paint input.view may not have arrived
    # yet. Without it the map's own controls and figures would be hidden on the
    # view they belong to. Hidden inputs stay in the DOM, so the draw observers
    # keep reading the opacity while the reader is on the other tab.
    #
    # The basemap switch is NOT here. It sits on the map beside the layer
    # selectors (see basemap_control), so `input$basemap` has no sidebar widget
    # behind it and is NULL until the reader touches it -- every read of it
    # treats NULL as "light", which is what renderLeaflet draws.
    conditionalPanel(
      condition = "input.view === 'map' || input.view === undefined",
      # The Color scale switch is NOT here either -- it sits on the map with the
      # basemap switch (see scale_control), so `input$scale` has no sidebar
      # widget behind it and is NULL until the reader touches it. scale_national()
      # treats NULL as "city", which is what the map opens on.
      #
      # Satellite is unreadable under an opaque choropleth, so the fill has to be
      # dimmable for the imagery to be worth switching to at all.
      sliderInput("fillop", "Fill opacity", min = 0.2, max = 1, value = 0.8,
                  step = 0.05, ticks = FALSE)
    ),
    # Group by acts on the chart alone, so it sits with the view that has the
    # chart. It was kept out of the sidebar while the sidebar held map controls
    # on every view, where it read as a fourth one; now that the sidebar is
    # split, the Data sidebar has no map controls for it to be confused with.
    conditionalPanel(
      condition = "input.view === 'national'",
      selectInput("eby", "Group by", choices = equity_by, selected = "grade")
    ),
    hr(),
    # Per-city figures live in the sidebar, beside the selectors that drive
    # them, which leaves the Map view itself as nothing but the map. Stacked
    # rather than in a row: at 330px there is no width for three across.
    conditionalPanel(
      condition = "input.view === 'map' || input.view === undefined",
      div(class = "side-stats",
          # No title: five blocks, A-D in the legend's grade colors plus the
          # city total in grey. The grade letters caption the blocks, so a
          # header would only repeat what the numbers already read as.
          card(class = "gcount-card", uiOutput("vb_n_grades")),
          card(class = "pres-card", uiOutput("vb_city_presence")),
          card(class = "pres-card", uiOutput("vb_city_rank"))),
      hr(),
      # Reading notes for the map, moved out of the banner: they explain this
      # view's figures and nothing else.
      helpText(
        HTML(paste(
          "Each polygon is shaded by modeled exposure on a log scale, darker",
          "meaning higher. The <b>Exposure Scaling</b> switch on the map chooses",
          "what the shading is relative to.",
          "<b>This City</b> fits the ramp to the selected city's",
          "own range, which shows the spread inside the city but makes colors",
          "meaningless between cities. <b>Full Sample</b> fits it to the national",
          "range for the selected source and exposure, so two cities can be",
          "compared directly, at the cost of a low-exposure city reading as",
          "almost uniformly pale, which is the honest answer at that scale. The",
          "national ends are clipped at the 2nd and 98th percentiles, so the",
          "darkest shade means at or past the top of the scale. Areas with no",
          "measurable exposure get their own shade, because an absence is not a",
          "low value."))
      ),
      helpText(
        HTML("Three fill views in the layer control (top-right of the map), one",
             "at a time. <b>HOLC (grade)</b> fills the graded neighborhoods by",
             "grade. <b>HOLC (exposure)</b> fills those same neighborhoods by",
             "exposure. <b>Tracts (exposure)</b> covers the whole city on the",
             "same color scale, so you can see where the graded zones sit in",
             "the city's own exposure surface rather than only how they compare",
             "with each other.")
      ),
      helpText(
        "Hover any polygon for its exposure and demographics.",
        "The same control turns the highway, rail, and airport layers on and",
        "off; the one matching the source you picked is drawn by default and",
        "follows the selector.",
        "Switch the basemap to Satellite, top-right above the layer control,",
        "and turn the fill opacity down to see the infrastructure on the",
        "ground beneath."
      )
    ),
    conditionalPanel(
      condition = "input.view === 'national'",
      helpText(
        HTML("Each sector is indexed to the median of its own leftmost group, so",
             "every panel starts at 1.0 and the last group's height is that",
             "sector's disparity ratio. Levels are not comparable between",
             "panels; the map carries level.")
      )
    )
  ),
  # Banner: the navbar and its container are flex rows by default, which would
  # hold the banner's blocks on one line, so both go to block flow. The bottom
  # rule runs the four HOLC grade colors, tying the banner to the legend.
  # Below that, compact the in-card "Group by" control: shiny's selectInput
  # ships a form-group bottom margin and a full-size label, both of which
  # inflate the card header out of proportion to the title beside it.
  tags$head(tags$style(HTML(
    ".navbar, .navbar > .container-fluid { display: block; }
     .navbar { padding: 1.6rem 0 1.3rem; background-color: #f6f6f4;
               border-bottom: 3px solid transparent;
               border-image: linear-gradient(to right, #76a865, #7cb5bd,
                             #ffff00, #d9838d) 1; }
     /* Long enough to wrap on a narrow window, so it is balanced rather than
        left to strand a word, and it takes the row's free space while the
        controls beside it keep their intrinsic width. */
     .app-brand-title { font-size: 1.8rem; font-weight: 700;
                        line-height: 1.15; letter-spacing: -.02em;
                        text-wrap: balance; margin: 0;
                        flex: 1 1 auto; min-width: 0; }
     .app-brand-lede { font-size: .9rem; line-height: 1.5; opacity: .85;
                       max-width: 95ch; margin-bottom: .4rem; }
     .app-brand-lede:last-child { margin-bottom: 0; }
     /* Sourcing and caveats: app-wide, so they sit here rather than in one
        view's sidebar, at a weight that keeps them out of the lede's way. */
     /* Dimmed, but less than a pure caveat block would be: the sourcing lines
        carry links now, and a link at .6 opacity does not read as clickable. */
     .app-brand-fine { font-size: .75rem; line-height: 1.45; opacity: .72;
                       max-width: 95ch; margin: .6rem 0 0; }
     .fine-h { font-weight: 700; letter-spacing: .06em; font-size: .68rem;
               text-transform: uppercase; }
     /* One source per row. The lines are short enough now that two columns
        would only make the reader's eye jump; the markers sit inside the text
        block rather than hanging outside it, so the list keeps the lede's
        left edge. */
     .fine-src { margin: .2rem 0 0; padding-left: 1.1rem; }
     .fine-src > li { margin-bottom: .15rem; }
     .fine-src b { font-weight: 600; opacity: .9; }
     .fine-src a { text-underline-offset: .15em; }
     /* Carries no links, so it can sit at the .5 opacity the sourcing rows
        cannot. */
     .app-brand-source { font-size: .72rem; line-height: 1.4; opacity: .5;
                         max-width: 95ch; margin: .5rem 0 0; }
     @media (max-width: 700px) {
       .app-brand-title { font-size: 1.5rem; }
       .navbar { padding: 1.1rem 0 .9rem; }
     }
     .da-note { margin-top: .45rem; font-size: .7rem; line-height: 1.35;
                opacity: .65; }
     /* Per-city figures in the sidebar. Stacked with a gap of their own, and
        the cards give up their fill behaviour: in a sidebar they size to their
        contents rather than stretching to a row's height. */
     .side-stats { display: flex; flex-direction: column; gap: .5rem; }
     .side-stats > .card { flex: 0 0 auto; }
     /* Collapse toggles, shared by the banner and the statistics region so both
        read as the same control. The chevron is rotated by CSS off Bootstrap's
        own aria-expanded state rather than swapped in JS, which keeps the arrow
        correct even when the panel is toggled from somewhere else. */
     .brand-head { display: flex; align-items: center;
                   justify-content: space-between; gap: 1.4rem;
                   margin-bottom: .85rem; }
     .brand-actions { display: flex; align-items: center; gap: .85rem;
                      flex: 0 0 auto; }
     .collapse-toggle { border: 1px solid rgba(0,0,0,.18); background: #fff;
                        border-radius: 999px; padding: .16rem .6rem;
                        font-size: .74rem; font-weight: 600; color: #33383d;
                        display: inline-flex; align-items: center; gap: .35rem;
                        flex: 0 0 auto; }
     .collapse-toggle:hover { background: #f0f0ee; }
     .ct-chev { font-size: .58rem; transition: transform .18s ease;
                display: inline-block; }
     .collapse-toggle[aria-expanded='false'] .ct-chev { transform: rotate(180deg); }
     /* Grade counts as stacked horizontal bars, no title above them. Bars are
        scaled to the city total rather than to the largest grade, so every bar
        reads as its share of the whole city and the grey total bar is the
        full-width reference the four sit inside. Vertical stacking puts the
        grade letters in a column the eye can run down in legend order. */
     .gcount-card { padding: .6rem .7rem; }
     .gcount { display: flex; flex-direction: column; gap: .3rem;
               height: 100%; justify-content: center; }
     .grow { display: flex; align-items: center; gap: .5rem; }
     .grow-lab { width: 2rem; flex: 0 0 auto; font-size: .74rem;
                 font-weight: 700; letter-spacing: .03em; opacity: .8; }
     .grow-track { flex: 1 1 auto; height: 1.2rem; border-radius: .25rem;
                   background: rgba(0,0,0,.06); }
     .grow-fill { height: 100%; border-radius: .25rem; min-width: 2px; }
     .grow-n { width: 3.4rem; flex: 0 0 auto; text-align: right;
               font-size: .95rem; font-weight: 700;
               font-variant-numeric: tabular-nums; }
     /* D-vs-A staircase: one row per grade, each bar a multiple of grade A's
        median. Capped in width rather than run to the full card, because at
        1400px the bars stop being a shape and become four long lines. The
        length axis is fixed across every sector x pollutant (grade_idx_max), so
        a flat selection draws short bars instead of being rescaled up to fill
        the same track a steep one does. */
     .da { max-width: 34rem; }
     .da-row { display: flex; align-items: center; gap: .45rem;
               margin-bottom: .3rem; }
     .da-lab { width: 1rem; flex: 0 0 auto; font-weight: 700; font-size: .8rem; }
     .da-track { flex: 1 1 auto; height: .7rem; border-radius: .35rem;
                 background: rgba(0,0,0,.08); position: relative; }
     .da-fill { height: 100%; border-radius: .35rem; }
     /* Parity tick at 1.0x, the grade-A baseline. Dark, because the staircase
        sits on a plain white card -- it was white when this was a primary-blue
        value_box, and a white tick on a white card is invisible. Grade A's own
        bar ends exactly on it, which is what makes the tick self-explaining. */
     .da-base { position: absolute; top: -.15rem; bottom: -.15rem; width: 1px;
                background: rgba(0,0,0,.35); z-index: 1; }
     .da-idx { width: 2.9rem; flex: 0 0 auto; text-align: right;
               font-size: .82rem; font-weight: 700;
               font-variant-numeric: tabular-nums; }
     .da-n { width: 4.4rem; flex: 0 0 auto; text-align: right;
             font-size: .68rem; opacity: .7;
             font-variant-numeric: tabular-nums; }
     .infra-key { margin-top: .3rem; font-size: .74rem; font-weight: 400;
                  opacity: .8; display: flex; flex-wrap: wrap; gap: .1rem .9rem;
                  align-items: center; }
     .infra-key > span { display: inline-flex; align-items: center;
                         gap: .3rem; }
     .ik-line { width: 1.1rem; height: 3px; border-radius: 1px; }
     .ik-dash { background-image: linear-gradient(to right,
                  transparent 45%, #ffffff 45%, #ffffff 62%, transparent 62%); }
     .ik-dot { width: .55rem; height: .55rem; border-radius: 50%;
               border: 1px solid #333; }
     .ik-note { opacity: .7; font-style: italic; }
     /* View toggle, drawn as a two-position switch: a sunken track holding both
        labels, with a raised thumb that slides under whichever is active. It
        sits at the banner's right edge, which is what makes it read as a
        page-level switch rather than a third map control.
        Active state is read off the anchor, a.nav-link.active, because that is
        what shiny actually serves inside a live session: buildTabset picks its
        markup from the current theme, so a BS5 page gets BS5 tabs and the li
        carries only .nav-item. (Rendering the same navset outside a session
        yields the legacy li.active form instead, which is a trap -- it is not
        what the browser receives.) Both spellings are matched anyway, so the
        switch cannot go dead again if that choice moves.
        Because the anchors do carry .nav-link, BS5's own .nav-underline rules
        engage and draw a bottom border under the label; it is cleared below,
        since an underline inside the pill is not what a switch looks like.
        The track carries its own solid colors rather than tinting whatever is
        behind it, so the contrast holds wherever the switch is placed.
        The nav is hidden until the script has moved it into #brand-views: the
        rule is on the element itself and the exception is scoped to the slot,
        so it can never flash in the position it is authored in. */
     #view { visibility: hidden; position: relative; display: inline-flex;
             gap: 0; padding: 3px; margin: 0; min-width: 10.5rem;
             background: #dfe1e0; border: 1px solid rgba(0,0,0,.14);
             border-radius: 999px;
             box-shadow: inset 0 1px 2px rgba(0,0,0,.10); }
     #brand-views #view { visibility: visible; }
     /* The thumb, filled in the theme's primary blue so the selected view is
        carried by hue and not only by position -- a white-on-grey thumb reads
        as texture, and which side it sits on is the thing that has to be
        unmissable. Two panels exactly, so its width is half the track and the
        active position is a single translate. */
     #view::before { content: ''; position: absolute; top: 3px; bottom: 3px;
                     left: 3px; width: calc(50% - 3px); border-radius: 999px;
                     background: #2780e3;
                     box-shadow: 0 1px 3px rgba(0,0,0,.28);
                     transition: transform .22s cubic-bezier(.4,0,.2,1); }
     #view:has(> li:nth-child(2) > a.active)::before,
     #view:has(> li:nth-child(2).active)::before { transform: translateX(100%); }
     /* Equal halves, or the thumb lands off the label it belongs to. */
     #view > li { flex: 1 1 0; min-width: 0; position: relative; z-index: 1; }
     #view > li > a { display: block; text-align: center; text-decoration: none;
                      padding: .26rem .95rem; border-radius: 999px;
                      border-bottom: 0; background: none;
                      font-size: .85rem; font-weight: 600; letter-spacing: .02em;
                      color: #495057; transition: color .18s ease; }
     #view > li > a:hover { color: #1d2124; border-bottom: 0; }
     #view > li > a.active,
     #view > li.active > a { color: #ffffff; border-bottom: 0; }
     /* Without :has() the thumb cannot follow the active panel, so drop it and
        paint the active label instead. Same look, no slide. */
     @supports not selector(:has(*)) {
       #view::before { display: none; }
       #view > li > a.active,
       #view > li.active > a { background: #2780e3; color: #ffffff;
                               box-shadow: 0 1px 3px rgba(0,0,0,.28); }
     }
     /* The empty .tabbable leaves the nav's line height behind once the list is
        gone; the panels start at the top of the content area instead. */
     .tabbable > .tab-content { margin-top: 0; }
     /* Segmented radio groups (Source, Exposure). No enclosing track: the buttons
        carry their own fill, offset from the sidebar rather than sitting in a
        tinted well. The sidebar is rgba(0,0,0,.05) over white, so an unselected
        button reads by being lighter than what is behind it and the selected
        one by the same blue the view switch uses. The active segment is painted
        straight off :checked, an adjacent-sibling match that needs no :has()
        and so no @supports fallback.
        Markup is div.radio > label > (input + span) -- see seg_radio(). The
        span next to the input is the segment surface; the label is only a
        click target, which is why it carries no padding of its own. */
     /* Basemap switch, drawn on the map above the layer selectors. Matches the
        layers control's own frame (white, 5px radius, the same drop shadow) so
        the two read as one stack of map controls rather than a stray widget. */
     .mapseg-wrap { background: #fff; border-radius: 5px; padding: 5px;
                    box-shadow: 0 1px 5px rgba(0,0,0,.4);
                    margin-bottom: 6px !important; }
     .mapseg-btns { display: flex; gap: 3px; }
     /* Caption, not a third button: smaller, uppercase, tracked out, no hover.
        Leaflet's layers control has no slot for a header, so it gets the same
        one through ::before -- one rule so the three boxes in the stack cannot
        drift apart. The content string is the only per-control part. */
     .mapseg-head,
     .leaflet-control-layers-list::before {
                    display: block;
                    font-size: .6rem; font-weight: 700; letter-spacing: .04em;
                    text-transform: uppercase; color: #6c757d;
                    margin: 0 0 3px 1px; line-height: 1.2; }
     /* Single quotes: this whole block is an R double-quoted string, so a
        double quote here ends it and the file stops parsing. Same trap as the
        attribute selectors in the banner CSS. */
     .leaflet-control-layers-list::before { content: 'Layers'; }
     .mapseg-btn { border: 0; border-radius: 3px; cursor: pointer;
               padding: .18rem .5rem; font-size: .72rem; font-weight: 600;
               line-height: 1.45; color: #495057; background: #eef0f2;
               transition: background-color .15s ease, color .15s ease; }
     .mapseg-btn:hover { background: #e0e4e8; color: #1d2124; }
     .mapseg-btn.active { background: #2780e3; color: #fff; }
     .mapseg-btn:focus-visible { outline: 2px solid #1a5fa8; outline-offset: 1px; }
     /* T-chart: the two axes side by side, equal columns so neither reads as
        the primary one. min-width:0 or a long label pushes its column wider
        than its half. */
     .seg-pair { display: flex; gap: .55rem; align-items: start; }
     .seg-pair > .seg-input { flex: 1 1 0; min-width: 0; }
     .seg-input { margin-bottom: .8rem; }
     .seg-input .control-label { margin-bottom: .3rem; font-weight: 600;
                                 font-size: .8rem; }
     .seg-input .shiny-options-group { display: flex; flex-direction: column;
                                       gap: 4px; }
     .seg-input .radio { position: relative; margin: 0; padding: 0;
                         min-width: 0; }
     .seg-input .radio > label { display: block; width: 100%; margin: 0;
                                 padding: 0; cursor: pointer; }
     /* Hidden but still the real radio: it keeps focus and arrow-key
        navigation, and the wrapping label makes the whole segment click it. */
     .seg-input .radio input[type='radio'] { position: absolute; opacity: 0;
                                             width: 1px; height: 1px;
                                             margin: 0; pointer-events: none; }
     .seg-input .radio input + span { display: block;
                                      padding: .28rem .8rem; border-radius: 999px;
                                      font-size: .8rem; line-height: 1.4;
                                      font-weight: 600; letter-spacing: .01em;
                                      color: #495057; background: #ffffff;
                                      transition: background-color .18s ease,
                                                  color .18s ease; }
     .seg-input .radio > label:hover input:not(:checked) + span { color: #1d2124;
                                       background: #e9edf1; }
     .seg-input .radio input:checked + span { background: #2780e3;
                                       color: #ffffff;
                                       box-shadow: 0 1px 3px rgba(0,0,0,.28); }
     /* Focus has to be drawn here: the real input is invisible, so without
        this the control is unusable by keyboard. */
     .seg-input .radio input:focus-visible + span {
       outline: 2px solid #1a5fa8; outline-offset: 1px; }
     /* Exposure key: ramp plus the nested zero swatch, one box. The swatch sits
        under a rule because it is a different kind of statement from the scale
        above it -- an absence, not a value on it -- and the rule is what stops
        it reading as the bottom step of the ramp. */
     /* Horizontal ramp. The vertical form spent 5.5rem on a 1.1rem-wide bar
        beside a tick column that held three short labels and was otherwise
        empty, and the box had to be wide enough for the caption anyway -- so
        the width was already paid for and the height was mostly air. Laid on
        its side the ramp costs about 1.4rem instead, and the caption's width
        sets the bar's length rather than fighting it. */
     .lg { max-width: 12rem; min-width: 9.5rem; }
     .lg-title { font-weight: 700; font-size: .74rem; line-height: 1.2; }
     .lg-sub { font-weight: 400; opacity: .75; font-size: .63rem;
               line-height: 1.3; margin-bottom: .3rem; }
     .lg-bar { height: .55rem; border-radius: 2px;
               border: 1px solid rgba(0,0,0,.2); }
     .lg-ticks { position: relative; height: .8rem; margin-top: .15rem; }
     .lg-tick { position: absolute; top: 0; font-size: .63rem; line-height: 1;
                white-space: nowrap; font-variant-numeric: tabular-nums; }
     .lg-zero { display: flex; align-items: center; gap: .35rem;
                margin-top: .3rem; padding-top: .3rem;
                border-top: 1px solid rgba(0,0,0,.15); font-size: .66rem; }
     .lg-sw { width: 1rem; height: .7rem; flex: 0 0 auto; border-radius: 2px;
              border: 1px solid rgba(0,0,0,.2); display: inline-block; }
     /* Per-city cards. One number each, sized between the grade-count numerals
        and a value_box headline, so the three stacked cards read as one family
        without any of them shouting. */
     .pres-card { padding: .6rem .75rem; }
     .pres { display: flex; flex-direction: column; height: 100%;
             justify-content: center; }
     .pres-lab { font-size: .74rem; font-weight: 600; opacity: .6;
                 letter-spacing: .01em; }
     .pres-val { font-size: 1.75rem; font-weight: 700; line-height: 1.1;
                 font-variant-numeric: tabular-nums; margin: .1rem 0 .15rem; }
     .pres-sub { font-size: .7rem; line-height: 1.35; opacity: .65; }
     .pres-track { position: relative; height: .55rem; border-radius: .3rem;
                   background: rgba(0,0,0,.08); margin: .35rem 0 .3rem; }
     .pres-fill { height: 100%; border-radius: .3rem; background: #7f8c8d; }
     /* The rank marker is a position on the national spread, not a quantity,
        so it is a tick on the track rather than a bar filling it. */
     .pres-mark { position: absolute; top: -.15rem; width: 3px;
                  height: .85rem; border-radius: 1px; background: #2c3e50;
                  transform: translateX(-1.5px); }"))),
  # A leaflet map initialised inside a tab panel comes back with a collapsed
  # canvas after the tab is hidden and shown again, because it sized itself
  # while the panel had no dimensions. leaflet tracks window resize by default,
  # so firing one after Bootstrap finishes the tab switch is enough; there is no
  # invalidateSize on leafletProxy to call instead.
  tags$head(tags$script(HTML(
    "$(document).on('shown.bs.tab', function() {
       window.dispatchEvent(new Event('resize'));
     });"))),
  # Move the view toggle into the banner slot beside the subtitle. The nav list
  # is relocated rather than rebuilt because everything that makes the toggle
  # work is attached to that element -- shiny's tab input binding reads it as
  # #view, and bootstrap drives the panels off each anchor's data-bs-target.
  # Both survive the move: bootstrap's tab handler is delegated on document and
  # targets panels by id, not by DOM adjacency, and jQuery keeps the binding
  # with the node. It runs on shiny:sessioninitialized so every ready-time
  # initialiser has already seen the nav where it was authored.
  tags$head(tags$script(HTML(
    "$(document).on('shiny:sessioninitialized', function() {
       $('#brand-views').append($('#view'));
     });"))),
  # Two views, one toggle. The map answers for a single city and the statistics
  # answer for the whole study frame; holding both on one screen meant every
  # number had to carry a basis note to stop a reader taking a national ratio
  # for the mapped city's. Splitting them makes the basis a property of the view
  # instead of an annotation on each figure. nav_panel values are set explicitly
  # because the sidebar's conditionalPanels match on them, and a title is a
  # label that may be reworded.
  navset_underline(
    id = "view",
    nav_panel(
      title = "Map", value = "map",
      card(
        full_screen = TRUE, height = 540,
        # The overlay key is static text rather than a leaflet legend: the
        # legend would have to appear and disappear with three independently
        # toggled groups, and a fixed key that names all three reads the same
        # whichever is on. It also carries the clip caveat, which a color legend
        # could not.
        card_header(textOutput("map_title", inline = TRUE),
                    uiOutput("map_note", inline = TRUE),
                    div(class = "infra-key",
                        span(span(class = "ik-line",
                                  style = "background:#1a1a1a;"),
                             "Interstates & freeways"),
                        span(span(class = "ik-line ik-dash",
                                  style = "background:#6a3d9a;"), "Rail track"),
                        span(span(class = "ik-dot",
                                  style = "background:#00e5ff;"), "Airports"),
                        span(class = "ik-note",
                             paste("toggle top right; clipped to the city view,",
                                   "so lines stop if you zoom out")))),
        leafletOutput("map", height = 500)
      )
    ),
    nav_panel(
      title = "Data", value = "national",
      # Full width. layout_columns with fill = FALSE keeps the box sized to its
      # contents instead of stretching to the panel.
      layout_columns(
        fill = FALSE, col_widths = 12,
        # The staircase alone: four grades, each indexed to grade A's median.
        # A D:A ratio on its own is an endpoint, and an endpoint is the wrong
        # summary for two of the three sectors: aviation NO2 ends at 0.99x but
        # peaks at grade B (1.09x), and road NO ends at 1.19x after peaking at C.
        # The shape says whether a gradient exists; the two ends do not.
        # Indexing also retires the old bars' worst property: they were
        # proportional-only, because the raw xp medians span 0.97 to 2.4e5 in
        # arbitrary model units and printing them adds a number nobody can
        # interpret. A multiple of grade A is a unit that reads on its own. Each
        # row keeps its n, because grade A is the small-N constraint everywhere
        # in this project.
        #
        # No title and no headline number. The headline was s$idx[["D"]], which
        # is exactly what the D row already prints, so it was the staircase's
        # bottom row at a larger size and carried nothing the rows did not. With
        # both gone there is no value left to box, so this is a plain card: the
        # rows caption themselves with their grade letter, index and n, and the
        # note under them names the sector and pollutant, which the old title
        # never did.
        card(uiOutput("da_panel"))
      ),
      card(
        # Group by is in this view's sidebar, with the rest of its controls.
        card_header(textOutput("chart_title", inline = TRUE)),
        plotOutput("equity", height = 340)
      )
    )
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # searchable city selector, defaulting to Chicago
  updateSelectizeInput(session, "city",
                       choices = city_choices,
                       selected = "Chicago, IL", server = TRUE)

  poll_label   <- reactive(poll_lab[[input$poll]])
  sector_label <- reactive(tolower(input$sector))

  # exposure slice for the current sector x pollutant (national)
  exp_sel <- reactive({
    exp[exp$sector == input$sector & exp$pollutant == input$poll,
        c("HOLCID", "xp", "logxp")]
  })

  # Frame-basis slice for the D:A box AND the equity chart. Both draw the
  # selected sector at the selected pollutant on the full 8,920-zone frame, so
  # they read one slice and agree by construction rather than by coincidence.
  # It reads `exp_frame` rather than `exp` because the frame is the
  # geometry-free table, which is all a plot needs.
  exp_sel_frame <- reactive({
    exp_frame[exp_frame$sector == input$sector &
              exp_frame$pollutant == input$poll, c("HOLCID", "xp")]
  })

  # NONZERO logxp values for the SELECTED CITY -- the domain of the per-city
  # scale, and the default. It reads the spread inside one city, at the cost of
  # colors meaning nothing between cities.
  #
  # The national scale (nat_domain, above) is the other half of that trade:
  # it compares cities directly, so a low-exposure city reads as uniformly
  # pale. Each scale answers its own question.
  #
  # Dropping zeros keeps the mass of no-exposure zones from compressing the ramp.
  #
  # ONE domain across both exposure layers, pooling the zone and tract values.
  # comb.R builds a zone's xp as the area-weighted mean of the tracts
  # intersecting it, so the two are the same quantity at two resolutions and a
  # shared ramp is honest. It is also the whole point of the tract layer: a
  # reader switching between them is asking where the graded zones sit in the
  # city's surface, and two independently-fitted ramps would answer a different
  # question in each layer while looking like they answered one.
  nz_vals <- reactive({
    v <- c(city_zones()$logxp, city_tracts()$logxp)
    v[!is.na(v) & v > 0]
  })

  # NULL until the reader touches the control on first paint, and "city" is the
  # default the UI declares, so every read treats NULL as "city".
  scale_national <- reactive(identical(input$scale, "national"))

  # The domain the ramp is fitted to. Per city it is that city's own nonzero
  # spread; nationally it is the trimmed range for the selected sector x
  # pollutant, which does not move as the reader changes city -- that fixedness
  # is the entire point, since it is what makes two cities' colors mean the same
  # thing. Returns NULL when there is nothing to draw.
  pal_domain <- reactive({
    if (scale_national()) {
      rng <- nat_domain[[paste(input$sector, input$poll)]]
      if (is.null(rng) || !all(is.finite(rng))) return(NULL)
      return(rng)
    }
    v <- nz_vals()
    if (!length(v)) return(NULL)
    range(v)
  })

  pal <- reactive({
    rng <- pal_domain()
    if (is.null(rng)) return(NULL)   # no ramp to draw
    # A per-city domain can collapse to a single distinct value (one zone with
    # exposure, or several with identical values), which colorNumeric rejects.
    # Pad it so those zones still get a ramp color instead of falling through to
    # the near-white "no data" fill. A national domain cannot collapse, but the
    # guard is cheap and covers both.
    if (rng[1] == rng[2]) rng <- rng + c(-1, 1) * max(abs(rng[1]) * 0.01, 1e-6)
    colorNumeric("YlOrRd", domain = rng, na.color = na_col)
  })

  # per-zone fill: zero -> grey, unmatched -> near-white, else the ramp. Returns
  # a PLAIN (unnamed) vector -- a named one serializes to a JS object and every
  # polygon renders as "[object Object]".
  # `rng` clamps the value into the palette's domain before coloring. Under the
  # per-city scale nothing is ever outside it, but the national domain is
  # trimmed at the 2nd and 98th percentiles, and colorNumeric hands anything
  # outside its domain the na.color -- which is the near-white "no data" fill.
  # Without the clamp the most exposed places in the country, the ones the map
  # most needs to show, would render as though nothing were known about them.
  fill_exposure <- function(z, p, rng = NULL) {
    out  <- rep(na_col, nrow(z))
    have <- !is.na(z$xp)
    nz   <- have & z$xp > 0
    if (!is.null(p) && any(nz)) {
      v <- z$logxp[nz]
      if (!is.null(rng)) v <- pmin(pmax(v, rng[1]), rng[2])
      out[nz] <- p(v)
    }
    out[have & z$xp == 0] <- zero_col
    unname(out)
  }

  # Tract exposure slice for the current sector x pollutant.
  exp_tract_sel <- reactive({
    exp_tract[exp_tract$sector == input$sector &
              exp_tract$pollutant == input$poll, c("GEOID", "xp", "logxp")]
  })

  # The selected city's tracts, joined to exposure. Membership comes from the
  # side table rather than a `place` column on the geometry, because a tract can
  # serve more than one city box and is carried only once.
  city_tracts <- reactive({
    req(input$city)
    ids <- tract_city$GEOID[tract_city$place == input$city]
    t <- tracts[tracts$GEOID %in% ids, ]
    if (any(st_geometry_type(t) == "GEOMETRYCOLLECTION"))
      t <- st_collection_extract(t, "POLYGON")
    left_join(t, exp_tract_sel(), by = "GEOID")
  })

  # selected city's zones joined to exposure
  city_zones <- reactive({
    req(input$city)
    z <- zones[zones$place == input$city, ]
    # a few HOLC zones are GEOMETRYCOLLECTIONs, which leaflet::addPolygons
    # can't render; keep only their polygonal parts (no-op for POLYGON /
    # MULTIPOLYGON zones, so per-city views are unaffected)
    if (any(st_geometry_type(z) == "GEOMETRYCOLLECTION"))
      z <- st_collection_extract(z, "POLYGON")
    left_join(z, exp_sel(), by = "HOLCID")
  })

  output$map_title <- renderText({
    req(input$city)
    sprintf("%s: %s %s exposure by HOLC zone",
            input$city, sector_label(), poll_label())
  })
  # An all-grey zone layer is a correct answer, not a broken one, but it does
  # not look like one. With every city back in the selector this is common
  # again, not the unreachable branch it was at a 100-zone cutoff: aviation is
  # absent from most small cities entirely. Say so in the header.
  #
  # It reports the ZONE layer specifically, and now says so, because the tract
  # layer can be populated in the same city and view -- a city whose graded
  # zones all read zero may still have exposure a few tracts over, which is
  # exactly the thing the tract layer was added to show.
  output$map_note <- renderUI({
    z <- city_zones()
    if (any(!is.na(z$xp) & z$xp > 0)) return(NULL)
    tr <- city_tracts()
    also <- any(!is.na(tr$xp) & tr$xp > 0)
    tags$span(
      style = "color:#996600; font-weight:400;",
      sprintf(". No graded zone here has measurable %s %s exposure%s",
              sector_label(), poll_label(),
              if (also) "; the tract layer does" else "")
    )
  })

  eby_label <- reactive(names(equity_by)[equity_by == input$eby])
  # The sector IS named here, and has to be: the chart draws one sector at a
  # time and follows the Source selector, so a header that omitted it would
  # leave the reader no way to tell which sector is on screen. This reverses the
  # earlier rule, which held while all three sectors were faceted side by side.
  output$chart_title <- renderText({
    grp <- if (input$eby == "grade") "HOLC grade"
           else sprintf("%s quintile", eby_label())
    sprintf("%s %s exposure by %s",
            tools::toTitleCase(sector_label()), poll_label(), grp)
  })

  # ---- value boxes ----
  # Intensity ratio only -- medians over zones that HAVE exposure, matching the
  # equity chart's boxes. Pooling in the zeros would report presence x intensity
  # as though it were intensity alone (rail noise: 19.3x pooled vs 3.1x here).
  # Both the headline ratio and the bars under it read from one place, so the
  # picture can never disagree with the number above it.
  # All four grades, not just the two the headline divides: the staircase under
  # the ratio is what distinguishes a gradient from a pair of endpoints that
  # happen to differ. Indexed to grade A, so the medians leave their arbitrary
  # model units behind and become multiples of the best-graded baseline.
  da_stats <- reactive({
    e   <- merge(exp_sel_frame(), frame[, c("HOLCID", "grade")], by = "HOLCID")
    e   <- e[!is.na(e$xp) & e$xp > 0, ]
    med <- sapply(grade_levels,
                  function(g) median(e$xp[e$grade == g], na.rm = TRUE))
    n   <- sapply(grade_levels, function(g) sum(e$grade == g))
    # A zero or missing grade-A median makes every index undefined rather than
    # infinite-looking, which the outputs below check for once.
    idx <- if (is.finite(med[["A"]]) && med[["A"]] > 0)
             med / med[["A"]] else setNames(rep(NA_real_, 4), grade_levels)
    list(med = med, n = n, idx = idx)
  })

  # Bar length is the grade's index on the fixed grade_idx_max axis, so lengths
  # mean the same thing across every sector x pollutant the selectors can reach.
  # The basis line stays even though this box now sits in a view whose sidebar
  # states the basis: the project keeps two number bases that differ materially
  # (rail noise D:A is 2.96x here against 2.0x on the mapped bundle), so any
  # figure quoted out of the app still has to say which one it came from. And
  # where the peak is not grade D the note says so -- D is the last row, not
  # necessarily the tallest, and for aviation and road it is not the maximum.
  output$da_panel <- renderUI({
    s <- da_stats()
    if (!is.finite(s$idx[["A"]]) || !is.finite(s$idx[["D"]]))
      return(div(class = "da-note",
                 "No zones with measurable exposure in one of the two grades."))
    bar <- function(g) {
      v <- s$idx[[g]]
      div(class = "da-row",
          span(class = "da-lab", g),
          div(class = "da-track",
              div(class = "da-base",
                  style = sprintf("left:%.1f%%;", 100 / grade_idx_max)),
              div(class = "da-fill",
                  style = sprintf("width:%.1f%%; background:%s;",
                                  if (is.finite(v))
                                    100 * min(v / grade_idx_max, 1) else 0,
                                  grade_cols[[g]]))),
          span(class = "da-idx",
               if (is.finite(v)) sprintf("%.1f×", v) else "n/a"),
          span(class = "da-n", sprintf("n=%s", format(s$n[[g]],
                                                      big.mark = ","))))
    }
    # Flagged only when the peak clears grade D by a margin the bars actually
    # show. Road NO peaks at C by 1.20 against 1.19, and calling that out as
    # "highest at grade C, not D" reports a rounding difference as a shape.
    idx  <- ifelse(is.finite(s$idx), s$idx, -Inf)
    peak <- grade_levels[which.max(idx)]
    if (max(idx) < s$idx[["D"]] * 1.05) peak <- "D"
    tagList(
      div(class = "da", lapply(grade_levels, bar)),
      div(class = "da-note",
          sprintf(paste("Median %s %s exposure by grade, each as a multiple of",
                        "grade A. Zones with measurable exposure only; zeros",
                        "excluded. All %s graded zones in %d cities.%s"),
                  sector_label(), poll_label(),
                  format(nrow(frame), big.mark = ","), frame_cities,
                  if (identical(peak, "D")) ""
                  else sprintf(" Highest at grade %s, not D.", peak))))
  })

  # Grade composition of the mapped city. A city with 12 grade-A zones and 300
  # grade-D zones supports a very different reading of the same choropleth than
  # an evenly graded one, and grade A is the binding constraint everywhere in
  # this project -- so the counts sit under the total. Chips are colored with
  # the same grade palette as the map legend and the chart. factor() over the
  # full level set keeps a grade with zero zones visible as "0" rather than
  # silently dropping out of the row.
  # Five stacked bars, no title above them: A-D in the legend's grade colors,
  # then the city total in grey. Bars are scaled to the total, so each grade
  # reads as its share of the city and the grey bar is the full-width reference
  # rather than a sixth category competing with the four. factor() over the full
  # level set keeps a grade with no zones visible as a labeled "0" row instead of
  # letting it vanish and leave a four-row stack that looks complete.
  output$vb_n_grades <- renderUI({
    z   <- st_drop_geometry(city_zones())
    n   <- table(factor(z$grade, levels = grade_levels))
    tot <- nrow(z)
    row <- function(lab, val, bg) div(
      class = "grow",
      title = sprintf("%s: %s of %s zones (%.0f%%)", lab,
                      format(val, big.mark = ","), format(tot, big.mark = ","),
                      100 * val / tot),
      span(class = "grow-lab", lab),
      div(class = "grow-track",
          div(class = "grow-fill",
              style = sprintf("width:%.1f%%; background:%s;",
                              if (tot > 0) 100 * val / tot else 0, bg))),
      span(class = "grow-n", format(val, big.mark = ",")))
    div(class = "gcount",
        lapply(grade_levels,
               function(g) row(g, as.integer(n[[g]]), grade_cols[[g]])),
        row("ALL", tot, "#c9c9c4"))
  })

  # Presence in the mapped city. The choropleth already shows the grey zones;
  # this puts a number on them, and it is the figure in this row that moves with
  # both the source and the type selector. Every graded zone in the city, so it
  # is the city's real share rather than a subset's. It counts zones, not
  # tracts: the tract layer has its own zero pattern, and folding the two into
  # one percentage would describe neither.
  output$vb_city_presence <- renderUI({
    z <- st_drop_geometry(city_zones())
    v <- z$xp[!is.na(z$xp)]
    lab <- div(class = "pres-lab", "Zones with none")
    if (!length(v))
      return(div(class = "pres", lab,
                 div(class = "pres-sub",
                     "No exposure data for this source and type.")))
    n0  <- sum(v == 0)
    pct <- 100 * n0 / length(v)
    div(
      class = "pres", lab,
      div(class = "pres-val", sprintf("%.0f%%", pct)),
      div(class = "pres-track",
          div(class = "pres-fill", style = sprintf("width:%.1f%%;", pct))),
      div(class = "pres-sub",
          sprintf(paste("%s of %s zones in %s have no measurable %s %s",
                        "exposure. These are the grey zones on the map: an",
                        "absence, not a low value."),
                  format(n0, big.mark = ","), format(length(v), big.mark = ","),
                  input$city, sector_label(), poll_label())))
  })

  # Where the mapped city sits in the national spread. This is the only figure
  # in the per-city row computed on the frame rather than the bundle, and it is
  # the bridge between the two views: the map shows the spread inside one city,
  # this says whether that city is a high- or low-exposure place to begin with.
  # City medians are taken over zones WITH exposure, matching the D:A box and
  # the chart, so a city with nothing measurable drops out of the comparison
  # rather than sinking to the bottom of it -- for aviation that is most of
  # them, which is why the denominator is stated rather than assumed to be 221.
  city_rank_stats <- reactive({
    req(input$city)
    e <- merge(exp_sel_frame(), frame[, c("HOLCID", "place")], by = "HOLCID")
    e <- e[!is.na(e$xp) & e$xp > 0, ]
    if (!nrow(e)) return(NULL)
    med <- tapply(e$xp, e$place, median, na.rm = TRUE)
    med <- med[is.finite(med)]
    i   <- match(input$city, names(med))
    list(n = length(med),
         pct = if (is.na(i)) NA_real_ else 100 * mean(med < med[[i]]))
  })

  output$vb_city_rank <- renderUI({
    s   <- city_rank_stats()
    lab <- div(class = "pres-lab", "City rank, nationally")
    if (is.null(s) || is.na(s$pct))
      return(div(class = "pres", lab,
                 div(class = "pres-sub",
                     sprintf(paste("No zone in %s has measurable %s %s",
                                   "exposure, so the city does not place."),
                             input$city, sector_label(), poll_label()))))
    div(
      class = "pres", lab,
      div(class = "pres-val", ordinal(s$pct)),
      div(class = "pres-track",
          div(class = "pres-mark", style = sprintf("left:%.1f%%;", s$pct))),
      div(class = "pres-sub",
          sprintf(paste("Percentile of median %s %s exposure in %s, among the",
                        "%d cities where any zone registers. Medians over",
                        "zones with exposure only."),
                  sector_label(), poll_label(), input$city, s$n)))
  })


  # ---- choropleth ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, preferCanvas = TRUE)) |>
      tiles_for("light") |>
      setView(lng = -96, lat = 38, zoom = 4) |>
      # Infrastructure gets its own pane above the zone fills. Without it the
      # lines would fall behind the polygons every time the choropleth redraws,
      # since leaflet stacks by insertion order within a pane and the fills are
      # re-added on every sector/pollutant change.
      addMapPane("infra", zIndex = 450) |>
      # base-layer switches fire 'baselayerchange' (not the overlay events that
      # drive leaflet's group-linked legends), so show only the active layer's
      # legend manually; the MutationObserver re-applies when the proxy re-adds
      # the legends after an input change.
      # Two of the three base layers are exposure fills sharing one ramp and one
      # legend, so the test is "is this the grade layer" rather than a match on
      # the exposure layer's own name -- adding a second exposure layer must not
      # mean a second thing to keep in sync here.
      # The grade layer's name is injected from fill_groups rather than written
      # here, so the labels can be reworded in one place without the legend
      # silently sticking on whichever key was showing.
      htmlwidgets::onRender(sprintf("
        function(el) {
          var map = this;
          var GRADE = '%s';
          function apply(name) {
            var exp = (name !== GRADE);
            el.querySelectorAll('.legend-exposure').forEach(function(e){ e.style.display = exp ? '' : 'none'; });
            el.querySelectorAll('.legend-grade').forEach(function(e){ e.style.display = exp ? 'none' : ''; });
          }
          // baselayerchange fires ONLY when the reader clicks the control --
          // proxy re-adds do not raise it, which is why the MutationObserver
          // below exists at all. That makes it the one honest signal of intent,
          // so it is what gets reported to the server as the remembered layer.
          // Reporting from the MutationObserver instead would echo back the
          // default mid-redraw, before the server has restored the selection,
          // and overwrite the very thing being restored.
          map.on('baselayerchange', function(e){
            apply(e.name);
            if (window.Shiny) Shiny.setInputValue('active_fill', e.name, {priority: 'event'});
          });
          new MutationObserver(function(){
            var c = el.querySelector('.leaflet-control-layers-base input:checked');
            apply(c ? c.parentNode.textContent.trim() : '%s');
          }).observe(el, { childList: true, subtree: true });
        }
      ", fill_groups[["grade"]], fill_groups[["zone_exp"]])) |>
      # Wiring for the hand-rolled basemap switch. The control is destroyed and
      # rebuilt by every clearControls(), so this re-arms whenever it reappears,
      # guarded by a data flag so one instance is wired once.
      #
      # disableClickPropagation is why the listener is bound to the control
      # itself and not delegated from the map container: it stops the click at
      # the control, so a listener further up would never see it. Without it a
      # double click on a button reaches the map and zooms.
      #
      # The active class is set here as well as server-side. Server-side is what
      # makes it correct; doing it locally too means the button responds on the
      # click instead of after the redraw returns.
      htmlwidgets::onRender("
        function(el) {
          function wire() {
            el.querySelectorAll('.mapseg-wrap').forEach(function(w) {
              if (w.dataset.wired) return;
              var seg = w.querySelector('.mapseg');
              if (!seg) return;
              w.dataset.wired = '1';
              L.DomEvent.disableClickPropagation(w);
              L.DomEvent.disableScrollPropagation(w);
              w.addEventListener('click', function(e) {
                var b = e.target.closest('.mapseg-btn');
                if (!b) return;
                w.querySelectorAll('.mapseg-btn').forEach(function(o) {
                  o.classList.remove('active');
                });
                b.classList.add('active');
                Shiny.setInputValue(seg.getAttribute('data-input'),
                                    b.getAttribute('data-val'));
              });
            });
          }
          wire();
          new MutationObserver(wire).observe(el, { childList: true, subtree: true });
        }
      ")
  })

  # Basemap swap lives in its own observer so tile handling stays separable from
  # shape drawing. The draw observer below still reads input$basemap, because
  # the border colors change with the base, so a basemap switch does redraw the
  # polygons -- it just no longer refits the view (see last_city). clearTiles
  # drops whatever is there, one layer for light and two for satellite, so the
  # two cases need no separate unwinding. ignoreInit because renderLeaflet has
  # already drawn the light base.
  observeEvent(input$basemap, {
    leafletProxy("map") |> clearTiles() |> tiles_for(input$basemap)
  }, ignoreInit = TRUE)

  # Refit only when the CITY changes. Sector, pollutant, and the two display
  # controls leave the geometry alone, so refitting on those threw away the
  # reader's zoom for no reason -- and with a basemap worth zooming into, that
  # matters more than it did.
  last_city <- reactiveVal(NULL)

  # Which fill the reader has chosen in the layers control, remembered across
  # redraws. Seeded with the layer the map opens on. Fed only by the JS
  # baselayerchange handler, which fires on a click and not on a proxy re-add,
  # so a redraw can never write its own default back in here. The name is
  # validated against fill_groups before it is trusted: it arrives as a display
  # string from the DOM, and a relabel that missed this path would otherwise
  # leave every fill hidden and the map blank.
  active_fill <- reactiveVal(unname(fill_groups[["zone_exp"]]))
  observeEvent(input$active_fill, {
    if (input$active_fill %in% unname(fill_groups)) active_fill(input$active_fill)
  })

  observe({
    z   <- city_zones()
    p   <- pal()
    dom <- pal_domain()
    # per-zone grade color as a PLAIN (unnamed) vector -- a named vector
    # serializes to a JS object and every polygon renders as "[object Object]"
    z$grade_col <- unname(grade_cols[as.character(z$grade)])
    z$exp_col   <- fill_exposure(z, p, dom)
    # a zero zone reads "none measured", not "0.000" -- the number invites the
    # reader to treat it as a small measurement rather than an absence
    xp_txt <- ifelse(is.na(z$xp), "n/a",
                     ifelse(z$xp == 0, "none measured", sprintf("%.3f", z$xp)))
    labs <- sprintf(
      paste0("<b>HOLC grade %s</b><br/>Estimated exposure: <b>%s</b><br/>",
             "Median income: <b>%s</b> &middot; Est. population: <b>%s</b>"),
      z$grade, xp_txt,
      dollar(z$inc, accuracy = 1),
      comma(z$pop_est, accuracy = 100)
    ) |> lapply(htmltools::HTML)

    # Borders do the work of separating adjacent zones, so they have to survive
    # both a dimmed fill and a dark base. Grey hairlines disappear on imagery;
    # white ones read against it, and a touch more weight holds the outline when
    # the fill is turned down.
    sat  <- identical(input$basemap, "sat")
    bcol <- if (sat) "#ffffff" else "#888888"
    bcol2 <- if (sat) "#ffffff" else "#666666"
    bw   <- if (sat) 0.8 else 0.4
    hcol <- if (sat) "#ffff00" else "#333333"

    # Tracts carry the same modeled quantity, so they are colored by the same
    # per-city ramp. Borders are lighter and thinner than the zones': this layer
    # is the city's surface, and there are roughly twice as many of them, so
    # zone-weight hairlines turn a large city into a mesh.
    tr <- city_tracts()
    tr$exp_col <- fill_exposure(tr, p, dom)
    tr_xp_txt <- ifelse(is.na(tr$xp), "n/a",
                        ifelse(tr$xp == 0, "none measured", sprintf("%.3f", tr$xp)))
    # pop_total here is the tract's own ACS count, not the zone layer's
    # density x area estimate -- at tract level the headcount is the source
    # figure, so it is reported without the "est." hedge the zones need.
    tr_labs <- sprintf(
      paste0("<b>Census tract %s</b><br/>Estimated exposure: <b>%s</b><br/>",
             "Median income: <b>%s</b> &middot; Population: <b>%s</b>"),
      tr$GEOID, tr_xp_txt,
      dollar(tr$inc, accuracy = 1),
      comma(tr$pop_total, accuracy = 1)
    ) |> lapply(htmltools::HTML)

    bb <- as.numeric(st_bbox(z))
    # clearGroup on the three fill groups, not clearShapes: the infrastructure
    # lines are shapes too, and clearing them here would wipe an overlay the
    # reader turned on every time they nudged the opacity slider.
    prox <- leafletProxy("map", data = z) |>
      clearGroup(fill_groups[["zone_exp"]]) |> clearGroup(fill_groups[["grade"]]) |>
      clearGroup(fill_groups[["tract_exp"]]) |> clearControls() |>
      # Exposure layer -- fill choropleth (thin neutral border)
      addPolygons(
        group = fill_groups[["zone_exp"]],
        fillColor = ~exp_col, fillOpacity = input$fillop,
        color = bcol, weight = bw, opacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = hcol,
                                            bringToFront = TRUE),
        label = labs
      ) |>
      # HOLC grade layer -- grade-colored fill choropleth (thin neutral border
      # so adjacent same-grade zones stay separable)
      addPolygons(
        group = fill_groups[["grade"]],
        fillColor = ~grade_col, fillOpacity = input$fillop,
        color = bcol2, weight = bw, opacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = hcol,
                                            bringToFront = TRUE),
        label = labs
      ) |>
      # Tract exposure -- the same ramp over the whole city view, so the graded
      # zones can be read against the surface they sit in rather than only
      # against each other.
      addPolygons(
        data = tr, group = fill_groups[["tract_exp"]],
        fillColor = tr$exp_col, fillOpacity = input$fillop,
        color = bcol, weight = bw * 0.6, opacity = 0.6,
        highlightOptions = highlightOptions(weight = 3, color = hcol,
                                            bringToFront = TRUE),
        label = tr_labs
      ) |>
      # mutually exclusive views -- fill choropleths can't co-read when stacked,
      # so switch between them. Listed unit-first so the two HOLC fills sit
      # together and the eye reads the unit before the variable; which one opens
      # is set by hiding the other two rather than by list position.
      # Added before the layers control so it sits above it in the corner, and
      # re-added here because clearControls() above drops it with the legends.
      addControl(basemap_control(input$basemap), position = "topright",
                 className = "mapseg-wrap") |>
      addControl(scale_control(input$scale), position = "topright",
                 className = "mapseg-wrap") |>
      addLayersControl(
        baseGroups = unname(fill_groups[c("grade", "zone_exp", "tract_exp")]),
        overlayGroups = unname(infra_groups),
        options = layersControlOptions(collapsed = FALSE)
      )

    # Restore the reader's fill choice instead of resetting to the default. This
    # observer re-runs on city, sector, exposure, color scale, opacity and
    # basemap, and every run rebuilds all three fills and the layers control, so
    # hiding two fixed groups here meant any of those six controls silently threw
    # the reader back to HOLC (exposure). isolate: a layer switch must not
    # retrigger the redraw that is restoring it.
    keep <- isolate(active_fill())
    for (g in unname(fill_groups))
      prox <- if (identical(g, keep)) showGroup(prox, g) else hideGroup(prox, g)

    # ONE exposure key: the ramp with the zero swatch nested beneath it, inside
    # the same box. A zero belongs to the same reading -- it is the absence the
    # ramp deliberately refuses to color -- so shipping it as a second box made
    # the reader assemble one scale out of two keys.
    #
    # Hand-rolled through addControl rather than addLegend, because addLegend
    # renders a continuous ramp OR a discrete swatch list and has no slot for
    # trailing content under the ramp. Building the markup here is what puts the
    # two in one box without depending on leaflet's internal legend structure.
    # It serves the zone and tract fills alike: one ramp, one key, which is what
    # makes the two layers comparable.
    has_zero <- any(!is.na(z$xp) & z$xp == 0) || any(!is.na(tr$xp) & tr$xp == 0)
    # No ramp and no zeros means the view carries no exposure information at
    # all, so there is nothing for a key to say; drawing an empty titled box
    # would only assert otherwise.
    if (!is.null(p) || has_zero)
      prox <- prox |>
        addControl(exposure_key(p, dom, has_zero, scale_national()),
                   position = "bottomright",
                   className = "info legend legend-exposure")

    prox <- prox |>
      addLegend("bottomright", colors = unname(grade_cols),
                labels = c("A", "B", "C", "D"),
                title = "HOLC grade", opacity = 1,
                className = "info legend legend-grade")

    # isolate the read: this observer writes last_city too, and a plain read
    # would make it depend on its own write and re-run forever
    if (!identical(isolate(last_city()), input$city)) {
      last_city(input$city)
      prox |> fitBounds(bb[1], bb[2], bb[3], bb[4])
    }
  })

  # ---- infrastructure overlay ------------------------------------------------
  # Drawn in its own observer, keyed on the city and the basemap only. The
  # geometry does not depend on sector or pollutant, so redrawing it whenever
  # those change would re-render thousands of polylines (Los Angeles carries
  # 3,628 highway and 4,009 rail segments) for no visible difference.
  city_infra <- reactive({
    req(input$city)
    lapply(infra, function(x) x[x$place == input$city, ])
  })

  # Which overlay the sector selector implies. Kept as a function so the draw
  # observer and the sector observer cannot drift apart.
  default_group <- function(sector) unname(infra_groups[[sector]])
  apply_infra_vis <- function(prox, sector) {
    keep <- default_group(sector)
    for (g in unname(infra_groups))
      prox <- if (identical(g, keep)) showGroup(prox, g) else hideGroup(prox, g)
    prox
  }

  # halo beneath, stroke on top -- see infra_style
  add_line_layer <- function(prox, x, group, style, weight, dash = NULL) {
    if (!nrow(x)) return(prox)
    prox |>
      addPolylines(data = x, group = group, color = style[["halo"]],
                   weight = weight + 1.8, opacity = 0.55, dashArray = dash,
                   options = pathOptions(pane = "infra", interactive = FALSE)) |>
      addPolylines(data = x, group = group, color = style[["main"]],
                   weight = weight, opacity = 0.95, dashArray = dash,
                   label = x$lab,
                   options = pathOptions(pane = "infra"))
  }

  observe({
    ci  <- city_infra()
    sty <- infra_style[[if (identical(input$basemap, "sat")) "sat" else "light"]]

    rd <- ci$road
    # Interstates carry more traffic than the other freeways in class 2 and are
    # the segment of the network the redlining literature actually argues about,
    # so they are drawn heavier. A segment with neither a route shield nor a
    # street name in HPMS gets a generic identity line rather than an empty one.
    rd_name <- ifelse(nzchar(rd$name) & !is.na(rd$name), rd$name,
                      "Limited-access highway")
    rd_aadt <- ifelse(!is.na(rd$aadt) & rd$aadt > 0,
                      format(round(rd$aadt), big.mark = ",",
                             scientific = FALSE, trim = TRUE),
                      NA_character_)
    rd$lab <- infra_lab(rd,
      "<b>", rd_name, "</b>",
      lab_row("Class", road_class[as.character(rd$fclass)]),
      lab_row("Traffic", rd_aadt, " vehicles/day"))

    rl <- ci$rail
    # The class is the identity line because the bundle carries no name for a
    # rail segment, and it is the thing worth saying: yard and spur are kept
    # deliberately, so the label should distinguish them from mainline rather
    # than calling all of it "Rail track".
    rl_class <- unname(rail_class[as.character(rl$net)])
    rl_class[is.na(rl_class)] <- "Rail track"
    rl_owner <- ifelse(is.na(rl$owner) | !nzchar(rl$owner), NA_character_,
                       ifelse(rl$owner %in% names(rail_owner),
                              sprintf("%s (%s)", rail_owner[rl$owner], rl$owner),
                              rl$owner))
    rl$lab <- infra_lab(rl, "<b>", rl_class, "</b>", lab_row("Owner", rl_owner))

    av <- ci$avi

    prox <- leafletProxy("map") |>
      clearGroup(unname(infra_groups))
    prox <- add_line_layer(prox, rd[rd$fclass == 2, ], infra_groups[["Road"]],
                           sty$road, weight = 1.4)
    prox <- add_line_layer(prox, rd[rd$fclass == 1, ], infra_groups[["Road"]],
                           sty$road, weight = 2.4)
    prox <- add_line_layer(prox, rl, infra_groups[["Rail"]], sty$rail,
                           weight = 1.8, dash = "5,5")
    if (nrow(av))
      # Towered airports get the larger marker. The map should not imply that a
      # municipal strip and O'Hare put the same aviation exposure on a
      # neighborhood, and a control tower is the coarse but honest proxy for
      # operations that ships with the FAA record.
      prox <- prox |>
        addCircleMarkers(
          data = av, group = infra_groups[["Aviation"]],
          radius = ifelse(av$towered, 7, 4.5), stroke = TRUE,
          color = "#123", weight = 1.2, opacity = 0.9,
          fillColor = avi_col, fillOpacity = 0.85,
          label = lapply(paste0(
            "<b>", av$name, "</b>",
            lab_row("Code", av$id),
            lab_row("Control tower", ifelse(av$towered, "Yes", "No"))),
            htmltools::HTML),
          options = pathOptions(pane = "infra"))

    # Applied here as well as in the sector observer below: the layers are added
    # visible, so a city change would otherwise leave all three on at once.
    apply_infra_vis(prox, isolate(input$sector))
  })

  # Sector change moves the overlay with it. This resets a manual toggle, which
  # is the deliberate tradeoff: the reader can always turn a layer back on, and
  # the alternative (remembering per-layer state across sector changes) makes
  # the map open in a state the selector does not explain.
  observeEvent(input$sector, {
    apply_infra_vis(leafletProxy("map"), input$sector)
  }, ignoreInit = TRUE)

  # ---- equity chart: the SELECTED sector, indexed to its baseline group ------
  #
  # One sector at a time, following the Source selector. This replaced three
  # faceted panels; what the facets bought was the sector comparison, and the
  # cost of the change is that the app no longer shows it -- a reader now has to
  # click through Rail, Road and Aviation and hold the three shapes in memory.
  # Note that the project's "show it, never state it" rule bars replacing that
  # with a sentence claiming rail is the most stratified sector, so the
  # comparison is currently not carried anywhere in the app.
  #
  # Why still indexed rather than in real units, now that there is nothing to
  # compare against: the raw units are arbitrary model output spanning five
  # orders of magnitude between sectors (median road NO is 2.6e5, aviation's is
  # 3.0), so an axis in them is a number nobody can read. Dividing by the
  # baseline group's median puts the baseline at 1.0 and makes the last group's
  # height the disparity ratio, which reads on its own. Levels are deliberately
  # discarded -- the map carries level, this carries inequality.
  #
  # Why a median line and not box plots: within-group spread is roughly six
  # times the between-group shift (rail noise grade A alone runs 0.18-2.39 IQR
  # against a D:A shift of 2.0x), so boxes overlap almost completely and bury
  # the staircase they are supposed to show. Thin IQR bars keep the spread
  # visible as context; the bold line carries the signal.
  output$equity <- renderPlot({
    eb  <- input$eby
    # frame, not zattr: the distribution behind the chart is every graded zone
    # in the study. This view is national throughout and reads no city input.
    #
    # One sector, the selected one. Quintile breaks are unaffected by the
    # narrowing: exp_frame carries every zone for every sector, so pooling three
    # sectors repeated each zone's demographic value exactly three times and the
    # quantiles were already the distinct-zone quantiles.
    nat <- merge(exp_sel_frame(), frame, by = "HOLCID")
    # guard before the quantile binning below, which fails obscurely ("invalid
    # number of intervals") rather than usefully on an empty slice
    validate(need(nrow(nat) > 0, "No exposure data for this pollutant."))

    if (eb == "grade") {
      nat$bin  <- factor(nat$grade, levels = grade_levels)
      xlab  <- "Grade"
      base_lab <- "grade A"
    } else {
      # quintile breaks are pooled across all sectors and cities, which is the
      # basis the whole view runs on
      br <- unique(quantile(nat[[eb]], probs = seq(0, 1, 0.2), na.rm = TRUE))
      labs_q <- paste0("Q", seq_len(length(br) - 1))
      binq <- function(v) cut(v, breaks = br, include.lowest = TRUE, labels = labs_q)
      nat$bin  <- binq(nat[[eb]])
      xlab  <- sprintf("%s quintile", eby_label())
      base_lab <- labs_q[1]
    }
    # Presence and intensity are separate disparities and pooling them distorts
    # both. A zone with no measurable exposure is not a low-exposure zone, and
    # the zero share is itself steeply graded (rail noise: 36.1% of grade-A
    # zones vs 13.8% of grade-D). Pooling drags the baseline group toward the
    # floor and inflates the rail-noise D:A ratio from 3.1x to 19.3x. So: the
    # line shows intensity among zones that have exposure, labels show the
    # share with none.
    zero_tab <- aggregate(list(pct_zero = nat$xp == 0),
                          list(bin = nat$bin), mean)
    nat  <- nat[!is.na(nat$xp) & nat$xp > 0, ]
    validate(need(nrow(nat) > 0,
                  "No zone in this view has measurable exposure for this pollutant."))

    # Index to the median of the baseline group. If that group has no zones with
    # exposure the fallback is the sector's own overall median, which keeps the
    # panel drawable; the baseline then is not 1.0, and the reference line makes
    # that visible rather than hiding it.
    base_v   <- nat$xp[nat$bin == levels(nat$bin)[1]]
    base_med <- median(base_v, na.rm = TRUE)
    if (!is.finite(base_med) || base_med <= 0)
      base_med <- median(nat$xp, na.rm = TRUE)
    nat$rel  <- nat$xp / base_med

    # Axis limits come from the IQR envelope, not the raw range: aviation NO
    # carries a rel value of 5.7e6, which would compress the panel into a
    # hairline. coord_cartesian zooms rather than filters, so every median and
    # quartile below is still computed on the full data.
    qs  <- unlist(lapply(split(nat$rel, nat$bin, drop = TRUE),
                         quantile, probs = c(.25, .75), na.rm = TRUE))
    ylo <- max(min(qs, na.rm = TRUE) * 0.7, 1e-3)
    yhi <- max(qs, na.rm = TRUE) * 1.4
    y_lab <- ylo * 0.75   # zero-share label strip, below the bars

    ggplot(nat, aes(x = bin, y = rel)) +
      # No tint. It marked the selected sector's panel while three were on
      # screen; with one panel there is nothing to distinguish it from.
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55",
                 linewidth = 0.4) +
      stat_summary(fun.min = function(v) quantile(v, .25),
                   fun.max = function(v) quantile(v, .75),
                   geom = "linerange", colour = "grey45", linewidth = 0.7,
                   alpha = 0.85) +
      stat_summary(fun = median, geom = "line", group = 1,
                   colour = "#2c3e50", linewidth = 1.1) +
      stat_summary(fun = median, geom = "point", aes(fill = bin),
                   shape = 21, size = 3.1, colour = "#2c3e50", stroke = 0.9) +
      geom_text(data = zero_tab, inherit.aes = FALSE,
                aes(x = bin, y = y_lab,
                    label = sprintf("%.0f%%", 100 * pct_zero)),
                size = 2.9, colour = "grey35") +
      scale_fill_manual(values = if (eb == "grade") grade_cols else
                          setNames(q_ramp(nlevels(nat$bin)), levels(nat$bin)),
                        guide = "none", drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      scale_y_log10() +
      coord_cartesian(ylim = c(y_lab * 0.85, yhi)) +
      labs(x = xlab,
           y = "Exposure",
           subtitle = paste0(
             sprintf("Indexed to the %s median (dashed line). ", base_lab),
             "Line: median among zones with exposure. Bars: IQR. ",
             "Grey %: share with none.",
             "\nLog axis, clipped to the IQR envelope.")) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.x = element_blank(),
            plot.subtitle = element_text(size = 9.5, colour = "grey30"))
  })
}

shinyApp(ui, server)
