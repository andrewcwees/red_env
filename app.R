# app.R ----------------------------------------------------------------------
# Redlining & Transportation-Sector Environmental Exposure -- interactive map
#
# A work-sample Shiny app built from the redenv research project (Weesner,
# Higashidani, ... Lim; UA). For a selected city it maps present-day
# transportation-sector environmental exposure across neighborhoods graded by
# the 1930s HOLC ("redlining") maps, and shows how exposure tracks HOLC grade.
#
#   * Map        -> HOLC zones filled by exposure (log scale) or by HOLC grade,
#                   for a chosen transportation sector x pollutant. The ramp is
#                   fitted to the selected city, so it reads the within-city
#                   spread and colors do not compare across cities. Zones with
#                   no measurable exposure get their own color, not the bottom
#                   of the ramp.
#   * Equity     -> national distribution across HOLC grades A-D (or a
#                   demographic quintile) for that sector x pollutant, with the
#                   selected city overlaid. Reports presence and intensity
#                   separately: boxes cover zones that HAVE exposure, labels
#                   give each group's share with none. Both track grade, and
#                   pooling them overstates the intensity gradient.
#   * Value boxes-> between the map and the chart, because two of the three read
#                   off the map above them: Grade D-vs-A intensity ratio (same
#                   basis as the chart), the selected city's zone count broken
#                   out by HOLC grade, and the anchor finding (rail is the most
#                   stratified sector) with the other two sectors' shares beside
#                   it rather than only the winner.
#
# Scope is deliberately hedged (see interactive_display_plan_2026-07-14.md):
# Rail + Road + Aviation, no combined index (CEI), descriptive framing only --
# no causal claims. This is a work sample, not a published result.
#
# Aviation is included even though it is heavily zero-inflated (~66% of zones
# have no measurable aviation exposure, against 8-22% for rail and 0% for road).
# The presence/intensity split above is what makes that reportable: the zeros
# read as an absence, not as a low value. Its gradients also disagree with the
# other two sectors -- aviation noise has the strongest %Hispanic gradient of
# any sector x pollutant here, while its income gradient runs the opposite way
# from rail and road. That disagreement is a finding, not a reason to omit it.
#
# Self-contained: reads only redenv_data.rds (baked by prep_data.R). Run with:
#   shiny::runApp()
#
# Derived from unpublished HUM Lab data; external sharing signed off by Chris
# Lim (2026-07-17). Keep the deployed app private until finished, then publish.

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

# city selector choices: "Birmingham, AL" -> "Birmingham, AL". The map is
# always a single city; there is no all-cities view.
city_choices <- setNames(cities$place, cities$place)

# anchor finding from the ranking table: which sector takes first place most
# often across the 18 sector x measure comparisons in sector_stratification.R.
# Sorted here so the value box shows the leader and the full standing, not just
# the winner -- the gap is the point (rail 61%, and aviation ahead of road).
rank_tab  <- as.data.frame(rank)
rank_tab  <- rank_tab[order(-rank_tab$pct_first), ]
rank_n    <- sum(rank_tab$n_first_place)
lead_sect <- rank_tab$sector[1]

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
    div(class = "app-brand-title", "Redlining & Transportation Exposure"),
    div(class = "app-brand-sub",
        sprintf(paste("Rail, road, and aviation exposure across 1930s",
                      "HOLC-graded neighborhoods in %d cities"),
                nrow(cities))),
    p(class = "app-brand-lede",
      "In the 1930s the Home Owners' Loan Corporation graded American",
      "neighborhoods A through D for mortgage risk, a ranking that tracked",
      "race and shaped where investment went for decades afterward. This map",
      "asks how present-day transportation emissions exposure sits on top of",
      "those grades."),
    p(class = "app-brand-lede",
      "Each polygon is one graded neighborhood, shaded by modeled rail, road,",
      "or aviation exposure on a log scale, darker meaning higher. The scale",
      "is fitted to the selected city, so it reads the spread within that city",
      "and colors are not comparable between cities. Zones with no measurable",
      "exposure get their own shade, because an absence is not a low value.",
      "The chart below the map pools every mapped city and regroups the same",
      "zones by HOLC grade or by present-day demographics. Descriptive, not",
      "causal.")
  ),
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 330,
    selectizeInput(
      "city", "City", choices = NULL,  # populated server-side (searchable)
      # count comes from the bundle so the filter in prep_data.R can change
      # without stranding a hardcoded number here
      options = list(placeholder = sprintf("Type to search %d cities...",
                                           nrow(cities)),
                     maxOptions = 300)
    ),
    selectInput("sector", "Emission Source", choices = sectors,
                selected = "Rail"),
    selectInput("poll", "Emission Type", choices = pollutants,
                selected = "Noise"),
    radioButtons("basemap", "Basemap", choices = basemaps,
                 selected = "light", inline = TRUE),
    # Satellite is unreadable under an opaque choropleth, so the fill has to be
    # dimmable for the imagery to be worth switching to at all.
    sliderInput("fillop", "Fill opacity", min = 0.2, max = 1, value = 0.8,
                step = 0.05, ticks = FALSE),
    hr(),
    # What the map shows now lives in the banner; what is left here is tied to
    # the controls beside it.
    helpText(
      "Hover a zone for its grade, exposure, and demographics.",
      "Use the layer control (top-right of the map) to switch between the",
      "exposure and HOLC-grade fill views.",
      "Switch the basemap to Satellite and turn the fill opacity down to see",
      "what the infrastructure driving the exposure actually is."
    ),
    helpText(
      HTML("<b>Chart:</b> exposure across all mapped cities, with the selected",
           "city's zones overlaid. Use <b>Group by</b>, above the chart, to",
           "regroup the same zones by HOLC grade or by a demographic quintile",
           "(income, %&nbsp;Hispanic, %&nbsp;Black): does the disparity follow",
           "redlining, income, or present-day demographics?")
    ),
    helpText(HTML(paste(
      "<small>All three emission sources are shown. Aviation is sparse",
      "&mdash; 54&ndash;78% of mapped zones have no measurable aviation",
      "exposure, so read",
      "its &ldquo;% none&rdquo; row alongside its boxes. No combined index (CEI)",
      "is reported. Source: redenv (Weesner &amp; Lim, UA). NEMO emissions (EPA",
      "NEI 2017); BTS noise; HOLC via Mapping Inequality; ACS 2019. Work sample",
      "&mdash; descriptive, not a published or causal result.</small>")))
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
     .app-brand-title { font-size: 2.05rem; font-weight: 700;
                        line-height: 1.1; letter-spacing: -.02em;
                        margin-bottom: .2rem; }
     .app-brand-sub { font-size: .95rem; font-weight: 400; opacity: .7;
                      line-height: 1.3; margin-bottom: .85rem; }
     .app-brand-lede { font-size: .9rem; line-height: 1.5; opacity: .85;
                       max-width: 95ch; margin-bottom: .4rem; }
     .app-brand-lede:last-child { margin-bottom: 0; }
     @media (max-width: 700px) {
       .app-brand-title { font-size: 1.5rem; }
       .navbar { padding: 1.1rem 0 .9rem; }
     }
     .chart-ctl { width: 230px; flex: 0 0 auto; }
     .chart-ctl .form-group, .chart-ctl .shiny-input-container { margin-bottom: 0; }
     .chart-ctl label { margin-bottom: .1rem; font-size: .78rem;
                        font-weight: 500; opacity: .75; }
     .chart-ctl .form-select, .chart-ctl select { padding-top: .2rem;
                        padding-bottom: .2rem; font-size: .85rem; }
     .vb-detail { margin-top: .45rem; display: flex; flex-wrap: wrap;
                  gap: .25rem; }
     .vb-chip { font-size: .72rem; font-weight: 600; line-height: 1;
                padding: .22rem .4rem; border-radius: .2rem; color: #1a1a1a;
                font-variant-numeric: tabular-nums; }
     .vb-bars { margin-top: .45rem; font-size: .72rem; }
     .vb-bar-note { opacity: .6; margin-bottom: .2rem; }
     .vb-bar-row { display: flex; align-items: center; gap: .35rem;
                   margin-bottom: .18rem; }
     .vb-bar-lab { width: 3.9rem; flex: 0 0 auto; }
     .vb-bar-track { flex: 1 1 auto; height: .42rem; border-radius: .21rem;
                     background: rgba(0,0,0,.12); }
     .vb-bar-fill { height: 100%; border-radius: .21rem; }
     .vb-bar-val { width: 2.2rem; flex: 0 0 auto; text-align: right;
                   font-variant-numeric: tabular-nums; }"))),
  card(
    full_screen = TRUE, height = 540,
    card_header(textOutput("map_title", inline = TRUE),
                uiOutput("map_note", inline = TRUE)),
    leafletOutput("map", height = 500)
  ),
  # The boxes sit between the map and the chart rather than above the map: two
  # of the three are read against the map (the city's grade composition, the
  # D:A ratio the choropleth is a picture of), so they belong after the reader
  # has seen it, and they no longer push the map itself below the fold.
  layout_columns(
    fill = FALSE,
    value_box(title = "Grade D vs. A intensity (zones with exposure)",
              value = textOutput("vb_ratio"), theme = "primary"),
    # The bare total says how much is on screen but not what it is made of, and
    # grade composition is uneven enough between cities to change how the map
    # reads -- so the count breaks out A-D, colored to match the legend.
    value_box(title = "Zones mapped in city",
              value = textOutput("vb_n"),
              uiOutput("vb_n_grades"),
              theme = "secondary"),
    # Naming only the winner hides how far ahead it is and buries that aviation
    # out-ranks road, so all three sectors' shares are shown.
    value_box(title = "Most stratified sector",
              value = textOutput("vb_rail"),
              uiOutput("vb_rank_bars"),
              theme = "light")
  ),
  card(
    height = 430,
    # The grouping control lives in this card, not the sidebar: it acts on the
    # chart alone, and next to the sidebar's city/sector/pollutant inputs (which
    # all drive the map) it read as a fourth map control.
    card_header(
      div(class = "d-flex justify-content-between align-items-center gap-3",
          textOutput("chart_title", inline = TRUE),
          div(class = "chart-ctl",
              selectInput("eby", "Group by", choices = equity_by,
                          selected = "grade", width = "100%")))
    ),
    plotOutput("equity", height = 340)
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

  # NONZERO logxp values for the SELECTED CITY -- the ramp domain and the legend
  # range. The ramp is fitted per city, not nationally: the map shows one city at
  # a time, and any single city occupies a narrow slice of the national spread,
  # so a national domain rendered most sector x pollutant views near-uniform
  # (road NO2 was close to a single color everywhere). The tradeoff is that
  # colors are NOT comparable between cities -- the legend and the sidebar say
  # so, and the equity chart below is where cross-city context lives. Dropping
  # zeros keeps the mass of no-exposure zones from compressing the ramp.
  nz_vals <- reactive({
    v <- city_zones()$logxp
    v[!is.na(v) & v > 0]
  })

  pal <- reactive({
    v <- nz_vals()
    if (!length(v)) return(NULL)   # no ramp to draw
    # A per-city domain can collapse to a single distinct value (one zone with
    # exposure, or several with identical values), which colorNumeric rejects.
    # Pad it so those zones still get a ramp color instead of falling through to
    # the near-white "no data" fill.
    rng <- range(v)
    if (rng[1] == rng[2]) rng <- rng + c(-1, 1) * max(abs(rng[1]) * 0.01, 1e-6)
    colorNumeric("YlOrRd", domain = rng, na.color = na_col)
  })

  # per-zone fill: zero -> grey, unmatched -> near-white, else the ramp. Returns
  # a PLAIN (unnamed) vector -- a named one serializes to a JS object and every
  # polygon renders as "[object Object]".
  fill_exposure <- function(z, p) {
    out  <- rep(na_col, nrow(z))
    have <- !is.na(z$xp)
    nz   <- have & z$xp > 0
    if (!is.null(p) && any(nz)) out[nz] <- p(z$logxp[nz])
    out[have & z$xp == 0] <- zero_col
    unname(out)
  }

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
  # An all-grey map is a correct answer, not a broken one, but it does not look
  # like one. At min_zones = 100 no city in the bundle is all-grey for any
  # sector x pollutant (it was 33 of 155 cities for aviation noise at a cutoff
  # of 20), so this branch is currently unreachable -- keep it, because the
  # cutoff is a display choice that may be lowered again, and individual zones
  # still go grey constantly (54-78% of zones for aviation).
  # Say so in the header rather than leaving the reader to guess.
  output$map_note <- renderUI({
    z <- city_zones()
    if (any(!is.na(z$xp) & z$xp > 0)) return(NULL)
    tags$span(
      style = "color:#996600; font-weight:400;",
      sprintf(" — no zone here has measurable %s %s exposure",
              sector_label(), poll_label())
    )
  })

  eby_label <- reactive(names(equity_by)[equity_by == input$eby])
  output$chart_title <- renderText({
    grp <- if (input$eby == "grade") "HOLC grades"
           else sprintf("%s quintiles", eby_label())
    sprintf("%s %s exposure across %s (pooled)",
            tools::toTitleCase(sector_label()), poll_label(), grp)
  })

  # ---- value boxes ----
  # Intensity ratio only -- medians over zones that HAVE exposure, matching the
  # equity chart's boxes. Pooling in the zeros would report presence x intensity
  # as though it were intensity alone (rail noise: 19.3x pooled vs 3.1x here).
  output$vb_ratio <- renderText({
    e <- merge(exp_sel(),
               as.data.frame(zones)[, c("HOLCID", "grade")], by = "HOLCID")
    e <- e[!is.na(e$xp) & e$xp > 0, ]
    mA <- median(e$xp[e$grade == "A"], na.rm = TRUE)
    mD <- median(e$xp[e$grade == "D"], na.rm = TRUE)
    if (is.na(mA) || is.na(mD) || mA == 0) "n/a" else sprintf("%.2fx", mD / mA)
  })
  output$vb_n   <- renderText(format(nrow(city_zones()), big.mark = ","))

  # Grade composition of the mapped city. A city with 12 grade-A zones and 300
  # grade-D zones supports a very different reading of the same choropleth than
  # an evenly graded one, and grade A is the binding constraint everywhere in
  # this project -- so the counts sit under the total. Chips are colored with
  # the same grade palette as the map legend and the chart. factor() over the
  # full level set keeps a grade with zero zones visible as "0" rather than
  # silently dropping out of the row.
  output$vb_n_grades <- renderUI({
    z <- st_drop_geometry(city_zones())
    n <- table(factor(z$grade, levels = grade_levels))
    div(class = "vb-detail",
        lapply(grade_levels, function(g)
          span(class = "vb-chip",
               style = sprintf("background:%s;", grade_cols[[g]]),
               sprintf("%s %s", g, format(as.integer(n[[g]]), big.mark = ",")))))
  })

  output$vb_rail <- renderText(sprintf("%s (%.0f%%)", lead_sect,
                                       rank_tab$pct_first[1]))

  # The full standing behind that headline. Bar widths are the share itself
  # (the three sum to 100%), not scaled to the leader, so the bars read as the
  # split of first-place finishes rather than as a ranking. The leader is
  # darker; the point of showing the other two is that road, the sector a
  # reader would expect to dominate, comes last.
  output$vb_rank_bars <- renderUI({
    div(class = "vb-bars",
        div(class = "vb-bar-note",
            sprintf("share of %d first-place finishes", rank_n)),
        lapply(seq_len(nrow(rank_tab)), function(i) {
          div(class = "vb-bar-row",
              title = sprintf("%s: %d of %d", rank_tab$sector[i],
                              rank_tab$n_first_place[i], rank_n),
              span(class = "vb-bar-lab", rank_tab$sector[i]),
              div(class = "vb-bar-track",
                  div(class = "vb-bar-fill",
                      style = sprintf("width:%.1f%%; background:%s;",
                                      rank_tab$pct_first[i],
                                      if (i == 1) "#2c3e50" else "#9aa7b1"))),
              span(class = "vb-bar-val",
                   sprintf("%.0f%%", rank_tab$pct_first[i])))
        }))
  })

  # ---- choropleth ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, preferCanvas = TRUE)) |>
      tiles_for("light") |>
      setView(lng = -96, lat = 38, zoom = 4) |>
      # base-layer switches fire 'baselayerchange' (not the overlay events that
      # drive leaflet's group-linked legends), so show only the active layer's
      # legend manually; the MutationObserver re-applies when the proxy re-adds
      # the legends after an input change.
      htmlwidgets::onRender("
        function(el) {
          var map = this;
          function apply(name) {
            var exp = (name === 'Exposure');
            el.querySelectorAll('.legend-exposure').forEach(function(e){ e.style.display = exp ? '' : 'none'; });
            el.querySelectorAll('.legend-grade').forEach(function(e){ e.style.display = exp ? 'none' : ''; });
          }
          map.on('baselayerchange', function(e){ apply(e.name); });
          new MutationObserver(function(){
            var c = el.querySelector('.leaflet-control-layers-base input:checked');
            apply(c ? c.parentNode.textContent.trim() : 'Exposure');
          }).observe(el, { childList: true, subtree: true });
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

  observe({
    z  <- city_zones()
    p  <- pal()
    # per-zone grade color as a PLAIN (unnamed) vector -- a named vector
    # serializes to a JS object and every polygon renders as "[object Object]"
    z$grade_col <- unname(grade_cols[as.character(z$grade)])
    z$exp_col   <- fill_exposure(z, p)
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

    bb <- as.numeric(st_bbox(z))
    prox <- leafletProxy("map", data = z) |>
      clearShapes() |> clearControls() |>
      # Exposure layer -- fill choropleth (thin neutral border)
      addPolygons(
        group = "Exposure",
        fillColor = ~exp_col, fillOpacity = input$fillop,
        color = bcol, weight = bw, opacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = hcol,
                                            bringToFront = TRUE),
        label = labs
      ) |>
      # HOLC grade layer -- grade-colored fill choropleth (thin neutral border
      # so adjacent same-grade zones stay separable)
      addPolygons(
        group = "HOLC grade",
        fillColor = ~grade_col, fillOpacity = input$fillop,
        color = bcol2, weight = bw, opacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = hcol,
                                            bringToFront = TRUE),
        label = labs
      ) |>
      # mutually exclusive views -- two fill choropleths can't co-read when
      # stacked, so switch between them (default: Exposure)
      addLayersControl(
        baseGroups = c("Exposure", "HOLC grade"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup("HOLC grade")

    # exposure legends: the ramp (nonzero only), then the zero swatch. Both are
    # classed legend-exposure so the baselayerchange handler toggles them together.
    if (!is.null(p))
      prox <- prox |>
        addLegend("bottomright", pal = p, values = nz_vals(),
                  title = paste0("Exposure<br/><small>log(xp+1), scaled ",
                                 "within city</small>"), opacity = 0.9,
                  className = "info legend legend-exposure")
    if (any(!is.na(z$xp) & z$xp == 0))
      prox <- prox |>
        addLegend("bottomright", colors = zero_col,
                  labels = "No measurable exposure", opacity = 0.9,
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

  # ---- equity chart: national distribution by grade or demographic quintile,
  #      with the selected city's zones overlaid as points --------------------
  output$equity <- renderPlot({
    req(input$city)
    eb  <- input$eby
    nat <- merge(exp_sel(), zattr, by = "HOLCID")
    # guard before the quantile binning below, which fails obscurely ("invalid
    # number of intervals") rather than usefully on an empty slice
    validate(need(nrow(nat) > 0,
                  "No exposure data for this sector and pollutant."))
    city <- merge(st_drop_geometry(city_zones())["HOLCID"], nat, by = "HOLCID")

    if (eb == "grade") {
      nat$bin  <- factor(nat$grade, levels = grade_levels)
      city$bin <- factor(city$grade, levels = grade_levels)
      fills <- grade_cols
      xlab  <- "HOLC grade (A = 'best' ... D = 'hazardous')"
    } else {
      # national quintile breaks; apply the SAME breaks to the city overlay
      br <- unique(quantile(nat[[eb]], probs = seq(0, 1, 0.2), na.rm = TRUE))
      labs_q <- paste0("Q", seq_len(length(br) - 1))
      binq <- function(v) cut(v, breaks = br, include.lowest = TRUE, labels = labs_q)
      nat$bin  <- binq(nat[[eb]])
      city$bin <- binq(city[[eb]])
      fills <- setNames(q_ramp(length(labs_q)), labs_q)
      xlab  <- sprintf("%s quintile (Q1 = lowest, Q5 = highest)", eby_label())
    }

    # Presence and intensity are separate disparities and pooling them distorts
    # both. A zone with no measurable exposure is not a low-exposure zone, and
    # the zero share is itself steeply graded (rail noise: 36.5% of grade-A
    # zones vs 13.8% of grade-D). Pooling drags grade A toward the floor and
    # inflates the rail-noise D:A ratio from 3.1x to 19.3x. So: boxes show
    # intensity among zones that have exposure, labels show the share with none.
    zero_tab <- aggregate(list(pct_zero = nat$xp == 0), list(bin = nat$bin), mean)
    nat  <- nat[!is.na(nat$xp) & nat$xp > 0, ]
    city <- city[!is.na(city$xp) & city$xp > 0, ]
    validate(need(nrow(nat) > 0,
                  "No zone in this view has measurable exposure for this sector and pollutant."))

    # reserve a strip beneath the boxes for the zero-share labels
    y_rng <- range(nat$logxp, na.rm = TRUE)
    y_lab <- y_rng[1] - 0.11 * max(diff(y_rng), 1e-6)

    ggplot(nat, aes(x = bin, y = logxp)) +
      geom_boxplot(aes(fill = bin), outlier.size = 0.5, width = 0.6,
                   alpha = 0.85, colour = "grey30") +
      geom_jitter(data = city, width = 0.15, height = 0, size = 1.6,
                  colour = "#111111", alpha = 0.8) +
      geom_text(data = zero_tab, inherit.aes = FALSE,
                aes(x = bin, y = y_lab,
                    label = sprintf("%.0f%% none", 100 * pct_zero)),
                size = 3.1, colour = "grey35") +
      scale_fill_manual(values = fills, guide = "none", drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      expand_limits(y = y_lab - 0.06 * max(diff(y_rng), 1e-6)) +
      labs(x = xlab, y = "log(exposure + 1), zones with exposure",
           # city goes on its own line -- appended inline it clips at the panel edge
           subtitle = paste0(
             "Boxes: intensity among zones with measurable exposure. ",
             "Labels: share with none.",
             sprintf("\nPoints: %s.", input$city))) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.x = element_blank())
  })
}

shinyApp(ui, server)
