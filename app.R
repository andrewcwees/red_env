# app.R ----------------------------------------------------------------------
# Redlining & Transportation-Sector Environmental Exposure -- interactive map
#
# A work-sample Shiny app built from the redenv research project (Weesner,
# Higashidani, ... Lim; UA). For a selected city it maps present-day
# transportation-sector environmental exposure across neighborhoods graded by
# the 1930s HOLC ("redlining") maps, and shows how exposure tracks HOLC grade.
#
#   * Map        -> a city's HOLC zones colored by exposure (log scale) for a
#                   chosen transportation sector x pollutant.
#   * Equity     -> national exposure distribution across HOLC grades A-D for
#                   that sector x pollutant, with the selected city overlaid.
#   * Value boxes-> Grade D-vs-A exposure ratio, zones mapped, and the anchor
#                   finding (rail is the most stratified sector).
#
# Scope is deliberately hedged (see interactive_display_plan_2026-07-14.md):
# Rail + Road only (Aviation excluded -- mostly-zero), no combined index (CEI),
# descriptive framing only -- no causal claims. This is a work sample, not a
# published result.
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
zattr  <- st_drop_geometry(zones)   # zone grade + demographics, no geometry

# ---- lookups ----------------------------------------------------------------
sectors    <- c("Rail" = "Rail", "Road" = "Road")
pollutants <- c("Noise (LAeq,24)" = "Noise", "NO" = "NO", "NO2" = "NO2",
                "PM2.5" = "PM2.5")
poll_lab   <- c(Noise = "noise", NO = "NO", NO2 = "NO2", PM2.5 = "PM2.5")

grade_levels <- c("A", "B", "C", "D")
# canonical HOLC grade colors (green/blue/yellow/red)
grade_cols <- c(A = "#76a865", B = "#7cb5bd", C = "#ffff00", D = "#d9838d")

# equity chart: group exposure by HOLC grade (default) or a demographic quintile
equity_by <- c("HOLC grade"     = "grade",
               "Median income"  = "inc",
               "% Hispanic"     = "pct_hisp",
               "% Black"        = "pct_blk")
# demographic quintile ramp (low -> high value, yellow -> dark red)
q_ramp <- colorRampPalette(c("#ffffcc", "#800026"))

# city selector choices: "Birmingham, AL" -> "Birmingham, AL"
city_choices <- setNames(cities$place, cities$place)

# anchor finding text from the ranking table
rail_pct <- rank$pct_first[rank$sector == "Rail"]

# ---- UI ---------------------------------------------------------------------
ui <- page_sidebar(
  title = "Redlining & Transportation Exposure",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 330,
    selectizeInput(
      "city", "City", choices = NULL,  # populated server-side (searchable)
      options = list(placeholder = "Type to search 221 cities...",
                     maxOptions = 300)
    ),
    radioButtons("sector", "Transportation sector", choices = sectors,
                 selected = "Rail", inline = TRUE),
    selectInput("poll", "Pollutant / measure", choices = pollutants,
                selected = "Noise"),
    selectInput("eby", "Equity chart: group exposure by", choices = equity_by,
                selected = "grade"),
    hr(),
    helpText(
      "Each polygon is a 1930s HOLC-graded neighborhood. Color shows present-day",
      "transportation-sector environmental exposure on a log scale (darker =",
      "higher). Hover a zone for its grade, exposure, and demographics.",
      "Use the layer control (top-right of the map) to switch between the",
      "exposure and HOLC-grade fill views."
    ),
    helpText(
      HTML("<b>Chart:</b> exposure nationally, grouped by HOLC grade or by a",
           "demographic quintile (income, %&nbsp;Hispanic, %&nbsp;Black), with",
           "the selected city's zones overlaid &mdash; the disparity story.")
    ),
    helpText(HTML(paste(
      "<small>Rail + Road only; aviation and any combined index are excluded as",
      "unreliable. Source: redenv (Weesner &amp; Lim, UA). NEMO emissions (EPA",
      "NEI 2017); BTS noise; HOLC via Mapping Inequality; ACS 2019. Work sample",
      "&mdash; descriptive, not a published or causal result.</small>")))
  ),
  layout_columns(
    fill = FALSE,
    value_box(title = "Grade D vs. A exposure (national)",
              value = textOutput("vb_ratio"), theme = "primary"),
    value_box(title = "Zones mapped in city",
              value = textOutput("vb_n"), theme = "secondary"),
    value_box(title = "Most stratified sector",
              value = textOutput("vb_rail"), theme = "light")
  ),
  card(
    full_screen = TRUE, height = 540,
    card_header(textOutput("map_title", inline = TRUE)),
    leafletOutput("map", height = 500)
  ),
  card(
    height = 400,
    card_header(textOutput("chart_title", inline = TRUE)),
    plotOutput("equity", height = 340)
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # searchable city selector, default to the best-covered city
  default_city <- cities$place[which.max(cities$n_zones)]
  updateSelectizeInput(session, "city", choices = city_choices,
                       selected = default_city, server = TRUE)

  poll_label   <- reactive(poll_lab[[input$poll]])
  sector_label <- reactive(tolower(input$sector))

  # exposure slice for the current sector x pollutant (national)
  exp_sel <- reactive({
    exp[exp$sector == input$sector & exp$pollutant == input$poll,
        c("HOLCID", "xp", "logxp")]
  })

  # national logxp domain -> colors comparable across cities
  pal <- reactive({
    v <- exp_sel()$logxp
    colorNumeric("YlOrRd", domain = v, na.color = "#cccccc")
  })

  # selected city's zones joined to exposure
  city_zones <- reactive({
    req(input$city)
    z <- zones[zones$place == input$city, ]
    left_join(z, exp_sel(), by = "HOLCID")
  })

  output$map_title <- renderText({
    req(input$city)
    sprintf("%s: %s %s exposure by HOLC zone",
            input$city, sector_label(), poll_label())
  })
  eby_label <- reactive(names(equity_by)[equity_by == input$eby])
  output$chart_title <- renderText({
    grp <- if (input$eby == "grade") "HOLC grades"
           else sprintf("%s quintiles", eby_label())
    sprintf("%s %s exposure across %s (national)",
            tools::toTitleCase(sector_label()), poll_label(), grp)
  })

  # ---- value boxes ----
  output$vb_ratio <- renderText({
    e <- merge(exp_sel(),
               as.data.frame(zones)[, c("HOLCID", "grade")], by = "HOLCID")
    mA <- median(e$xp[e$grade == "A"], na.rm = TRUE)
    mD <- median(e$xp[e$grade == "D"], na.rm = TRUE)
    if (is.na(mA) || mA == 0) "n/a" else sprintf("%.2fx", mD / mA)
  })
  output$vb_n   <- renderText(format(nrow(city_zones()), big.mark = ","))
  output$vb_rail <- renderText(sprintf("Rail (%.0f%%)", rail_pct))

  # ---- choropleth ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
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

  observe({
    z  <- city_zones()
    p  <- pal()
    # per-zone grade color as a PLAIN (unnamed) vector -- a named vector
    # serializes to a JS object and every polygon renders as "[object Object]"
    z$grade_col <- unname(grade_cols[as.character(z$grade)])
    labs <- sprintf(
      paste0("<b>HOLC grade %s</b><br/>%s %s exposure: <b>%.3f</b><br/>",
             "Median income: <b>%s</b><br/>%% Hispanic: <b>%s</b> &middot; ",
             "%% Black: <b>%s</b>"),
      z$grade, sector_label(), poll_label(), z$xp,
      dollar(z$inc, accuracy = 1),
      percent(z$pct_hisp, accuracy = 1), percent(z$pct_blk, accuracy = 1)
    ) |> lapply(htmltools::HTML)

    bb <- as.numeric(st_bbox(z))
    leafletProxy("map", data = z) |>
      clearShapes() |> clearControls() |>
      # Exposure layer -- fill choropleth (thin neutral border)
      addPolygons(
        group = "Exposure",
        fillColor = ~p(logxp), fillOpacity = 0.8,
        color = "#888888", weight = 0.4, opacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#333",
                                            bringToFront = TRUE),
        label = labs
      ) |>
      # HOLC grade layer -- grade-colored fill choropleth (thin neutral border
      # so adjacent same-grade zones stay separable)
      addPolygons(
        group = "HOLC grade",
        fillColor = ~grade_col, fillOpacity = 0.8,
        color = "#666666", weight = 0.4, opacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#333",
                                            bringToFront = TRUE),
        label = labs
      ) |>
      # mutually exclusive views -- two fill choropleths can't co-read when
      # stacked, so switch between them (default: Exposure)
      addLayersControl(
        baseGroups = c("Exposure", "HOLC grade"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup("HOLC grade") |>
      addLegend("bottomright", pal = p, values = ~logxp,
                title = "Exposure<br/><small>log(xp+1)</small>", opacity = 0.9,
                className = "info legend legend-exposure") |>
      addLegend("bottomright", colors = unname(grade_cols),
                labels = c("A", "B", "C", "D"),
                title = "HOLC grade", opacity = 1,
                className = "info legend legend-grade") |>
      fitBounds(bb[1], bb[2], bb[3], bb[4])
  })

  # ---- equity chart: national distribution by grade or demographic quintile,
  #      with the selected city's zones overlaid as points --------------------
  output$equity <- renderPlot({
    eb  <- input$eby
    nat <- merge(exp_sel(), zattr, by = "HOLCID")
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

    ggplot(nat, aes(x = bin, y = logxp)) +
      geom_boxplot(aes(fill = bin), outlier.size = 0.5, width = 0.6,
                   alpha = 0.85, colour = "grey30") +
      geom_jitter(data = city, width = 0.15, height = 0, size = 1.6,
                  colour = "#111111", alpha = 0.8) +
      scale_fill_manual(values = fills, guide = "none", drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      labs(x = xlab, y = "log(exposure + 1)",
           subtitle = sprintf("Boxes: all U.S. graded zones. Points: %s.",
                              input$city)) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.x = element_blank())
  })
}

shinyApp(ui, server)
