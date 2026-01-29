library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(dplyr)
library(tibble)
library(shinybusy)
library(shinyjs)
library(viridisLite)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(data.table)
library(leaflet.extras2)
library(shinyWidgets)
library(ggforce)
library(ggimage)
library(shinyalert)
library(sf)
library(htmltools)
library(ggtext)
library(tidytext)
library(later)
# thematic::thematic_shiny()

# ---- Theme ----
theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0B417C",
  secondary = "#0F5AAB",
  base_font = font_google("Lato")
)

# ---- Load data ----
load("data/app/all_data.Rdata")

# Dropdown data
df <- all_data$metadata %>%
  distinct(marine_park, network)

# FishNClips data
dat <- readRDS("data/fishnclips/dat.RDS") %>%
  dplyr::rename(latitude = latitude_dd, longitude = longitude_dd)

commonwealth.mp <- readRDS("data/fishnclips/commonwealth.mp.RDS")
state.mp <- readRDS("data/fishnclips/state.mp.RDS")
ngari.mp <- readRDS("data/fishnclips/ngari.mp.RDS")

state.pal <- colorFactor(
  c("#bfaf02", "#7bbc63", "#fdb930", "#b9e6fb", "#ccc1d6"),
  state.mp$zone
)

commonwealth.pal <- colorFactor(
  c("#f6c1d9", "#7bbc63", "#fdb930", "#fff7a3", "#b9e6fb", "#ccc1d6"),
  commonwealth.mp$zone
)

zone_cols <- c(
  "Habitat Protection Zone" = "#F1E189",
  "Multiple Use Zone"       = "#9CC9E6",
  "National Park Zone"      = "#6DBD6D",
  "Special Purpose Zone"    = "#4F88C6"
)

# ---- Method colours ----
method_cols <- c(
  "stereo-BRUV" = "#2E8B57",
  "UVC"         = "#F39C12",
  "stereo-ROV"  = "#8E44AD",
  "Other"       = "#7f8c8d"
)


method_to_id <- function(method) {
  # stable, valid outputId (letters/numbers/_ only)
  paste0(
    "assemblage_map_",
    gsub("[^a-z0-9]+", "_", tolower(method))
  )
}

# ---- Base map ----
base_map <- function(max_zoom = 18) {
  leaflet() |>
    addTiles(options = tileOptions(minZoom = 4, maxZoom = max_zoom)) |>
    addMapPane("polys",  zIndex = 410) |>
    addMapPane("points", zIndex = 420) |>
    addPolygons(
      data = commonwealth.mp,
      color = "black", weight = 1,
      fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
      group = "Australian Marine Parks",
      popup = ~ZoneName,
      options = pathOptions(pane = "polys")
    ) |>
    addLegend(
      pal = commonwealth.pal,
      values = commonwealth.mp$zone,
      opacity = 1,
      title = "Australian Marine Park Zones",
      position = "bottomleft",
      group = "Australian Marine Parks"
    ) |>
    addLayersControl(
      baseGroups = c("World Imagery", "Open Street Map"),
      overlayGroups = c("Australian Marine Parks", "Sampling locations"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topright"
    )
}

# ---- Method legend HTML builder (single container, updated each time) ----
render_method_legend_html <- function(methods_present) {
  methods_present <- methods_present[methods_present %in% names(method_cols)]
  cols <- unname(method_cols[methods_present])
  
  items <- paste0(
    "<div style='display:flex;align-items:center;margin:2px 0;'>",
    "<span style='width:12px;height:12px;background:", cols,
    ";display:inline-block;margin-right:6px;border:1px solid rgba(255,255,255,0.6)'></span>",
    methods_present,
    "</div>",
    collapse = ""
  )
  
  paste0(
    "<div class='leaflet-control leaflet-bar' style='background:white;padding:8px 10px;border-radius:6px;'>",
    "<div style='font-weight:700;margin-bottom:6px;'>Method</div>",
    items,
    "</div>"
  )
}

# TODO - make this the real data
park_method_summary <- all_data$metadata %>%
  mutate(
    method = case_when(
      str_detect(tolower(method), "bruv") ~ "stereo-BRUV",
      str_detect(tolower(method), "uvc")  ~ "UVC",
      str_detect(tolower(method), "rov")  ~ "stereo-ROV",
      TRUE ~ "Other"
    )
  ) %>%
  filter(method %in% c("stereo-BRUV", "UVC", "stereo-ROV")) %>%
  count(marine_park, method, name = "deployments") %>%
  mutate(
    fish_counted = 0,
    fish_species = 0,
    other_species = 0,
    length_measurements = 0,
    years_min = 2010, years_max = 2024,
    depth_min_m = 0, depth_max_m = 50,
    avg_depth_m = 25,
    deployments_with_benthos = 0,
    deployments_with_relief = 0,
    synthesis_id = "demo"
  )

# ---- 1) pick/standardise the columns we need ----
prep_effort_df <- function(metadata) {
  metadata %>%
    mutate(
      year = suppressWarnings(as.integer(stringr::str_sub(as.character(date_time), 1, 4))),
      method = factor(method, levels = c("stereo-BRUV", "UVC", "stereo-ROV"))
    ) %>%
    filter(method %in% levels(method), !is.na(year))
}

t <- prep_effort_df(all_data$metadata)
names(t) %>% sort()

ggplot_theme_base <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 14),
    axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
  )

ggplot_theme_md <- function() {
  ggplot_theme_base %+replace%
    ggplot2::theme(
      axis.text.y = ggtext::element_markdown(size = 12, lineheight = 1.1)
    )
}

spinnerPlotOutput <- function(outputId, ...) {
  withSpinner(
    plotOutput(outputId, ...),
    type = 6
  )
}