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
# library(RGraphics)
library(ggimage) # for adding icons
library(shinyalert)

# Load the data
# dropdown_data <- read.csv(here::here("data/dropdowns.csv"), stringsAsFactors = FALSE)

thematic::thematic_shiny()

# TODO - Italic species names in the top ten plots
# TODO - think about if the Depth range and Average depth is from the data or of the park
# TODO - Add stereo-boss deployments to the stats
# TODO - Add a method column in the gs for bruv or boss to calc the deploymnet numbers

# TODO - change the way that the habitat is plotted in the dropdowns - need one selectionfor each habitat type, because can't display them all on a map due to the prediction and error

# Define the theme using bslib ----
theme <- bs_theme(
  bg = "#FFFFFF",  # Background color
  fg = "#000000",  # Foreground color
  primary = "#007BFF",  # Primary color
  secondary = "#6C757D",  # Secondary color
  base_font = font_google("Lato")  # Use Lato font from Google Fonts
)

# Generate 100 random points for the Leaflet map (dummy data) ----
set.seed(123)
dummy_points <- data.frame(
  lat = runif(100, min = -35.2, max = -33.5),  # Latitude range for SW corner of WA
  lng = runif(100, min = 114.5, max = 116.5)   # Longitude range for SW corner of WA
)

# Load data ----
load("data/app/all_data.Rdata")

# FISHNCLIPS
dat <- readRDS("data/fishnclips/dat.RDS") %>%
  dplyr::rename(latitude = latitude_dd, longitude = longitude_dd)

commonwealth.mp <- readRDS("data/fishnclips/commonwealth.mp.RDS")
state.mp <- readRDS("data/fishnclips/state.mp.RDS")
ngari.mp <- readRDS("data/fishnclips/ngari.mp.RDS")

state.pal <- colorFactor(c("#bfaf02", # conservation
                           "#7bbc63", # sanctuary = National Park
                           "#fdb930", # recreation
                           "#b9e6fb", # general use
                           '#ccc1d6' # special purpose
), state.mp$zone)

commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                                  "#7bbc63", # National Park
                                  "#fdb930", # Recreational Use
                                  "#fff7a3", # Habitat Protection
                                  '#b9e6fb', # Multiple Use
                                  '#ccc1d6'# Special Purpose
), commonwealth.mp$zone)

# Make icon for images and videos----
# html_legend <- "<div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>
# <div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>
# <div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>"

html_legend <- "<div style='padding: 0px; padding-bottom: 0px;'><b style='padding-top:0; padding-bottom:10px; margin: 0;'> Marker Legend </b><br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_yellow.png?raw=true'
style='width:30px;height:30px;'> Fish highlights <br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_green.png?raw=true'
style='width:30px;height:30px;'> Habitat imagery (stereo-BRUV)<br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_pink.png?raw=true'
style='width:30px;height:30px;'> Habitat imagery (BOSS)<br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_purple.png?raw=true'
style='width:30px;height:30px;'> 3D models"


n <- 100
data <- data.frame(
  x = seq(1, n),
  y = rpois(n, 100)
)

ggplot_theme <- 
ggplot2::theme_bw() +
  ggplot2::theme( # use theme_get() to see available options
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_blank(),
    # legend.position = "top",
    text = ggplot2::element_text(size = 12),
    strip.text.y = ggplot2::element_text(size = 12, angle = 0),
    axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
    axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
    axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    strip.background = ggplot2::element_blank(),
    
    strip.text = ggplot2::element_text(size = 14, angle = 0),
    
    plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
  )

add_legend <- function(map, colors, labels, sizes, opacity = 1, group, title) { #map, 
  colorAdditions <- glue::glue(
    "{colors}; border-radius: 50%; width:{sizes}px; height:{sizes}px"
  )
  labelAdditions <- glue::glue(
    "<div style='display: inline-block; height: {sizes}px; ",
    "margin-top: 4px;line-height: {sizes}px;'>{labels}</div>"
  )
  
  return(
    leaflet::addLegend(map,
                       colors = colorAdditions,
                       labels = labelAdditions,
                       opacity = opacity,
                       title = title,
                       position = "topright",
                       group = group
    )
  )
}

