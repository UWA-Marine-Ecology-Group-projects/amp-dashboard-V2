library(readr)
library(dplyr)
library(googlesheets4)
library(stringr)
library(httr)
library(jsonlite)

# Read in rasters and tags ----
raster_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1BJLDy9pCjXSdFIJ-xczRC9Wj3kBkYLYnHnPczWoX9Eo/edit?usp=sharing",
                         sheet = "raster_tags") %>%
  dplyr::filter(!is.na(tile_service_url)) %>%
  dplyr::mutate(metric = dashboard_metric)

# Get raster min and max values ----
CheckEM::ga_api_set_token()

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# URL for the API endpoint
url <- paste0("https://dev.globalarchive.org/api/data/SynthesisRasterFile/")

# Include the token in the request headers
headers <- add_headers(Authorization = paste("Token", token))

# Send GET request with token-based authentication
response <- GET(url, headers)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content
  data <- content(response, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(data, flatten = TRUE)
  
  # View the parsed data
  print(parsed_data)
} else {
  # Print error message
  stop("Failed to fetch data: ", status_code(response))
}

raster_min_max <- parsed_data$results %>%
  dplyr::rename(tile_service_url = tiles_url_template) %>%
  glimpse

raster_data <- left_join(raster_raw, raster_min_max) %>%
  dplyr::mutate(tile_service_url = if_else(estimate %in% "Error", str_replace_all(tile_service_url, "viridis", "plasma"), tile_service_url)) %>%
  dplyr::mutate(tile_service_url = if_else(!estimate %in% "Error", str_replace_all(tile_service_url, "viridis", "jet"), tile_service_url)) %>%
  dplyr::rename(min = min_value, max = max_value) %>%
  dplyr::filter(!is.na(min) | !is.na(max)) %>%
  glimpse

saveRDS(raster_data, file = here::here("data/app/raster_data.RDS"))
