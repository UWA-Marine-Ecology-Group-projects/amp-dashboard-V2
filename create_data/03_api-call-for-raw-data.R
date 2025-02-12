library(readr)
library(dplyr)
library(googlesheets4)
library(stringr)
library(httr)
library(jsonlite)
library(purrr)
library(tidyr)
library(sf)

# Read in list of synthesis datasets ----
synth_datasets <- read_sheet("https://docs.google.com/spreadsheets/d/127e_ccNKWdMskgvk2URs9KkMUCrDIzcY4O2cRSd3u1g/edit?gid=0#gid=0",
                         sheet = "synthesis_datasets") %>%
  dplyr::filter(!is.na(synthesis_id)) %>%
  dplyr::filter(uploaded_to_ga %in% "Yes")

2

# Set GA token
# CheckEM::ga_api_set_token()

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

metadata_temp <- data.frame()
count_temp <- data.frame()
length_temp <- data.frame()

for(synthesis_id in unique(synth_datasets$synthesis_id)){
  
  CheckEM::ga_api_all_data(
    synthesis_id = as.character(synthesis_id),
    token = token,
    dir = paste0("data/raw/", synthesis_id, "/"),
    include_zeros = FALSE
  )
  
  metadata <- metadata %>% 
    dplyr::mutate(synthesis_id = synthesis_id)
  
  count <- count %>% 
    dplyr::mutate(synthesis_id = synthesis_id)
  
  length <- length %>% 
    dplyr::mutate(synthesis_id = synthesis_id)
  
  metadata_temp <- bind_rows(metadata_temp, metadata)
  count_temp <- bind_rows(count_temp, count)
  length_temp <- bind_rows(length_temp, length)
  
}

metadata_combined <- metadata_temp %>%
  dplyr::glimpse()

# Calculate campaign mid points to get date
campaign_dates <- metadata_combined %>%
  dplyr::group_by(synthesis_id, campaignid) %>%
  dplyr::summarise(date_midpoint = mean(date_time))

count_combined <- count_temp %>%
  dplyr::glimpse()

length_combined <- length_temp %>%
  dplyr::glimpse()

# Caluclate stats ----
total_number <- count_combined %>%
  dplyr::filter(count > 0) %>%
  dplyr::group_by(synthesis_id) %>%
  dplyr::summarise(fish_counted = sum(count))

species_richness <- count_combined %>%
  dplyr::filter(count > 0) %>%
  dplyr::distinct(synthesis_id, family, genus, species) %>%
  count(synthesis_id, name = "fish_species")

number_deployments <- metadata_combined %>%
  count(synthesis_id, name = "bruvs_deployed")

stats <- left_join(total_number, species_richness) %>%
  left_join(number_deployments) %>%
  pivot_longer(!synthesis_id, names_to = "metric", values_to = "value") %>%
  left_join(synth_datasets) %>%
  glimpse()

# Calculate abundance per species ----
top_species <- count_combined %>%
  dplyr::group_by(synthesis_id, family, genus, species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::select(synthesis_id, scientific_name, australian_common_name, total_number) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  left_join(synth_datasets) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(synthesis_id) %>%
  dplyr::slice_max(order_by = total_number, n = 10)

# Format data for leaflet maps ----
bubble_data <- count_combined %>%
  dplyr::left_join(metadata_combined) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::select(synthesis_id, sample_url, family, genus, species, count, scientific_name, australian_common_name) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  left_join(synth_datasets) %>%
  dplyr::glimpse()

names(bubble_data)

metric_bubble_data <- count_combined %>%
  dplyr::left_join(metadata_combined) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::select(synthesis_id, sample_url, family, genus, species, count, longitude_dd, latitude_dd, date_time, depth_m, status, successful_count, scientific_name, australian_common_name) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  dplyr::group_by(synthesis_id, sample_url) %>%
  dplyr::summarise(total_abundance = sum(count, na.rm = TRUE),
                   species_richness = n_distinct(family, genus, species)) %>%
  full_join(metadata_combined) %>%
  replace_na(list(total_abundance = 0, species_richness = 0)) %>%
  dplyr::select(synthesis_id, sample_url, total_abundance, species_richness) %>%
  pivot_longer(!c(synthesis_id, sample_url), names_to = "metric", values_to = "value") %>%
  dplyr::left_join(metadata_combined) %>%
  left_join(synth_datasets) %>%
  glimpse()
  
# Summarised data for temporal plots----
# TODO come back to this



# 1. Convert metadata_combined to an sf object (assuming lat/lon in WGS84)
metadata_sf <- metadata_combined %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)  # EPSG:4326 for WGS84

# 2. Read the shapefile
marine_parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp")

# 3. Ensure the CRS matches (convert marine_parks if needed)
if (st_crs(marine_parks) != st_crs(metadata_sf)) {
  marine_parks <- st_transform(marine_parks, st_crs(metadata_sf))
}

# 4. Perform the spatial join (left join to keep all points)
metadata_with_zone <- st_join(metadata_sf, marine_parks, left = TRUE) %>%
  st_drop_geometry() %>%
  dplyr::mutate(depth_class = cut(
    depth_m,
    breaks = c(-Inf, 30, 70, 200, Inf),
    labels = c("0-30 m", "30-70 m", "70-200 m", "200 m +"),
    right = FALSE  # Ensures 30 is included in "30-70", etc.
  ))

# Get all unique species within each synthesis
species_per_synthesis <- count_combined %>%
  dplyr::distinct(synthesis_id, family, genus, species)

# Get all unique samples (each sample belongs to only one synthesis)
samples_per_synthesis <- metadata_with_zone %>%
  dplyr::distinct(synthesis_id, sample_url)

# Expand to ensure every sample in a synthesis gets all species observed in that synthesis
temporal_data <- samples_per_synthesis %>%
  dplyr::left_join(species_per_synthesis, by = "synthesis_id") %>%
  dplyr::left_join(count_combined, by = c("synthesis_id", "sample_url", "family", "genus", "species")) %>%
  dplyr::mutate(count = replace_na(count, 0))  %>% # Fill missing counts with 0
  dplyr::left_join(metadata_with_zone) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  dplyr::select(synthesis_id, campaignid, sample_url, family, genus, species, count, zone, depth_class, display_name) %>%
  dplyr::group_by(synthesis_id, campaignid, family, genus, species, zone, depth_class, display_name) %>%
  dplyr::summarise(average_abundance = mean(count)) %>%
  left_join(campaign_dates)%>%
  left_join(synth_datasets)
  
# Heteroscarus acroptilus

# 1. for each synthesis choose mid point off campaign as the date
# 2. overlay zoning
# 3. Summarise depth classes
# 4. temporal plot (probs on server
# 5. make sure that zeros are included

# Metadata to add zeros=----

synthesis_metadata <- metadata_combined %>%
  left_join(synth_datasets)

# Save dataframes for app ----
saveRDS(stats, file = here::here("data/app/stats.RDS"))
saveRDS(top_species, file = here::here("data/app/top_species.RDS"))
saveRDS(bubble_data, file = here::here("data/app/bubble_data.RDS"))
saveRDS(synthesis_metadata, file = here::here("data/app/synthesis_metadata.RDS"))
saveRDS(metric_bubble_data, file = here::here("data/app/metric_bubble_data.RDS"))
saveRDS(temporal_data, file = here::here("data/app/temporal_data.RDS"))
