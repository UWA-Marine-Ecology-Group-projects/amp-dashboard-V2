library(readr)
library(dplyr)
library(googlesheets4)
library(stringr)
library(httr)
library(jsonlite)
library(purrr)
library(tidyr)

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
  dplyr::select(synthesis_id, family, genus, species, count, longitude_dd, latitude_dd, date_time, depth_m, status, successful_count, scientific_name, australian_common_name) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  left_join(synth_datasets) %>%
  dplyr::glimpse()

# Summarised data for temporal plots----
# TODO come back to this

# Save dataframes for app ----
saveRDS(stats, file = here::here("data/app/stats.RDS"))
saveRDS(top_species, file = here::here("data/app/top_species.RDS"))
saveRDS(bubble_data, file = here::here("data/app/bubble_data.RDS"))
