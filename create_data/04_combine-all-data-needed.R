library(readr)
library(dplyr)
library(googlesheets4)
library(stringr)

set.seed(1)


# Read in files created in earlier scripts ----
# Read in metadata ----
metadata_bruv <- read_rds("data/app/metadata.rds") %>%
  dplyr::mutate(method = "stereo-BRUV")

# ---- helper: jitter lon/lat in a park-safe way ----
jitter_coords <- function(df, lon_col = "longitude_dd", lat_col = "latitude_dd",
                          lon_sd = 0.02, lat_sd = 0.02) {
  df %>%
    mutate(
      "{lon_col}" := .data[[lon_col]] + rnorm(n(), 0, lon_sd),
      "{lat_col}" := .data[[lat_col]] + rnorm(n(), 0, lat_sd)
    )
}

# ---- pick which parks get which methods ----
parks <- sort(unique(metadata_bruv$marine_park))

n_all3 <- round(length(parks) * 0.35)
n_two  <- round(length(parks) * 0.35)

parks_all3 <- sample(parks, n_all3)
parks_remaining <- setdiff(parks, parks_all3)
parks_two <- sample(parks_remaining, n_two)
parks_one <- setdiff(parks, c(parks_all3, parks_two))

two_method_extra <- tibble(
  marine_park = parks_two,
  extra = sample(c("UVC", "stereo-ROV"), length(parks_two), replace = TRUE)
)

parks_with_uvc <- c(parks_all3, two_method_extra$marine_park[two_method_extra$extra == "UVC"])
parks_with_rov <- c(parks_all3, two_method_extra$marine_park[two_method_extra$extra == "stereo-ROV"])

# ---- per-park sample sizes ----
park_sizes <- tibble(
  marine_park = parks,
  n_bruv = sample(30:120, length(parks), replace = TRUE),
  n_uvc  = sample(10:80,  length(parks), replace = TRUE),
  n_rov  = sample(5:50,   length(parks), replace = TRUE)
)

# ---- subsample helper that supports per-group n ----
subsample_by_group_n <- function(df, n_df, n_col) {
  df %>%
    left_join(n_df %>% select(marine_park, keep_n = all_of(n_col)), by = "marine_park") %>%
    group_by(marine_park) %>%
    group_modify(~ slice_sample(.x, n = min(nrow(.x), .y$keep_n[1]))) %>%
    ungroup() %>%
    select(-keep_n)
}

# ---- BRUV base (subsampled) ----
metadata_bruv_sub <- subsample_by_group_n(metadata_bruv, park_sizes, "n_bruv")

# ---- clone + subsample + jitter for a method ----
make_method_clone <- function(df_source, method_name, size_col, lon_sd, lat_sd) {
  subsample_by_group_n(df_source, park_sizes, size_col) %>%
    mutate(method = method_name) %>%
    jitter_coords(lon_sd = lon_sd, lat_sd = lat_sd)
}

metadata_uvc_all <- make_method_clone(
  df_source = metadata_bruv,
  method_name = "UVC",
  size_col = "n_uvc",
  lon_sd = 0.01, lat_sd = 0.01
)

metadata_rov_all <- make_method_clone(
  df_source = metadata_bruv,
  method_name = "stereo-ROV",
  size_col = "n_rov",
  lon_sd = 0.015, lat_sd = 0.015
)

# ---- keep only parks that “have” those methods ----
metadata_uvc_sub <- metadata_uvc_all %>% filter(marine_park %in% parks_with_uvc)
metadata_rov_sub <- metadata_rov_all %>% filter(marine_park %in% parks_with_rov)

# ---- final dummy metadata ----
metadata <- bind_rows(
  metadata_bruv_sub,
  metadata_uvc_sub,
  metadata_rov_sub
) %>%
  filter(!is.na(longitude_dd), !is.na(latitude_dd)) %>%
  mutate(
    method = case_when(
      str_detect(tolower(method), "bruv") ~ "stereo-BRUV",
      str_detect(tolower(method), "uvc")  ~ "UVC",
      str_detect(tolower(method), "rov")  ~ "stereo-ROV",
      TRUE ~ "Other"
    )
  ) %>%
  dplyr::filter(!(marine_park %in% "Geographe Marine Park" & method %in% "stereo-ROV"))

# sanity check: parks with 1/2/3 methods
print(
  metadata %>%
    distinct(marine_park, method) %>%
    count(marine_park, name = "n_methods") %>%
    count(n_methods)
)

# Read in raster data ----
raster_data <- read_rds("data/app/raster_data.rds")

# Read in basic stats ----
stats <- read_rds("data/app/stats.RDS")
top_species <- read_rds("data/app/top_species.RDS") %>%
  dplyr::filter(ecosystem_component %in% "Demersal fish") %>%
  dplyr::mutate(method = "stereo-BRUV")

bubble_data <- read_rds("data/app/bubble_data.RDS")
synthesis_metadata <- read_rds("data/app/synthesis_metadata.RDS")
metric_bubble_data <- read_rds("data/app/metric_bubble_data.RDS")
temporal_data <- read_rds("data/app/temporal_data.RDS")

# Read in length data for histograms ----
length_combined <- read_rds("data/app/length_combined.RDS")

# Read in dropdown information ----
dropdown_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "dropdowns")
2

# Fishes of Australia codes ----

dbca_googlesheet_url <- "https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?usp=sharing"

foa_species_codes <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "fishes_of_australia") %>%
  CheckEM::clean_names() %>%
  dplyr::select(-c(number)) %>%
  dplyr::mutate(species = case_when(
    genus %in% "Ophthalmolepis" & species %in% "lineolata" ~ "lineolatus",
    .default = as.character(species)
  )) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  dplyr::select(display_name, url)

foa_genus_codes <- readRDS("data/app/genus_foa_codes.RDS") %>%
  dplyr::mutate(species = "spp") %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  dplyr::select(display_name, url)

foa_codes <- bind_rows(foa_species_codes, foa_genus_codes) %>%
  dplyr::select(display_name, url) 

# Get method data source----
method_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                          sheet = "simplified_dummy_data") %>%
  dplyr::distinct(ecosystem_condition, method, network, marine_park_or_area)

# Read in network information ----
# networks_and_parks <- read_csv("data/app/networks-and-parks.csv")

# # Read in summary data (temp) ----
# summary_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
#                             sheet = "summary_data")

# Read in data for text ----
text_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                        sheet = "simplified_text_data")

# read in condition plot information ----
# Define the folder path containing the .rds files for the condition plots
folder_path <- "plots/condition"

# Get the list of .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)

# Function to extract "marine-park", "metric", and "years" from filename
extract_file_info <- function(filename) {
  parts <- strsplit(tools::file_path_sans_ext(basename(filename)), "_")[[1]]
  list(network = parts[1], marine_park = parts[2], metric = parts[3], years = parts[4])
}

# Create a dataframe containing file information
file_info <- do.call(rbind, lapply(rds_files, function(f) {
  info <- extract_file_info(f)
  data.frame(file = f, network = info$network, marine_park = info$marine_park, metric = info$metric, years = info$years,
             stringsAsFactors = FALSE)
})) %>%
  dplyr::mutate(file = stringr::str_replace_all(file, "inst/shiny/amp-dashboard/","")) #%>%
  #dplyr::filter(marine_park %in% c("Geographe Marine Park", 
 #                                  "Ningaloo Marine Park", 
  #                                 "South-west Corner Marine Park (Western Arm)"))

# read in temporal plot information ----
# For the temporal plots
# Define the folder path containing the .rds files for the condition plots
folder_path <- "plots/temporal"

# Get the list of .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Function to extract "marine-park", "metric", and "years" from filename
extract_file_info <- function(filename) {
  parts <- strsplit(tools::file_path_sans_ext(basename(filename)), "_")[[1]]
  list(network = parts[1], marine_park = parts[2], metric = parts[3], depth_classes = parts[4])
}

# Create a dataframe containing file information
temporal_file_info <- do.call(rbind, lapply(rds_files, function(f) {
  info <- extract_file_info(f)
  data.frame(file = f, network = info$network, marine_park = info$marine_park, metric = info$metric, depth_classes = info$depth_classes,
             stringsAsFactors = FALSE)
})) %>%
  dplyr::mutate(file = stringr::str_replace_all(file, "inst/shiny/amp-dashboard/",""))


foa_codes <- data.table::data.table(foa_codes)

# Read in dummy trend data from henry's google sheet ----
henry_url <- "https://docs.google.com/spreadsheets/d/1PG3q_5GqesA500j0h0xdizqYPlmXr8hQMnEX-z0Vvjc/edit?usp=sharing"

trend_data <- read_sheet(henry_url, "write.demersal.fish.metrics")
trend_data_fish <- read_sheet(henry_url, "write.demersal.fish.species")
raster_tags <- read_sheet(henry_url, "raster_tags")

# Combine all information together -----
all_data <- structure(
  list(
    # networks_and_parks = networks_and_parks,
    file_info = file_info,
    temporal_file_info = temporal_file_info,
    metadata = metadata,
    dropdown_data = dropdown_data,
    raster_data = raster_data,
    # summary_data = summary_data,
    text_data = text_data,
    method_data = method_data,
    stats = stats,
    top_species = top_species,
    bubble_data = bubble_data,
    metric_bubble_data = metric_bubble_data,
    synthesis_metadata = synthesis_metadata,
    temporal_data = temporal_data,
    foa_codes = foa_codes,
    length_combined = length_combined,
    trend_data = trend_data,
    trend_data_fish = trend_data_fish,
    raster_tags = raster_tags
  ),
  class = "data"
)

# Save ----
save(all_data, file = here::here("data/app/all_data.Rdata"))

