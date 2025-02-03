library(readr)
library(dplyr)
library(googlesheets4)
library(stringr)

# Read in files created in earlier scripts ----
# Read in metadata ----
metadata <- read_rds("data/metadata.rds")

# Read in raster data ----
raster_data <- read_rds("data/raster_data.rds")

# Read in dropdown information ----
dropdown_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "dropdowns")
2

# Get method data source----
method_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                          sheet = "simplified_dummy_data") %>%
  dplyr::distinct(ecosystem_condition, method, network, marine_park_or_area)

# Read in network information ----
networks_and_parks <- read_csv("data/networks-and-parks.csv")

# Read in summary data (temp) ----
summary_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                            sheet = "summary_data")

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
  dplyr::mutate(file = stringr::str_replace_all(file, "inst/shiny/amp-dashboard/",""))

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


# Combine all information together -----

all_data <- structure(
  list(
    networks_and_parks = networks_and_parks,
    file_info = file_info,
    temporal_file_info = temporal_file_info,
    metadata = metadata,
    dropdown_data = dropdown_data,
    raster_data = raster_data,
    summary_data = summary_data,
    text_data = text_data,
    method_data = method_data
  ),
  class = "data"
)

# Save ----
save(all_data, file = here::here("data/all_data.Rdata"))
