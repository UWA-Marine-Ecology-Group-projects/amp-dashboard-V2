library(readr)
library(dplyr)
library(googlesheets4)

# # Read in metadata ----
# meg_labsheets_bruvs <- read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing",
#                                   sheet = "BRUVs CampaignTrack") %>%
#   dplyr::filter(!is.na(network)) %>%
#   dplyr::select(campaignid, network, marine_park)
# 
# 2
# 
# temp_metadata <- data.frame()
# 
# for(campaign in unique(meg_labsheets_bruvs$campaignid)){
# 
#   print(campaign)
# 
#   campaign_metadata <- read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing",
#                                   sheet = campaign) %>%
#     mutate(across(everything(), as.character)) %>%
#     dplyr::mutate(campaignid = campaign)
# 
#   temp_metadata <- bind_rows(temp_metadata, campaign_metadata)
# 
# }
# 
# fish_metadata <- temp_metadata %>%
#   dplyr::select(campaignid, opcode, latitude_dd, longitude_dd, depth_m, date_time) %>%
#   dplyr::left_join(meg_labsheets_bruvs) %>%
#   dplyr::mutate(latitude_dd = as.numeric(latitude_dd),
#                 longitude_dd = as.numeric(longitude_dd)) %>%
#   dplyr::filter(!is.na(latitude_dd)) %>%
#   dplyr::mutate(ecosystem_component = "Demersal fish") %>%
#   dplyr::mutate(depth_m = as.numeric(depth_m))
# 
# saveRDS(fish_metadata, file = here::here("data/app/fish_metadata.RDS"))

fish_metadata <- read_rds(here::here("data/app/fish_metadata.RDS"))

fake_lobster <- tibble(
  campaignid = rep("ABR2025", 10),
  opcode = sprintf("OP%03d", 1:10),
  latitude_dd = round(runif(10, -28.8, -28.3), 5),
  longitude_dd = round(runif(10, 113.6, 114.1), 5),
  depth_m = sample(5:60, 10, replace = TRUE),
  date_time = "2025-03-01T08:00:00+08:00"
)

# TODO fix why the fish_metadata now doesm't have date_time
lobster_metadata <- read_rds(here::here("data/app/lobster.RDS")) %>%
  dplyr::mutate(date_time = as.character(date_time)) %>%
  dplyr::mutate(ecosystem_component = "Mobile macro invertebrates") %>%
  dplyr::mutate(network = "South-west") %>%
  dplyr::mutate(marine_park = "Abrolhos Marine Park")

final_metadata <- bind_rows(fish_metadata, lobster_metadata)

saveRDS(final_metadata, file = here::here("data/app/metadata.RDS"))
