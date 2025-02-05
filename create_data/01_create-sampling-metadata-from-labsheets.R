library(readr)
library(dplyr)
library(googlesheets4)

# Read in metadata ----
meg_labsheets_bruvs <- read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing",
                                  sheet = "BRUVs CampaignTrack") %>%
  dplyr::filter(!is.na(network)) %>%
  dplyr::select(campaignid, network, marine_park)

temp_metadata <- data.frame()

for(campaign in unique(meg_labsheets_bruvs$campaignid)){
  
  print(campaign)
  
  campaign_metadata <- read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing",
                                  sheet = campaign) %>%
    mutate(across(everything(), as.character)) %>%
    dplyr::mutate(campaignid = campaign)
  
  temp_metadata <- bind_rows(temp_metadata, campaign_metadata)
  
}

metadata <- temp_metadata %>%
  dplyr::select(campaignid, opcode, latitude_dd, longitude_dd, depth_m, date_time) %>%
  dplyr::left_join(meg_labsheets_bruvs) %>%
  dplyr::mutate(latitude_dd = as.numeric(latitude_dd),
                longitude_dd = as.numeric(longitude_dd)) %>%
  dplyr::filter(!is.na(latitude_dd))

saveRDS(metadata, file = here::here("data/app/metadata.RDS"))
