library(tidyverse)

# Checking that DURIN species are in the correct plots
spp.durin = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                     na.strings=c("","NA")) |>
  #Filter to just DURIN
  drop_na(DURIN_plot) |>
  # Separate out the plot ID for identifying species
  separate(DURIN_plot, into = c("site.abbrv", "habitat.abbrv", "spp.abbrv", "rep.abbrv")) |>
  # Create abbreviated species codes
  mutate(spp.code = case_when(
    species == "Calluna vulgaris" ~ "CV",
    species == "Vaccinium vitis-idaea" ~ "VV",
    species == "Vaccinium myrtillus" ~ "VM",
    species == "Empetrum nigrum" ~ "EN",
    TRUE ~ "unknown"
  )) |>
  # Filter to problem leaves
  filter(spp.code != spp.abbrv) |>
  # Make it human readable
  relocate(c(spp.code, spp.abbrv), .after = envelope_ID)
