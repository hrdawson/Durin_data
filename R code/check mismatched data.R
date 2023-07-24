library(tidyverse)

# DURIN ----
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

write.csv(spp.durin, "output/2023.07.24_checks_spp.durin.csv")

# Check that DURIN habitats have been correctly assigned
habitat.durin = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                     na.strings=c("","NA")) |>
  #Filter to just DURIN
  drop_na(DURIN_plot) |>
  # Separate out the plot ID for identifying species
  separate(DURIN_plot, into = c("site.abbrv", "habitat.abbrv", "spp.abbrv", "rep.abbrv")) |>
  # Create abbreviated species codes
  mutate(habitat.assigned = case_when(
    habitat == "Open" ~ "O",
    species == "Forested" ~ "F",
    TRUE ~ "unknown"
  )) |>
  # Filter to problem leaves
  filter(habitat.assigned != habitat.abbrv) |>
  # Make it human readable
  relocate(c(habitat.assigned, habitat.abbrv), .after = envelope_ID)

write.csv(habitat.durin, "output/2023.07.24_checks_habitat.durin.csv")

# DroughtNet ----
DN.metadata = read.csv("output/DroughtNet plot metadata.csv") |>
  select(-c(X)) |>
  rename(DroughtTrt.correct = DroughtTrt)

age.DN = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                     na.strings=c("","NA")) |>
  #Filter to just DroughtNet
  drop_na(DroughNet_plotID) |>
  # Manually create verified age class
  # NOTE this is incorrect for a handful of recollected plants:
  # PIO 3.2 CV, PIO 3.1 CV (x2)
  mutate(ageClass.correct = case_when(
    day == 5 ~ "Pioneer",
    day == 3 ~ "Mature"
  )) |>
  # Add verified metadata
  left_join(DN.metadata) |>
  filter(ageClass != ageClass.correct) |>
  relocate(c(day, month, ageClass, ageClass.correct, DroughtTrt, DroughtTrt.correct), .after = envelope_ID)
