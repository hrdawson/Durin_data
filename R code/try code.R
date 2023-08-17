# Read in TRY data ----
trydata = read.csv("raw_data/TRY/28371.csv") |>
  filter(TraitID %in% c(1, 3110, 3112, 3114, 11, 3116, 3117, 3106)) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    TraitID %in% c(1, 3110, 3112, 3114) ~ "leaf_area",
    TraitID %in% c(11, 3116, 3117) ~ "SLA",
    TraitID == 3106 ~ "plant_height",
    TRUE ~ "Unknown"
  ))

table(trydata$TraitName)
table(trydata$DataName)

# Read in DURIN data ----
durin.subset.try = read.csv("output/2023.08.16_cleanDURIN.csv") %>%
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  filter(siteID == "Lygra") |>
  filter(project == "Field - Traits") |>
  filter(DroughtTrt %in% c(NA, "Amb (0)"))

