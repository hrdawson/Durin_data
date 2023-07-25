# DURIN checks ----
## Visualize data to see probable errors ----
durin = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                             na.strings=c("","NA")) |>
  # Change plant height to numeric
  mutate(plant_height = as.numeric(plant_height))

# Plot all variables against each other
library(GGally)

ggpairs(durin |> select(siteID, species, habitat, plant_nr, leaf_nr, leaf_age, plant_height, bulk_nr_leaves, wet_mass_g, leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm))

png("visualizations/durin_allvariables.png", width = 24, height = 24, units = "in", res = 300)
dev.off()

## Hunt down errors ----
### Calculate means ----
library(rstatix)
durin.means = durin |>
  select(plant_height:leaf_thickness_3_mm) |>
  get_summary_stats(type = "mean_ci")

### Thickness errors ----
error.durin.thickness = durin |>
  # Values pulled from durin.means object
  filter(leaf_thickness_1_mm > 0.278*10 |
           leaf_thickness_2_mm > 0.280*10 |
           leaf_thickness_3_mm > 0.321*10 |
           leaf_thickness_1_mm < 0.278/10 |
           leaf_thickness_2_mm < 0.280/10 |
           leaf_thickness_3_mm < 0.321/10) |>
  relocate(c(leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm), .after = envelope_ID)

### Weight errors ----
error.durin.weight = durin |>
  # Values pulled from durin.means object
  filter(wet_mass_g > 0.025 *10 |
           wet_mass_g < 0.025/10) |>
  relocate(wet_mass_g, .after = envelope_ID)

### Height errors ----
error.durin.height = durin |>
  # Values pulled from durin.means object
  filter(plant_height > 17.258 * 5 |
           plant_height < 17.258/10) |>
  relocate(plant_height, .after = envelope_ID)

### Missing bulk numbers (to be counted on scan) ----
error.durin.bulk = durin |>
  # Values pulled from durin.means object
  filter(is.na(bulk_nr_leaves) &
           species %in% c("Empetrum nigrum", "Calluna vulgaris")) |>
  relocate(bulk_nr_leaves, .after = envelope_ID)

write.csv(error.durin.bulk, "output/error.durin.bulk.csv")

# Make temporary object without erroneous leaves ----
library(tidylog)

list.nobulk = as.list(error.durin.bulk$envelope_ID)

durin.noerrors = durin |>
  # Filter out measurement errors
  filter(!envelope_ID %in% c("AUX7373", "BSD3874", "CVP9320", "DDZ3156", "DEN0101",
                             "AFH1727", "ARK3594", "ERV2714", "AYX2273", "BPF4529",
                             "CSP7326", "DSD6681", "AOQ0411")) |>
  # Filter out treatment errors
  filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
                             "CMH5663", "DAI1197", "BHR0925", "AUZ1311", "BOW7206",
                             "DBV0943", "CWZ4784", "EDV5508", "EDR6459", "AEG7270",
                             "EEN3300")) |>
  # Filter out missing bulk leaves
  filter(!envelope_ID %in% list.nobulk) |>
  # Correct the spelling of Senja
  mutate(siteID = replace(siteID, siteID == "Senje", "Senja")) |>
  # Scale for bulk leaves
  mutate(wet_mass_g.avg = case_when(
    species == "Calluna vulgaris" ~ wet_mass_g/bulk_nr_leaves,
    species == "Empetrum nigrum" ~ wet_mass_g/bulk_nr_leaves,
    TRUE ~ wet_mass_g
  ))
