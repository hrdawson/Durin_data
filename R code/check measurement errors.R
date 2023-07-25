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
           leaf_thickness_3_mm > 0.321*10)

### Weight errors ----
error.durin.weight = durin |>
  # Values pulled from durin.means object
  filter(wet_mass_g > 0.025 *10) |>
  relocate(wet_mass_g, .after = envelope_ID)
