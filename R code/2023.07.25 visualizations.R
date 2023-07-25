# Visualize the basic variables against each other

durin.viz = durin.noerrors |>
  # DURIN plots only
  drop_na(DURIN_plot) |>
  # Select DURIN only columns
  select(envelope_ID:DURIN_plot, plant_nr:leaf_thickness_3_mm) %>%
  # Tidy in long form
  pivot_longer(cols = wet_mass_g:leaf_thickness_3_mm, names_to = "trait", values_to = "value") %>%
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness"))
