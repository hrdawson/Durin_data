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
                         "leaf_thickness")) |>
  # Factor the sites
  mutate(siteID = factor(siteID, levels = c("Lygra", "Sogndal", "Senja", "Kautokeino")))

# visualize data ----
library(ggh4x)

ggplot(durin.viz %>% filter(leaf_age != "" & habitat != ""),
       aes(interaction(habitat, siteID), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "") +
  theme_bw()
