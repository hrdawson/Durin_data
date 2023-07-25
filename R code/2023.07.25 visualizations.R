# Visualize the basic variables against each other

durin.viz = durin.noerrors |>
  # DURIN plots only
  drop_na(DURIN_plot) |>
  select(-bulk_nr_leaves) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") %>%
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

# DroughtNet----
DN.viz = durin.noerrors |>
  # DURIN plots only
  drop_na(DroughNet_plotID) |>
  # Select DroughtNet only columns
  select(envelope_ID:project, ageClass:leaf_thickness_3_mm) %>%
  select(-bulk_nr_leaves) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") %>%
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # Factor the sites
  mutate(siteID = factor(siteID, levels = c("Lygra", "Sogndal","Tjøtta", "Senja", "Kautokeino")))

# visualize data ----
library(ggh4x)

## Leaf thickness ----
ggplot(DN.viz %>% filter(leaf_age != "" & ageClass != "" & DroughtTrt != "") |> filter(trait == "leaf_thickness"),
       aes(interaction(DroughtTrt, ageClass), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ siteID) +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "") +
  theme_bw()

## Weight ----
ggplot(DN.viz %>% filter(leaf_age != "" & ageClass != "" & DroughtTrt != "") |> filter(trait == "wet_mass_g"),
       aes(interaction(DroughtTrt, ageClass), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ siteID) +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "") +
  theme_bw()

## Height ----
ggplot(DN.viz %>% filter(leaf_age != "" & ageClass != "" & DroughtTrt != "") |> filter(trait == "plant_height"),
       aes(interaction(DroughtTrt, ageClass), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ siteID) +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "") +
  theme_bw()
