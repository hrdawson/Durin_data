# Manually clean errors ----
durin = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                 na.strings=c("","NA")) |>
  # Change plant height to numeric
  mutate(plant_height = as.numeric(plant_height)) |>
  # Correct plot names
  mutate(
    DURIN_plot = case_when(
      envelope_ID =="DAP6500"~ "KA_F_VV_2",
      envelope_ID =="EXQ8322"~ "KA_O_EN_1",
      envelope_ID =="EYR2971"~ "KA_O_EN_2",
      envelope_ID =="EYV3590"~ "KA_O_EN_2",
      envelope_ID =="EZA6532"~ "KA_O_EN_3",
      envelope_ID =="DFW2204"~ "KA_O_EN_4",
      envelope_ID =="EYG4750"~ "KA_O_EN_4",
      envelope_ID =="EYK4044"~ "KA_O_EN_4",
      envelope_ID =="DHD0172"~ "KA_O_VM_2",
      envelope_ID =="DHH3325"~ "KA_O_VM_2",
      envelope_ID =="DFM4890"~ "KA_O_VM_4",
      envelope_ID =="BMB6959"~ "LY_F_EN_5",
      envelope_ID =="BIL0759"~ "LY_F_VM_2",
      envelope_ID =="AYB7940"~ "LY_O_EN_1",
      envelope_ID =="AYF1252"~ "LY_O_EN_1",
      envelope_ID =="AYN9607"~ "LY_O_EN_1",
      envelope_ID =="AUZ1311"~ "LY_O_EN_3",
      envelope_ID =="AWC3830"~ "LY_O_EN_3",
      envelope_ID =="AVY7377"~ "LY_O_EN_4",
      envelope_ID =="BHR0925"~ "LY_O_EN_5",
      envelope_ID =="AST3380"~ "LY_O_VM_4",
      envelope_ID =="AYP7221"~ "LY_O_VV_2",
      envelope_ID =="AZB3929"~ "LY_O_VV_2",
      envelope_ID =="BFY4922"~ "LY_O_VV_2",
      envelope_ID =="AYS6617"~ "LY_O_VV_3",
      envelope_ID =="AZJ4306"~ "LY_O_VV_5",
      envelope_ID =="EVA9626"~ "SE_F_VV_4",
      envelope_ID =="CWZ4784"~ "SE_O_VM_5",
      envelope_ID =="DBV0943"~ "SE_O_VV_1",
      envelope_ID =="DBV0943"~ "SE_O_VV_1",
      envelope_ID =="CYR2242"~ "SE_O_VV_3",
      envelope_ID =="CZD0880"~ "SE_O_VV_4",
      envelope_ID =="CMH5663"~ "SO_F_VM_3",
      envelope_ID =="CMX4054"~ "SO_F_VM_4",
      envelope_ID =="CKI5874"~ "SO_F_VV_1",
      TRUE ~ DURIN_plot
    ),
    # Correct habitats
    habitat = case_when(
      envelope_ID =="EXQ8322"~"Open",
      envelope_ID =="EYR2971"~"Open",
      envelope_ID =="EYV3590"~"Open",
      envelope_ID =="EZA6532"~"Open",
      envelope_ID =="DFW2204"~"Open",
      envelope_ID =="EYG4750"~"Open",
      envelope_ID =="EYK4044"~"Open",
      envelope_ID =="DHD0172"~"Open",
      envelope_ID =="DHH3325"~"Open",
      envelope_ID =="DFM4890"~"Open",
      envelope_ID =="BIL0759"~"Forested",
      envelope_ID =="AZB3929"~"Open",
      envelope_ID =="EVA9626"~"Forested",
      envelope_ID =="CYR2242"~"Open",
      envelope_ID =="CZD0880"~"Open",
      envelope_ID =="BOW7206"~"Forested",
      TRUE ~ habitat
    ),
    # Correct plot numbers
    # For some reason the case_when won't work
    # Redo manually
    # plotNR = case_when(
    #   envelope_ID =="BFY4922" ~ 2,
    #   TRUE ~ plotNR
    # ),
    plotNR = replace(plotNR, envelope_ID == "BFY4922", 2),
    # Correct leaf ages
    leaf_age = case_when(
      envelope_ID =="AZG5994"~"old",
      envelope_ID =="EVO1776"~"young",
      envelope_ID =="EVS4445"~"young",
      TRUE ~ leaf_age
    ),
    # Correct thicknesses
    leaf_thickness_1_mm = case_when(
      envelope_ID =="DSD6681"~0.0107,
      envelope_ID =="CSP7326"~0.259,
      TRUE ~ leaf_thickness_1_mm
    ),
    leaf_thickness_2_mm = case_when(
      envelope_ID =="BPF4529"~0.221,
      TRUE ~ leaf_thickness_2_mm
    ),
    # Correct plant heights
    plant_height = case_when(
      envelope_ID =="AWH7022"~11.6,
      envelope_ID =="DUX5951"~14.8,
      envelope_ID =="DNA2455"~15.4,
      envelope_ID =="ERV2714"~15.5,
      envelope_ID =="DFX0733"~17.0,
      envelope_ID =="DGF1762"~17.0,
      envelope_ID =="ANO6821"~17.5,
      envelope_ID =="CHP1113"~41.2,
      TRUE ~ plant_height
    ),
    # Correct species
    species = case_when(
      envelope_ID =="ALL1763"~"Vaccinium vitis-idaea",
      envelope_ID =="AWF5086"~"Vaccinium vitis-idaea",
      envelope_ID =="BBM8747"~"Vaccinium vitis-idaea",
      envelope_ID =="DAI1197"~"Vaccinium vitis-idaea",
      envelope_ID =="DZX9994"~"Vaccinium vitis-idaea",
      TRUE ~ species
    ),
    # Correct wet mass
    wet_mass_g = case_when(
    envelope_ID =="DDZ3156"~0.0138,
    envelope_ID =="DEN0101"~0.0254,
    envelope_ID =="ARK3594"~0.0282,
    envelope_ID =="AFH1727"~0.0333,
    envelope_ID =="BSD3874"~0.0468,
    TRUE ~ wet_mass_g)
  )
