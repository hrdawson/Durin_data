library(BIEN)

# List traits ----
BIEN_trait_list()

# Download traits for each of our focal species ----
vv.traits = BIEN_trait_species("Vaccinium vitis-idaea")
en.traits = BIEN_trait_species("Empetrum nigrum")

# Check if we have sufficient coverage -----
table(vv.traits$trait_name)
table(en.traits$trait_name)
