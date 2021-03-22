### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

### Script for modifying data and get general data ready for analysis ###

# Single trait data prep =============================================
#### Load indvidual traits and transformed dataframes ####
# Mericarp traits ####
meri_length <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp Length.csv")
meri_length <- meri_length %>% mutate_at(vars(ID,
                                        Herbarium,
                                        continent,
                                        island_group,
                                        mainland_island,
                                        galapagos_other,
                                        island_group,
                                        galapagos_island,
                                        finch_beak), list(factor))
meri_width <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp Width.csv")
meri_width <- meri_width %>% mutate_at(vars(ID,
                                              Herbarium,
                                              continent,
                                              island_group,
                                              mainland_island,
                                              galapagos_other,
                                              island_group,
                                              galapagos_island,
                                              finch_beak), list(factor))
meri_depth <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp Depth.csv")
meri_depth <- meri_depth %>% mutate_at(vars(ID,
                                            Herbarium,
                                            continent,
                                            island_group,
                                            mainland_island,
                                            galapagos_other,
                                            island_group,
                                            galapagos_island,
                                            finch_beak), list(factor))
meri_spine.length <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp Spine Length.csv")
meri_spine.length <- meri_spine.length %>% mutate_at(vars(ID,
                                            Herbarium,
                                            continent,
                                            island_group,
                                            mainland_island,
                                            galapagos_other,
                                            island_group,
                                            galapagos_island,
                                            finch_beak), list(factor))
meri_tip.distance <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp Tip Distance.csv")
meri_tip.distance <- meri_tip.distance %>% mutate_at(vars(ID,
                                            Herbarium,
                                            continent,
                                            island_group,
                                            mainland_island,
                                            galapagos_other,
                                            island_group,
                                            galapagos_island,
                                            finch_beak), list(factor))
meri_spine.number <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp Spine Number.csv")
meri_spine.number <- meri_spine.number %>% mutate_at(vars(ID,
                                            Herbarium,
                                            continent,
                                            island_group,
                                            mainland_island,
                                            galapagos_other,
                                            island_group,
                                            galapagos_island,
                                            finch_beak), list(factor))
meri_lower.spines <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp Lower Spines.csv")
meri_lower.spines <- meri_lower.spines %>% mutate_at(vars(ID,
                                            Herbarium,
                                            continent,
                                            island_group,
                                            mainland_island,
                                            galapagos_other,
                                            island_group,
                                            galapagos_island,
                                            lower_spines,
                                            finch_beak), list(factor))

# Flower traits ####
petal_length <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Flower length.csv")
#str(petal_length)
petal_length <- petal_length %>% mutate_at(vars(ID,
                                                          Herbarium,
                                                          continent,
                                                          island_group,
                                                          mainland_island,
                                                          galapagos_other,
                                                          island_group,
                                                          galapagos_island,
                                                          finch_beak), list(factor))
# Leaf traits ####
leaf_length <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Leaf length.csv")
leaf_length <- leaf_length %>% mutate_at(vars(ID,
                                                herbarium,
                                                continent,
                                                island_group,
                                                mainland_island,
                                                galapagos_other,
                                                island_group,
                                                galapagos_island,
                                                finch_beak), list(factor))
leaflet_length <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Leaflet length.csv")
leaflet_length <- leaflet_length %>% mutate_at(vars(ID,
                                              herbarium,
                                              continent,
                                              island_group,
                                              mainland_island,
                                              galapagos_other,
                                              island_group,
                                              galapagos_island,
                                              finch_beak), list(factor))

#### Galapagos only data ####
gal_meri_length <- filter(meri_length, galapagos_other == "Galapagos")
gal_meri_width <- filter(meri_width, galapagos_other == "Galapagos")
gal_meri_depth <- filter(meri_depth, galapagos_other == "Galapagos")
gal_meri_spine.length <- filter(meri_spine.length, galapagos_other == "Galapagos")
gal_meri_tip.distance <- filter(meri_tip.distance, galapagos_other == "Galapagos")
gal_meri_spine.number <- filter(meri_spine.number, galapagos_other == "Galapagos")
gal_meri_lower.spines <- filter(meri_lower.spines, galapagos_other == "Galapagos")
gal_petal_length <- filter(petal_length, galapagos_other == "Galapagos")
gal_leaf_length <- filter(leaf_length, galapagos_other == "Galapagos")
gal_leaflet_length <- filter(leaflet_length, galapagos_other == "Galapagos")

# With all this you are ready to run the models per trait script ####




# Multivariate databases ==================================================

#### Mericarp data ####
mericarp <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_mericarp_data_clean.csv")
mericarp <- as_tibble(mericarp)
names(mericarp)
str(mericarp)
mericarp <- filter(mericarp, !is.na(ind_num))


# Convert mericarp variables into factors
mericarp <- mericarp %>% mutate_at(vars(ID,
                                        Herbarium,
                                        continent,
                                        island_group,
                                        mainland_island,
                                        galapagos_other,
                                        island_group,
                                        galapagos_island,
                                        finch_beak,
                                        lower_spines,
                                        country,
                                        location), list(factor))

#str(mericarp)
#Year is a number instead of a factor.

# Mericarp1 removed all NAs for ALL traits. 
# This resulted in half of the mericarp data removed 
# because spine length was not measured for most of the herbarium data
# mericarp1 <- as.tibble(mericarp)
# mericarp1 <- filter(mericarp1, !is.na(mericarp1[,18]))
# mericarp1 <- select(mericarp1, !notes)
# str(mericarp1)
# write_csv(mericarp1, "C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp data without NAs all traits.csv")

mericarp1 <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp data without NAs all traits.csv")
str(mericarp1)
names(mericarp1)
mericarp1 <- mericarp1 %>% mutate_at(vars(ID,
                                        Herbarium,
                                        continent,
                                        island_group,
                                        mainland_island,
                                        galapagos_other,
                                        island_group,
                                        galapagos_island,
                                        finch_beak,
                                        country,
                                        location), list(factor))


# Mericarp 2 removes spine length from the analysis to include most of the mericarp data
# mericarp2 <- select(mericarp, !spine_length)
# mericarp2 <- filter(mericarp2, !is.na(mericarp2$lower_spines))
# write_csv(mericarp2, "C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp data without NAs spine lenght removed.csv")

mericarp2 <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Mericarp data without NAs spine lenght removed.csv")
str(mericarp2)
names(mericarp2)
mericarp2 <- mericarp2 %>% mutate_at(vars(ID,
                                          Herbarium,
                                          continent,
                                          island_group,
                                          mainland_island,
                                          galapagos_other,
                                          island_group,
                                          galapagos_island,
                                          finch_beak,
                                          country,
                                          location), list(factor))
mericarp2 <- filter(mericarp2, !is.na(mericarp2$year_collected))

### Removing spine length we have more locaiton for multivariate analysis
### For all traits we have mainly Florida and Galapagos

#### Leaf data ####
leaf <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_leaves_data_plus CDRS herbarium_clean.csv")
leaf <- as_tibble(leaf)

# It is important to remove NAs

# Convert leaf variables into factors
leaf <- leaf %>% mutate_at(vars(ID,
                                continent,
                                mainland_island,
                                galapagos_other,
                                island_group,
                                galapagos_island,
                                finch_beak,
                                country,
                                province_state,
                                herbarium), list(factor))

#str(leaf)
leaf <- filter(leaf, !is.na(leaf$number_of_leaflets))


#### Flower data ####
flower <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_flower_data_clean.csv")
flower <- as_tibble(flower)
names(flower)

# Covert flower data variables as factors
flower <- flower %>% mutate_at(vars(ID,
                                    continent,
                                    mainland_island,
                                    galapagos_other,
                                    island_group,
                                    galapagos_island,
                                    finch_beak,
                                    country,
                                    province_state,
                                    Herbarium), list(factor))
#str(flower)
# FLower does not have NAs
# Mericarp data has two datasets. Leaf data has one. Flower data has one.
  