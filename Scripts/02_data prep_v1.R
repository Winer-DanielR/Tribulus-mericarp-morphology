### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

### Script for modifying data and get general data ready for analysis ###

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
                                        spine_num,
                                        lower_spines,
                                        country,
                                        location), list(factor))

#str(mericarp)
#Year is a number instead of a factor.

#### Leaf data ####
leaf <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_leaves_data_plus CDRS herbarium_clean.csv")
leaf <- as_tibble(leaf)
names(leaf)

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
