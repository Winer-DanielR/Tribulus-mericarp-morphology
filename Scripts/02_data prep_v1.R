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

str(mericarp)
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

str(leaf)

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
str(flower)

