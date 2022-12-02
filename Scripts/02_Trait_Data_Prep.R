# Script 02. Data loading, subsets and transformations ####
# By: Daniel Reyes Corral

# This script was used once to create the individual databases per trait
# that later will be used for each model
# Convert columns into factors
# Export datasets into processed data folder

## 02_01 Mericarp dataset: ####
# Tribulus_mericarp_data_clean.csv

mericarp <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Tribulus_mericarp_data_clean.csv")
mericarp <- mericarp %>% mutate_at(vars(ID,
                                              Herbarium,
                                              continent,
                                              country,
                                              island_group,
                                              mainland_island,
                                              galapagos_other,
                                              island_group,
                                              galapagos_island,
                                              finch_beak,
                                              lower_spines), list(factor))
str(mericarp)

# 02_02 Flower dataset ####
# Tribulus_flower_data_clean.csv
flower <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Tribulus_flower_data_clean.csv")
flower <- flower %>% mutate_at(vars(ID,
                                        Herbarium,
                                        continent,
                                        country,
                                        island_group,
                                        mainland_island,
                                        galapagos_other,
                                        island_group,
                                        galapagos_island,
                                        finch_beak), list(factor))
str(flower)
flower <- rename(flower, Temp = Bio_1,
                 Temp_S = Bio_4,
                 Prec = Bio_12,
                 varP = Bio_15)

# Remove NAs Petal length
flower <- filter(flower, !is.na(petal_length))
flower <- filter(flower, !is.na(Temp_S))

### Flower for Galapagos and Other Islands ####
#flower_galapagos_other <- filter(flower, !galapagos_other == "mainland")
flower_mainland <- filter(flower, mainland_island == "continent")
flower_galapagos <- filter(flower, !galapagos_other == "other")

flower_galapagos_mainland <- bind_rows(flower_mainland, flower_galapagos)

# Other analysis subsets: 
# 02_03 Galapagos only with mainland data ####
# Additional analysis to test the effect of Galapagos Islands by removing 
# Other islands from models
# 
# # Made a separate mainland only dataset
mericarp_mainland <- filter(mericarp, mainland_island == "continent")

# # Made a separate Galapagos only dataset
gal_mericarp <- filter(mericarp, galapagos_other == "Galapagos")

# Combined these datasets
mericarp_mainland_gal <- bind_rows(mericarp_mainland, gal_mericarp)
 
# Transformed traits into factors
mericarp_mainland_gal <- mericarp_mainland_gal %>% mutate_at(vars(ID,
                                         Herbarium,
                                         continent,
                                         country,
                                         island_group,
                                         mainland_island,
                                         galapagos_other,
                                         island_group,
                                         galapagos_island,
                                         lower_spines,
                                         finch_beak), list(factor))
 
str(mericarp_mainland_gal)
 
 
##### Length ####
meri_length_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, length, 28:31)
meri_length_mainland_gal <- filter(meri_length_mainland_gal, !is.na(length))
 
# ##### Width ####
meri_width_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, width, 28:31)
meri_width_mainland_gal <- filter(meri_width_mainland_gal, !is.na(width))
 
# ##### Depth ####
meri_depth_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, depth, 28:31)
meri_depth_mainland_gal <- filter(meri_depth_mainland_gal, !is.na(depth))
 
# ##### Spine tip distance ####
meri_tip_distance_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, tip_distance, 28:31) #Has zeroes in the data
meri_tip_distance_mainland_gal <- filter(meri_tip_distance_mainland_gal, !is.na(tip_distance))
 
# ##### Spine tip distance without zero ####
meri_tip_distance_wozero_mainland_gal <- dplyr::filter(meri_tip_distance_mainland_gal, !tip_distance == 0)
# 
# ##### Lower spines ####
meri_lower_spines_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, lower_spines, 28:31) #As factor
meri_lower_spines_mainland_gal <- filter(meri_lower_spines_mainland_gal, !is.na(lower_spines))

