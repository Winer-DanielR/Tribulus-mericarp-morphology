# Script 02. Data loading, subsets and transformations ####
# By: Daniel Reyes Corral

# This script was used once to create the individual databases per trait
# that later will be used for each model
# Convert columns into factors
# Export datasets into processed data folder

# EXTRA: For this analysis, we removed samples from islands from Africa: Cape Verde
# and Shungo-Mbili islands (n=8). Given that these sample come from single plant vouchers
# and are not representative of the whole island system they come from.
# This only concern the mericarp database. 

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
                                              lower_spines,
                                              location), list(factor))
str(mericarp)

## Remove of African samples ####
mericarp <-mericarp[!(mericarp$location=="Cape Verde Islands" | mericarp$location=="Shungu-Mbili Island"),]


# Separate each trait, remove NAs

### Length ####
meri_length <- dplyr::select(mericarp, ind_num:mericarp_num, length, country:varP)
meri_length <- filter(meri_length, !is.na(length))
meri_length <- filter(meri_length, !is.na(Temp_S))

###  Width ####
meri_width <- dplyr::select(mericarp, ind_num:mericarp_num, width, country:varP)
meri_width <- filter(meri_width, !is.na(width))
meri_width <- filter(meri_width, !is.na(Temp_S))

### Depth ####
meri_depth <- dplyr::select(mericarp, ind_num:mericarp_num, depth, country:varP)
meri_depth <- filter(meri_depth, !is.na(depth))
meri_depth <- filter(meri_depth, !is.na(Temp_S))

### Spine tip distance ####
meri_tip_distance <- dplyr::select(mericarp, ind_num:mericarp_num, tip_distance, country:varP) #Has zeroes in the data
meri_tip_distance <- filter(meri_tip_distance, !is.na(tip_distance))
meri_tip_distance <- filter(meri_tip_distance, !is.na(Temp_S))


### Spine tip distance without zero #####
# We removed mericarps without upper spines from analysis.These mericarps had a tip distance of 0.
meri_tip_distance_wozero <- dplyr::filter(meri_tip_distance, !tip_distance == 0)

### Lower spines #####
meri_lower_spines <- dplyr::select(mericarp, ind_num:mericarp_num, lower_spines, country:varP) #As factor
meri_lower_spines <- filter(meri_lower_spines, !is.na(lower_spines))
meri_lower_spines <- filter(meri_lower_spines, !is.na(Temp_S))



## Traits not used in the analysis ####

### Spine length #####
# Spine length was another trait we collected but we did not had for all samples.
# Herbarium data was collected by Marc and during that time we did not measured spine length.
# meri_spine_length <- dplyr::select(mericarp, ind_num:mericarp_num, spine_length) #Has zeroes in the data
# meri_spine_length <- filter(meri_spine_length, !is.na(spine_length))
# Spine length without zero
# meri_spine_length_wozero <- dplyr::filter(meri_spine_length, !spine_length == 0)
### Spine number ####
# meri_spine_num <- select(mericarp, ind_num:mericarp_num, spine_num) #As factor
### Upper spines ####
# Upper spines was another trait we considered just to test a similar model as lower spines
# and to include the mericarps without upper spines in the analysis.
# meri_upper_spines <- dplyr::select(mericarp, ind_num:mericarp_num, upper_spines) #As factor
# meri_upper_spines <- filter(meri_upper_spines, !is.na(upper_spines))


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

# # Remove NAs Petal length
flower <- filter(flower, !is.na(petal_length))
flower <- filter(flower, !is.na(Temp_S))
# 
# ### Flower for Galapagos and Other Islands ####
flower_galapagos_other <- filter(flower, !galapagos_other == "mainland")

