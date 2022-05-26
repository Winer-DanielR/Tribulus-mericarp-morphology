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

# Separate each trait, remove NAs

### Length ####
meri_length <- dplyr::select(mericarp, ind_num:mericarp_num, length, Temp:varP)
meri_length <- filter(meri_length, !is.na(length))
meri_length <- filter(meri_length, !is.na(Temp_S))

###  Width ####
meri_width <- dplyr::select(mericarp, ind_num:mericarp_num, width, Temp:varP)
meri_width <- filter(meri_width, !is.na(width))
meri_width <- filter(meri_width, !is.na(Temp_S))

### Depth ####
meri_depth <- dplyr::select(mericarp, ind_num:mericarp_num, depth, Temp:varP)
meri_depth <- filter(meri_depth, !is.na(depth))
meri_depth <- filter(meri_depth, !is.na(Temp_S))

### Spine tip distance ####
meri_tip_distance <- dplyr::select(mericarp, ind_num:mericarp_num, tip_distance, Temp:varP) #Has zeroes in the data
meri_tip_distance <- filter(meri_tip_distance, !is.na(tip_distance))
meri_tip_distance <- filter(meri_tip_distance, !is.na(Temp_S))


### Spine tip distance without zero #####
# We removed mericarps without upper spines from analysis.These mericarps had a tip distance of 0.
meri_tip_distance_wozero <- dplyr::filter(meri_tip_distance, !tip_distance == 0)

### Lower spines #####
meri_lower_spines <- dplyr::select(mericarp, ind_num:mericarp_num, lower_spines, Temp:varP) #As factor
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

# Remove NAs Petal length
flower <- filter(flower, !is.na(petal_length))
flower <- filter(flower, !is.na(Temp_S))

### Flower for Galapagos and Other Islands ####
flower_galapagos_other <- filter(flower, !galapagos_other == "mainland")

       # 02_03 Galapagos only with mainland data ####
# Additional analysis to test the effect of other islands by removing them from models

# Made a separate mainland only dataset
mericarp_mainland <- filter(mericarp, mainland_island == "mainland")
# Made a separate Galapagos only dataset
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
meri_length_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, length)
meri_length_mainland_gal <- filter(meri_length_mainland_gal, !is.na(length))

##### Width ####
meri_width_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, width)
meri_width_mainland_gal <- filter(meri_width_mainland_gal, !is.na(width))

##### Depth ####
meri_depth_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, depth)
meri_depth_mainland_gal <- filter(meri_depth_mainland_gal, !is.na(depth))

##### Spine tip distance ####
meri_tip_distance_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, tip_distance) #Has zeroes in the data
meri_tip_distance_mainland_gal <- filter(meri_tip_distance_mainland_gal, !is.na(tip_distance))

##### Spine tip distance without zero ####
meri_tip_distance_wozero_mainland_gal <- dplyr::filter(meri_tip_distance_mainland_gal, !tip_distance == 0)

##### Lower spines ####
meri_lower_spines_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, lower_spines) #As factor
meri_lower_spines_mainland_gal <- filter(meri_lower_spines_mainland_gal, !is.na(lower_spines))

# Other datasets: Galapagos only data ####
# # 02_03 Galapagos only data
# # Filter Galapagos only data for aditional analysis
# # I did not use this as the finch community analysis did not showed significant results
# # #### 02_03_01 Galapagos only mericarp data
# gal_mericarp <- filter(mericarp, galapagos_other == "Galapagos")
# # ##### Length
# gal_meri_length <- filter(meri_length, galapagos_other == "Galapagos")
# ##### Width
# gal_meri_width <- filter(meri_width, galapagos_other == "Galapagos")
# ##### Depth
# gal_meri_depth <- filter(meri_depth, galapagos_other == "Galapagos")
# ##### Spine length
# gal_meri_spine_length <- filter(meri_spine_length, galapagos_other == "Galapagos")
# gal_meri_spine_length_wozero <- filter(gal_meri_spine_length, !spine_length == 0)
# ##### Spine tip distance
# gal_meri_tip_distance <- filter(meri_tip_distance, galapagos_other == "Galapagos")
# gal_meri_tip_distance_wozero <- filter(gal_meri_tip_distance, !tip_distance == 0)
# ##### Lower spines
# gal_meri_lower_spines <- filter(meri_lower_spines, galapagos_other == "Galapagos")
# ##### Upper spines
# gal_meri_upper_spines <- filter(meri_upper_spines, galapagos_other == "Galapagos")
# 
# #### 02_03_02 Flower dataset
# gal_petal_length <- filter(flower, galapagos_other == "Galapagos")

