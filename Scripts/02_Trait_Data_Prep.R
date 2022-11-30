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
# flower <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Tribulus_flower_data_clean.csv")
# flower <- flower %>% mutate_at(vars(ID,
#                                         Herbarium,
#                                         continent,
#                                         country,
#                                         island_group,
#                                         mainland_island,
#                                         galapagos_other,
#                                         island_group,
#                                         galapagos_island,
#                                         finch_beak), list(factor))
# str(flower)
# flower <- rename(flower, Temp = Bio_1,
#                  Temp_S = Bio_4,
#                  Prec = Bio_12,
#                  varP = Bio_15)
# 
# # Remove NAs Petal length
# flower <- filter(flower, !is.na(petal_length))
# flower <- filter(flower, !is.na(Temp_S))
# 
# ### Flower for Galapagos and Other Islands ####
# flower_galapagos_other <- filter(flower, !galapagos_other == "mainland")

# Other analysis subsets ####
# 02_03 Galapagos only with mainland data ####
# # Additional analysis to test the effect of other islands by removing them from models
# 
# # Made a separate mainland only dataset
# mericarp_mainland <- filter(mericarp, mainland_island == "mainland")
# # Made a separate Galapagos only dataset
# gal_mericarp <- filter(mericarp, galapagos_other == "Galapagos")
# # Combined these datasets
# mericarp_mainland_gal <- bind_rows(mericarp_mainland, gal_mericarp)
# 
# # Transformed traits into factors
# mericarp_mainland_gal <- mericarp_mainland_gal %>% mutate_at(vars(ID,
#                                         Herbarium,
#                                         continent,
#                                         country,
#                                         island_group,
#                                         mainland_island,
#                                         galapagos_other,
#                                         island_group,
#                                         galapagos_island,
#                                         lower_spines,
#                                         finch_beak), list(factor))
# 
# str(mericarp_mainland_gal)
# 
# 
# ##### Length ####
# meri_length_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, length)
# meri_length_mainland_gal <- filter(meri_length_mainland_gal, !is.na(length))
# 
# ##### Width ####
# meri_width_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, width)
# meri_width_mainland_gal <- filter(meri_width_mainland_gal, !is.na(width))
# 
# ##### Depth ####
# meri_depth_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, depth)
# meri_depth_mainland_gal <- filter(meri_depth_mainland_gal, !is.na(depth))
# 
# ##### Spine tip distance ####
# meri_tip_distance_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, tip_distance) #Has zeroes in the data
# meri_tip_distance_mainland_gal <- filter(meri_tip_distance_mainland_gal, !is.na(tip_distance))
# 
# ##### Spine tip distance without zero ####
# meri_tip_distance_wozero_mainland_gal <- dplyr::filter(meri_tip_distance_mainland_gal, !tip_distance == 0)
# 
# ##### Lower spines ####
# meri_lower_spines_mainland_gal <- dplyr::select(mericarp_mainland_gal, ind_num:mericarp_num, lower_spines) #As factor
# meri_lower_spines_mainland_gal <- filter(meri_lower_spines_mainland_gal, !is.na(lower_spines))

# 02_04 Mean datasets: Estimating means from Galapagos and Florida ####
# First I need to filter Galapagos and Florida populations since these are the ones with most data points
# I can filter them out from the mericarp general dataset

# other_mericarp <- filter(mericarp, !grepl("Florida", ID))
# other_mericarp <- filter(other_mericarp, !grepl("Galapagos", galapagos_other))
# # This dataset filters out Florida and Galapagos
# 
# galapagos_mericarp <- filter(mericarp, galapagos_other == "Galapagos")
# # This datset is for Galapagos only data, for summary
# 
# florida_mericarp <- filter(mericarp, grepl("Florida", ID))
# # This dataset is for Florida only data, for summary
# 
# # With each dataset separated by group I can summarize them by ID to reduce
# # their number.Caveat: I am not summarizing lower spines here. Just the continous
# # variables. I can ask how to summarize the lower spine data. Perhaps with proportions per ID?
# 
# # Summaries selects most of the column factors and estimates mean
# # sd, se, var.
# 
# other_mericarp_mean <- other_mericarp %>% 
#   group_by(ID, Herb_name, 
#            Herb_number,
#            Herbarium,
#            year_collected,
#            continent,
#            mainland_island,
#            galapagos_other,
#            island_group,
#            galapagos_island,
#            finch_beak,
#            country,
#            location
#            ) %>% 
#   summarise_each(funs(mean,sd,var, se = sd(.)/sqrt(n()),
#            ), length,
#            width,
#            depth,
#            tip_distance,
#            Temp,
#            Temp_S,
#            Prec,
#            varP)
# 
# 
# galapagos_mericarp_mean <- galapagos_mericarp %>% 
#   group_by(ID, Herb_name, 
#            Herb_number,
#            Herbarium,
#            year_collected,
#            continent,
#            mainland_island,
#            galapagos_other,
#            island_group,
#            galapagos_island,
#            finch_beak,
#            country,
#            location
#   ) %>% 
#   summarise_each(funs(mean,sd,var, se = sd(.)/sqrt(n()),
#   ), length,
#   width,
#   depth,
#   tip_distance,
#   Temp,
#   Temp_S,
#   Prec,
#   varP)
# 
# florida_mericarp_mean <- florida_mericarp %>% 
#   group_by(ID, Herb_name, 
#            Herb_number,
#            Herbarium,
#            year_collected,
#            continent,
#            mainland_island,
#            galapagos_other,
#            island_group,
#            galapagos_island,
#            finch_beak,
#            country,
#            location
#   ) %>% 
#   summarise_each(funs(mean,sd,var, se = sd(.)/sqrt(n()),
#   ), length,
#   width,
#   depth,
#   tip_distance,
#   Temp,
#   Temp_S,
#   Prec,
#   varP)
# 
# ## Adding mean datasets into a mericarp_mean dataset
# mericarp_means <- bind_rows(galapagos_mericarp_mean,
#                             florida_mericarp_mean,
#                             other_mericarp_mean)
# 
# ## The other dataset that I could do is to integrate the galapagos
# # and florida means into the individual other mericarp dataset.
# 
# galapagos_mericarp_mean <- rename(galapagos_mericarp_mean, length = length_mean,
#                                   width = width_mean,
#                                   depth = depth_mean,
#                                   tip_distance = tip_distance_mean,
#                                   Temp = Temp_mean,
#                                   Temp_S = Temp_S_mean,
#                                   Prec = Prec_mean,
#                                   varP = varP_mean)
# 
# florida_mericarp_mean <- rename(florida_mericarp_mean, length = length_mean,
#                                   width = width_mean,
#                                   depth = depth_mean,
#                                   tip_distance = tip_distance_mean,
#                                   Temp = Temp_mean,
#                                   Temp_S = Temp_S_mean,
#                                   Prec = Prec_mean,
#                                   varP = varP_mean)
# 
# # Create a dataset with mean galapagos and florida samples per ID, but individual samples
# # from other mericarps
# 
# galapagos_mericarp_mean <- select(galapagos_mericarp_mean, c(1:21))
# florida_mericarp_mean <- select(florida_mericarp_mean, c(1:21))
# 
# mericarp_ind <- bind_rows(galapagos_mericarp_mean, florida_mericarp_mean, other_mericarp)
# mericarp_ind <- select(mericarp_ind, !c(22:31))
# 
# # Notes: mericarp_means has the mean by ID of all mericarps traits
# # This condense florida, galapagos and other datasets. 
# # mericarp_ind contains the means of galapagos and florida but the individual 
# # measurements of the other mericarps. Which is what we could use for the review
# # mericarp_mean has 315 observations
# # mericarp_ind has 561 observations
# # the galapagos_mericarp_mean have 147 observations from 3745 individual obs
# # florida mericarp mean have 116 observations from 1124 indvidual obs
# # other mericarps means have 52 observations our of 298 individual ones.
# 
# # Next steps for mean mericarps ####
# # The meericarp dataset is the one with biased sampling. I will export these datasets
# # and similar to the original analysis separate them in individual traits and run the models
# 
