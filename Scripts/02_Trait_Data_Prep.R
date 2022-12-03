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
flower_galapagos_other <- filter(flower, !galapagos_other == "mainland")

# 02_04 Mean datasets: Estimating means from Galapagos and Florida ####
# First I need to filter Galapagos and Florida populations since these are the ones with most data points
# I can filter them out from the mericarp general dataset

 other_mericarp <- filter(mericarp, !grepl("Florida", ID))
 other_mericarp <- filter(other_mericarp, !grepl("Galapagos", galapagos_other))
# # This dataset filters out Florida and Galapagos
# 
 galapagos_mericarp <- filter(mericarp, galapagos_other == "Galapagos")
# # This datset is for Galapagos only data, for summary
# 
 florida_mericarp <- filter(mericarp, grepl("Florida", ID))
# # This dataset is for Florida only data, for summary
# 
# # With each dataset separated by group I can summarize them by ID to reduce
# # their number.
 
 # CAVEAT: I am not summarizing lower spines here. Just the continuous
 # variables. I can ask how to summarize the lower spine data. Perhaps with proportions per ID?

 # I would argue that the analysis with and without Galapagos and Galapaogos only may be better because
 # it includes lower spines.
 
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
galapagos_mericarp_mean <- galapagos_mericarp %>%
  group_by(ID, Herb_name,
           Herb_number,
           Herbarium,
           year_collected,
           continent,
           mainland_island,
           galapagos_other,
           island_group,
           galapagos_island,
           finch_beak,
           country,
           location
  ) %>%
  summarise_each(funs(mean,sd,var, se = sd(.)/sqrt(n()),
  ), length,
  width,
  depth,
  tip_distance,
  Temp,
  Temp_S,
  Prec,
  varP)

florida_mericarp_mean <- florida_mericarp %>%
  group_by(ID, Herb_name,
           Herb_number,
           Herbarium,
           year_collected,
           continent,
           mainland_island,
           galapagos_other,
           island_group,
           galapagos_island,
           finch_beak,
           country,
           location
  ) %>%
  summarise_each(funs(mean,sd,var, se = sd(.)/sqrt(n()),
  ), length,
  width,
  depth,
  tip_distance,
  Temp,
  Temp_S,
  Prec,
  varP)
# 
# ## Adding mean datasets into a mericarp_mean dataset
# mericarp_means <- bind_rows(galapagos_mericarp_mean,
#                             florida_mericarp_mean,
#                             other_mericarp_mean)
# 
# ## The other dataset that I could do is to integrate the galapagos
# # and florida means into the individual other mericarp dataset.
# 
galapagos_mericarp_mean <- rename(galapagos_mericarp_mean, length = length_mean,
                                  width = width_mean,
                                  depth = depth_mean,
                                  tip_distance = tip_distance_mean,
                                  Temp = Temp_mean,
                                  Temp_S = Temp_S_mean,
                                  Prec = Prec_mean,
                                  varP = varP_mean)

florida_mericarp_mean <- rename(florida_mericarp_mean, length = length_mean,
                                  width = width_mean,
                                  depth = depth_mean,
                                  tip_distance = tip_distance_mean,
                                  Temp = Temp_mean,
                                  Temp_S = Temp_S_mean,
                                  Prec = Prec_mean,
                                  varP = varP_mean)

# # Create a dataset with mean galapagos and florida samples per ID, but individual samples
# # from other mericarps
# 
 galapagos_mericarp_mean <- select(galapagos_mericarp_mean, c(1:21))
 florida_mericarp_mean <- select(florida_mericarp_mean, c(1:21))
# 
 mericarp_ind <- bind_rows(galapagos_mericarp_mean, florida_mericarp_mean, other_mericarp)
 mericarp_ind <- select(mericarp_ind, !c(22:31))
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
