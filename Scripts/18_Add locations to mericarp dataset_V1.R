# # Include location data into mericarp dataset
# 
# # The mericarp dataset has location information from different sets
# # Here I want to join these datasets to extract the location info and include it into the
# # mericarp dataset
# # Other datasets include the galagpos info
# 
# # Mericarp dataset
# mericarp <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Tribulus_mericarp_data_clean.csv")
# str(mericarp)
# # Tribulus_flower_data_clean.csv
# # flower <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Tribulus_flower_data_clean.csv")
# # flower <- flower %>% mutate_at(vars(ID,
# #                                     Herbarium,
# #                                     continent,
# #                                     country,
# #                                     island_group,
# #                                     mainland_island,
# #                                     galapagos_other,
# #                                     island_group,
# #                                     galapagos_island,
# #                                     finch_beak), list(factor))
# # str(flower)
# # 
# # # Select location data from flower and ID
# # flower_location <- select(flower, ID, latitude, longitude)
# # 
# # # Join datasets by ID and extract locations
# # 
# # mericarp <- full_join(mericarp, flower_location, by = "ID")
# # # Remove any locations that don't match the mericarp IDs
# # mericarp <- filter(mericarp, !is.na(ind_num))
# # 
# 
# #write_csv(mericarp1, "Tribulus_mericarp_data_clean_bioclim.csv")
# 
# ## Upload mericarp bio datasets
# mericarp_bio1 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Climate data/Mericarp_Bio1.csv")
# mericarp_bio4 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Climate data/Mericarp_Bio4.csv")
# mericarp_bio12 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Climate data/Mericarp_Bio12.csv")
# mericarp_bio15 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Climate data/Mericarp_Bio15.csv")
# 
# 
# # Select ID and the bioclimate variable
# mericarp_bio1 <- select(mericarp_bio1, ind_num, Bio11)
# mericarp_bio4 <- select(mericarp_bio4, ind_num, Bio41)
# mericarp_bio12 <- select(mericarp_bio12, ind_num, Bio121)
# mericarp_bio15 <- select(mericarp_bio15, ind_num, Bio151)
# 
# 
# # Join original dataset with the bioclamates datasets by ID
# mericarp1 <- left_join(mericarp, mericarp_bio1, copy=F)
# # Remove duplicates that for some reason apeared
# mericarp1 <- mericarp1 %>%
#   distinct(.keep_all = T)
# 
# mericarp1 <- left_join(mericarp1, mericarp_bio4)
# # Remove duplicates that for some reason apeared
# mericarp1 <- mericarp1 %>%
#   distinct(.keep_all = TRUE)
# 
# mericarp1 <- left_join(mericarp1, mericarp_bio12)
# # Remove duplicates that for some reason apeared
# mericarp1 <- mericarp1 %>%
#   distinct(.keep_all = TRUE)
# mericarp1 <- left_join(mericarp1, mericarp_bio15)
# # Remove duplicates that for some reason apeared
# mericarp1 <- mericarp1 %>%
#   distinct(.keep_all = TRUE)
# # 
# # 
# # 
