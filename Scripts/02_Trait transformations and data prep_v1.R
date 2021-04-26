### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

### Data transformation ###
# This script was used once to create the individual databases with transformed data
# These databases were exported and are found in the processed data folder

mericarp <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_mericarp_data_clean.csv")
mericarp <- mericarp %>% mutate_at(vars(ID,
                                              Herbarium,
                                              continent,
                                              country,
                                              island_group,
                                              mainland_island,
                                              galapagos_other,
                                              island_group,
                                              galapagos_island,
                                              finch_beak), list(factor))
str(mericarp)

### Mericarp data ####

# Length
meri_length <- dplyr::select(mericarp, ind_num:mericarp_num, length)
meri_length <- filter(meri_length, !is.na(length))

# Width
meri_width <- dplyr::select(mericarp, ind_num:mericarp_num, width)
meri_width <- filter(meri_width, !is.na(width))

# Depth
meri_depth <- dplyr::select(mericarp, ind_num:mericarp_num, depth)
meri_depth <- filter(meri_depth, !is.na(depth))

# Spine length
meri_spine_length <- dplyr::select(mericarp, ind_num:mericarp_num, spine_length) #Has zeroes in the data
meri_spine_length <- filter(meri_spine_length, !is.na(spine_length))

# Spine length without zero
meri_spine_length_wozero <- dplyr::filter(meri_spine_length, !spine_length == 0)

# Tip distance
meri_tip_distance <- dplyr::select(mericarp, ind_num:mericarp_num, tip_distance) #Has zeroes in the data
meri_tip_distance <- filter(meri_tip_distance, !is.na(tip_distance))

# Spine tip distance without zero
meri_tip_distance_wozero <- dplyr::filter(meri_tip_distance, !tip_distance == 0)

# # Spine number
# meri_spine_num <- select(mericarp, ind_num:mericarp_num, spine_num) #As factor

# Lower spines
meri_lower_spines <- dplyr::select(mericarp, ind_num:mericarp_num, lower_spines) #As factor
meri_lower_spines <- filter(meri_lower_spines, !is.na(lower_spines))

# Upper spines
meri_upper_spines <- dplyr::select(mericarp, ind_num:mericarp_num, upper_spines) #As factor
meri_upper_spines <- filter(meri_upper_spines, !is.na(upper_spines))

# Flower data ####

flower <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_flower_data_clean.csv")
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

# Petal length
flower <- filter(flower, !is.na(petal_length))

# flower$petal_length_log <- log(flower$petal_length) #log transformation
# flower$petal_length_sqr <- sqrt(flower$petal_length) #square root transformation

# Leaf data ####

leaf <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_leaves_data_plus CDRS herbarium_clean.csv")
leaf <- leaf %>% mutate_at(vars(ID,
                                    herbarium,
                                    continent,
                                    country,
                                    island_group,
                                    mainland_island,
                                    galapagos_other,
                                    island_group,
                                    galapagos_island,
                                    finch_beak), list(factor))


str(leaf)
# Leaf length
leaf_length <- filter(leaf, !is.na(leaf_length))
# leaf$leaf_length_log <- log(leaf$leaf_length) #log transformation
# leaf$leaf_length_sqr <- sqrt(leaf$leaf_length) #square root transformation


# Leaflet length
leaflet_length <- filter(leaf, !is.na(leaflet_length))
# leaf$leaflet_length_log <- log(leaf$leaflet_length) #log transformation
# leaf$leaflet_length_sqr <- sqrt(leaf$leaflet_length) #square root transformation

# Leaf number
# leaf$leaf_num_log <- log(leaf$leaf_num)

# Galapagos only data ####

gal_mericarp <- filter(mericarp, galapagos_other == "Galapagos")

gal_meri_length <- filter(meri_length, galapagos_other == "Galapagos")

gal_meri_width <- filter(meri_width, galapagos_other == "Galapagos")

gal_meri_depth <- filter(meri_depth, galapagos_other == "Galapagos")

gal_meri_spine_length <- filter(meri_spine_length, galapagos_other == "Galapagos")
gal_meri_spine_length_wozero <- filter(gal_meri_spine_length, !spine_length == 0)

gal_meri_tip_distance <- filter(meri_tip_distance, galapagos_other == "Galapagos")
gal_meri_tip_distance_wozero <- filter(gal_meri_tip_distance, !tip_distance == 0)

gal_meri_lower_spines <- filter(meri_lower_spines, galapagos_other == "Galapagos")

gal_meri_upper_spines <- filter(meri_upper_spines, galapagos_other == "Galapagos")

gal_petal_length <- filter(flower, galapagos_other == "Galapagos")

gal_leaf_length <- filter(leaf_length, galapagos_other == "Galapagos")

gal_leaflet_length <- filter(leaflet_length, galapagos_other == "Galapagos")


# Data transformation. Log and Square root per variable ###
# ### Log + 1 when 0 is present. (Spine_length, Tip_distance)
# ### Factor data? Spine number and lower spine? Will be transformed as any other data. Ask Marc.
# # cars binomial regression, create a variable if >0 = 1 and = 0 = 0. 
# 
# Data transformation mericarps ###
# # Length
# meri_length$length_log <- log(meri_length$length) #log transformation
# meri_length$length_sqr <- sqrt(meri_length$length) #square root transformation
# 
# # Width
# meri_width$width_log <- log(meri_width$width) 
# meri_width$width_sqr <- sqrt(meri_width$width) 
# 
# # Depth
# meri_depth$depth_log <- log(meri_depth$depth) 
# meri_depth$depth_sqr <- sqrt(meri_depth$depth)
# 
# # Spine length
# meri_spine_length$spine_length_log <- log(meri_spine_length$spine_length + 1) #Zeroes present
# meri_spine_length$spine_length_sqr <- sqrt(meri_spine_length$spine_length)
# 
# # Tip distance
# meri_tip_distance$tip_distance_log <- log(meri_tip_distance$tip_distance + 1) #Zeroes present
# meri_tip_distance$tip_distance_sqr <- sqrt(meri_tip_distance$tip_distance)
# To export CVS tables per trait ####
#write_csv(gal_mericarp, "C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Galapagos Mericarp.csv")
