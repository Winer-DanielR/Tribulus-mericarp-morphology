### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

### Data transformation ###
### Run diagnostic function script first!

##### Extract the variables into a single data frame per variable ####

### Mericarp data ####

# Length
meri_length <- select(mericarp, ind_num:mericarp_num, length)
# Width
meri_width <- select(mericarp, ind_num:mericarp_num, width)
# Depth
meri_depth <- select(mericarp, ind_num:mericarp_num, depth)
# Spine length
meri_spine_length <- select(mericarp, ind_num:mericarp_num, spine_length) #Has zeroes in the data
# Tip distance
meri_tip_distance <- select(mericarp, ind_num:mericarp_num, tip_distance) #Has zeroes in the data
# Spine number
meri_spine_num <- select(mericarp, ind_num:mericarp_num, spine_num) #As factor
# Lower spines
meri_lower_spines <- select(mericarp, ind_num:mericarp_num, lower_spines) #As factor



##### Data transformation. Log and Square root per variable ####
### Log + 1 when 0 is present. (Spine_length, Tip_distance)
### Factor data? Spine number and lower spine? Will be transformed as any other data. Ask Marc.
# cars binomial regression, create a variable if >0 = 1 and = 0 = 0. 

# Data transformation mericarps ####
# Length
meri_length$length_log <- log(meri_length$length) #log transformation
meri_length$length_sqr <- sqrt(meri_length$length) #square root transformation

# Width
meri_width$width_log <- log(meri_width$width) 
meri_width$width_sqr <- sqrt(meri_width$width) 

# Depth
meri_depth$depth_log <- log(meri_depth$depth) 
meri_depth$depth_sqr <- sqrt(meri_depth$depth)
# Depth has NAs Removes some of Sofias populations
meri_depth <- filter(meri_depth, !is.na(depth))

# Spine length
meri_spine_length$spine_length_log <- log(meri_spine_length$spine_length + 1) #Zeroes present
meri_spine_length$spine_length_sqr <- sqrt(meri_spine_length$spine_length)
# Spine length has NAs.Removes Grant collection, 2019 Galapagos populations and Herbarium samples
meri_spine_length <- filter(meri_spine_length, !is.na(spine_length))

# Tip distance
meri_tip_distance$tip_distance_log <- log(meri_tip_distance$tip_distance + 1) #Zeroes present
meri_tip_distance$tip_distance_sqr <- sqrt(meri_tip_distance$tip_distance)
# Tip distance has NAs. Removes: 2019 Galapagos populations and Some Herbarium samples
meri_tip_distance <- filter(meri_tip_distance, !is.na(tip_distance))

# Flower data transformation ####
# Petal length
flower$petal_length_log <- log(flower$petal_length) #log transformation
flower$petal_length_sqr <- sqrt(flower$petal_length) #square root transformation

# Leaf data transformation ####
# Leaf length
leaf$leaf_length_log <- log(leaf$leaf_length) #log transformation
leaf$leaf_length_sqr <- sqrt(leaf$leaf_length) #square root transformation
leaf_length <- filter(leaf, !is.na(leaf_length))


# Leaflet length
leaf$leaflet_length_log <- log(leaf$leaflet_length) #log transformation
leaf$leaflet_length_sqr <- sqrt(leaf$leaflet_length) #square root transformation
leaflet_length <- filter(leaf, !is.na(leaflet_length))

# Leaf number
leaf$leaf_num_log <- log(leaf$leaf_num)

#### Filter datasets for Galapagos only data ####
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

##### To export CVS tables per trait ####
#write_csv(leaflet_length, "C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Leaflet Length.csv")
