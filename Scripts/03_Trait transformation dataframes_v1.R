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
leaflet_length <- filter(leaf, !is.na(leaflet_length))

# Leaflet length
leaf$leaflet_length_log <- log(leaf$leaflet_length) #log transformation
leaf$leaflet_length_sqr <- sqrt(leaf$leaflet_length) #square root transformation


##### Diagnostic of skew and kurtosis ######
par(mfrow=c(3,3)) # To show each data transformation in a row

# Mericarp data ####
# Length
diagnostic(meri_length$length) # Kurtosis=0.79116 Skew=0.415412
diagnostic(meri_length$length_log) # Kurtosis=0.297 Skew=-0.3279 <--
diagnostic(meri_length$length_sqr) # Kurtosis=0.2232 Skew=0.03719

#hist(meri_length$length, main = "Length Raw (mm)")
#hist(meri_length$length_log, main = "Length Log")
#hist(meri_length$length_sqr, main = "Length Squared Root")

# Width
diagnostic(meri_width$width)  # K = 1.4591 Skew=0.577
diagnostic(meri_width$width_log) # Kurtosis=0.8601 Skew=-0.333
diagnostic(meri_width$width_sqr) # Kurtosis=0.7279 Skew=0.11903 <--

#hist(meri_width$width, main = "Width Raw (mm)")
#hist(meri_width$width_log, main = "Width Log")
#hist(meri_width$width_sqr, main = "Width Squared Root")

# Depth 
diagnostic(meri_depth$depth) # Kurtosis=0.4944 Skew=0.2754
diagnostic(meri_depth$depth_log) # Kurtosis=1.3367 Skew=-0.486
diagnostic(meri_depth$depth_sqr) # Kurtosis=0.5262 Skew=-0.07915 <--

#hist(meri_depth$depth, main = "Depth Raw (mm)")
#hist(meri_depth$depth_log, main = "Depth Log")
#hist(meri_depth$depth_sqr, main = "Depth Squared Root")

# Spine Length
diagnostic(meri_spine_length$spine_length) # Raw Kurtosis=1.7114 Skew=-0.288 <--
diagnostic(meri_spine_length$spine_length_log) # Log Kurtosis=7.407063 Skew=-2.4216
diagnostic(meri_spine_length$spine_length_sqr) # Square root Kurtosis=6.96184 Skew=-2.24091

#hist(meri_spine_length$spine_length, main = "Spine Length Raw (mm)")
#hist(meri_spine_length$spine_length_log, main = "Spine Length Log")
# hist(trib_spine_length$spine_length_sqr, main = "Spine Length Squared Root")

# Spine distance
diagnostic(meri_tip_distance$tip_distance) # Kurtosis=1.0616 Skew=-0.8111 <--
diagnostic(meri_tip_distance$tip_distance_log) # Kurtosis=7.3483 Skew=-2.7055
diagnostic(meri_tip_distance$tip_distance_sqr) # Kurtosis=5.8474 Skew=-2.3230

# hist(trib_tip_distance$tip_distance, main = "Tip distance Raw (mm)")
# hist(trib_tip_distance$tip_distance_log, main = "Tip distance Log")
# hist(trib_tip_distance$tip_distance_sqr, main = "Tip distance Square Root")

# Flower data ####
# Petal length
diagnostic(flower$petal_length) # Kurtosis=-0.2732 Skew=-0.11984 <--
diagnostic(flower$petal_length_log) # Kurtosis=0.9109 Skew=-0.9009
diagnostic(flower$petal_length_sqr) # Kurtosis=0.04273 Skew=-0.486167

# Leaf data ####
# Leaf length
diagnostic(leaf_length$leaf_length) # Kurtosis=1.70458 Skew=1.083742
diagnostic(leaf_length$leaf_length_log) # Kurtosis=-0.26895 Skew=-0.13528 <--
diagnostic(leaf_length$leaf_length_sqr) # Kurtosis=0.11726 Skew=0.447739

# Leaflet length
diagnostic(leaflet_length$leaflet_length) # Kurtosis=2.08336 Skew=1.16933
diagnostic(leaflet_length$leaflet_length_log) # Kurtosis=-0.15100 Skew=0.06675 <--
diagnostic(leaflet_length$leaflet_length_sqr) # Kurtosis=0.45665 Skew=0.58911

##### To export CVS tables per trait ####
#write_csv(leaflet_length, "C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Leaflet Length.csv")
