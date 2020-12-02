### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

### Data transformation ###
### Run diagnostic function script first!

##### Extract the variables into a single dataframe per variable ####
# Length
trib_length <- select(tribulus_mericarp, ID:individual_sample, length)
# Width
trib_width <- select(tribulus_mericarp, ID:individual_sample, width)
# Depth
trib_depth <- select(tribulus_mericarp, ID:individual_sample, depth)
# Spine length
trib_spine_length <- select(tribulus_mericarp, ID:individual_sample, spine_length) #Has zeroes in the data
# Tip distance
trib_tip_distance <- select(tribulus_mericarp, ID:individual_sample, tip_distance) #Has zeroes in the data
# Spine number
trib_spine_num <- select(tribulus_mericarp, ID:individual_sample, spine_num) #As factor
# Lower spines
trib_lower_spines <- select(tribulus_mericarp, ID:individual_sample, lower_spines) #As factor


##### Data transformation. Log and Square root per variable ####
### Log + 1 when 0 is present. (Spine_length, Tip_distance)
### Factor data? Spine number and lower spine? Will be transformed as any other data. Ask Marc.

# Length
trib_length$length_log <- log(trib_length$length) #log transformation
trib_length$length_sqr <- sqrt(trib_length$length) #square root transformation

# Width
trib_width$width_log <- log(trib_width$width) 
trib_width$width_sqr <- sqrt(trib_width$width) 

# Depth
trib_depth$depth_log <- log(trib_depth$depth) 
trib_depth$depth_sqr <- sqrt(trib_depth$depth)
# Depth has NAs Removes some of Sofias populations
trib_depth <- filter(trib_depth, !is.na(depth))

# Spine length
trib_spine_length$spine_length_log <- log(trib_spine_length$spine_length + 1) #Zeroes present
trib_spine_length$spine_length_sqr <- sqrt(trib_spine_length$spine_length)
# Spine length has NAs.Removes Grant collection, 2019 Galapagos populations and Herbarium samples
trib_spine_length <- filter(trib_spine_length, !is.na(spine_length))

# Tip distance
trib_tip_distance$tip_distance_log <- log(trib_tip_distance$tip_distance + 1) #Zeroes present
trib_tip_distance$tip_distance_sqr <- sqrt(trib_tip_distance$tip_distance)
# Tip distance has NAs. Removes Grant collections, 2019 Galapagos populations and Some Herbarium
trib_tip_distance <- filter(trib_tip_distance, !is.na(tip_distance))



##### Diagnostics for skewness and kurtosis ######

# Length
diagnostic(trib_length$length) #To estimate skewness and kurtosis. Used raw variable
diagnostic(trib_length$length_log) #Log transformed
diagnostic(trib_length$length_sqr) #Square root transformed

hist(trib_length$length, main = "Length Raw (mm)")
hist(trib_length$length_log, main = "Length Log")
hist(trib_length$length_sqr, main = "Length Squared Root")

# Width
diagnostic(trib_width$width) # Used raw variable
diagnostic(trib_width$width_log) # Log transformed
diagnostic(trib_width$width_sqr) # Square root transformed

hist(trib_width$width, main = "Width Raw (mm)")
hist(trib_width$width_log, main = "Width Log")
hist(trib_width$width_sqr, main = "Width Squared Root")

# Depth 
# Used dataset without NAs for diagnosis
diagnostic(trib_depth$depth) # Used raw variable
diagnostic(trib_depth$depth_log) # Log transformed
diagnostic(trib_depth$depth_sqr) # Square root transformed

hist(trib_depth$depth, main = "Depth Raw (mm)")
hist(trib_depth$depth_log, main = "Depth Log")
hist(trib_depth$depth_sqr, main = "Depth Squared Root")

# Spine Length
diagnostic(trib_spine_length$spine_length) # Raw
diagnostic(trib_spine_length$spine_length_log) # Log
diagnostic(trib_spine_length$spine_length_sqr) # Square root

hist(trib_spine_length$spine_length, main = "Spine Length Raw (mm)")
hist(trib_spine_length$spine_length_log, main = "Spine Length Log")
hist(trib_spine_length$spine_length_sqr, main = "Spine Length Squared Root")

# Spine distance
diagnostic(trib_tip_distance$tip_distance)
diagnostic(trib_tip_distance$tip_distance_log)
diagnostic(trib_tip_distance$tip_distance_sqr)

hist(trib_tip_distance$tip_distance, main = "Tip distance Raw (mm)")
hist(trib_tip_distance$tip_distance_log, main = "Tip distance Log")
hist(trib_tip_distance$tip_distance_sqr, main = "Tip distance Square Root")



##### To export CVS tables per trait ####
# write_csv(trib_tip_distance, "C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tip distance.csv")
