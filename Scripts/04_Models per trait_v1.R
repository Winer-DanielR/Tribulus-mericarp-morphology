### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

#### Linear models per trait ####

#### Model: trait ~ mainland + year. ####
# Is there and effect between island and mainland populations? #

#### Length ####
# Raw data
length_full_raw <- lm(length ~ mainland_island + year_collected, data = trib_length) #Full model
Anova(length_full_raw)
#length_null_raw <- lm(length ~ year_collected, data = trib_length) #Null model without mainland effect

# Log data
length_full_log <- lm(length_log ~ mainland_island + year_collected, data = trib_length)
Anova(length_full_log)
#length_null_log <- lm(length_log ~ year_collected, data = trib_length)

# Square root
length_full_sqr <- lm(length_sqr ~ mainland_island + year_collected, data = trib_length)
Anova(length_full_sqr)
#length_null_sqr <- lm(length_sqr ~ year_collected, data = trib_length)

#### Width ####
# Raw data
width_full_raw <- lm(width ~ mainland_island + year_collected, data = trib_width) #Full model
Anova(width_full_raw)
#width_null_raw <- lm(width ~ year_collected, data = trib_width) #Null model without mainland effect

# Log data
width_full_log <- lm(width_log ~ mainland_island + year_collected, data = trib_width) #Full model
Anova(width_full_log)
#width_null_log <- lm(width_log ~ year_collected, data = trib_width)
# Square root
width_full_sqr <- lm(width_sqr ~ mainland_island + year_collected, data = trib_width) #Full model
Anova(width_full_sqr)
#width_null_sqr <- lm(width_sqr ~ year_collected, data = trib_width)

#### Depth ####
# Raw data
depth_full_raw <- lm(depth ~ mainland_island + year_collected, data = trib_depth) #Full model
Anova(depth_full_raw)
#depth_null_raw <- lm(depth ~ year_collected, data = trib_depth) #Null model without mainland effect
# Log data
depth_full_log <- lm(depth_log ~ mainland_island + year_collected, data = trib_depth) #Full model
Anova(depth_full_log)
#depth_null_log <- lm(depth_log ~ year_collected, data = trib_depth)
# Square root
depth_full_sqr <- lm(depth_sqr ~ mainland_island + year_collected, data = trib_depth) #Full model
Anova(depth_full_sqr)
#depth_null_sqr <- lm(depth_sqr ~ year_collected, data = trib_depth)

#### Spine Length ####
# Raw data
spine_full_raw <- lm(spine_length ~ mainland_island + year_collected, data = trib_spine_length, family = "binomial") #Full model
Anova(spine_full_raw)
#spine_null_raw <- lm(spine_length ~ year_collected, data = trib_spine_length) #Null model without mainland effect

# Log data
spine_full_log <- lm(spine_length_log ~ mainland_island + year_collected, data = trib_spine_length) #Full model
Anova(spine_full_log)
#spine_null_log <- lm(spine_length_log ~ year_collected, data = trib_spine_length)
# Square root
spine_full_sqr <- lm(spine_length_sqr ~ mainland_island + year_collected, data = trib_spine_length) #Full model
Anova(spine_full_sqr)
#spine_null_sqr <- lm(spine_length_sqr ~ year_collected, data = trib_spine_length)

#### Tip distance ####
# Raw data
tip_dist_full_raw <- lm(tip_distance ~ mainland_island + year_collected, data = trib_tip_distance) #Full model
Anova(tip_dist_full_raw)

#tip_dist_null_raw <- lm(tip_distance ~ year_collected, data = trib_tip_distance) #Null model without mainland effect
# Log data
tip_dist_full_log <- lm(tip_distance_log ~ mainland_island + year_collected, data = trib_tip_distance) #Full model
Anova(tip_dist_full_log)

#tip_dist_null_log <- lm(tip_distance_log ~ year_collected, data = trib_tip_distance)

# Square root
tip_dist_full_sqr <- lm(tip_distance_sqr ~ mainland_island + year_collected, data = trib_tip_distance) #Full model
Anova(tip_dist_full_sqr)

#tip_dist_null_sqr <- lm(tip_distance_sqr ~ year_collected, data = trib_tip_distance)

#### Spine number ####
spine_number_full <- glm(spine_num ~ mainland_island + year_collected, data = trib_spine_num, family = "binomial")
Anova(spine_number_full)
#spine_number_null <- lm(spine_num ~ year_collected, data = trib_spine_num)

#### Lower spines ####
lower_spines_full <- glm(lower_spines ~ mainland_island + year_collected, data = trib_lower_spines, family = "binomial")
Anova(lower_spines_full)
#lower_spines_null <- lm(lower_spines ~ year_collected, data = trib_lower_spines)



# ##### Checking assumptions #####
# #Used full model for diagnostics
# 
# par(mfrow=c(3,2)) # To show each data transformation in a row
# 
# ### Length ####
# # Raw
# diagnostic(length_full_raw) 
# hist(resid(length_full_raw)) 
# # Log
# diagnostic(length_full_log) 
# hist(resid(length_full_log)) 
# # Square
# diagnostic(length_full_sqr) 
# hist(resid(length_full_sqr)) 
# 
# 
# #### Width ####
# # Raw
# diagnostic(width_full_raw) 
# hist(resid(width_full_raw)) 
# # Log
# diagnostic(width_full_log) 
# hist(resid(width_full_log)) 
# # Square
# diagnostic(width_full_sqr) 
# hist(resid(width_full_sqr))
# 
# 
# #### Depth ####
# # Raw
# diagnostic(depth_full_raw) 
# hist(resid(depth_full_raw)) 
# # Log
# diagnostic(depth_full_log) 
# hist(resid(depth_full_log)) 
# # Square
# diagnostic(depth_full_sqr) 
# hist(resid(depth_full_sqr)) 
# 
# 
# #### Spine length ####
# # Raw
# diagnostic(spine_full_raw) 
# hist(resid(spine_full_raw)) 
# # Log
# diagnostic(spine_full_log) 
# hist(resid(spine_full_log)) 
# # Square
# diagnostic(spine_full_sqr) 
# hist(resid(spine_full_sqr)) 
# 
# 
# #### Tip distance ####
# # Raw
# diagnostic(tip_dist_full_raw) 
# hist(resid(tip_dist_full_raw)) 
# # Log
# diagnostic(tip_dist_full_log) 
# hist(resid(tip_dist_full_log)) 
# # Square
# diagnostic(tip_dist_full_sqr) 
# hist(resid(tip_dist_full_sqr)) 

# #### Number of spines ####
# # Raw
# diagnostic(spine_number_full) 
# hist(resid(spine_number_full)) 
# # Cant use the same model for factors check with Marc
# 
# #### Lower spines ####
# # Raw
# diagnostic(lower_spines_full) 
# hist(resid(lower_spines_full)) 
# # Cant use the same model for factors check with Marc

#### Model: trait ~ finch_beak + year. ####
# Is there and effect between finch_beak communities per trait? #

#### Filter datasets for Galapagos only data ####
gal_length <- filter(trib_length, location == "Galapagos")
gal_width <- filter(trib_width, location == "Galapagos")
gal_depth <- filter(trib_depth, location == "Galapagos")
gal_spine_length <- filter(trib_spine_length, location == "Galapagos")
gal_tip_distance <- filter(trib_tip_distance, location == "Galapagos")
gal_spine_number <- filter(trib_spine_num, location == "Galapagos")
gal_lower_spines <- filter(trib_lower_spines, location == "Galapagos")


#### Length ####
# Raw data
length_beak_raw <- lm(length ~ finch_beak + year_collected, data = gal_length) #Full model
Anova(length_beak_raw)


#### Width ####
# Raw data
width_beak_raw <- lm(width ~ finch_beak + year_collected, data = gal_width) #Full model
Anova(width_beak_raw)

#### Depth ####
# Raw data
depth_beak_raw <- lm(depth ~ finch_beak + year_collected, data = gal_depth) #Full model
Anova(depth_beak_raw)

#### Spine Length ####
# Raw data
spine_beak_raw <- lm(spine_length ~ finch_beak + year_collected, data = gal_spine_length) #Full model
Anova(spine_beak_raw)

#### Tip distance ####
# Raw data
tip_dist_beak_raw <- lm(tip_distance ~ finch_beak + year_collected, data = gal_tip_distance) #Full model
Anova(tip_dist_beak_raw)

#### Spine number ####
spine_number_beak <- glm(spine_num ~ finch_beak + year_collected, data = gal_spine_number, family = "binomial")
Anova(spine_number_beak)

#### Lower spines ####
lower_spines_beak <- glm(lower_spines ~ finch_beak + year_collected, data = gal_lower_spines, family = "binomial")
Anova(lower_spines_beak)



