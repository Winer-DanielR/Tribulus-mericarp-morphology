### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

#### Linear models per trait ####

#### Model: trait ~ mainland + year. ####
# Is there and effect between island and mainland populations? #

#### Length ####
# Raw data
length_full_raw <- lm(length ~ mainland_island + year_collected, data = trib_length) #Full model
length_null_raw <- lm(length ~ year_collected, data = trib_length) #Null model without mainland effect
# Log data
length_full_log <- lm(length_log ~ mainland_island + year_collected, data = trib_length)
length_null_log <- lm(length_log ~ year_collected, data = trib_length)
# Square root
length_full_sqr <- lm(length_sqr ~ mainland_island + year_collected, data = trib_length)
length_null_sqr <- lm(length_sqr ~ year_collected, data = trib_length)

#### Width ####
# Raw data
width_full_raw <- lm(width ~ mainland_island + year_collected, data = trib_width) #Full model
width_null_raw <- lm(width ~ year_collected, data = trib_width) #Null model without mainland effect
# Log data
width_full_log <- lm(width_log ~ mainland_island + year_collected, data = trib_width) #Full model
width_null_log <- lm(width_log ~ year_collected, data = trib_width)
# Square root
width_full_sqr <- lm(width_sqr ~ mainland_island + year_collected, data = trib_width) #Full model
width_null_sqr <- lm(width_sqr ~ year_collected, data = trib_width)

#### Depth ####
# Raw data
depth_full_raw <- lm(depth ~ mainland_island + year_collected, data = trib_depth) #Full model
depth_null_raw <- lm(depth ~ year_collected, data = trib_depth) #Null model without mainland effect
# Log data
depth_full_log <- lm(depth_log ~ mainland_island + year_collected, data = trib_depth) #Full model
depth_null_log <- lm(depth_log ~ year_collected, data = trib_depth)
# Square root
depth_full_sqr <- lm(depth_sqr ~ mainland_island + year_collected, data = trib_depth) #Full model
depth_null_sqr <- lm(depth_sqr ~ year_collected, data = trib_depth)

#### Spine Length ####
# Raw data
spine_full_raw <- lm(spine_length ~ mainland_island + year_collected, data = trib_spine_length) #Full model
spine_null_raw <- lm(spine_length ~ year_collected, data = trib_spine_length) #Null model without mainland effect
# Log data
spine_full_log <- lm(spine_length_log ~ mainland_island + year_collected, data = trib_spine_length) #Full model
spine_null_log <- lm(spine_length_log ~ year_collected, data = trib_spine_length)
# Square root
spine_full_sqr <- lm(spine_length_sqr ~ mainland_island + year_collected, data = trib_spine_length) #Full model
spine_null_sqr <- lm(spine_length_sqr ~ year_collected, data = trib_spine_length)

#### Tip distance ####
# Raw data
tip_dist_full_raw <- lm(tip_distance ~ mainland_island + year_collected, data = trib_tip_distance) #Full model
tip_dist_null_raw <- lm(tip_distance ~ year_collected, data = trib_tip_distance) #Null model without mainland effect
# Log data
tip_dist_full_log <- lm(tip_distance_log ~ mainland_island + year_collected, data = trib_tip_distance) #Full model
tip_dist_null_log <- lm(tip_distance_log ~ year_collected, data = trib_tip_distance)
# Square root
tip_dist_full_sqr <- lm(tip_distance_sqr ~ mainland_island + year_collected, data = trib_tip_distance) #Full model
tip_dist_null_sqr <- lm(tip_distance_sqr ~ year_collected, data = trib_tip_distance)

#### Spine number ####
spine_number_full <- lm(spine_num ~ mainland_island + year_collected, data = trib_spine_num)
spine_number_null <- lm(spine_num ~ year_collected, data = trib_spine_num)

#### Lower spines ####
lower_spines_full <- lm(lower_spines ~ mainland_island + year_collected, data = trib_lower_spines)
lower_spines_null <- lm(lower_spines ~ year_collected, data = trib_lower_spines)

##### Checking assumptions #####
#Used full model for diagnostics

### Length ####
# Raw
diagnostic(length_full_raw) 
hist(resid(length_full_raw)) 
# Log
diagnostic(length_full_log) 
hist(resid(length_full_log)) 
# Square
diagnostic(length_full_sqr) 
hist(resid(length_full_sqr)) 


#### Width ####
# Raw
diagnostic(width_full_raw) 
hist(resid(width_full_raw)) 
# Log
diagnostic(width_full_log) 
hist(resid(width_full_log)) 
# Square
diagnostic(width_full_sqr) 
hist(resid(width_full_sqr))


#### Depth ####
# Raw
diagnostic(depth_full_raw) 
hist(resid(depth_full_raw)) 
# Log
diagnostic(depth_full_log) 
hist(resid(depth_full_log)) 
# Square
diagnostic(depth_full_sqr) 
hist(resid(depth_full_sqr)) 


#### Spine length ####
# Raw
diagnostic(spine_full_raw) 
hist(resid(spine_full_raw)) 
# Log
diagnostic(spine_full_log) 
hist(resid(spine_full_log)) 
# Square
diagnostic(spine_full_sqr) 
hist(resid(spine_full_sqr)) 


#### Tip distance ####
# Raw
diagnostic(tip_dist_full_raw) 
hist(resid(tip_dist_full_raw)) 
# Log
diagnostic(tip_dist_full_log) 
hist(resid(tip_dist_full_log)) 
# Square
diagnostic(tip_dist_full_sqr) 
hist(resid(tip_dist_full_sqr)) 


#### Number of spines ####
# Raw
diagnostic(spine_number_full) 
hist(resid(spine_number_full)) 
# Cant use the same model for factors check with Marc

#### Lower spines ####
# Raw
diagnostic(lower_spines_full) 
hist(resid(lower_spines_full)) 
# Cant use the same model for factors check with Marc



##### ANOVA of full and null models #####

#### Length ####
# Raw
anova(length_full_raw, length_null_raw)
# Log
anova(length_full_log, length_null_log)
# Square
anova(length_full_sqr, length_null_sqr)

#### Width ####
# Raw
anova(width_full_raw, width_null_raw)
# Log
anova(width_full_log, width_null_log)
# Square
anova(width_full_sqr, width_null_sqr)

#### Depth ####
# Raw
anova(depth_full_raw, depth_null_raw)
# Log
anova(depth_full_log, depth_null_log)
# Square
anova(depth_full_sqr, depth_null_sqr)

#### Spine length ####
# Raw
anova(spine_full_raw, spine_null_raw)
# Log
anova(spine_full_log, spine_null_log)
# Square
anova(spine_full_sqr, spine_null_sqr)

#### Tip distance ####
# Raw
anova(tip_dist_full_raw, tip_dist_null_raw)
# Log
anova(tip_dist_full_log, tip_dist_null_log)
# Square
anova(tip_dist_full_sqr, tip_dist_null_sqr)

##### EMMEANS #####

#### Length ####
emmean_length_raw <- emmeans(length_full_raw, specs = "mainland_island")
emmean_length_log <- emmeans(length_full_log, specs = "mainland_island")
emmean_length_sqr <- emmeans(length_full_sqr, specs = "mainland_island")

#### Width ####
emmean_width_raw <- emmeans(width_full_raw, specs = "mainland_island")
emmean_width_log <- emmeans(width_full_log, specs = "mainland_island")
emmean_width_sqr <- emmeans(width_full_sqr, specs = "mainland_island")

#### Depth ####
emmean_depth_raw <- emmeans(depth_full_raw, specs = "mainland_island")
emmean_depth_log <- emmeans(depth_full_log, specs = "mainland_island")
emmean_depth_sqr <- emmeans(depth_full_sqr, specs = "mainland_island")

#### Spine length ####
emmean_spine_raw <- emmeans(spine_full_raw, specs = "mainland_island")
emmean_spine_log <- emmeans(spine_full_log, specs = "mainland_island")
emmean_spine_sqr <- emmeans(spine_full_sqr, specs = "mainland_island")

#### Tip distance ####
emmean_tip_dist_raw <- emmeans(tip_dist_full_raw, specs = "mainland_island")
emmean_tip_dist_log <- emmeans(tip_dist_full_log, specs = "mainland_island")
emmean_tip_dist_sqr <- emmeans(tip_dist_full_sqr, specs = "mainland_island")