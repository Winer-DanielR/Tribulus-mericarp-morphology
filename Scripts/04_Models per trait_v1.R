### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
# By: Daniel Reyes Corral

# Linear models per trait ####

# Model 1: trait ~ mainland/island + year ####
# What is the effect of mericarp, flower and leaf morphology size between
# island and mainland populations of Tribulus cistoides

#### Mericarp data ####
# Run simple linear mixed effect model:
# mainland/island and year as variables
# with ID as random effect per trait
# each model uses the untrasformed data, log transformed and square root transformed

#### Length ####
# For length, the untransformed data seems the best
meri_length_m1<- lmer(length ~ mainland_island +
                       year_collected +
                      (1|ID),
                      data = meri_length,
                      REML = F)
meri_length_m2<- lmer(log(length) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length,
                      REML = F)
meri_length_m3<- lmer(sqrt(length) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length,
                      REML = F)
# type III test
#ANOVA_length_m1 <-as.data.frame(Anova(meri_length_m1))
#summary(meri_length_m1)

# Diagnostic
# I used a custom diagnostic function and the testResiduals function
# from the DHARMa package. This function shows the QQ residuals, dispersion test
# and outlier tests. 

# Diagnostic custom function
#par(mfrow = c(1, 3))
#diagnostic(resid(meri_length_m1))
#diagnostic(resid(meri_length_m2))
#diagnostic(resid(meri_length_m3))

# Diagnostics with DHARMA
#testResiduals(meri_length_m1)
#testResiduals(meri_length_m2)
#testResiduals(meri_length_m3)

# After selecting the data transformation that converged the most I decided to
# only show the output of that model.

#### Length mainland galapagos ####
# For length, the untransformed data seems the best
meri_mainland_gal_length_m1<- lmer(length ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length_mainland_gal,
                      REML = F)
meri_mainland_gal_length_m2<- lmer(log(length) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length_mainland_gal,
                      REML = F)
meri_mainland_gal_length_m3<- lmer(sqrt(length) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length_mainland_gal,
                      REML = F)
# type III test
Anova(meri_mainland_gal_length_m1)
#ANOVA_length_m1 <-as.data.frame(Anova(meri_length_m1))
#summary(meri_length_m1)

# Diagnostic
# I used a custom diagnostic function and the testResiduals function
# from the DHARMa package. This function shows the QQ residuals, dispersion test
# and outlier tests. 

# Diagnostic custom function
#par(mfrow = c(1, 3))
# diagnostic(resid(meri_mainland_gal_length_m1))
# diagnostic(resid(meri_mainland_gal_length_m2))
# diagnostic(resid(meri_mainland_gal_length_m3))

# Diagnostics with DHARMA
# testResiduals(meri_mainland_gal_length_m1)
# testResiduals(meri_mainland_gal_length_m2)
# testResiduals(meri_mainland_gal_length_m3)

# After selecting the data transformation that converged the most I decided to
# only show the output of that model.


#### Width ####
# For width, squared transformed seemed the best_
meri_width_m1 <- lmer(width ~ mainland_island +
                     year_collected +
                     (1|ID),
                     data = meri_width, 
                     REML = F)
meri_width_m2 <- lmer(log(width) ~ mainland_island +
                       year_collected +
                       (1|ID),
                     data = meri_width, 
                     REML = F)
meri_width_m3 <- lmer(sqrt(width) ~ mainland_island +
                       year_collected +
                       (1|ID),
                     data = meri_width, 
                     REML = F)
# # type III test
Anova(meri_width_m3)
# # Diagnostic
 
# # Residual histograms
# diagnostic(resid(meri_width_m1))
# diagnostic(resid(meri_width_m2))
# diagnostic(resid(meri_width_m3))

# # DHARMa
# testResiduals(meri_width_m1)
# testResiduals(meri_width_m2)
# testResiduals(meri_width_m3)

#### Width mainland galapagos ####
# For width, log transformed seemed the best
meri_width_mainland_gal_m1 <- lmer(width ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_width_mainland_gal, 
                      REML = F)
meri_width_mainland_gal_m2 <- lmer(log(width) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_width_mainland_gal, 
                      REML = F)
meri_width_mainland_gal_m3 <- lmer(sqrt(width) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_width_mainland_gal, 
                      REML = F)
# # type III test
Anova(meri_width_mainland_gal_m2)
# # Diagnostic

# # Residual histograms
# diagnostic(resid(meri_width_mainland_gal_m1))
# diagnostic(resid(meri_width_mainland_gal_m2))
# diagnostic(resid(meri_width_mainland_gal_m3))

# # DHARMa
# testResiduals(meri_width_mainland_gal_m1)
# testResiduals(meri_width_mainland_gal_m2)
# testResiduals(meri_width_mainland_gal_m3)

#### Depth mainland galapagos ####
# For depth untransformed data seems to work the best
meri_depth_mainland_gal_m1 <- lmer(depth ~ mainland_island +
                    year_collected +
                    (1|ID),
                    data=meri_depth_mainland_gal, 
                    REML = F)
meri_depth_mainland_gal_m2 <- lmer(log(depth) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data=meri_depth_mainland_gal, 
                      REML = F)
meri_depth_mainland_gal_m3 <- lmer(sqrt(depth) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data=meri_depth_mainland_gal, 
                      REML = F)
# type III test
Anova(meri_depth_mainland_gal_m1)

# # Diagnostic
hist(resid(meri_depth_mainland_gal_m1), breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_depth_mainland_gal_m1))
# diagnostic(resid(meri_depth_m2))
# diagnostic(resid(meri_depth_m3))
# 
# # DHARMa
# testResiduals(meri_depth_mainland_gal_m1)
# testResiduals(meri_depth_mainland_gal_m2)
# testResiduals(meri_depth_mainland_gal_m3)

#### Depth ####
# For depth untransformed data seems to work the best
meri_depth_m1 <- lmer(depth ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
meri_depth_m2 <- lmer(log(depth) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
meri_depth_m3 <- lmer(sqrt(depth) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
# type III test
Anova(meri_depth_m1)

# # Diagnostic
# hist(resid(meri_depth_m1), breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_depth_m1))
# diagnostic(resid(meri_depth_m2))
# diagnostic(resid(meri_depth_m3))
# 
# # DHARMa
# testResiduals(meri_depth_m1)
# testResiduals(meri_depth_m2)
# testResiduals(meri_depth_m3)



#### Spine length ####
# I think the untransformed works best but the 0s may affect the results
meri_spine_length_m1 <-lmer(spine_length ~ mainland_island +
                           year_collected +
                           (1|ID),
                           data=meri_spine_length,
                           REML=F)

meri_spine_length_m2 <-lmer(log(spine_length + 1) ~ mainland_island +
                              year_collected +
                              (1|ID),
                            data=meri_spine_length,
                            REML=F)

meri_spine_length_m3 <-lmer(sqrt(spine_length) ~ mainland_island +
                              year_collected +
                              (1|ID),
                            data=meri_spine_length,
                            REML=F)

# type III test
Anova(meri_spine_length_m1)
 
# Diagnostic
# # Residual histograms
# diagnostic(resid(meri_spine_length_m1))
# diagnostic(resid(meri_spine_length_m2))
# diagnostic(resid(meri_spine_length_m3))
# hist(resid(meri_spine_length_m1), breaks = 20)

# # DHARMa
# testResiduals(meri_spine_length_m1)
# testResiduals(meri_spine_length_m2)
# testResiduals(meri_spine_length_m3)
# testZeroInflation(meri_depth_m1)

##### Removed Zero Spine Length ####
# Square transformed data seems works best
meri_spine_length_wozero_m1 <-lmer(spine_length ~ mainland_island +
                              year_collected +
                              (1|ID),
                            data=meri_spine_length_wozero,
                            REML=F)

meri_spine_length_wozero_m2 <-lmer(log(spine_length + 1) ~ mainland_island +
                              year_collected +
                              (1|ID),
                            data=meri_spine_length_wozero,
                            REML=F)

meri_spine_length_wozero_m3 <-lmer(sqrt(spine_length) ~ mainland_island +
                              year_collected +
                              (1|ID),
                            data=meri_spine_length_wozero,
                            REML=F)

# type III test
Anova(meri_spine_length_wozero_m3)

# Diagnostic

# # Residual histograms
# diagnostic(resid(meri_spine_length_wozero_m1))
# diagnostic(resid(meri_spine_length_wozero_m2))
# diagnostic(resid(meri_spine_length_wozero_m3))




# # DHARMa
# testResiduals(meri_spine_length_wozero_m1)
# testResiduals(meri_spine_length_wozero_m2)
# testResiduals(meri_spine_length_wozero_m3)
# plotResiduals(meri_spine_length_wozero_m3)

# Filter Spine Lenght without zero ###
# Removed values > 10
# Untransformed data seems to work best but either model without zeros or filtered

spine_length_filter <- filter(meri_spine_length_wozero, !(spine_length >= 10))
hist(spine_length_filter$spine_length, breaks = 20)

spine_length_filter_m1 <-lmer(spine_length ~ mainland_island +
                                     year_collected +
                                     (1|ID),
                                   data=spine_length_filter,
                                   REML=F)

spine_length_filter_m2 <-lmer(log(spine_length + 1) ~ mainland_island +
                                     year_collected +
                                     (1|ID),
                                   data=spine_length_filter,
                                   REML=F)

spine_length_filter_m3 <-lmer(sqrt(spine_length) ~ mainland_island +
                                     year_collected +
                                     (1|ID),
                                   data=spine_length_filter,
                                   REML=F)

# type III test
Anova(spine_length_filter_m3)

# Diagnostic

# # Residual histograms
#diagnostic(resid(spine_length_filter_m1))
#diagnostic(resid(spine_length_filter_m2))
#diagnostic(resid(spine_length_filter_m3))

# # DHARMa
#testResiduals(spine_length_filter_m1)
# testResiduals(spine_length_filter_m2)
# testResiduals(spine_length_filter_m3)
# # plotResiduals(spine_length_filter_m1)


#### Tip distance ####
# Raw data looks the best
meri_tip_distance_m1 <- lmer(tip_distance ~ mainland_island +
                                 year_collected +
                                 (1|ID),
                             data=meri_tip_distance,REML=F)

meri_tip_distance_m2 <- lmer(log(tip_distance + 1) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip_distance,REML=F)

meri_tip_distance_m3 <- lmer(sqrt(tip_distance) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip_distance,REML=F)


# # type III test
Anova(meri_tip_distance_m1)

# # Diagnostic
# hist(resid(meri_spine_length_m1), breaks = 20)
# hist(meri_tip_distance$tip_distance, breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_tip_distance_m1))
# diagnostic(resid(meri_tip_distance_m2))
# diagnostic(resid(meri_tip_distance_m3))

# # DHARMa
# testResiduals(meri_tip_distance_m1)
# testResiduals(meri_tip_distance_m2)
# testResiduals(meri_tip_distance_m3)


###### Removed Zero Tip distance ####
# Raw data looks the best
meri_tip_distance_m4 <- lmer(tip_distance ~ mainland_island +
                               year_collected +
                               (1|ID),
                               na.action = na.exclude,
                             data=meri_tip_distance_wozero,REML=F)

meri_tip_distance_m5 <- lmer(log(tip_distance + 1) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip_distance_wozero,REML=F)

meri_tip_distance_m6 <- lmer(sqrt(tip_distance) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip_distance_wozero,REML=F)


# # type III test
Anova(meri_tip_distance_m4)

# Diagnostic
 # diagnostic(resid(meri_tip_distance_m4))
 # diagnostic(resid(meri_tip_distance_m5))
 # diagnostic(resid(meri_tip_distance_m6))

# # DHARMa
# testResiduals(meri_tip_distance_m1)
# testResiduals(meri_tip_distance_m2)
# testResiduals(meri_tip_distance_m3)

# Check residual distributions after removing mericarps without spines.
 
meri_tip_distance_wozero$residuals <- resid(meri_tip_distance_m4)
hist(resid(meri_tip_distance_m4), breaks = 20)
hist(meri_tip_distance_wozero$tip_distance, breaks = 20)

# Filter residuals that are lower than 10
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero,
                                          !residuals < -5)
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero_filter,
                                          !residuals > 5)

# Removes specimen 383

hist(meri_tip_distance_wozero_filter$residuals, breaks = 20)
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero_filter, !is.na(residuals))


###### Removed Zero Tip distance mainland galapagos ####
# Raw data looks the best
meri_tip_distance_mainland_galapagos_m4 <- lmer(tip_distance ~ mainland_island +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_mainland_gal,REML=F)

meri_tip_distance_mainland_galapagos_m5 <- lmer(log(tip_distance + 1) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip_distance_wozero_mainland_gal,REML=F)

meri_tip_distance_mainland_galapagos_m6 <- lmer(sqrt(tip_distance) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip_distance_wozero_mainland_gal,REML=F)


# # type III test
Anova(meri_tip_distance_mainland_galapagos_m4)
Anova(meri_tip_distance_mainland_galapagos_m5)
Anova(meri_tip_distance_mainland_galapagos_m6)


# Diagnostic
diagnostic(resid(meri_tip_distance_mainland_galapagos_m4))
diagnostic(resid(meri_tip_distance_mainland_galapagos_m5))
diagnostic(resid(meri_tip_distance_mainland_galapagos_m6))

# # DHARMa
testResiduals(meri_tip_distance_mainland_galapagos_m4)
testResiduals(meri_tip_distance_mainland_galapagos_m5)
testResiduals(meri_tip_distance_mainland_galapagos_m6)

# Check residual distributions after removing mericarps without spines.

meri_tip_distance_wozero_mainland_gal$residuals <- resid(meri_tip_distance_mainland_galapagos_m4)
hist((meri_tip_distance_wozero_mainland_gal_filter$residuals), breaks = 20)

# Filter residuals that are lower than 10
meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal,
                                          !residuals < -5)
meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal_filter,
                                          !residuals > 5)


meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal_filter, !is.na(residuals))



##### Filter Tip distance ####
# Raw data works best

meri_tip_distance_m7 <- lmer(tip_distance ~ mainland_island +
                                      year_collected +
                                      (1|ID),
                                    na.action = na.exclude,
                                    data=meri_tip_distance_wozero_filter,REML=F)

meri_tip_distance_m8 <- lmer(log(tip_distance) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_filter,REML=F)

meri_tip_distance_m9 <- lmer(sqrt(tip_distance) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_filter,REML=F)


# Diagnostic
 # diagnostic(resid(meri_tip_distance_m7))
 # diagnostic(resid(meri_tip_distance_m8))
 # diagnostic(resid(meri_tip_distance_m9))

#Anova
Anova(meri_tip_distance_m7)

# DHARMa
# testResiduals(meri_tip_distance_m7)

##### Filter Tip distance mainland gal ####
# Raw data works best

meri_tip_distance_mainland_gal_m7 <- lmer(tip_distance ~ mainland_island +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)

meri_tip_distance_mainland_gal_m8 <- lmer(log(tip_distance) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)

meri_tip_distance_mainland_gal_m9 <- lmer(sqrt(tip_distance) ~ mainland_island +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)


# Diagnostic
# diagnostic(resid(meri_tip_distance_mainland_gal_m7))
# diagnostic(resid(meri_tip_distance_mainland_gal_m8))
# diagnostic(resid(meri_tip_distance_mainland_gal_m9))

#Anova
Anova(meri_tip_distance_mainland_gal_m7)

# DHARMa
# testResiduals(meri_tip_distance_mainland_gal_m7)

#### Lower spines ####
# meri_lower_spines_m1 <- glm(lower_spines ~ mainland_island + 
#                                    year_collected, 
#                              data = meri_lower_spines, 
#                              family = "binomial")

meri_lower_spines_m1_glmm <- glmmTMB(factor(lower_spines) ~ mainland_island +
                                       (1|ID),
                                     data = meri_lower_spines,
                                     family = binomial)

meri_lower_spines_mainland_gal_glmm <- glmmTMB(factor(lower_spines) ~ mainland_island +
                                       (1|ID),
                                     data = meri_lower_spines_mainland_gal,
                                     family = binomial)

str(meri_lower_spines)
# # Type III test
# Anova(meri_lower_spines_m1)
Anova(meri_lower_spines_m1_glmm)
Anova(meri_lower_spines_mainland_gal_glmm)

# Diagnostic
# # Residual histograms
#diagnostic(resid(meri_lower_spines_m1_glmm))


###### Upper spines ####
# meri_upper_spines_m1 <- glm(upper_spines ~ mainland_island + 
#                                year_collected, 
#                              data = meri_upper_spines, 
#                              family = "binomial")

meri_upper_spines_m1_glmm <- glmmTMB(upper_spines ~ mainland_island +
                                       (1|ID),
                                     data = meri_upper_spines,
                                     family = binomial)

# # Type III test
#Anova(meri_upper_spines_m1)
Anova(meri_upper_spines_m1_glmm)

# # Diagnostic
# 
# # Residual histograms
#diagnostic(resid(meri_upper_spines_m1_glmm))

# # DHARMa
# testResiduals(meri_lower_spines_m1)

# Petal length data ####
# The raw data works best
flower_m1 <- lmer(petal_length ~ mainland_island +
               year_collected +
               (1|ID),
               data=flower,
               na.action = na.exclude,
               REML=F)

flower_m2 <- lmer(log(petal_length) ~ mainland_island +
                    year_collected +
                    (1|ID),
                  data=flower,
                  REML=F)

flower_m3 <- lmer(sqrt(petal_length) ~ mainland_island +
                    year_collected +
                    (1|ID),
                  data=flower,
                  REML=F)

# #type III test using lmertest
Anova(flower_m1)
# 
# # Diagnostic
# Filter the highest residuals
#hist(flower$petal_length, breaks = 20)

# Create a column with model residuals to filter the dataset
flower$residuals <- resid(flower_m1) 
hist(resid(flower_m1), breaks = 20)
# Filter residuals outside -5 and 5
flower_filter <- filter(flower, !is.na(residuals))
flower_filter <- filter(flower_filter, !residuals >=5)
flower_filter <- filter(flower_filter, !residuals <= -5)
hist(flower_filter$residuals, breaks = 20)

# This filter removed specimens 240, 351, 320 (<-5) and
# 454, 319, 207, 249, 340 (>5)

# Filtered model
# Raw data works best in the filter model
flower_m4 <- lmer(petal_length ~ mainland_island +
                    year_collected +
                    (1|ID),
                  data=flower_filter,
                  na.action = na.exclude,
                  REML=F)

flower_m5 <- lmer(log(petal_length) ~ mainland_island +
                    year_collected +
                    (1|ID),
                  data=flower_filter,
                  na.action = na.exclude,
                  REML=F)

flower_m6 <- lmer(sqrt(petal_length) ~ mainland_island +
                    year_collected +
                    (1|ID),
                  data=flower_filter,
                  na.action = na.exclude,
                  REML=F)

# #type III test using lmertest
Anova(flower_m4)

# # Residual histograms
# diagnostic(resid(flower_m1))
# diagnostic(resid(flower_m2))
# diagnostic(resid(flower_m3))
#diagnostic(resid(flower_m4))
# diagnostic(resid(flower_m5))
# diagnostic(resid(flower_m6))
 
# # DHARMa
# testResiduals(flower_m4)
# testResiduals(flower_m2)
# testResiduals(flower_m3)
#testResiduals(flower_m4)

# Leaves data ####
# run simple linear mixed effects models on each leaf treat

###### Leaf length ####
# square root transformed data works best
leaf_length_m1 <- lmer(leaf_length ~ mainland_island +
                       year_collected +
                       (1|ID),data=leaf_length,REML=F)

leaf_length_m2 <- lmer(log(leaf_length) ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m3 <- lmer(sqrt(leaf_length) ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
# # Type II test
Anova(leaf_length_m3)
# 
# # Diagnostic
# 
# # Residual histograms
# diagnostic(resid(leaf_length_m1))
# diagnostic(resid(leaf_length_m2))
#diagnostic(resid(leaf_length_m3))
# 
# # DHARMa
# testResiduals(leaf_length_m1)
# testResiduals(leaf_length_m2)
# testResiduals(leaf_length_m3)

#### Leaflet length ####
# Log transformed data works best
leaflet_length_m1 <- lmer(leaflet_length ~ mainland_island +
                         year_collected +
                         (1|ID),
                        data=leaflet_length,
                         REML = F)

leaflet_length_m2 <- lmer(log(leaflet_length) ~ mainland_island +
                          year_collected +
                          (1|ID),
                        data=leaflet_length,
                        REML = F)

leaflet_length_m3 <- lmer(sqrt(leaflet_length) ~ mainland_island +
                          year_collected +
                          (1|ID),
                        data=leaflet_length,
                        REML = F)
# # Type III test
Anova(leaflet_length_m2)
# 
# # Diagnostic
# 
# # Diagnostic function
# diagnostic(resid(leaflet_length_m1))
#diagnostic(resid(leaflet_length_m2))
# diagnostic(resid(leaflet_length_m3))
# 
# # DHARMa
# testResiduals(leaflet_length_m1)
# testResiduals(leaflet_length_m2)
# testResiduals(leaflet_length_m3)


#### Leaflet number ####
leaflet_num_m1 <- lmer(number_of_leaflets ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaflet_num_m2 <- lmer(log(number_of_leaflets) ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaflet_num_m3 <- lmer((number_of_leaflets)**2 ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaflet_num_m4 <- glm(number_of_leaflets ~ mainland_island +
                        year_collected,
                      family = gaussian,
                      data=leaf_length)

leaflet_num_m5 <- glmmTMB(number_of_leaflets ~ mainland_island +
                            (1|ID), data = leaf_length,
                          family = nbinom1())

plot(leaflet_num_m3)

Anova(leaflet_num_m5)

# # Diagnostic
# # Residual histograms
#diagnostic(resid(leaflet_num_m5))
# 
# # DHARMa
# testResiduals(leaflet_num_m1)


# Model 2 trait ~ galapagos/other islands ####
# Is there an effect between tribulus in galapagos compared to other island systems
# This comparison is possible with flower and leaf datasets

#### Petal length data ####
# Used filter data
# The squared data seems to work best
flower_m7 <- lmer(petal_length ~ galapagos_other +
                    year_collected +
                    (1|ID),
                  data=flower_filter,
                  REML=F)

flower_m8 <- lmer(log(petal_length) ~ galapagos_other +
                    year_collected +
                    (1|ID),
                  data=flower_filter,
                  REML=F)

flower_m9 <- lmer(sqrt(petal_length) ~ galapagos_other +
                    year_collected +
                    (1|ID),
                  data=flower_filter,
                  REML=F)

# #type III test
Anova(flower_m9)

# # Diagnostic
# 
# # Residual histograms
# diagnostic(resid(flower_m7))
# diagnostic(resid(flower_m8))
#diagnostic(resid(flower_m9))
# 
# # DHARMa
# testResiduals(flower_m7)
# testResiduals(flower_m8)
# testResiduals(flower_m9)

#### Leaf length ####
# Log transformed data works best
leaf_length_m4 <- lmer(leaf_length ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m5 <- lmer(log(leaf_length) ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m6 <- lmer(sqrt(leaf_length) ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
# # Type III test
Anova(leaf_length_m5)
# 
# # Diagnostic
# 
# # Residual histograms
# diagnostic(resid(leaf_length_m4))
#diagnostic(resid(leaf_length_m5))
# diagnostic(resid(leaf_length_m6))
# 
# # DHARMa
# testResiduals(leaf_length_m4)
# testResiduals(leaf_length_m5)
# testResiduals(leaf_length_m6)

#### Leaflet length ####
# Log transformed data works best
leaflet_length_m4 <- lmer(leaflet_length ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)

leaflet_length_m5 <- lmer(log(leaflet_length) ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)

leaflet_length_m6 <- lmer(sqrt(leaflet_length) ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
# # Type III test
Anova(leaflet_length_m5)
# 
# # Diagnostic
# 
# # Residual histograms
# diagnostic(resid(leaflet_length_m4))
#diagnostic(resid(leaflet_length_m5))
# diagnostic(resid(leaflet_length_m6))
# 
# # DHARMa
# testResiduals(leaflet_length_m4)
# testResiduals(leaflet_length_m5)
# testResiduals(leaflet_length_m6)

#### Leaflet number ####
leaflet_num_m6 <- glm(number_of_leaflets ~ galapagos_other +
                        year_collected,
                      family = poisson,
                      data=leaf_length)

leaflet_num_m7 <- glm(number_of_leaflets ~ galapagos_other +
                        year_collected,
                      family = gaussian,
                      data=leaf_length)

leaflet_num_m8 <- glmmTMB(number_of_leaflets ~ galapagos_other +
                            (1|ID), data = leaf_length,
                          family = nbinom1())


Anova(leaflet_num_m8)
 
# # Diagnostic
# # Residual histograms
# diagnostic(resid(leaflet_num_m8))
# 
# # DHARMa
# testResiduals(leaflet_num_m2)

# Model 3 trait ~ finch beak ####
# What is the effect of finch community within the galapagos islands
#### Mericarp data ####
##### Length ####
# For length, the raw data seems the best_

meri_length_m4 <- lmer(length ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_length,
                      REML = F)
meri_length_m5 <- lmer(log(length) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_length,
                      REML = F)
meri_length_m6 <- lmer(sqrt(length) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_length,
                      REML = F)
# # type III test
Anova(meri_length_m4)
# 
# # Diagnostic
# 
# # Residuals histogram
# diagnostic(resid(meri_length_m4))
 # diagnostic(resid(meri_length_m5))
 # diagnostic(resid(meri_length_m6))
# 
# # DHARMA <- this is the way with DHAMa
# testResiduals(meri_length_m4)
# testResiduals(meri_length_m5)
# testResiduals(meri_length_m6)
# 
# plotResiduals(meri_length_m4)
# plotResiduals(meri_length_m5)
# plotResiduals(meri_length_m6)

##### Width ####
# For width, squared transformed seemed the best_
meri_width_m4 <- lmer(width ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width, 
                      REML = F)
meri_width_m5 <- lmer(log(width) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width, 
                      REML = F)
meri_width_m6 <- lmer(sqrt(width) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width, 
                      REML = F)
# # type III test
Anova(meri_width_m4)
# 
# # Diagnostic
# 
# # Residual histograms
#  diagnostic(resid(meri_width_m4))
#  diagnostic(resid(meri_width_m5))
#  diagnostic(resid(meri_width_m6))
# # 
# # DHARMa
# testResiduals(meri_width_m4)
# testResiduals(meri_width_m5)
# testResiduals(meri_width_m6)

# Filter width
# Square transformed data
hist(gal_meri_width$width, breaks = 20)
gal_meri_width_filter <- filter(gal_meri_width, !width >= 6) 
 
meri_width_m7 <- lmer(width ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width_filter, 
                      REML = F)

meri_width_m8 <- lmer(log(width) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width_filter, 
                      REML = F)

meri_width_m9 <- lmer(sqrt(width) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width_filter, 
                      REML = F)

# # Residual histograms
# diagnostic(resid(meri_width_m7))
# diagnostic(resid(meri_width_m8))
# diagnostic(resid(meri_width_m9))
#  

##### Depth ####
# For depth untransformed data seems to work the best
meri_depth_m4 <- lmer(depth ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data=gal_meri_depth, 
                      REML = F)
meri_depth_m5 <- lmer(log(depth) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data=gal_meri_depth, 
                      REML = F)
meri_depth_m6 <- lmer(sqrt(depth) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data=gal_meri_depth, 
                      REML = F)
# # type III test
Anova(meri_depth_m4)

# # Diagnostic
# 
# # Residual histograms
# diagnostic(resid(meri_depth_m4))
# diagnostic(resid(meri_depth_m5))
# diagnostic(resid(meri_depth_m6))
# 
# # DHARMa
# testResiduals(meri_depth_m4)
# testResiduals(meri_depth_m5)
# testResiduals(meri_depth_m6)

##### Spine length ####
# I think the untransformed works best but the 0s may affect the results
meri_spine_length_m4 <-lmer(spine_length ~ finch_beak +
                              year_collected +
                              (1|ID),
                            data=gal_meri_spine_length,
                            REML=F)

meri_spine_length_m5 <-lmer(log(spine_length + 1) ~ finch_beak +
                              year_collected +
                              (1|ID),
                            data=gal_meri_spine_length,
                            REML=F)

meri_spine_length_m6 <-lmer(sqrt(spine_length) ~ finch_beak +
                              year_collected +
                              (1|ID),
                            data=gal_meri_spine_length,
                            REML=F)

# # type III test
Anova(meri_spine_length_m4)
# 
# # Diagnostic
# 
# # Residual histograms
#  diagnostic(resid(meri_spine_length_m4))
#  diagnostic(resid(meri_spine_length_m5))
#  diagnostic(resid(meri_spine_length_m6))
# # 
# # DHARMa
# testResiduals(meri_spine_length_m4)
# testResiduals(meri_spine_length_m5)
# testResiduals(meri_spine_length_m6)

##### No Zero Spine Length #### 
# No transformed data works best
 meri_spine_length_m7 <-lmer(spine_length ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_spine_length_wozero,
                             REML=F)
 
 meri_spine_length_m8 <-lmer(log(spine_length + 1) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_spine_length_wozero,
                             REML=F)
 
 meri_spine_length_m9 <-lmer(sqrt(spine_length) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_spine_length_wozero,
                             REML=F)
 
 # # type III test
 Anova(meri_spine_length_m7)
 # 
 # # Diagnostic
 # 
 # # Residual histograms
 # diagnostic(resid(meri_spine_length_m7))
# diagnostic(resid(meri_spine_length_m8))
# diagnostic(resid(meri_spine_length_m9))
  
 # Filter the highest residuals
 hist(gal_meri_spine_length_wozero$spine_length, breaks = 20)
 
 # Create a column with model residuals to filter the dataset
 gal_meri_spine_length_wozero$residuals <- resid(meri_spine_length_m7) 
 hist(resid(meri_spine_length_m7), breaks = 20)
 
##### Tip distance ####
# Raw data looks the best
meri_tip_distance_m10 <- lmer(tip_distance ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance,REML=F)

meri_tip_distance_m11 <- lmer(log(tip_distance + 1) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance,REML=F)

meri_tip_distance_m12 <- lmer(sqrt(tip_distance) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance,REML=F)


# # type III test
Anova(meri_tip_distance_m10)
# # Diagnostic
hist(gal_meri_tip_distance$tip_distance, breaks = 20)

# # Diagnostic of residuals
# diagnostic(resid(meri_tip_distance_m10))
# diagnostic(resid(meri_tip_distance_m11))
# diagnostic(resid(meri_tip_distance_m12))
# # 
# # DHARMa
# testResiduals(meri_tip_distance_m4)
# testResiduals(meri_tip_distance_m5)
# testResiduals(meri_tip_distance_m6)

##### No zero tip distance ####

meri_tip_distance_m13 <- lmer(tip_distance ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero,
                             REML=F)

meri_tip_distance_m14 <- lmer(log(tip_distance + 1) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero,
                             REML=F)

meri_tip_distance_m15 <- lmer(sqrt(tip_distance) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero,
                             REML=F)

# # type III test
Anova(meri_tip_distance_m13)
# # Diagnostic
hist(gal_meri_tip_distance_wozero$tip_distance, breaks = 20)

# # Diagnostic of residuals
# diagnostic(resid(meri_tip_distance_m13))
# diagnostic(resid(meri_tip_distance_m14))
# diagnostic(resid(meri_tip_distance_m15))

# Filter residuals no zero data
# Create a column with model residuals to filter the dataset
gal_meri_tip_distance_wozero$residuals <- resid(meri_tip_distance_m13) 
hist(resid(meri_tip_distance_m13), breaks = 20)
# Filter residuals -9
gal_meri_tip_distance_wozero_filter <- 
  filter(gal_meri_tip_distance_wozero, !residuals <= -9)

##### Filter Tip distance ####
# Raw data works best
meri_tip_distance_m16 <- lmer(tip_distance ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero_filter,
                             REML=F)

meri_tip_distance_m17 <- lmer(log(tip_distance + 1) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero_filter,
                             REML=F)

meri_tip_distance_m18 <- lmer(sqrt(tip_distance) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero_filter,
                             REML=F)

# # type III test
Anova(meri_tip_distance_m16)

# # Diagnostic of residuals
# diagnostic(resid(meri_tip_distance_m16))
# diagnostic(resid(meri_tip_distance_m17))
# diagnostic(resid(meri_tip_distance_m18))

##### Lower spines ####
# Ask about the outcomes of this model_ How can I diagnose this one?

meri_lower_spines_m2_glmm <- glmmTMB(lower_spines ~ finch_beak +
                                       (1|ID),
                                     data = gal_meri_lower_spines,
                                     family = binomial)

 # meri_lower_spines_m2 <- glm(lower_spines ~ finch_beak + 
 #                                   year_collected, 
 #                             data = gal_meri_lower_spines, 
 #                             family = "binomial")

# # Type III test
Anova(meri_lower_spines_m2_glmm)
# Anova(meri_lower_spines_m2)
 
# # Diagnostic
# diagnostic(resid(meri_lower_spines_m2_glmm))
# 
# # DHARMa
testResiduals(meri_lower_spines_m2_glmm)

##### Upper spines ####
# Ask about the outcomes of this model_ How can I diagnose this one?

meri_upper_spines_m2_glmm <- glmmTMB(upper_spines ~ finch_beak +
                                       (1|ID),
                                     data = gal_meri_upper_spines,
                                     family = binomial)

meri_upper_spines_m2 <- glm(lower_spines ~ finch_beak + 
                              year_collected, 
                            data = gal_meri_lower_spines, 
                            family = "binomial")

# # Type III test
Anova(meri_upper_spines_m2)
Anova(meri_upper_spines_m2_glmm)

# # Diagnostic
# diagnostic(resid(meri_lower_spines_m2))

#### Flowers ####
##### Petal length data ####
# The square root data works best_ However I think there is too few datapoints_
flower_m10 <- lmer(petal_length ~ finch_beak +
                    year_collected +
                    (1|ID),
                  data=gal_petal_length,
                  REML=F)

flower_m11 <- lmer(log(petal_length) ~ finch_beak +
                    year_collected +
                    (1|ID),
                  data=gal_petal_length,
                  REML=F)

flower_m12 <- lmer(sqrt(petal_length) ~ finch_beak +
                    year_collected +
                    (1|ID),
                  data=gal_petal_length,
                  REML=F)

# #type III test using lmertest
Anova(flower_m12)
# 
# # Diagnostic of residuals
# diagnostic(resid(flower_m10))
# diagnostic(resid(flower_m11))
# diagnostic(resid(flower_m12))
# 
# # DHARMa
# testResiduals(flower_m7)
# testResiduals(flower_m8)
# testResiduals(flower_m9)

#### Leaves data ####

###### Leaf length ####
# Squared transformed data works best
leaf_length_m7 <- lmer(leaf_length ~ finch_beak +
                         year_collected +
                         (1|ID),data=gal_leaf_length,REML=F)

leaf_length_m8 <- lmer(log(leaf_length) ~ finch_beak +
                         year_collected +
                         (1|ID),data=gal_leaf_length,REML=F)

leaf_length_m9 <- lmer(sqrt(leaf_length) ~ finch_beak +
                         year_collected +
                         (1|ID),data=gal_leaf_length,REML=F)
# # Type II test
Anova(leaf_length_m9)
# 
# # Diagnostic
 # diagnostic(resid(leaf_length_m7))
 # diagnostic(resid(leaf_length_m8))
# diagnostic(resid(leaf_length_m9))
# 
# # DHARMa
# testResiduals(leaf_length_m7)
# testResiduals(leaf_length_m8)
# testResiduals(leaf_length_m9)

##### Leaflet length ####
# Log transformed data works best
leaflet_length_m7 <- lmer(leaflet_length ~ finch_beak +
                            year_collected +
                            (1|ID),
                          data=gal_leaflet_length,
                          REML = F)

leaflet_length_m8 <- lmer(log(leaflet_length) ~ finch_beak +
                            year_collected +
                            (1|ID),
                          data=gal_leaflet_length,
                          REML = F)

leaflet_length_m9 <- lmer(sqrt(leaflet_length) ~ finch_beak +
                            year_collected +
                            (1|ID),
                          data=gal_leaflet_length,
                          REML = F)
# # Type III test
Anova(leaflet_length_m8)
# 
# # Diagnostic
# 
# # Residual histograms
# diagnostic(resid(leaflet_length_m7))
# diagnostic(resid(leaflet_length_m8))
# diagnostic(resid(leaflet_length_m9))
# 
# # DHARMa
# testResiduals(leaflet_length_m7)
# testResiduals(leaflet_length_m8)
# testResiduals(leaflet_length_m9)


##### Leaflet number ####
leaflet_num_m9 <- glm(number_of_leaflets ~ finch_beak +
                        year_collected,
                      family = poisson,
                      data=gal_leaf_length)
 
 leaflet_num_m10 <- glm(number_of_leaflets ~ finch_beak +
                         year_collected,
                       family = gaussian,
                       data=gal_leaf_length)
 
 leaflet_num_m11 <- glmmTMB(number_of_leaflets ~ finch_beak +
                             (1|ID), data = gal_leaf_length,
                           family = nbinom1())
 

# # Test
Anova(leaflet_num_m11)
# 
# # Diagnostic
# # Residual histograms
# diagnostic(resid(leaflet_num_m3))
# 
# # DHARMa
# testResiduals(leaflet_num_m3)

# LS means ####
# Use emmeans package for this_ Using "response" allows me to compare on the original scale for transformed data_
# This gives me an idea of how much change there is between the factors of interest_
# Using the that that worked best for each trait and model
#the following estimates LS means for the mainland_island effect for each response

# Model 1: Mainland vs Island ####

#### Mericarp ####
# Seems that mericarps in islands are larger_ Well defended?
##### Length ####
EM_length <- emmeans(meri_length_m1, ~ mainland_island)
plot(EM_length, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length)
# ((island mean/mainland mean)-1)*100%
((6.15/5.75 - 1)* 100) # Mericarps ~ 7% longer on islands

##### Width ####
EM_width <- emmeans(meri_width_m3, ~ mainland_island, type = "response")
plot(EM_width, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width)
((3.12/2.95 - 1)*100) # Mericarps ~ 5.76% wider on islands

##### Depth ####
EM_depth <- emmeans(meri_depth_m1, ~ mainland_island)
plot(EM_depth, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth)
((4.78/4.26 - 1)*100) # Mericarps ~ 12% deeper on islands

##### Spine Length ####
# Zero filter data
EM_spine <- emmeans(meri_spine_length_wozero_m3, ~ mainland_island)
plot(EM_spine, comparisons = T) + labs(title = "Mericarp Spine Length")
pwpp(EM_spine)
((2.12/2.08 - 1)* 100) # Upper spines are 1.92% longer on islands than on the mainland 
##### Tip distance ####
# Zero filter data
EM_tip_dist <- emmeans(meri_tip_distance_m7, ~ mainland_island)
plot(EM_tip_dist, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist)
((9/8.43 - 1)*100) # Mericarps ~ 6.66% more separated on islands

##### Lower Spine ####
#Glmm
EM_lower <- emmeans(meri_lower_spines_m1_glmm, ~ mainland_island, type = "response")
plot(EM_lower, comparisons = T) + labs(title = "Mericarp Lower Spines")
pwpp(EM_lower)
((0.407/0.998 - 1)*100)
#Glm
# EM_lower1 <- emmeans(meri_lower_spines_m1, ~ mainland_island, type = "response")
# plot(EM_lower1, comparisons = T) + labs(title = "Mericarp Lower Spines")
# pwpp(EM_lower1)

##### Upper Spines ####
EM_upper <- emmeans(meri_upper_spines_m1_glmm, ~ mainland_island)
plot(EM_lower) + labs(title = "Mericarp Lower Spines")

#Glm
# EM_upper1 <- emmeans(meri_upper_spines_m1, ~ mainland_island)
# plot(EM_upper1) + labs(title = "Mericarp Lower Spines")

##### Length mainland galapagos ####
EM_length_mainland_gal <- emmeans(meri_mainland_gal_length_m1, ~ mainland_island)
plot(EM_length_mainland_gal, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length_mainland_gal)
# ((island mean/mainland mean)-1)*100%
((6.15/5.75 - 1)* 100) # Mericarps ~ 7% longer on islands

##### Width mainland galapagos log transformed ####
EM_width_mainland_gal <- emmeans(meri_width_mainland_gal_m2, ~ mainland_island, type = "response")
plot(EM_width_mainland_gal, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width_mainland_gal)
((3.12/2.95 - 1)*100) # Mericarps ~ 5.76% wider on islands

##### Depth mainland galapagos ####
EM_depth_mainland_gal <- emmeans(meri_depth_mainland_gal_m1, ~ mainland_island)
plot(EM_depth_mainland_gal, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth_mainland_gal)
((4.78/4.26 - 1)*100) # Mericarps ~ 12% deeper on islands

##### Tip distance mainland galapagos ####
# Zero filter data
EM_tip_dist_mainland_gal <- emmeans(meri_tip_distance_mainland_gal_m7, ~ mainland_island)
plot(EM_tip_dist_mainland_gal, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist_mainland_gal)
((9/8.43 - 1)*100) # Mericarps ~ 6.66% more separated on islands

##### Lower Spines mainland galapagos ####
#Glmm
EM_lower_mainland_gal <- emmeans(meri_lower_spines_mainland_gal_glmm, ~ mainland_island, type = "response")
plot(EM_lower_mainland_gal, comparisons = T) + labs(title = "Mericarp Lower Spines")
pwpp(EM_lower_mainland_gal)
((0.407/0.998 - 1)*100)


#### Flower ####
EM_flower <- emmeans(flower_m1, ~ mainland_island)
plot(EM_flower, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower)
((16.3/16.7 - 1)*100) # Flowers are -2% smaller on islands

#### Leaf ####
##### Leaf length ####
EM_leaf <- emmeans(leaf_length_m3, ~ mainland_island, type = "response")
plot(EM_leaf, comparisons = T) + labs(title = "Leaf Length")
pwpp(EM_leaf)
((30.4/25.2 - 1)*100) # Leafs on islands are ~ 20% longer than mainland

##### Leaflet length ####
EM_leaflet <- emmeans(leaflet_length_m2, ~ mainland_island, type = "response")
plot(EM_leaflet, comparisons = T) + labs(title = "Leaflet Length")
pwpp(EM_leaflet)
((8.23/7.65 - 1)*100) # Leaflets are ~ 7.5% larger on islands

##### Leaflet Number ####
EM_leaflet_num <- emmeans(leaflet_num_m5, ~ mainland_island)
plot(EM_leaflet_num, comparisons = T) + labs(title = "Leaflet Number")
pwpp(EM_leaflet_num)
((14.2/12.8 - 1)*100) # Leaflet number are ~11% higher on islands

# Model 2: Galapagos and other islands ####
# Seems that in Galapagos the flowers and the leaves are smaller compared to other islands_
#### Flower ####
EM_flower2 <- emmeans(flower_m9, ~ galapagos_other, type = "response")
plot(EM_flower2, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower2)
((9.18/17.04 - 1)*100) #FLowers in Galapagos are 46% shorter than other islands

#### Leaf ####
##### Leaf length ####
EM_leaf2 <- emmeans(leaf_length_m5, ~ galapagos_other, type = "response")
plot(EM_leaf2, comparisons = T) + labs(title = "Leaf Length")
pwpp(EM_leaf2)
((23.9/30.9 - 1)*100) #Flowers in Galapagos are 22.6% shorter than in other islands

##### Leaflet length ####
EM_leaflet2 <- emmeans(leaflet_length_m5, ~ galapagos_other, type = "response")
plot(EM_leaflet2, comparisons = T) + labs(title = "Leaflet Length")
pwpp(EM_leaflet2)
((7.06/8.67 - 1)*100) # Leaflet length in Galpagos is 18.56% smaller than in other islands

##### Leaflet number ####
EM_leaflet_num2 <- emmeans(leaflet_num_m8, ~ galapagos_other, type = "response")
plot(EM_leaflet_num2, comparisons = T) + labs(title = "Leaflet Number")
pwpp(EM_leaflet_num2)
((14.1/14.4 - 1)*100) # Leaflet number are 2% fewer in Galapagos than other islands

# Model 3: Finch communities within Galapagos ####

#### Mericarp ####
##### Length ####
EM_length3 <- emmeans(meri_length_m4, ~ finch_beak)
plot(EM_length3, comparisons = T) + labs(title = "Mericarp Length")
pwpp(EM_length3)
((6.40/5.97 - 1)*100) # Mericarps are 7.20% longer on islands with large seed predators than on islands wihtout

##### Width ####
EM_width3 <- emmeans(meri_width_m9, ~ finch_beak, type = "response")
plot(EM_width3, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width3)
contrast(regrid(EM_width3))
((3.14/3.11 - 1)*100)

##### Depth ####
EM_depth3 <- emmeans(meri_depth_m4, ~ finch_beak)
plot(EM_depth3, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth3)
((4.76/4.83 - 1)*100) # - 1.45%

##### Spine Length ####
 EM_spine3 <- emmeans(meri_spine_length_m7, ~ finch_beak)
 plot(EM_spine3, comparisons = T) + labs(title = "Mericarp Spine Length")
 pwpp(EM_spine3)
##### Tip distance ####
EM_tip_dist3 <- emmeans(meri_tip_distance_m16, ~ finch_beak)
plot(EM_tip_dist3) + labs(title = "Mericarp Tip Distance")
pwpp(EM_tip_dist3)
((9.14/8.08 - 1)*100)
##### Lower Spines ####
#Glmm
EM_lower3 <- emmeans(meri_lower_spines_m2_glmm, ~ finch_beak, type = "response")
plot(EM_lower3, comparisons = T) + labs(title = "Mericarp Lower Spine")
pwpp(EM_lower3)
((0.0787/0.2489 - 1)*100)

#Glm
# EM_lower4 <- emmeans(meri_lower_spines_m2, ~ finch_beak)
# plot(EM_lower4, comparisons = T) + labs(title = "Mericarp Lower Spines")
# pwpp(EM_lower4)
# ((0.472/-0.542 - 1)*100)

##### Upper Spines ####
# EM_upper3 <- emmeans(meri_upper_spines_m2_glmm, ~ finch_beak, type = "response")
# plot(EM_upper3) + labs(title = "Mericarp Lower Spine")
# 
# #Glm
# EM_upper4 <- emmeans(meri_upper_spines_m2, ~ finch_beak)
# plot(EM_upper4) + labs(title = "Mericarp Lower Spines")

#### Flower ####
EM_flower3 <- emmeans(flower_m12, ~ finch_beak, type = "response")
plot(EM_flower3, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower3)
((9.12/8.59 - 1)*100) # 6.16%

#### Leaf ####

##### Leaf length ####
EM_leaf3 <- emmeans(leaf_length_m9, ~ finch_beak, type = "response")
plot(EM_leaf3, comparisons = T) + labs(title = "Leaf Length")
pwpp(EM_length3)
((6.42/6.10 - 1)*100) # 7.20%

##### Leaflet length ####
EM_leaflet3 <- emmeans(leaflet_length_m8, ~ finch_beak, type = "response")
plot(EM_leaflet3, comparisons = T) + labs(title = "Leaflet Length")
pwpp(EM_leaflet3)
((7.05/6.17 - 1)*100) # 14.26%

##### Leaflet number ####
EM_leaflet_num3 <- emmeans(leaflet_num_m10, ~ finch_beak)
plot(EM_leaflet_num3) + labs(title = "Leaflet Number")
pwpp(EM_leaflet_num3)
((14.04/14.10 - 1)*100)
