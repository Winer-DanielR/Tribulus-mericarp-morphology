### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral

#### Linear models per trait ####

# Model 1: trait ~ mainland/island + year_ ####
# Is there and effect between island and mainland populations? #

#### Mericarp data ####
# run simple linear mixed effect model, with ID as random effect per trait
# For diagnostics you can change to trait_log or trait_sqr to test transformed data
# 
###### Length ####
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
Anova(meri_length_m1)

# Diagnostic

# Residuals histogram
par(mfrow = c(1, 3))
diagnostic(resid(meri_length_m1))
# diagnostic(resid(meri_length_m2))
# diagnostic(resid(meri_length_m3))

# DHARMA <- this is the way with DHAMa
testResiduals(meri_length_m1)
# testResiduals(meri_length_m2)
# testResiduals(meri_length_m3)
# 
# plotResiduals(meri_length_m1)
# plotResiduals(meri_length_m2)
# plotResiduals(meri_length_m3)

##### Width ####
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
diagnostic(resid(meri_width_m3))

# # DHARMa
# testResiduals(meri_width_m1)
# testResiduals(meri_width_m2)
testResiduals(meri_width_m3)
# plotResiduals(meri_width_m3)

##### Depth ####
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
# # type III test
Anova(meri_depth_m1)
# 
# # Diagnostic
# 
# # Residual histograms
diagnostic(resid(meri_depth_m1))
# diagnostic(resid(meri_depth_m2))
# diagnostic(resid(meri_depth_m3))
# 
# # DHARMa
testResiduals(meri_depth_m1)
# testResiduals(meri_depth_m2)
# testResiduals(meri_depth_m3)


##### Spine length ####
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
diagnostic(resid(meri_spine_length_m1))
# diagnostic(resid(meri_spine_length_m2))
# diagnostic(resid(meri_spine_length_m3))

# # DHARMa
# testResiduals(meri_spine_length_m1)
# testResiduals(meri_spine_length_m2)
# testResiduals(meri_spine_length_m3)
# testZeroInflation(meri_depth_m1)

##### No zero Spine Length ####
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
diagnostic(resid(meri_spine_length_wozero_m3))

hist(resid(meri_spine_length_wozero_m3), breaks = 20)
hist(meri_spine_length_wozero$spine_length, breaks = 20)

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
Anova(spine_length_filter_m1)

# Diagnostic

# # Residual histograms
diagnostic(resid(spine_length_filter_m1))
# diagnostic(resid(spine_length_filter_m2))
# diagnostic(resid(spine_length_filter_m3))

# # DHARMa
testResiduals(spine_length_filter_m1)
# testResiduals(spine_length_filter_m2)
# testResiduals(spine_length_filter_m3)
# # plotResiduals(spine_length_filter_m1)


##### Tip distance ####
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
hist(resid(meri_spine_length_m1), breaks = 20)
hist(meri_tip_distance$tip_distance, breaks = 20)

# # Residual histograms
diagnostic(resid(meri_tip_distance_m1))
# diagnostic(resid(meri_tip_distance_m2))
# diagnostic(resid(meri_tip_distance_m3))

# # DHARMa
testResiduals(meri_tip_distance_m1)
# testResiduals(meri_tip_distance_m2)
# testResiduals(meri_tip_distance_m3)


###### No zero Tip distance ####
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

# Filter residuals
meri_tip_distance_wozero$residuals <- resid(meri_tip_distance_wozero_m1)
hist(resid(meri_tip_distance_m4), breaks = 20)
hist(meri_tip_distance_wozero$tip_distance, breaks = 20)

# Filter residuals that are lower than 10
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero,
                                          !residuals < -9)
# Removes specimen 383

hist(meri_tip_distance_wozero_filter$residuals, breaks = 20)
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero_filter, !is.na(residuals))

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
diagnostic(resid(meri_tip_distance_m7))
# diagnostic(resid(meri_tip_distance_m8))
# diagnostic(resid(meri_tip_distance_m9))

#Anova
Anova(meri_tip_distance_m7)

# DHARMa
testResiduals(meri_tip_distance_m7)

###### Lower spines ####
meri_lower_spines_m1 <- glm(lower_spines ~ mainland_island + 
                                   year_collected, 
                             data = meri_lower_spines, 
                             family = "binomial")

meri_lower_spines_m1_glmm <- glmmTMB(lower_spines ~ mainland_island +
                                       (1|ID),
                                     data = meri_lower_spines,
                                     family = binomial)

# # Type III test
Anova(meri_lower_spines_m1)
Anova(meri_lower_spines_m1_glmm)

# # Diagnostic
# 
# # Residual histograms
diagnostic(resid(meri_lower_spines_m1_glmm))


###### Upper spines ####
meri_upper_spines_m1 <- glm(upper_spines ~ mainland_island + 
                               year_collected, 
                             data = meri_upper_spines, 
                             family = "binomial")

meri_upper_spines_m1_glmm <- glmmTMB(upper_spines ~ mainland_island +
                                       (1|ID),
                                     data = meri_upper_spines,
                                     family = binomial)

# # Type III test
Anova(meri_upper_spines_m1)
Anova(meri_upper_spines_m1_glmm)

# # Diagnostic
# 
# # Residual histograms
diagnostic(resid(meri_upper_spines_m1_glmm))

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
hist(flower$petal_length, breaks = 20)

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
diagnostic(resid(flower_m4))
# diagnostic(resid(flower_m5))
# diagnostic(resid(flower_m6))
 
# # DHARMa
# testResiduals(flower_m4)
# testResiduals(flower_m2)
# testResiduals(flower_m3)
testResiduals(flower_m4)

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
diagnostic(resid(leaf_length_m3))
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
# Anova(leaflet_length_m2)
# 
# # Diagnostic
# 
# # Diagnostic function
# diagnostic(resid(leaflet_length_m1))
diagnostic(resid(leaflet_length_m2))
# diagnostic(resid(leaflet_length_m3))
# 
# # DHARMa
# testResiduals(leaflet_length_m1)
# testResiduals(leaflet_length_m2)
# testResiduals(leaflet_length_m3)


#### Leaflet number ####
leaflet_num_m1 <- glm(number_of_leaflets ~ mainland_island +
                       year_collected,
                       family = poisson,
                       data=leaf_length)

Anova(leaflet_num_m1)

# # Diagnostic
# # Residual histograms
diagnostic(resid(leaflet_num_m1))
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
diagnostic(resid(flower_m9))
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
# Anova(leaf_length_m5)
# 
# # Diagnostic
# 
# # Residual histograms
# diagnostic(resid(leaf_length_m4))
diagnostic(resid(leaf_length_m5))
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
diagnostic(resid(leaflet_length_m5))
# diagnostic(resid(leaflet_length_m6))
# 
# # DHARMa
# testResiduals(leaflet_length_m4)
# testResiduals(leaflet_length_m5)
# testResiduals(leaflet_length_m6)

#### Leaflet number ####
leaflet_num_m2 <- glm(number_of_leaflets ~ galapagos_other +
                        year_collected,
                      family = poisson,
                      data=leaf_length)

Anova(leaflet_num_m2)
 
# # Diagnostic
# # Residual histograms
# diagnostic(resid(leaflet_num_m2))
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
 diagnostic(resid(meri_length_m4))
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
 diagnostic(resid(meri_width_m4))
 diagnostic(resid(meri_width_m5))
 diagnostic(resid(meri_width_m6))
# 
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
diagnostic(resid(meri_width_m9))
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
diagnostic(resid(meri_depth_m4))
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
 diagnostic(resid(meri_spine_length_m4))
 diagnostic(resid(meri_spine_length_m5))
 diagnostic(resid(meri_spine_length_m6))
# 
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
 diagnostic(resid(meri_spine_length_m7))
# diagnostic(resid(meri_spine_length_m8))
# diagnostic(resid(meri_spine_length_m9))
  
 # Filter the highest residuals
 hist(gal_meri_spine_length_wozero$spine_length, breaks = 20)
 
 # Create a column with model residuals to filter the dataset
 gal_meri_spine_length_wozero$residuals <- resid(meri_spine_length_m7) 
 hist(resid(meri_spine_length_m7), breaks = 20)
 
##### Tip distance ####
# Raw data looks the best
meri_tip_distance_m4 <- lmer(tip_distance ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance,REML=F)

meri_tip_distance_m5 <- lmer(log(tip_distance + 1) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance,REML=F)

meri_tip_distance_m6 <- lmer(sqrt(tip_distance) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance,REML=F)


# # type III test
Anova(meri_tip_distance_m4)
# # Diagnostic
hist(gal_meri_tip_distance$tip_distance, breaks = 20)

# # Diagnostic of residuals
diagnostic(resid(meri_tip_distance_m4))
diagnostic(resid(meri_tip_distance_m5))
diagnostic(resid(meri_tip_distance_m6))
# 
# # DHARMa
# testResiduals(meri_tip_distance_m4)
# testResiduals(meri_tip_distance_m5)
# testResiduals(meri_tip_distance_m6)

##### No zero tip distance ####

meri_tip_distance_m7 <- lmer(tip_distance ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero,
                             REML=F)

meri_tip_distance_m8 <- lmer(log(tip_distance + 1) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero,
                             REML=F)

meri_tip_distance_m9 <- lmer(sqrt(tip_distance) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero,
                             REML=F)

# # type III test
Anova(meri_tip_distance_m7)
# # Diagnostic
hist(gal_meri_tip_distance_wozero$tip_distance, breaks = 20)

# # Diagnostic of residuals
diagnostic(resid(meri_tip_distance_m7))
# diagnostic(resid(meri_tip_distance_m8))
# diagnostic(resid(meri_tip_distance_m9))

# Filter residuals no zero data
# Create a column with model residuals to filter the dataset
gal_meri_tip_distance_wozero$residuals <- resid(meri_tip_distance_m7) 
hist(resid(meri_tip_distance_m7), breaks = 20)
# Filter residuals -9
gal_meri_tip_distance_wozero_filter <- 
  filter(gal_meri_tip_distance_wozero, !residuals <= -9)

##### Filter Tip distance ####
# Raw data works best
meri_tip_distance_m10 <- lmer(tip_distance ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero_filter,
                             REML=F)

meri_tip_distance_m11 <- lmer(log(tip_distance + 1) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero_filter,
                             REML=F)

meri_tip_distance_m12 <- lmer(sqrt(tip_distance) ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip_distance_wozero_filter,
                             REML=F)

# # type III test
Anova(meri_tip_distance_m10)

# # Diagnostic of residuals
diagnostic(resid(meri_tip_distance_m10))
# diagnostic(resid(meri_tip_distance_m11))
# diagnostic(resid(meri_tip_distance_m12))

##### Lower spines ####
# Ask about the outcomes of this model_ How can I diagnose this one?

meri_lower_spines_m2_glmm <- glmmTMB(lower_spines ~ finch_beak +
                                       (1|ID),
                                     data = gal_meri_lower_spines,
                                     family = binomial)

meri_lower_spines_m2 <- glm(lower_spines ~ finch_beak + 
                                   year_collected, 
                             data = gal_meri_lower_spines, 
                             family = "binomial")

# # Type III test
Anova(meri_lower_spines_m2_glmm)
Anova(meri_lower_spines_m2)
 
# # Diagnostic
# diagnostic(resid(meri_lower_spines_m2))
# 
# # DHARMa
# testResiduals(meri_lower_spines_m2)

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
 diagnostic(resid(flower_m12))
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
 diagnostic(resid(leaf_length_m9))
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
 diagnostic(resid(leaflet_length_m8))
# diagnostic(resid(leaflet_length_m9))
# 
# # DHARMa
# testResiduals(leaflet_length_m7)
# testResiduals(leaflet_length_m8)
# testResiduals(leaflet_length_m9)


##### Leaflet number ####
leaflet_num_m3 <- glm(number_of_leaflets ~ finch_beak +
                        year_collected,
                      family = poisson,
                      data=gal_leaf_length)

# # Test
Anova(leaflet_num_m3)
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
EM_length <- emmeans(meri_length_m1, ~ mainland_island) # Is transformed data
plot(EM_length) + labs(title = "Mericarp Length")

##### Width ####
EM_width <- emmeans(meri_width_m3, ~ mainland_island, type = "response")
plot(EM_width) + labs(title = "Mericarp Width")

##### Depth ####
EM_depth <- emmeans(meri_depth_m1, ~ mainland_island)
plot(EM_depth) + labs(title = "Mericarp Depth")

##### Spine Length ####
# Zero filter data
EM_spine <- emmeans(meri_spine_length_wozero_m3, ~ mainland_island)
plot(EM_spine) + labs(title = "Mericarp Spine Length")

##### Tip distance ####
# Zero filter data
EM_tip_dist <- emmeans(meri_tip_distance_m7, ~ mainland_island)
plot(EM_tip_dist) + labs(title = "Mericarp Tip distance")

##### Lower Spine ####
EM_lower <- emmeans(meri_lower_spines_m1_glmm, ~ mainland_island)
plot(EM_lower) + labs(title = "Mericarp Lower Spines")

#Glm
EM_lower1 <- emmeans(meri_lower_spines_m1, ~ mainland_island)
plot(EM_lower1) + labs(title = "Mericarp Lower Spines")

##### Upper Spines ####
EM_upper <- emmeans(meri_upper_spines_m1_glmm, ~ mainland_island)
plot(EM_lower) + labs(title = "Mericarp Lower Spines")

#Glm
EM_upper1 <- emmeans(meri_upper_spines_m1, ~ mainland_island)
plot(EM_upper1) + labs(title = "Mericarp Lower Spines")

#### Flower ####
EM_flower <- emmeans(flower_m1, ~ mainland_island)
plot(EM_flower) + labs(title = "Flower Length")

#### Leaf ####
##### Leaf length ####
EM_leaf <- emmeans(leaf_length_m3, ~ mainland_island, type = "response")
plot(EM_leaf) + labs(title = "Leaf Length")

##### Leaflet length ####
EM_leaflet <- emmeans(leaflet_length_m2, ~ mainland_island, type = "response")
plot(EM_leaflet) + labs(title = "Leaflet Length")

##### Leaflet Number ####
EM_leaflet_num <- emmeans(leaflet_num_m1, ~ mainland_island)
plot(EM_leaflet_num) + labs(title = "Leaflet Number")

# Model 2: Galapagos and other islands ####
# Seems that in Galapagos the flowers and the leaves are smaller compared to other islands_
#### Flower ####
EM_flower2 <- emmeans(flower_m9, ~ galapagos_other, type = "response")
plot(EM_flower2) + labs(title = "Flower Length")

#### Leaf ####
##### Leaf length ####
EM_leaf2 <- emmeans(leaf_length_m5, ~ galapagos_other, type = "response")
#plot(EM_leaf2) + labs(title = "Leaf Length")

##### Leaflet length ####
EM_leaflet2 <- emmeans(leaflet_length_m5, ~ galapagos_other, type = "response")
#plot(EM_leaflet2) + labs(title = "Leaflet Length")

##### Leaflet number ####
EM_leaflet_num2 <- emmeans(leaflet_num_m2, ~ galapagos_other)
#plot(EM_leaflet_num2) + labs(title = "Leaflet Number")

# Model 3: Finch communities within Galapagos ####

#### Mericarp ####
##### Length ####
EM_length3 <- emmeans(meri_length_m4, ~ finch_beak)
#plot(EM_length3) + labs(title = "Mericarp Length")

##### Width ####
EM_width3 <- emmeans(meri_width_m6, ~ finch_beak, type = "response")
#plot(EM_width3) + labs(title = "Mericarp Width")

##### Depth ####
EM_depth3 <- emmeans(meri_depth_m4, ~ finch_beak)
#plot(EM_depth3) + labs(title = "Mericarp Depth")

##### Spine Length ####
EM_spine3 <- emmeans(meri_spine_length_m7, ~ finch_beak)
plot(EM_spine3) + labs(title = "Mericarp Spine Length")

##### Tip distance ####
EM_tip_dist3 <- emmeans(meri_tip_distance_m10, ~ finch_beak)
plot(EM_tip_dist3) + labs(title = "Mericarp Tip Distance")

##### Lower Spines ####
EM_lower3 <- emmeans(meri_lower_spines_m2_glmm, ~ finch_beak, type = "response")
plot(EM_lower3) + labs(title = "Mericarp Lower Spine")

#Glm
EM_lower4 <- emmeans(meri_lower_spines_m2, ~ finch_beak)
plot(EM_lower4) + labs(title = "Mericarp Lower Spines")

##### Upper Spines ####
EM_upper3 <- emmeans(meri_upper_spines_m2_glmm, ~ finch_beak, type = "response")
plot(EM_upper3) + labs(title = "Mericarp Lower Spine")

#Glm
EM_upper4 <- emmeans(meri_upper_spines_m2, ~ finch_beak)
plot(EM_upper4) + labs(title = "Mericarp Lower Spines")

#### Flower ####
EM_flower3 <- emmeans(flower_m12, ~ finch_beak)
plot(EM_flower3) + labs(title = "Flower Length")

#### Leaf ####

##### Leaf length ####
EM_leaf3 <- emmeans(leaf_length_m9, ~ finch_beak, type = "response")
#plot(EM_leaf3) + labs(title = "Leaf Length")

##### Leaflet length ####
EM_leaflet3 <- emmeans(leaflet_length_m8, ~ finch_beak, type = "response")
#plot(EM_leaflet3) + labs(title = "Leaflet Length")

##### Leaflet number ####
EM_leaflet_num3 <- emmeans(leaflet_num_m3, ~ finch_beak)
#plot(EM_leaflet_num3) + labs(title = "Leaflet Number")
