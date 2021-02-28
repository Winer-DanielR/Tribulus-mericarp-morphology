### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral

#### Linear models per trait ####

# Model 1: trait ~ mainland/island + year. ####
# Is there and effect between island and mainland populations? #

# Mericarp data ####
# run simple linear mixed effect model, with ID as random effect per trait
# For diagnostics you can change to trait_log or trait_sqr to test transformed data
# 
# Length ####
# For length, the squared data seems the best.
meri_length_m1<- lmer(length ~ mainland_island +
                       year_collected +
                      (1|ID),
                      data = meri_length,
                      REML = F)
meri_length_m2<- lmer(length_log ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length,
                      REML = F)
meri_length_m3<- lmer(length_sqr ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length,
                      REML = F)
# type III test
Anova(meri_length_m3)

# Diagnostic

# Diagnostic function
diagnostic(meri_length$length)
diagnostic(meri_length$length_log)
diagnostic(meri_length$length_sqr)

# Residuals histogram
hist(resid(meri_length_m1))
hist(resid(meri_length_m2))
hist(resid(meri_length_m3))

# DHARMA <- this is the way with DHAMa
testResiduals(meri_length_m1)
testResiduals(meri_length_m2)
testResiduals(meri_length_m3)

plotResiduals(meri_length_m1)
plotResiduals(meri_length_m2)
plotResiduals(meri_length_m3)

# Width ####
# For width, squared transformed seemed the best.
meri_width_m1 <- lmer(width ~ mainland_island +
                     year_collected +
                     (1|ID),
                     data = meri_width, 
                     REML = F)
meri_width_m2 <- lmer(width_log ~ mainland_island +
                       year_collected +
                       (1|ID),
                     data = meri_width, 
                     REML = F)
meri_width_m3 <- lmer(width_sqr ~ mainland_island +
                       year_collected +
                       (1|ID),
                     data = meri_width, 
                     REML = F)
# type III test
Anova(meri_width_m3)
# Diagnostic

# Diagnostic function
diagnostic(meri_width$width)
diagnostic(meri_width$width_log)
diagnostic(meri_width$width_sqr)

# Residual histograms
hist(resid(meri_width_m1))
hist(resid(meri_width_m2))
hist(resid(meri_width_m3))

# DHARMa
testResiduals(meri_width_m1)
testResiduals(meri_width_m2)
testResiduals(meri_width_m3)
plotResiduals(meri_width_m3)

# Depth ####
# For depth untransformed data seems to work the best
meri_depth_m1 <- lmer(depth ~ mainland_island +
                    year_collected +
                    (1|ID),
                    data=meri_depth, 
                    REML = F)
meri_depth_m2 <- lmer(depth_log ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
meri_depth_m3 <- lmer(depth_sqr ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
# type III test
Anova(meri_depth_m1)

# Diagnostic

# Diagnostic function
diagnostic(meri_depth$depth)
diagnostic(meri_depth$depth_log)
diagnostic(meri_depth$depth_sqr)

# Residual histograms
hist(resid(meri_depth_m1))
hist(resid(meri_depth_m2))
hist(resid(meri_depth_m3))

# DHARMa
testResiduals(meri_depth_m1)
testResiduals(meri_depth_m2)
testResiduals(meri_depth_m3)

# Spine length ####
# I think the untransformed works best but the 0s may affect the results
meri_spine.length_m1 <-lmer(spine_length ~ mainland_island +
                           year_collected +
                           (1|ID),
                           data=meri_spine.length,
                           REML=F)

meri_spine.length_m2 <-lmer(spine_length_log ~ mainland_island +
                              year_collected +
                              (1|ID),
                            data=meri_spine.length,
                            REML=F)

meri_spine.length_m3 <-lmer(spine_length_sqr ~ mainland_island +
                              year_collected +
                              (1|ID),
                            data=meri_spine.length,
                            REML=F)

# type III test
Anova(meri_spine.length_m1)

# Diagnostic

# Diagnostic function
diagnostic(meri_spine.length$spine_length)
diagnostic(meri_spine.length$spine_length_log)
diagnostic(meri_spine.length$spine_length_sqr)

# Residual histograms
hist(resid(meri_spine.length_m1))
hist(resid(meri_spine.length_m2))
hist(resid(meri_spine.length_m3))

# DHARMa
testResiduals(meri_spine.length_m1)
testResiduals(meri_spine.length_m2)
testResiduals(meri_spine.length_m3)
testZeroInflation(meri_depth_m1)

# Tip distance ####
# Raw data looks the best
meri_tip.distance_m1 <- lmer(tip_distance ~ mainland_island +
                                 year_collected +
                                 (1|ID),
                             data=meri_tip.distance,REML=F)

meri_tip.distance_m2 <- lmer(tip_distance_log ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip.distance,REML=F)

meri_tip.distance_m3 <- lmer(tip_distance_sqr ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data=meri_tip.distance,REML=F)


# type III test
Anova(meri_tip.distance_m3)
# Diagnostic

# Diagnostic function
diagnostic(meri_tip.distance$tip_distance)
diagnostic(meri_tip.distance$tip_distance_log)
diagnostic(meri_tip.distance$tip_distance_sqr)

# Residual histograms
hist(resid(meri_tip.distance_m1))
hist(resid(meri_tip.distance_m2))
hist(resid(meri_tip.distance_m3))

# DHARMa
testResiduals(meri_tip.distance_m1)
testResiduals(meri_tip.distance_m2)
testResiduals(meri_tip.distance_m3)


# Spine number #### 
# Ask about count data can you transform it?
meri_spine.number_m1 <- glm(spine_num ~ mainland_island + 
                                  year_collected,
                              data = meri_spine.number,
                              family = poisson)

# type III test
Anova(meri_spine.number_m1)
# Diagnostic

# Residual histograms
hist(resid(meri_spine.number_m1))

# DHARMa
testResiduals(meri_spine.number_m1)

#### Lower spines ####
meri_lower.spines_m1 <- glm(lower_spines ~ mainland_island + 
                                  year_collected, 
                            data = meri_lower.spines, 
                            family = "binomial")
# Type III test
Anova(meri_lower.spines_m1)
# Diagnostic

# Residual histograms
hist(resid(meri_lower.spines_m1))

# DHARMa
testResiduals(meri_lower.spines_m1)

# Petal length data ####
# The raw data works best
flower_m1 <- lmer(petal_length ~ mainland_island +
               year_collected +
               (1|ID),
               data=petal_length,
               REML=F)

flower_m2 <- lmer(petal_length_log ~ mainland_island +
                    year_collected +
                    (1|ID),
                  data=petal_length,
                  REML=F)

flower_m3 <- lmer(petal_length_sqr ~ mainland_island +
                    year_collected +
                    (1|ID),
                  data=petal_length,
                  REML=F)

#type III test using lmertest
Anova(flower_m1)

# Diagnostic

# Diagnostic function
diagnostic(petal_length$petal_length)
diagnostic(petal_length$petal_length_log)
diagnostic(petal_length$petal_length_sqr)

# Residual histograms
hist(resid(flower_m1))
hist(resid(flower_m2))
hist(resid(flower_m3))

# DHARMa
testResiduals(flower_m1)
testResiduals(flower_m2)
testResiduals(flower_m3)

# Leaves data ####
# run simple linear mixed effects models on each leaf treat

# Leaf length ####
# Log transformed data works best
leaf_length_m1 <- lmer(leaf_length ~ mainland_island +
                       year_collected +
                       (1|ID),data=leaf_length,REML=F)

leaf_length_m2 <- lmer(leaf_length_log ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m3 <- lmer(leaf_length_sqr ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
# Type II test
Anova(leaf_length_m1)

# Diagnostic

# Diagnostic function
diagnostic(leaf_length$leaf_length)
diagnostic(leaf_length$leaf_length_log)
diagnostic(leaf_length$leaf_length_sqr)

# Residual histograms
hist(resid(leaf_length_m1))
hist(resid(leaf_length_m2))
hist(resid(leaf_length_m3))

# DHARMa
testResiduals(leaf_length_m1)
testResiduals(leaf_length_m2)
testResiduals(leaf_length_m3)

# Leaflet length ####
# Log transformed data works best
leaflet_length_m1 <- lmer(leaflet_length ~ mainland_island +
                         year_collected +
                         (1|ID),
                        data=leaflet_length,
                         REML = F)

leaflet_length_m2 <- lmer(leaflet_length_log ~ mainland_island +
                          year_collected +
                          (1|ID),
                        data=leaflet_length,
                        REML = F)

leaflet_length_m3 <- lmer(leaflet_length_sqr ~ mainland_island +
                          year_collected +
                          (1|ID),
                        data=leaflet_length,
                        REML = F)
# Type III test
Anova(leaflet_length_m2)

# Diagnostic

# Diagnostic function
diagnostic(leaflet_length$leaflet_length)
diagnostic(leaflet_length$leaflet_length_log)
diagnostic(leaflet_length$leaflet_length_sqr)

# Residual histograms
hist(resid(leaflet_length_m1))
hist(resid(leaflet_length_m2))
hist(resid(leaflet_length_m3))

# DHARMa
testResiduals(leaflet_length_m1)
testResiduals(leaflet_length_m2)
testResiduals(leaflet_length_m3)


# Leaflet number ####
leaflet_num_m1 <- glm(number_of_leaflets ~ mainland_island +
                       year_collected,
                       family = poisson,
                       data=leaf_length)

Anova(leaflet_num_m1)

# Diagnostic
# Residual histograms
hist(resid(leaflet_num_m1))

# DHARMa
testResiduals(leaflet_num_m1)


# Model 2 trait ~ galapagos/other islands ####
# Is there an effect between tribulus in galapagos compared to other island systems
# This comparison is possible with flower and leaf datasets

# Petal length data ####
# The squared data seems to work best
flower_m4 <- lmer(petal_length ~ galapagos_other +
                    year_collected +
                    (1|ID),
                  data=petal_length,
                  REML=F)

flower_m5 <- lmer(petal_length_log ~ galapagos_other +
                    year_collected +
                    (1|ID),
                  data=petal_length,
                  REML=F)

flower_m6 <- lmer(petal_length_sqr ~ galapagos_other +
                    year_collected +
                    (1|ID),
                  data=petal_length,
                  REML=F)

#type III test
Anova(flower_m4)
anova(flower_m4,
      flower_m5,
      flower_m6)
# Diagnostic

# Residual histograms
hist(resid(flower_m4))
hist(resid(flower_m5))
hist(resid(flower_m6))

# DHARMa
testResiduals(flower_m4)
testResiduals(flower_m5)
testResiduals(flower_m6)

# Leaf length ####
# Log transformed data works best
leaf_length_m4 <- lmer(leaf_length ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m5 <- lmer(leaf_length_log ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m6 <- lmer(leaf_length_sqr ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
# Type III test
Anova(leaf_length_m5)

# Diagnostic

# Residual histograms
hist(resid(leaf_length_m4))
hist(resid(leaf_length_m5))
hist(resid(leaf_length_m6))

# DHARMa
testResiduals(leaf_length_m4)
testResiduals(leaf_length_m5)
testResiduals(leaf_length_m6)

# Leaflet length ####
# Log transformed data works best
leaflet_length_m4 <- lmer(leaflet_length ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)

leaflet_length_m5 <- lmer(leaflet_length_log ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)

leaflet_length_m6 <- lmer(leaflet_length_sqr ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
# Type III test
Anova(leaflet_length_m4)

# Diagnostic

# Residual histograms
hist(resid(leaflet_length_m4))
hist(resid(leaflet_length_m5))
hist(resid(leaflet_length_m6))

# DHARMa
testResiduals(leaflet_length_m4)
testResiduals(leaflet_length_m5)
testResiduals(leaflet_length_m6)

# Leaflet number ####
leaflet_num_m2 <- glm(number_of_leaflets ~ galapagos_other +
                        year_collected,
                      family = poisson,
                      data=leaf_length)

Anova(leaflet_num_m2)

# Diagnostic
# Residual histograms
hist(resid(leaflet_num_m2))

# DHARMa
testResiduals(leaflet_num_m2)

# Model 3 trait ~ finch beak ####
# What is the effect of finch community within the galapagos islands

# Length ####
# For length, the raw data seems the best.
meri_length_m4 <- lmer(length ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_length,
                      REML = F)
meri_length_m5 <- lmer(length_log ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_length,
                      REML = F)
meri_length_m6 <- lmer(length_sqr ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_length,
                      REML = F)
# type III test
Anova(meri_length_m4)

# Diagnostic

# Residuals histogram
hist(resid(meri_length_m4))
hist(resid(meri_length_m5))
hist(resid(meri_length_m6))

# DHARMA <- this is the way with DHAMa
testResiduals(meri_length_m4)
testResiduals(meri_length_m5)
testResiduals(meri_length_m6)

plotResiduals(meri_length_m4)
plotResiduals(meri_length_m5)
plotResiduals(meri_length_m6)

# Width ####
# For width, squared transformed seemed the best.
meri_width_m4 <- lmer(width ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width, 
                      REML = F)
meri_width_m5 <- lmer(width_log ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width, 
                      REML = F)
meri_width_m6 <- lmer(width_sqr ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width, 
                      REML = F)
# type III test
Anova(meri_width_m4)

# Diagnostic

# Residual histograms
hist(resid(meri_width_m4))
hist(resid(meri_width_m5))
hist(resid(meri_width_m6))

# DHARMa
testResiduals(meri_width_m4)
testResiduals(meri_width_m5)
testResiduals(meri_width_m6)

# Depth ####
# For depth untransformed data seems to work the best
meri_depth_m4 <- lmer(depth ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data=gal_meri_depth, 
                      REML = F)
meri_depth_m5 <- lmer(depth_log ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data=gal_meri_depth, 
                      REML = F)
meri_depth_m6 <- lmer(depth_sqr ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data=gal_meri_depth, 
                      REML = F)
# type III test
Anova(meri_depth_m4)

# Diagnostic

# Residual histograms
hist(resid(meri_depth_m4))
hist(resid(meri_depth_m5))
hist(resid(meri_depth_m6))

# DHARMa
testResiduals(meri_depth_m4)
testResiduals(meri_depth_m5)
testResiduals(meri_depth_m6)

# Spine length ####
# I think the untransformed works best but the 0s may affect the results
meri_spine.length_m4 <-lmer(spine_length ~ finch_beak +
                              year_collected +
                              (1|ID),
                            data=gal_meri_spine.length,
                            REML=F)

meri_spine.length_m5 <-lmer(spine_length_log ~ finch_beak +
                              year_collected +
                              (1|ID),
                            data=gal_meri_spine.length,
                            REML=F)

meri_spine.length_m6 <-lmer(spine_length_sqr ~ finch_beak +
                              year_collected +
                              (1|ID),
                            data=gal_meri_spine.length,
                            REML=F)

# type III test
Anova(meri_spine.length_m4)

# Diagnostic

# Residual histograms
hist(resid(meri_spine.length_m4))
hist(resid(meri_spine.length_m5))
hist(resid(meri_spine.length_m6))

# DHARMa
testResiduals(meri_spine.length_m4)
testResiduals(meri_spine.length_m5)
testResiduals(meri_spine.length_m6)

# Tip distance ####
# Raw data looks the best
meri_tip.distance_m4 <- lmer(tip_distance ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip.distance,REML=F)

meri_tip.distance_m5 <- lmer(tip_distance_log ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip.distance,REML=F)

meri_tip.distance_m6 <- lmer(tip_distance_sqr ~ finch_beak +
                               year_collected +
                               (1|ID),
                             data=gal_meri_tip.distance,REML=F)


# type III test
Anova(meri_tip.distance_m4)
# Diagnostic

# Diagnostic function

# Residual histograms
hist(resid(meri_tip.distance_m4))
hist(resid(meri_tip.distance_m5))
hist(resid(meri_tip.distance_m6))

# DHARMa
testResiduals(meri_tip.distance_m4)
testResiduals(meri_tip.distance_m5)
testResiduals(meri_tip.distance_m6)

# Spine number #### 
# Ask about count data can you transform it?
meri_spine.number_m2 <- glm(spine_num ~ finch_beak + 
                              year_collected,
                            data = gal_meri_spine.number,
                            family = poisson)

# type III test
Anova(meri_spine.number_m2)
# Diagnostic

# Residual histograms
hist(resid(meri_spine.number_m2))

# DHARMa
testResiduals(meri_spine.number_m2)

#### Lower spines ####
meri_lower.spines_m2 <- glm(lower_spines ~ finch_beak + 
                              year_collected, 
                            data = gal_meri_lower.spines, 
                            family = "binomial")
# Type III test
Anova(meri_lower.spines_m2)
# Diagnostic

# Residual histograms
hist(resid(meri_lower.spines_m2))

# DHARMa
testResiduals(meri_lower.spines_m2)

# Petal length data ####
# The raw data works best
flower_m7 <- lmer(petal_length ~ finch_beak +
                    year_collected +
                    (1|ID),
                  data=gal_petal_length,
                  REML=F)

flower_m8 <- lmer(petal_length_log ~ finch_beak +
                    year_collected +
                    (1|ID),
                  data=gal_petal_length,
                  REML=F)

flower_m9 <- lmer(petal_length_sqr ~ finch_beak +
                    year_collected +
                    (1|ID),
                  data=gal_petal_length,
                  REML=F)

#type III test using lmertest
Anova(flower_m9)

# Diagnostic

# Residual histograms
hist(resid(flower_m7))
hist(resid(flower_m8))
hist(resid(flower_m9))

# DHARMa
testResiduals(flower_m7)
testResiduals(flower_m8)
testResiduals(flower_m9)

# Leaves data ####
# run simple linear mixed effects models on each leaf treat

# Leaf length ####
# Log transformed data works best
leaf_length_m7 <- lmer(leaf_length ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m2 <- lmer(leaf_length_log ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m3 <- lmer(leaf_length_sqr ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
# Type II test
Anova(leaf_length_m1)

# Diagnostic

# Diagnostic function
diagnostic(leaf_length$leaf_length)
diagnostic(leaf_length$leaf_length_log)
diagnostic(leaf_length$leaf_length_sqr)

# Residual histograms
hist(resid(leaf_length_m1))
hist(resid(leaf_length_m2))
hist(resid(leaf_length_m3))

# DHARMa
testResiduals(leaf_length_m1)
testResiduals(leaf_length_m2)
testResiduals(leaf_length_m3)

# Leaflet length ####
# Log transformed data works best
leaflet_length_m1 <- lmer(leaflet_length ~ mainland_island +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)

leaflet_length_m2 <- lmer(leaflet_length_log ~ mainland_island +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)

leaflet_length_m3 <- lmer(leaflet_length_sqr ~ mainland_island +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
# Type III test
Anova(leaflet_length_m2)

# Diagnostic

# Diagnostic function
diagnostic(leaflet_length$leaflet_length)
diagnostic(leaflet_length$leaflet_length_log)
diagnostic(leaflet_length$leaflet_length_sqr)

# Residual histograms
hist(resid(leaflet_length_m1))
hist(resid(leaflet_length_m2))
hist(resid(leaflet_length_m3))

# DHARMa
testResiduals(leaflet_length_m1)
testResiduals(leaflet_length_m2)
testResiduals(leaflet_length_m3)


# Leaflet number ####
leaflet_num_m1 <- glm(number_of_leaflets ~ mainland_island +
                        year_collected,
                      family = poisson,
                      data=leaf_length)

Anova(leaflet_num_m1)

# Diagnostic
# Residual histograms
hist(resid(leaflet_num_m1))

# DHARMa
testResiduals(leaflet_num_m1)




# Estimates of LS means ####

# mainland/island effect. Model 1 ####

#the following estimates LS means for the mainland_island effect for each response

emm_options(opt.digits=F)

# Mericarp ####
emmeans(mericarp_length_m1, ~mainland_island)
emmeans(mericarp_width_m1, ~mainland_island)
emmeans(mericarp_depth_m1, ~mainland_island)
emmeans(mericarp_spine.length_m1, ~mainland_island)
emmeans(mericarp_tip.distance_m1, ~mainland_island)
emmeans(mericarp_spine.number_m1, ~mainland_island)
emmeans(mericarp_lower.spines_m1, ~mainland_island)

# Flower ####
emmeans(flower_m1,~mainland_island)

# Leaf ####
emmeans(leaf_length_m1,~mainland_island)
emmeans(leaflet_length_m1,~mainland_island)
emmeans(leaflet_num_m1,~mainland_island)






#### Length ####
# Raw data
mericarp_length_m2<- lm(length ~ finch_beak + year_collected, data = gal_length) #Full model
Anova(mericarp_length_m2)


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



# Diagnostic for models ######
# Model 1 mainland/island ####
attach(mericarp)
attach(flower)
attach(leaf)
hist(length)
hist(petal_length)
hist(leaf_length)

diagnostic(length)
plot(mericarp_length_m1)

diagnostic(width)
diagnostic(depth)

diagnostic(petal_length)

diagnostic(leaf_length)
diagnostic(leaflet_length)
diagnostic(leaf_num)






