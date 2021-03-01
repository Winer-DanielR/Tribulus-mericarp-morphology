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
meri_length_m3<- lmer(sqrt(length) ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = meri_length,
                      REML = F)
# type III test
Anova(meri_length_m3)

# Diagnostic

# Residuals histogram
diagnostic(resid(meri_length_m1))
diagnostic(resid(meri_length_m2))
diagnostic(resid(meri_length_m3))

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
meri_width_m3 <- lmer(sqrt(width) ~ mainland_island +
                       year_collected +
                       (1|ID),
                     data = meri_width, 
                     REML = F)
# type III test
Anova(meri_width_m3)
# Diagnostic

# Residual histograms
diagnostic(resid(meri_width_m1))
diagnostic(resid(meri_width_m2))
diagnostic(resid(meri_width_m3))

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

# Residual histograms
diagnostic(resid(meri_depth_m1))
diagnostic(resid(meri_depth_m2))
diagnostic(resid(meri_depth_m3))

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

# Residual histograms
diagnostic(resid(meri_spine.length_m1))
diagnostic(resid(meri_spine.length_m2))
diagnostic(resid(meri_spine.length_m3))

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

# Residual histograms
diagnostic(resid(meri_tip.distance_m1))
diagnostic(resid(meri_tip.distance_m2))
diagnostic(resid(meri_tip.distance_m3))

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
diagnostic(resid(meri_spine.number_m1))

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
diagnostic(resid(meri_lower.spines_m1))

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

# Residual histograms
diagnostic(resid(flower_m1))
diagnostic(resid(flower_m2))
diagnostic(resid(flower_m3))

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

leaf_length_m2 <- lmer(log(leaf_length) ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m3 <- lmer(leaf_length_sqr ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
# Type II test
Anova(leaf_length_m1)

# Diagnostic

# Residual histograms
diagnostic(resid(leaf_length_m1))
diagnostic(resid(leaf_length_m2))
diagnostic(resid(leaf_length_m3))

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

leaflet_length_m2 <- lmer(log(leaflet_length) ~ mainland_island +
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
diagnostic(resid(leaflet_length_m1))
diagnostic(resid(leaflet_length_m2))
diagnostic(resid(leaflet_length_m3))

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
diagnostic(resid(leaflet_num_m1))

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

flower_m6 <- lmer(sqrt(petal_length) ~ galapagos_other +
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
diagnostic(resid(flower_m4))
diagnostic(resid(flower_m5))
diagnostic(resid(flower_m6))

# DHARMa
testResiduals(flower_m4)
testResiduals(flower_m5)
testResiduals(flower_m6)

# Leaf length ####
# Log transformed data works best
leaf_length_m4 <- lmer(leaf_length ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m5 <- lmer(log(leaf_length) ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)

leaf_length_m6 <- lmer(leaf_length_sqr ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
# Type III test
Anova(leaf_length_m5)

# Diagnostic

# Residual histograms
diagnostic(resid(leaf_length_m4))
diagnostic(resid(leaf_length_m5))
diagnostic(resid(leaf_length_m6))

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

leaflet_length_m5 <- lmer(log(leaflet_length) ~ galapagos_other +
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
diagnostic(resid(leaflet_length_m4))
diagnostic(resid(leaflet_length_m5))
diagnostic(resid(leaflet_length_m6))

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
diagnostic(resid(leaflet_num_m2))

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
diagnostic(resid(meri_length_m4))
diagnostic(resid(meri_length_m5))
diagnostic(resid(meri_length_m6))

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
meri_width_m6 <- lmer(sqrt(width) ~ finch_beak +
                        year_collected +
                        (1|ID),
                      data = gal_meri_width, 
                      REML = F)
# type III test
Anova(meri_width_m4)

# Diagnostic

# Residual histograms
diagnostic(resid(meri_width_m4))
diagnostic(resid(meri_width_m5))
diagnostic(resid(meri_width_m6))

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
diagnostic(resid(meri_depth_m4))
diagnostic(resid(meri_depth_m5))
diagnostic(resid(meri_depth_m6))

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
diagnostic(resid(meri_spine.length_m4))
diagnostic(resid(meri_spine.length_m5))
diagnostic(resid(meri_spine.length_m6))

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

# Diagnostic of residuals
diagnostic(resid(meri_tip.distance_m4))
diagnostic(resid(meri_tip.distance_m5))
diagnostic(resid(meri_tip.distance_m6))

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
diagnostic(resid(meri_spine.number_m2))

# DHARMa
testResiduals(meri_spine.number_m2)

#### Lower spines ####
# Ask about the outcomes of this model. How can I diagnose this one?
meri_lower.spines_m2 <- glm(lower_spines ~ finch_beak + 
                              year_collected, 
                            data = gal_meri_lower.spines, 
                            family = "binomial")
# Type III test
Anova(meri_lower.spines_m2)

# Diagnostic
diagnostic(resid(meri_lower.spines_m2))

# DHARMa
testResiduals(meri_lower.spines_m2)

# Petal length data ####
# The raw data works best. However I think there is too few datapoints.
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

# Diagnostic of residuals
diagnostic(resid(flower_m7))
diagnostic(resid(flower_m8))
diagnostic(resid(flower_m9))

# DHARMa
testResiduals(flower_m7)
testResiduals(flower_m8)
testResiduals(flower_m9)

# Leaves data ####

# Leaf length ####
# Squared transformed data works best
leaf_length_m7 <- lmer(leaf_length ~ finch_beak +
                         year_collected +
                         (1|ID),data=gal_leaf_length,REML=F)

leaf_length_m8 <- lmer(leaf_length_log ~ finch_beak +
                         year_collected +
                         (1|ID),data=gal_leaf_length,REML=F)

leaf_length_m9 <- lmer(sqrt(leaf_length) ~ finch_beak +
                         year_collected +
                         (1|ID),data=gal_leaf_length,REML=F)
# Type II test
Anova(leaf_length_m7)

# Diagnostic
diagnostic(resid(leaf_length_m7))
diagnostic(resid(leaf_length_m8))
diagnostic(resid(leaf_length_m9))

# DHARMa
testResiduals(leaf_length_m7)
testResiduals(leaf_length_m8)
testResiduals(leaf_length_m9)

# Leaflet length ####
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

leaflet_length_m9 <- lmer(leaflet_length_sqr ~ finch_beak +
                            year_collected +
                            (1|ID),
                          data=gal_leaflet_length,
                          REML = F)
# Type III test
Anova(leaflet_length_m7)

# Diagnostic

# Residual histograms
diagnostic(resid(leaflet_length_m7))
diagnostic(resid(leaflet_length_m8))
diagnostic(resid(leaflet_length_m9))

# DHARMa
testResiduals(leaflet_length_m7)
testResiduals(leaflet_length_m8)
testResiduals(leaflet_length_m9)


# Leaflet number ####
leaflet_num_m3 <- glm(number_of_leaflets ~ finch_beak +
                        year_collected,
                      family = poisson,
                      data=gal_leaf_length)

# Test
Anova(leaflet_num_m3)

# Diagnostic
# Residual histograms
diagnostic(resid(leaflet_num_m3))

# DHARMa
testResiduals(leaflet_num_m3)

# LS means ####
# Use emmeans package for this. Using "response" allows me to compare on the original scale for transformed data.
# This gives me an idea of how much change there is between the factors of interest.
# Using the that that worked best for each trait and model
#the following estimates LS means for the mainland_island effect for each response

# Model 1: Mainland vs Island ####

# Mericarp ####
# Seems that mericarps in islands are larger. Well defended?
EM_length <- emmeans(meri_length_m3, ~ mainland_island, type = "response") # Is transformed data
plot(EM_length) + labs(title = "Mericarp Length")
EM_width <- emmeans(meri_width_m3, ~ mainland_island, type = "response")
plot(EM_width) + labs(title = "Mericarp Width")
EM_depth <- emmeans(meri_depth_m1, ~ mainland_island)
plot(EM_depth) + labs(title = "Mericarp Depth")
EM_spine <- emmeans(meri_spine.length_m1, ~ mainland_island)
plot(EM_spine) + labs(title = "Mericarp Spine Length")
EM_tip.dist <- emmeans(meri_tip.distance_m1, ~ mainland_island)
plot(EM_tip.dist) + labs(title = "Mericarp Tip distance")
EM_spine.num <- emmeans(meri_spine.number_m1, ~ mainland_island, type = "response")
plot(EM_spine.num) + labs(title = "Mericarp Spine Number")
EM_lower <- emmeans(meri_lower.spines_m1, ~ mainland_island)
plot(EM_lower) + labs(title = "Mericarp Lower Spines")

# Flower ####
EM_flower <- emmeans(flower_m1, ~ mainland_island)
plot(EM_flower) + labs(title = "Flower Length")

# Leaf ####
EM_leaf <- emmeans(leaf_length_m2, ~ mainland_island, type = "response")
plot(EM_leaf) + labs(title = "Leaf Length")
EM_leaflet <- emmeans(leaflet_length_m2, ~ mainland_island, type = "response")
plot(EM_leaflet) + labs(title = "Leaflet Length")
EM_leaflet.num <- emmeans(leaflet_num_m1, ~ mainland_island)
plot(EM_leaflet.num) + labs(title = "Leaflet Number")

# Model 2: Galapagos and other islands ####
# Seems that in Galapagos the flowers and the leaves are smaller compared to other islands.
# Flower ####
EM_flower2 <- emmeans(flower_m6, ~ galapagos_other, type = "response")
plot(EM_flower2) + labs(title = "Flower Length")

# Leaf ####
EM_leaf2 <- emmeans(leaf_length_m5, ~ galapagos_other, type = "response")
plot(EM_leaf2) + labs(title = "Leaf Length")
EM_leaflet2 <- emmeans(leaflet_length_m5, ~ galapagos_other, type = "response")
plot(EM_leaflet2) + labs(title = "Leaflet Length")
EM_leaflet.num2 <- emmeans(leaflet_num_m2, ~ galapagos_other)
plot(EM_leaflet.num2) + labs(title = "Leaflet Number")

# Model 3: Finch communities within Galapagos ####

# Mericarp ####
EM_length3 <- emmeans(meri_length_m4, ~ finch_beak)
plot(EM_length3) + labs(title = "Mericarp Length")
EM_width3 <- emmeans(meri_width_m6, ~ finch_beak, type = "response")
plot(EM_width3) + labs(title = "Mericarp Width")
EM_depth3 <- emmeans(meri_depth_m4, ~ finch_beak)
plot(EM_depth3) + labs(title = "Mericarp Depth")
EM_spine3 <- emmeans(meri_spine.length_m4, ~ finch_beak)
plot(EM_spine3) + labs(title = "Mericarp Spine Length")
EM_tip.dist3 <- emmeans(meri_tip.distance_m4, ~ finch_beak)
plot(EM_tip.dist3) + labs(title = "Mericarp Tip Distance")
EM_spine.num3 <- emmeans(meri_spine.number_m2, ~ finch_beak)
plot(EM_spine.num3) + labs(title = "Mericarp Spine Number")
EM_lower3 <- emmeans(meri_lower.spines_m2, ~ finch_beak)
plot(EM_lower3) + labs(title = "Mericarp Lower Spine")

# Flower ####
EM_flower3 <- emmeans(flower_m7, ~ finch_beak)
plot(EM_flower3) + labs(title = "Flower Length")

# Leaf ####
EM_leaf3 <- emmeans(leaf_length_m9, ~ finch_beak, type = "response")
plot(EM_leaf3) + labs(title = "Leaf Length")
EM_leaflet3 <- emmeans(leaflet_length_m8, ~ finch_beak, type = "response")
plot(EM_leaflet3) + labs(title = "Leaflet Length")
EM_leaflet.num3 <- emmeans(leaflet_num_m3, ~ finch_beak)
plot(EM_leaflet.num3) + labs(title = "Leaflet Number")
