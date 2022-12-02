# Script 04.Univariate models with year and bioclimate variables as covariate ####
# By: Daniel Reyes Corral

# This script uses the individual datasets per trait from script 02 to run the models
# Models are organized by group comparisons and datasets. All models uses bioclim data

# We got lat/long information for each location and extracted bioclimate variables from WorldClim (https://worldclim.org/). 
# We used variables bio1, bio4, bio12, bio15. 
# We projected the data in Qgis to fill gaps for island locations, 
# we also used (Weigelt et al., 2013) dataset as equivalent for locations closer to ours.
# 
# Bio1 =  Annual Mean Temperature - Temp
# Bio4 = Temperature Seasonality (Standard Dev. x 100) - Temp_S
# Bio12 = Annual precipitation - Prec
# Bio15 = Precipitation Seasonality (coefficient of variation) - varP

# Model 1 compares mainland and island populations (used for mericarps and flowers)
# Model 2 compares Galapagos with the other island systems (used for flowers)
# For each trait I test model assumptions and transform data accordingly for convergence
# Data transformations are either log or squared roots.
# I made some of the outputs comments because I do need the models for making the plots
# but model comparisons were done before.
# Emmeans estimates:
# Use emmeans package for this_ Using "response" allows me to compare on the original scale for transformed data_
# This gives me an idea of how much change there is between the factors of interest_
# Using the that that worked best for each trait and model
# the following estimates LS means for the mainland_island effect for each response

# 04_01 Model 1: Mainland - Island ####
# 04_01_01 Mericarp data ####
## Length ####
# For length, the untransformed data seems the best
#### Raw data ####
meri_length_bioclim <- lmer(length ~ mainland_island +
                        year_collected +
                        Herbarium +
                        Temp +
                        Temp_S +
                        Prec +
                        varP +
                        (1|ID),
                      data = meri_length,
                      REML = F)
#### Log transformed ####
# meri_length_m2<- lmer(log(length) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data = meri_length,
#                       REML = F)
#### Square-root transformed ####
# meri_length_m3<- lmer(sqrt(length) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data = meri_length,
#                       REML = F)

#### ANOVA type II test ####
# Use the Anova function from the car package

Anova(meri_length_bioclim)

#summary(meri_length_m1)

#### Model Diagnostics ####
# I used a custom diagnostic functionfrom script 01 and the testResiduals function
# from the DHARMa package. This function shows the QQ residuals, dispersion test
# and outlier tests. 

# Diagnostic custom function
# par(mfrow = c(1, 3))
# Residual histograms distributions
# diagnostic(resid(meri_length_m1))
# diagnostic(resid(meri_length_m2))
# diagnostic(resid(meri_length_m3))

# Diagnostics with DHARMA
# testResiduals(meri_length_m1)
# testResiduals(meri_length_m2)
# testResiduals(meri_length_m3)

# After selecting the data transformation that converged the most I decided to
# only show the output of that model.

## Emmeans estimates: Length ####
EM_length_bioclim <- emmeans(meri_length_bioclim, ~ mainland_island)

### Emmean plot: Length ####
plot(EM_length_bioclim, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length_bioclim)
### Percentage difference ####
# ((island mean/mainland mean)-1)*100%
((5.718/5.695 - 1)* 100) # Mericarps ~ 0.40% longer on islands

## Width ####
# For width, log transformed seemed the best
#### Raw data ####
# meri_width_m1 <- lmer(width ~ mainland_island +
#                         year_collected +
#                         Herbarium +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data = meri_width, 
#                       REML = F)
#### Log transformed ####
meri_width_bioclim <- lmer(log(width) ~ mainland_island +
                        year_collected +
                        Herbarium +
                        Temp +
                        Temp_S +
                        Prec +
                        varP +
                        (1|ID),
                      data = meri_width, 
                      REML = F)
#### Square-root transformed ####
# meri_width_m3 <- lmer(sqrt(width) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data = meri_width, 
#                       REML = F)
#### ANOVA type II test ####
Anova(meri_width_bioclim)

#### Model Diagnostics ####
# Residual histogram distributions
 # diagnostic(resid(meri_width_m1))
 # diagnostic(resid(meri_width_m2))
 # diagnostic(resid(meri_width_m3))

# # DHARMa
 # testResiduals(meri_width_m1)
 # testResiduals(meri_width_m2)
 # testResiduals(meri_width_m3)

## Emmean estimates: Width ####
EM_width_bioclim <- emmeans(meri_width_bioclim, ~ mainland_island, type = "response")
### Emmean plot: Width ####
plot(EM_width_bioclim, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width_bioclim)
### Percentage difference ####
((3.15/3.04 - 1)*100) # Mericarps ~ 3.618% wider on islands

## Depth ####
# For depth untransformed data seems to work the best
#### Raw data ####
meri_depth_bioclim <- lmer(depth ~ mainland_island +
                        year_collected +
                        Herbarium +
                        Temp +
                        Temp_S +
                        Prec +
                        varP +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
#### Log transformed data ####
# meri_depth_m2 <- lmer(log(depth) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data=meri_depth, 
#                       REML = F)
#### Square-root transformed data ####
# meri_depth_m3 <- lmer(sqrt(depth) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data=meri_depth, 
#                       REML = F)
#### ANOVA type II test ####
Anova(meri_depth_bioclim)

#### Model Diagnostics ####
# hist(resid(meri_depth_m1), breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_depth_m1))
#  diagnostic(resid(meri_depth_m2))
#  diagnostic(resid(meri_depth_m3))
# 
# # DHARMa
 # testResiduals(meri_depth_m1)
 # testResiduals(meri_depth_m2)
 # testResiduals(meri_depth_m3)

## Emmeans estimates: Depth ####
EM_depth_bioclim <- emmeans(meri_depth_bioclim, ~ mainland_island)
### Emmeans plot: Depth ####
plot(EM_depth_bioclim, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth_bioclim)
### Percentage difference ####
((4.53/4.35 - 1)*100) # Mericarps ~ 4.13% deeper on islands

## Spine Tip distance ####
# For spine tip distance I ran models with all the samples.
# This includes mericarps without upper spines (Spine tip distance=0)

##### Models with all mericarps ####
# meri_tip_distance_m1 <- lmer(tip_distance ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              data=meri_tip_distance,REML=F)
# 
# meri_tip_distance_m2 <- lmer(log(tip_distance + 1) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              data=meri_tip_distance,REML=F)
# 
# meri_tip_distance_m3 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              data=meri_tip_distance,REML=F)
# 

# ANOVA type II test
# Anova(meri_tip_distance_m1)

# Model Diagnostics
# Residual distributions
# hist(resid(meri_spine_length_m1), breaks = 20)
# Spine tip distance distribution
# hist(meri_tip_distance$tip_distance, breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_tip_distance_m1))
# diagnostic(resid(meri_tip_distance_m2))
# diagnostic(resid(meri_tip_distance_m3))

# # DHARMa
# testResiduals(meri_tip_distance_m1)
# testResiduals(meri_tip_distance_m2)
# testResiduals(meri_tip_distance_m3)


##### Removed Zero Tip distance ####
# Then, I tried the models again. This time, removing the mericarps with
# spine tip distance of 0.
# This model did not converged well. There are some outliers.
# Raw data looks the best
meri_tip_distance_bioclim4 <- lmer(tip_distance ~ mainland_island +
                               year_collected +
                               Temp +
                               Temp_S +
                               Prec +
                               varP +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero,REML=F)

# meri_tip_distance_bioclim5 <- lmer(log(tip_distance + 1) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              data=meri_tip_distance_wozero,REML=F)

# meri_tip_distance_bioclim6 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              data=meri_tip_distance_wozero,REML=F)


# ANOVA type II test
#Anova(meri_tip_distance_m4)

# Model Diagnostics
 # diagnostic(resid(meri_tip_distance_m4))
 # diagnostic(resid(meri_tip_distance_m5))
 # diagnostic(resid(meri_tip_distance_m6))

# The residual distribution showed some outliers. 
# Check residual distributions after removing mericarps without spines.
# I included the residual column into the dataset
meri_tip_distance_wozero$residuals <- resid(meri_tip_distance_bioclim4)
 # hist(resid(meri_tip_distance_m4), breaks = 20)
 # hist(meri_tip_distance_wozero$tip_distance, breaks = 20)

# # DHARMa
 # testResiduals(meri_tip_distance_m4)
 # testResiduals(meri_tip_distance_m5)
 # testResiduals(meri_tip_distance_m6)


# Based on the residual distributions and the trait distributions I filter the data

##### Filter residuals (used in analysis) ####
# I filter residuals that are lower than -5 and larger than 5.
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero,
                                          !residuals < -5)
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero_filter,
                                          !residuals > 5)

# This removes specimen no.383

# Create a new filtered dataset for spine tip distance
# hist(meri_tip_distance_wozero_filter$residuals, breaks = 20)
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero_filter, !is.na(residuals))

# Ran the models with this filter data and raw data works best
###### Raw data ####
meri_tip_distance_bioclim7 <- lmer(tip_distance ~ mainland_island +
                               year_collected +
                               Herbarium +
                               Temp +
                               Temp_S +
                               Prec +
                               varP +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_filter,REML=F)
###### Log transformed data ####
# meri_tip_distance_m8 <- lmer(log(tip_distance) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance_wozero_filter,REML=F)
###### Squared-root data ####
# meri_tip_distance_m9 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance_wozero_filter,REML=F)


# Diagnostic
 # diagnostic(resid(meri_tip_distance_m7))
 # diagnostic(resid(meri_tip_distance_m8))
 # diagnostic(resid(meri_tip_distance_m9))

#Anova
Anova(meri_tip_distance_bioclim7)

# DHARMa
# testResiduals(meri_tip_distance_m7)

## Emmeans estimates: Spine tip distance ####
# Zero filter data
EM_tip_dist_bioclim <- emmeans(meri_tip_distance_bioclim7, ~ mainland_island)
### Emmeans plot: Spine tip distance ####
plot(EM_tip_dist_bioclim, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist_bioclim)
### Percentage difference ####
((9.43/8.86 - 1)*100) # Mericarps ~ 6.43% more separated on islands

## Lower spines ####
# I tried to fit a glm
# For lower spines I used a glmm model and removed year as a covariate.
# meri_lower_spines_m1 <- glm(lower_spines ~ mainland_island + 
#                                    year_collected, 
#                              data = meri_lower_spines, 
#                              family = "binomial")

meri_lower_spines_bioclim <- glmmTMB(factor(lower_spines) ~ mainland_island +
                                       year_collected +
                                       Herbarium +
                                       Temp +
                                       Temp_S +
                                       Prec +
                                       varP +
                                       (1|ID),
                                     data = meri_lower_spines,
                                     family = binomial)
# str(meri_lower_spines)
### ANOVA Type II test ####
Anova(meri_lower_spines_bioclim)

### Model Diagnostics ####
# # Residual histograms
#diagnostic(resid(meri_lower_spines_m1_glmm))

## Emmeans estimates: Lower spines ####
#Glmm
EM_lower_bioclim <- emmeans(meri_lower_spines_bioclim, ~ mainland_island, type = "response")
### Emmeans plot: Lower spines ####
plot(EM_lower_bioclim, comparisons = T) + labs(title = "Mericarp Lower Spines")
pwpp(EM_lower_bioclim)

# 04_01_02 Flower dataset ####
## Petal length ####
# The raw data works best
### Raw data ####
flower_bioclim <- lmer(petal_length ~ mainland_island +
                    year_collected +
                    Temp +
                    Temp_S +
                    Prec +
                    varP +
                    (1|ID),
                  data=flower,
                    REML=F)
### Log transformed ####
# flower_m2 <- lmer(log(petal_length) ~ mainland_island +
#                     year_collected +
#                     Temp +
#                     Temp_S +
#                     Prec +
#                     varP +
#                     (1|ID),
#                   data=flower,
#                   REML=F)
### Square-root transformed ####
# flower_m3 <- lmer(sqrt(petal_length) ~ mainland_island +
#                     year_collected +
#                     Temp +
#                     Temp_S +
#                     Prec +
#                     varP +
#                     (1|ID),
#                   data=flower,
#                   REML=F)

### ANOVA type II test ####
Anova(flower_bioclim)

### Model diagnostics ####
#hist(flower$petal_length, breaks = 20)
#diagnostic(resid(flower_bioclim))
# 
# testResiduals(flower_m1)
# testResiduals(flower_m2)
# testResiduals(flower_m3)

## Emmean estimates: Petal length ####
EM_flower_bioclim <- emmeans(flower_bioclim, ~ mainland_island)
### Emmean plot: Petal length ####
plot(EM_flower_bioclim, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower_bioclim)
### Percentange difference ####
((17.07/17.04 - 1)*100) # Flowers are 0.17% smaller on islands

# 03_04 Spine tip distance compared between lower spines ####
## Spine Tip distance ####
# For spine tip distance I ran models with all the samples.
# This includes mericarps without upper spines (Spine tip distance=0)

# Ran the models with this filter data and raw data works best
###### Raw data ####
meri_tip_distance_lower_bioclim <- lmer(tip_distance ~ lower_spines +
                                   year_collected +
                                     Herbarium +
                                     Temp +
                                     Temp_S + 
                                     Prec +
                                     varP +
                                   (1|ID),
                                 na.action = na.exclude,
                                 data=meri_tip_distance_lower_filter,REML=F)
###### Log transformed data ####
# meri_tip_distance_lower8 <- lmer(log(tip_distance) ~ lower_spines +
#                                    year_collected +
#                                    (1|ID),
#                                  na.action = na.exclude,
#                                  data=meri_tip_distance_lower_filter,REML=F)
###### Squared-root data ####
# meri_tip_distance_lower9 <- lmer(sqrt(tip_distance) ~ lower_spines +
#                                    year_collected +
#                                    (1|ID),
#                                  na.action = na.exclude,
#                                  data=meri_tip_distance_lower_filter,REML=F)


# Diagnostic
# diagnostic(resid(meri_tip_distance_lower7))
# diagnostic(resid(meri_tip_distance_lower8))
# diagnostic(resid(meri_tip_distance_lower9))

#Anova
Anova(meri_tip_distance_lower_bioclim)

# DHARMa
# testResiduals(meri_tip_distance_lower7)

## Emmeans estimates: Spine tip distance ####
# Zero filter data
EM_tip_dist_lower_bioclim <- emmeans(meri_tip_distance_lower_bioclim, ~ lower_spines)
### Emmeans plot: Spine tip distance ####
plot(EM_tip_dist_lower_bioclim, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist_lower_bioclim)
### Percentage difference ####
((9.11/7.60 - 1)*100) # Mericarps ~ 19% more separated on mericarps with lower spines














