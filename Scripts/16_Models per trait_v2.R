# Script 16.Univariate models with year as random factor ####
# By: Daniel Reyes Corral

# Same as script 02 but with year as random factor this time
# Based on comments given by Andrew and Handry lab folks


# Model 1: Mainland - Island ####
# 16_01 Mericarp data ####
## Length ####
# For length, the squared-root transformed data seems to work best
#### Raw data ####
meri_length_y1<- lmer(length ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data = meri_length,
                      REML = F)
#### Log transformed ####
meri_length_y2<- lmer(log(length) ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data = meri_length,
                      REML = F)
#### Square-root transformed ####
meri_length_y3<- lmer(sqrt(length) ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data = meri_length,
                      REML = F)

#### ANOVA type II test ####
# Use the Anova function from the car package
# Anova(meri_length_y3)
#ANOVA_length_m1 <-as.data.frame(Anova(meri_length_m1))
#summary(meri_length_m1)

#### Model Diagnostics ####
# I used a custom diagnostic functionfrom script 01 and the testResiduals function
# from the DHARMa package. This function shows the QQ residuals, dispersion test
# and outlier tests. 

# Diagnostic custom function
#par(mfrow = c(1, 3))
# Residual histograms distributions
# diagnostic(resid(meri_length_y1))
# diagnostic(resid(meri_length_y2))
# diagnostic(resid(meri_length_y3))

# Diagnostics with DHARMA
# testResiduals(meri_length_y1)
# testResiduals(meri_length_y2)
# testResiduals(meri_length_y3)

# The outcome of the models with year as random effect does change the percentage difference
# of the estimated emmeans. The model is still significant.

## Emmeans estimates: Length ####
EM_length_year <- emmeans(meri_length_y3, ~ mainland_island, type = "response")

### Emmean plot: Length ####
plot(EM_length_year, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length_year)
### Percentage difference ####
# ((island mean/mainland mean)-1)*100%
((6.22/5.89 - 1)* 100) # Mericarps ~ 5.6% longer on islands

## Width ####
# For width, log transformed data seemed the best
#### Raw data ####
meri_width_y1 <- lmer(width ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data = meri_width_f, 
                      REML = F)
#### Log transformed ####
meri_width_y2 <- lmer(log(width) ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data = meri_width_f, 
                      REML = F)
#### Square-root transformed ####
meri_width_y3 <- lmer(sqrt(width) ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data = meri_width_f, 
                      REML = F)
#### ANOVA type II test ####
Anova(meri_width_y2)

#### Model Diagnostics ####
# Residual histogram distributions
 diagnostic(resid(meri_width_y1))
 diagnostic(resid(meri_width_y2))
 diagnostic(resid(meri_width_y3))

hist(resid(meri_width_y2), breaks = 50)
# Possible outliers, the distribution of residuals for model 2 seems 
# normal, but there are a couple of individuals that seem outliers
hist(meri_width$width, breaks = 50)

# Filter residuals above and below 0.5
meri_width_f <- filter(meri_width,!width > 6)
hist(meri_width_f$width, breaks = 50)

# # DHARMa
 testResiduals(meri_width_y1)
 testResiduals(meri_width_y2)
 testResiduals(meri_width_y3)

## Emmean estimates: Width ####
EM_width_year <- emmeans(meri_width_y3, ~ mainland_island, type = "response")
### Emmean plot: Width ####
plot(EM_width_year, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width_year)
### Percentage difference ####
((3.23/3.03 - 1)*100) # Mericarps ~ 6.6% wider on islands


## Depth ####
# For depth untransformed data seems to work the best
#### Raw data ####
meri_depth_y1 <- lmer(depth ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
#### Log transformed data ####
meri_depth_y2 <- lmer(log(depth) ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
#### Square-root transformed data ####
meri_depth_y3 <- lmer(sqrt(depth) ~ mainland_island +
                        (1|year_collected) +
                        (1|ID),
                      data=meri_depth, 
                      REML = F)
#### ANOVA type II test ####
 Anova(meri_depth_y3)

#### Model Diagnostics ####
 hist(resid(meri_depth_y1), breaks = 50)

# # Residual histograms
 diagnostic(resid(meri_depth_y1))
 diagnostic(resid(meri_depth_y2))
 diagnostic(resid(meri_depth_y3))
# 
# # DHARMa
 testResiduals(meri_depth_y1)
 testResiduals(meri_depth_y2)
 testResiduals(meri_depth_y3)

## Emmeans estimates: Depth ####
EM_depth_year <- emmeans(meri_depth_y1, ~ mainland_island)
### Emmeans plot: Depth ####
plot(EM_depth_year, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth_year)
### Percentage difference ####
((4.88/4.51 - 1)*100) # Mericarps ~ 8% deeper on islands

## Spine Tip distance ####
# For spine tip distance I ran models with all the samples.
# This includes mericarps without upper spines (Spine tip distance=0)

##### Models with all mericarps ####
# Raw data looks the best
meri_tip_distance_y1 <- lmer(tip_distance ~ mainland_island +
                        (1|year_collected) +
                               (1|ID),
                             data=meri_tip_distance,REML=F)

meri_tip_distance_y2 <- lmer(log(tip_distance + 1) ~ mainland_island +
                        (1|year_collected) +
                               (1|ID),
                             data=meri_tip_distance,REML=F)

meri_tip_distance_y3 <- lmer(sqrt(tip_distance) ~ mainland_island +
                               (1|year_collected) +
                               (1|ID),
                             data=meri_tip_distance,REML=F)


# ANOVA type II test
# Anova(meri_tip_distance_y1)

# Model Diagnostics
# Residual distributions
# hist(resid(meri_spine_length_y1), breaks = 20)
# Spine tip distance distribution
# hist(meri_tip_distance$tip_distance, breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_tip_distance_y1))
# diagnostic(resid(meri_tip_distance_y2))
# diagnostic(resid(meri_tip_distance_y3))

# # DHARMa
# testResiduals(meri_tip_distance_y1)
# testResiduals(meri_tip_distance_y2)
# testResiduals(meri_tip_distance_y3)


##### Removed Zero Tip distance ####
# Then, I tried the models again. This time, removing the mericarps with
# spine tip distance of 0.
# This model did not converged well. There are some outliers.
# Raw data looks the best
meri_tip_distance_y4 <- lmer(tip_distance ~ mainland_island +
                               (1|year_collected) +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero,REML=F)

meri_tip_distance_y5 <- lmer(log(tip_distance + 1) ~ mainland_island +
                                     (1|year_collected) +
                               (1|ID),
                             data=meri_tip_distance_wozero,REML=F)

meri_tip_distance_y6 <- lmer(sqrt(tip_distance) ~ mainland_island +
                                     (1|year_collected) +
                               (1|ID),
                             data=meri_tip_distance_wozero,REML=F)


# ANOVA type II test
# Anova(meri_tip_distance_y4)

# Model Diagnostics
# diagnostic(resid(meri_tip_distance_y4))
# diagnostic(resid(meri_tip_distance_y5))
# diagnostic(resid(meri_tip_distance_y6))

# The residual distribution showed some outliers. 
# Check residual distributions after removing mericarps without spines.
# I included the residual column into the dataset
meri_tip_distance_wozero_y$residuals <- resid(meri_tip_distance_y4)
# hist(resid(meri_tip_distance_y4), breaks = 20)
# hist(meri_tip_distance_wozero$tip_distance, breaks = 20)

# # DHARMa
# testResiduals(meri_tip_distance_y1)
# testResiduals(meri_tip_distance_y2)
# testResiduals(meri_tip_distance_y3)


# Based on the residual distributions and the trait distributions I filter the data

##### Filter residuals (used in analysis) ####
# I filter residuals that are lower than -5 and larger than 5.
meri_tip_distance_wozero_filter_y <- filter(meri_tip_distance_wozero_y,
                                          !residuals < -5)
meri_tip_distance_wozero_filter_y <- filter(meri_tip_distance_wozero_filter_y,
                                          !residuals > 5)

# This removes specimen no.383

# Create a new filtered dataset for spine tip distance
# hist(meri_tip_distance_wozero_filter_y$residuals, breaks = 20)
meri_tip_distance_wozero_filter_y <- filter(meri_tip_distance_wozero_filter_y, !is.na(residuals))

# Ran the models with this filter data and raw data works best
###### Raw data ####
meri_tip_distance_y7 <- lmer(tip_distance ~ mainland_island +
                               (1|year_collected) +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_filter,REML=F)
###### Log transformed data ####
meri_tip_distance_y8 <- lmer(log(tip_distance) ~ mainland_island +
                               (1|year_collected) +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_filter,REML=F)
###### Squared-root data ####
meri_tip_distance_y9 <- lmer(sqrt(tip_distance) ~ mainland_island +
                               (1|year_collected) +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_filter,REML=F)


# Diagnostic
# diagnostic(resid(meri_tip_distance_y7))
# diagnostic(resid(meri_tip_distance_y8))
# diagnostic(resid(meri_tip_distance_y9))

#Anova
# Anova(meri_tip_distance_y7)

# DHARMa
# testResiduals(meri_tip_distance_y7)

## Emmeans estimates: Spine tip distance ####
# Zero filter data
EM_tip_dist_year <- emmeans(meri_tip_distance_y7, ~ mainland_island)
### Emmeans plot: Spine tip distance ####
plot(EM_tip_dist_year, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist_year)
### Percentage difference ####
((9/8.43 - 1)*100) # Mericarps ~ 6.66% more separated on islands

## Lower spines ####
# I tried to fit a glm
# For lower spines I used a glmm model and removed year as a covariate.
# 

meri_lower_spines_y1_glmm <- glmmTMB(factor(lower_spines) ~ mainland_island +
                                        (1|year_collected) +
                                       (1|ID),
                                     data = meri_lower_spines,
                                     family = binomial)
# str(meri_lower_spines)
### ANOVA Type II test ####
# Anova(meri_lower_spines_y1_glmm)

### Model Diagnostics ####
# # Residual histograms
#diagnostic(resid(meri_lower_spines_y1_glmm))

## Emmeans estimates: Lower spines ####
#Glmm
EM_lower_year <- emmeans(meri_lower_spines_y1_glmm, ~ mainland_island, type = "response")
### Emmeans plot: Lower spines ####
plot(EM_lower_year, comparisons = T) + labs(title = "Mericarp Lower Spines")
pwpp(EM_lower_year)
### Percentage differences ####
((0.407/0.998 - 1)*100)

# 04_02 Flower dataset ####
## Petal length ####
# The raw data works best
### Raw data ####
flower_y1 <- lmer(petal_length ~ mainland_island +
                    (1|year_collected) +
                    (1|ID),
                  data=flower,
                  na.action = na.exclude,
                  REML=F)
### Log transformed ####
flower_y2 <- lmer(log(petal_length) ~ mainland_island +
                          (1|year_collected) +
                    (1|ID),
                  data=flower,
                  REML=F)
### Square-root transformed ####
flower_y3 <- lmer(sqrt(petal_length) ~ mainland_island +
                          (1|year_collected) +
                    (1|ID),
                  data=flower,
                  REML=F)

### ANOVA type II test ####
# Anova(flower_y1)

### Model diagnostics ####
# hist(flower$petal_length, breaks = 20)
# The distribution of petal length have some outliers.

# Create a column with model residuals to filter the dataset
flower$residuals <- resid(flower_y1) 
hist(resid(flower_y1), breaks = 20)
# Filter residuals outside -5 and 5
flower_filter_year <- filter(flower, !is.na(residuals))
flower_filter_year <- filter(flower_filter_year, !residuals >=5)
flower_filter_year <- filter(flower_filter_year, !residuals <= -5)
hist(flower_filter_year$residuals, breaks = 20)

# This filter removed specimens 240, 351, 320 (<-5) and
# 454, 319, 207, 249, 340 (>5)

# Filtered model
# Raw data works best in the filter model
flower_y4 <- lmer(petal_length ~ mainland_island +
                          (1|year_collected) +
                    (1|ID),
                  data=flower_filter,
                  na.action = na.exclude,
                  REML=F)

flower_y5 <- lmer(log(petal_length) ~ mainland_island +
                          (1|year_collected) +
                    (1|ID),
                  data=flower_filter,
                  na.action = na.exclude,
                  REML=F)

flower_y6 <- lmer(sqrt(petal_length) ~ mainland_island +
                          (1|year_collected) +
                    (1|ID),
                  data=flower_filter,
                  na.action = na.exclude,
                  REML=F)

# #type III test using lmertest
# Anova(flower_y4)

# # Residual histograms
# diagnostic(resid(flower_y1))
# diagnostic(resid(flower_y2))
# diagnostic(resid(flower_y3))
#diagnostic(resid(flower_y4))
# diagnostic(resid(flower_y5))
# diagnostic(resid(flower_y6))

# # DHARMa
# testResiduals(flower_y1)
# testResiduals(flower_y4)
# testResiduals(flower_y3)
#testResiduals(flower_y4)

## Emmean estimates: Petal length ####
EM_flower_year <- emmeans(flower_y4, ~ mainland_island)
### Emmean plot: Petal length ####
plot(EM_flower_year, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower_year)
### Percentange difference ####
((16.3/16.7 - 1)*100) # Flowers are -2% smaller on islands

# 04_03 Mainland-Galapagos analysis for mericarps ####
# This analysis is for the Galapagos - mainland comparisons

# Model 1: Mainland - Galapagos ####
## Length ####
# For length, the untransformed data seems the best
### Raw data ####
meri_mainland_gal_length_y1<- lmer(length ~ mainland_island +
                                           (1|year_collected) +
                                     (1|ID),
                                   data = meri_length_mainland_gal,
                                   REML = F)
### Log transformed data ####
meri_mainland_gal_length_y2<- lmer(log(length) ~ mainland_island +
                                           (1|year_collected) +
                                     (1|ID),
                                   data = meri_length_mainland_gal,
                                   REML = F)
### Root-squared data ####
meri_mainland_gal_length_y3<- lmer(sqrt(length) ~ mainland_island +
                                           (1|year_collected) +
                                     (1|ID),
                                   data = meri_length_mainland_gal,
                                   REML = F)
### ANOVA type II test ####
Anova(meri_mainland_gal_length_y1)
#summary(meri_length_y1)

### Model Diagnostics ####
# Diagnostic custom function
#par(mfrow = c(1, 3))
# diagnostic(resid(meri_mainland_gal_length_y1))
# diagnostic(resid(meri_mainland_gal_length_y2))
# diagnostic(resid(meri_mainland_gal_length_y3))

# Diagnostics with DHARMA
# testResiduals(meri_mainland_gal_length_m1)
# testResiduals(meri_mainland_gal_length_m2)
# testResiduals(meri_mainland_gal_length_m3)

# After selecting the data transformation that converged the most I decided to
# only show the output of that model.

## Emmean estimates: Length ####
EM_length_mainland_gal_year <- emmeans(meri_mainland_gal_length_y1, ~ mainland_island)
### Emmean plot: Length ####
plot(EM_length_mainland_gal_year, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length_mainland_gal_year)
### Percentage difference #####
# ((island mean/mainland mean)-1)*100%
((6.15/5.75 - 1)* 100) # Mericarps ~ 7% longer on islands

## Width ####
# For width, log transformed seemed the best
### Raw data ####
meri_width_mainland_gal_y1 <- lmer(width ~ mainland_island +
                                     (1|year_collected) +
                                     (1|ID),
                                   data = meri_width_mainland_gal, 
                                   REML = F)
### Log transformed ####
meri_width_mainland_gal_y2 <- lmer(log(width) ~ mainland_island +
                                           (1|year_collected) +
                                     (1|ID),
                                   data = meri_width_mainland_gal, 
                                   REML = F)
### Squared-root transformed ####
meri_width_mainland_gal_y3 <- lmer(sqrt(width) ~ mainland_island +
                                           (1|year_collected) +
                                     (1|ID),
                                   data = meri_width_mainland_gal, 
                                   REML = F)
### ANOVA type II test ####
#Anova(meri_width_mainland_gal_y2)

### Model Diagnostics ####
# Residual histograms
# diagnostic(resid(meri_width_mainland_gal_y1))
# diagnostic(resid(meri_width_mainland_gal_y2))
# diagnostic(resid(meri_width_mainland_gal_y3))

# # DHARMa
# testResiduals(meri_width_mainland_gal_y1)
# testResiduals(meri_width_mainland_gal_y2)
# testResiduals(meri_width_mainland_gal_y3)

## Emmean estimates: Width ####
# Log transformed
EM_width_mainland_gal_year <- emmeans(meri_width_mainland_gal_y2, ~ mainland_island, type = "response")
### Emmean plot: Width ####
plot(EM_width_mainland_gal_year, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width_mainland_gal_year)
### Percentage difference ####
((3.12/2.95 - 1)*100) # Mericarps ~ 5.76% wider on islands

## Depth ####
# For depth untransformed data seems to work the best
### Raw data ####
meri_depth_mainland_gal_y1 <- lmer(depth ~ mainland_island +
                                     (1|year_collected) +
                                     (1|ID),
                                   data=meri_depth_mainland_gal, 
                                   REML = F)
### Log transformed ####
meri_depth_mainland_gal_y2 <- lmer(log(depth) ~ mainland_island +
                                           (1|year_collected) +
                                     (1|ID),
                                   data=meri_depth_mainland_gal, 
                                   REML = F)
### Square-root transformed ####
meri_depth_mainland_gal_y3 <- lmer(sqrt(depth) ~ mainland_island +
                                           (1|year_collected) +
                                     (1|ID),
                                   data=meri_depth_mainland_gal, 
                                   REML = F)
### ANOVA type II test ####
# Anova(meri_depth_mainland_gal_y1)

### Model Diagnostics ####
# hist(resid(meri_depth_mainland_gal_y1), breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_depth_mainland_gal_y1))
# diagnostic(resid(meri_depth_m2))
# diagnostic(resid(meri_depth_m3))
# 
# # DHARMa
# testResiduals(meri_depth_mainland_gal_y1)
# testResiduals(meri_depth_mainland_gal_y2)
# testResiduals(meri_depth_mainland_gal_y3)

## Emmeans: Depth ####
EM_depth_mainland_gal_year <- emmeans(meri_depth_mainland_gal_y1, ~ mainland_island)
### Emmeans plot: Depth #####
plot(EM_depth_mainland_gal_year, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth_mainland_gal_year)
### Porcentage difference ####
((4.78/4.26 - 1)*100) # Mericarps ~ 12% deeper on islands

## Spine tip distance ####
# Raw data looks the best
meri_tip_distance_mainland_galapagos_y4 <- lmer(tip_distance ~ mainland_island +
                                                  (1|year_collected) +
                                                  (1|ID),
                                                na.action = na.exclude,
                                                data=meri_tip_distance_wozero_mainland_gal,REML=F)

meri_tip_distance_mainland_galapagos_y5 <- lmer(log(tip_distance + 1) ~ mainland_island +
                                                        (1|year_collected) +
                                                  (1|ID),
                                                data=meri_tip_distance_wozero_mainland_gal,REML=F)

meri_tip_distance_mainland_galapagos_y6 <- lmer(sqrt(tip_distance) ~ mainland_island +
                                                        (1|year_collected) +
                                                  (1|ID),
                                                data=meri_tip_distance_wozero_mainland_gal,REML=F)


# # type III test
# Anova(meri_tip_distance_mainland_galapagos_y4)
# Anova(meri_tip_distance_mainland_galapagos_y5)
# Anova(meri_tip_distance_mainland_galapagos_y6)


# Diagnostic
# diagnostic(resid(meri_tip_distance_mainland_galapagos_y4))
# diagnostic(resid(meri_tip_distance_mainland_galapagos_y5))
# diagnostic(resid(meri_tip_distance_mainland_galapagos_y6))

# # DHARMa
# testResiduals(meri_tip_distance_mainland_galapagos_y4)
# testResiduals(meri_tip_distance_mainland_galapagos_y5)
# testResiduals(meri_tip_distance_mainland_galapagos_y6)

# Check residual distributions after removing mericarps without spines.
meri_tip_distance_wozero_mainland_gal$residuals <- resid(meri_tip_distance_mainland_galapagos_y4)
hist((meri_tip_distance_wozero_mainland_gal_filter$residuals), breaks = 20)

# Filter residuals that are lower than 10
meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal,
                                                       !residuals < -5)
meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal_filter,
                                                       !residuals > 5)


meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal_filter, !is.na(residuals))


### Filter Tip distance mainland gal ####
# Raw data works best
#### Raw data ####
meri_tip_distance_mainland_gal_y7 <- lmer(tip_distance ~ mainland_island +
                                            (1|year_collected) +
                                            (1|ID),
                                          na.action = na.exclude,
                                          data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)
#### Log transformed data ####
meri_tip_distance_mainland_gal_y8 <- lmer(log(tip_distance) ~ mainland_island +
                                                  (1|year_collected) +
                                            (1|ID),
                                          na.action = na.exclude,
                                          data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)
#### Square root data ####
meri_tip_distance_mainland_gal_y9 <- lmer(sqrt(tip_distance) ~ mainland_island +
                                                  (1|year_collected) +
                                            (1|ID),
                                          na.action = na.exclude,
                                          data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)




#### ANOVA Type II test ####
Anova(meri_tip_distance_mainland_gal_y7)

#### Model Diagnostics ####
# diagnostic(resid(meri_tip_distance_mainland_gal_y7))
# diagnostic(resid(meri_tip_distance_mainland_gal_y8))
# diagnostic(resid(meri_tip_distance_mainland_gal_y9))

# DHARMa
# testResiduals(meri_tip_distance_mainland_gal_y7)

## Emmeans estimates: Spine tip distance ####
# Zero filter data
EM_tip_dist_mainland_gal_year <- emmeans(meri_tip_distance_mainland_gal_y7, ~ mainland_island)
### Emmean plot: Spine tip distance ####
plot(EM_tip_dist_mainland_gal_year, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist_mainland_gal_year)
### Percentage difference ####
((9/8.43 - 1)*100) # Mericarps ~ 6.66% more separated on islands

## Lower spines ####
meri_lower_spines_mainland_gal_glmm_year <- glmmTMB(factor(lower_spines) ~ mainland_island +
                                                 (1|year_collected) +
                                                            (1|ID),
                                               data = meri_lower_spines_mainland_gal,
                                               family = binomial)
### ANOVA Type II test ####
# Anova(meri_lower_spines_mainland_gal_glmm_year)

## Emmeans estimates: Lower Spines ####
#Glmm
EM_lower_mainland_gal_year <- emmeans(meri_lower_spines_mainland_gal_glmm_year, ~ mainland_island, type = "response")
### Emmean plot: Lower spines ####
plot(EM_lower_mainland_gal_year, comparisons = T) + labs(title = "Mericarp Lower Spines")
pwpp(EM_lower_mainland_gal_year)
### Percentage difference ####
((0.407/0.998 - 1)*100)


# Model 2: Galapagos - Other Islands ####
# Is there an effect between tribulus in galapagos compared to other island systems
# This comparison is possible with flower and leaf datasets
# 04_04 Flower dataset ####
## Petal length data ####
# I used the flower filter data from before
# The squared data seems to work best
### Raw data ####
flower_y7 <- lmer(petal_length ~ galapagos_other +
                    (1|year_collected) +
                    (1|ID),
                  data=flower_filter,
                  REML=F)
### Log transformed ####
flower_y8 <- lmer(log(petal_length) ~ galapagos_other +
                          (1|year_collected) +
                    (1|ID),
                  data=flower_filter,
                  REML=F)
### Square root transformed ####
flower_y9 <- lmer(sqrt(petal_length) ~ galapagos_other +
                          (1|year_collected) +
                    (1|ID),
                  data=flower_filter,
                  REML=F)

### ANOVA type II test ####
# Anova(flower_m9)

### Model Diagnostics ####
# # Residual histograms
# diagnostic(resid(flower_y7))
# diagnostic(resid(flower_y8))
#diagnostic(resid(flower_y9))
# 
# # DHARMa
# testResiduals(flower_y7)
# testResiduals(flower_y8)
# testResiduals(flower_y9)

## Emmean estimates: Petal length ####
EM_flower2_year <- emmeans(flower_y9, ~ galapagos_other, type = "response")
### Emmean plot: Petal length ####
plot(EM_flower2_year, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower2_year)
### Percentage difference ####
((9.18/17.04 - 1)*100) #FLowers in Galapagos are 46% shorter than other islands
















