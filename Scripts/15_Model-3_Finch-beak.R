#Script 15: Model 3: finch beak ####

# What is the effect of finch community within the galapagos islands
## Mericarp data ####
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

## Flowers ####
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

## Leaves data ####

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

# Emmean estimates and plots ####
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

# Model 3: Finch communities within Galapagos ####
## Mericarps ####
### Length ####
EM_length
plot_length2 <- plot(EM_length3, comparisons = T, plotit = F)

ggplot_length2 <- ggplot(plot_length2, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Length
         
         (χ2 = 3.0175, P = 0.08237)
         ", x = "Absence        Presence", 
       y = "Mean Length (mm)") 

### Width ####
plot_width2 <- plot(EM_width3, comparisons = T, plotit = F)

ggplot_width2 <- ggplot(plot_width2, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Width
         
         (χ2 = 0.2936, P = 0.5879)
         ", x = "Absence        Presence", 
       y = "Mean Width (mm)") 
### Depth ####
plot_depth2 <- plot(EM_depth3, comparisons = T, plotit = F)

ggplot_depth2 <- ggplot(plot_depth2, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Depth
         
         (χ2 = 0.7951, P = 0.3726)
         ", x = "Absence        Presence", 
       y = "Mean Depth (mm)") 

### Tip distance ####
plot_spine2 <- plot(EM_tip_dist3, comparisons = T, plotit = F)

ggplot_spine2 <- ggplot(plot_spine2, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Tip Distance
         
         (χ2 = 1.3133, P = 0.2518)
         ", x = "Absence        Presence", 
       y = "Spine Tip Distance (mm)") 

### Lower spines ####
plot_lower2 <- plot(EM_lower3, comparisons = T, plotit = F)

ggplot_lower2 <- ggplot(plot_lower2, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Lower Spine
         
         (χ2 = 2.4872, P = 0.1148)
         ", x = "Absence        Presence", 
       y = "Lower spines") 

## Flower ####
plot_flower3 <- plot(EM_flower3, comparisons = T, plotit = F)

ggplot_flower3 <- ggplot(plot_flower3, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Petal Length
         
         (χ2 = 0.3459, P = 0.5564)
         ", x = "Absence        Presence", 
       y = "Petal Length (mm)") 

# Model 3: Finch Beak ####

## Mericarps ####

# Preparing the data summary by ID and mainland/island column
mericarp_summary_model3 <- mericarp_gal %>% group_by(ID, finch_beak) %>% summarize(mean_length = mean(length),
                                                                                   mean_width = mean(width),
                                                                                   mean_depth = mean(depth),
                                                                                   mean_tip_distance = mean(tip_distance),
                                                                                   mean_lower_spines = mean(lower_spines))
mericarp_summary_model3 <- mericarp_summary_model3 %>% column_to_rownames("ID")
mericarp_summary_traits_model3 <- dplyr::select(mericarp_summary_model3, !finch_beak)

### PCA ####
# leaves:
mericarp_pca_model3 <- prcomp(mericarp_summary_traits_model3, scale = T)

# Visualize eigenvalues (scree plot):
fviz_eig(mericarp_pca_model3)

# Graph of individuals. Individuals with similar profile are grouped together
fviz_pca_ind(mericarp_pca_model3, repel = T, geom = c("point"), habillage = mericarp_summary_model3$finch_beak, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
# Variables
fviz_pca_var(mericarp_pca_model3,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
#Biplot
fviz_pca_biplot(mericarp_pca_model3, repel = T,
                geom = c("point"),
                habillage = mericarp_summary_model3$finch_beak,
                col.var = "black",
                addEllipses = T
)


