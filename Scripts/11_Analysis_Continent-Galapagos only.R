# # 04_02 Mainland-Galapagos analysis for mericarps ####
# # This analysis is for the Galapagos - mainland comparisons
# 
# # 04_02_01 Model 1: Mainland - Galapagos ####
# ## Length ####
# # For length, the untransformed data seems the best
# ### Raw data ####
# meri_mainland_gal_length_m1<- lmer(length ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data = meri_length_mainland_gal,
#                       REML = F)
# ### Log transformed data ####
# meri_mainland_gal_length_m2<- lmer(log(length) ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data = meri_length_mainland_gal,
#                       REML = F)
# ### Root-squared data ####
# meri_mainland_gal_length_m3<- lmer(sqrt(length) ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data = meri_length_mainland_gal,
#                       REML = F)
# ### ANOVA type II test ####
# Anova(meri_mainland_gal_length_m1)
# #ANOVA_length_m1 <-as.data.frame(Anova(meri_length_m1))
# #summary(meri_length_m1)
# 
# ### Model Diagnostics ####
# # Diagnostic custom function
# #par(mfrow = c(1, 3))
# # diagnostic(resid(meri_mainland_gal_length_m1))
# # diagnostic(resid(meri_mainland_gal_length_m2))
# # diagnostic(resid(meri_mainland_gal_length_m3))
# 
# # Diagnostics with DHARMA
# # testResiduals(meri_mainland_gal_length_m1)
# # testResiduals(meri_mainland_gal_length_m2)
# # testResiduals(meri_mainland_gal_length_m3)
# 
# # After selecting the data transformation that converged the most I decided to
# # only show the output of that model.
# 
# ## Emmean estimates: Length ####
# EM_length_mainland_gal <- emmeans(meri_mainland_gal_length_m1, ~ mainland_island)
# ### Emmean plot: Length ####
# plot(EM_length_mainland_gal, comparisons = TRUE) + labs(title = "Mericarp Length")
# pwpp(EM_length_mainland_gal)
# ### Percentage difference #####
# # ((island mean/mainland mean)-1)*100%
# ((6.15/5.75 - 1)* 100) # Mericarps ~ 7% longer on islands
# 
# ## Width ####
# # For width, log transformed seemed the best
# ### Raw data ####
# meri_width_mainland_gal_m1 <- lmer(width ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data = meri_width_mainland_gal, 
#                       REML = F)
# ### Log transformed ####
# meri_width_mainland_gal_m2 <- lmer(log(width) ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data = meri_width_mainland_gal, 
#                       REML = F)
# ### Squared-root transformed ####
# meri_width_mainland_gal_m3 <- lmer(sqrt(width) ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data = meri_width_mainland_gal, 
#                       REML = F)
# ### ANOVA type II test ####
# #Anova(meri_width_mainland_gal_m2)
# 
# ### Model Diagnostics ####
# # Residual histograms
# # diagnostic(resid(meri_width_mainland_gal_m1))
# # diagnostic(resid(meri_width_mainland_gal_m2))
# # diagnostic(resid(meri_width_mainland_gal_m3))
# 
# # # DHARMa
# # testResiduals(meri_width_mainland_gal_m1)
# # testResiduals(meri_width_mainland_gal_m2)
# # testResiduals(meri_width_mainland_gal_m3)
# 
# ## Emmean estimates: Width ####
# # Log transformed
# EM_width_mainland_gal <- emmeans(meri_width_mainland_gal_m2, ~ mainland_island, type = "response")
# ### Emmean plot: Width ####
# plot(EM_width_mainland_gal, comparisons = T) + labs(title = "Mericarp Width")
# pwpp(EM_width_mainland_gal)
# ### Percentage difference ####
# ((3.12/2.95 - 1)*100) # Mericarps ~ 5.76% wider on islands
# 
# ## Depth ####
# # For depth untransformed data seems to work the best
# ### Raw data ####
# meri_depth_mainland_gal_m1 <- lmer(depth ~ mainland_island +
#                     year_collected +
#                     (1|ID),
#                     data=meri_depth_mainland_gal, 
#                     REML = F)
# ### Log transformed ####
# meri_depth_mainland_gal_m2 <- lmer(log(depth) ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data=meri_depth_mainland_gal, 
#                       REML = F)
# ### Square-root transformed ####
# meri_depth_mainland_gal_m3 <- lmer(sqrt(depth) ~ mainland_island +
#                         year_collected +
#                         (1|ID),
#                       data=meri_depth_mainland_gal, 
#                       REML = F)
# ### ANOVA type II test ####
# # Anova(meri_depth_mainland_gal_m1)
# 
# ### Model Diagnostics ####
# # hist(resid(meri_depth_mainland_gal_m1), breaks = 20)
# 
# # # Residual histograms
# # diagnostic(resid(meri_depth_mainland_gal_m1))
# # diagnostic(resid(meri_depth_m2))
# # diagnostic(resid(meri_depth_m3))
# # 
# # # DHARMa
# # testResiduals(meri_depth_mainland_gal_m1)
# # testResiduals(meri_depth_mainland_gal_m2)
# # testResiduals(meri_depth_mainland_gal_m3)
# 
# ## Emmeans: Depth ####
# EM_depth_mainland_gal <- emmeans(meri_depth_mainland_gal_m1, ~ mainland_island)
# ### Emmeans plot: Depth #####
# plot(EM_depth_mainland_gal, comparisons = T) + labs(title = "Mericarp Depth")
# pwpp(EM_depth_mainland_gal)
# ### Porcentage difference ####
# ((4.78/4.26 - 1)*100) # Mericarps ~ 12% deeper on islands
# 
# ## Spine tip distance ####
# # Raw data looks the best
# meri_tip_distance_mainland_galapagos_m4 <- lmer(tip_distance ~ mainland_island +
#                                                         year_collected +
#                                                         (1|ID),
#                                                 na.action = na.exclude,
#                                                 data=meri_tip_distance_wozero_mainland_gal,REML=F)
# 
# meri_tip_distance_mainland_galapagos_m5 <- lmer(log(tip_distance + 1) ~ mainland_island +
#                                                         year_collected +
#                                                         (1|ID),
#                                                 data=meri_tip_distance_wozero_mainland_gal,REML=F)
# 
# meri_tip_distance_mainland_galapagos_m6 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                                         year_collected +
#                                                         (1|ID),
#                                                 data=meri_tip_distance_wozero_mainland_gal,REML=F)
# 
# 
# # # type III test
# # Anova(meri_tip_distance_mainland_galapagos_m4)
# # Anova(meri_tip_distance_mainland_galapagos_m5)
# # Anova(meri_tip_distance_mainland_galapagos_m6)
# 
# 
# # Diagnostic
# # diagnostic(resid(meri_tip_distance_mainland_galapagos_m4))
# # diagnostic(resid(meri_tip_distance_mainland_galapagos_m5))
# # diagnostic(resid(meri_tip_distance_mainland_galapagos_m6))
# 
# # # DHARMa
# # testResiduals(meri_tip_distance_mainland_galapagos_m4)
# # testResiduals(meri_tip_distance_mainland_galapagos_m5)
# # testResiduals(meri_tip_distance_mainland_galapagos_m6)
# 
# # Check residual distributions after removing mericarps without spines.
# meri_tip_distance_wozero_mainland_gal$residuals <- resid(meri_tip_distance_mainland_galapagos_m4)
# 
# # Filter residuals that are lower than 10
# meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal,
#                                                        !residuals < -5)
# meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal_filter,
#                                                        !residuals > 5)
# 
# 
# meri_tip_distance_wozero_mainland_gal_filter <- filter(meri_tip_distance_wozero_mainland_gal_filter, !is.na(residuals))
# 
# hist((meri_tip_distance_wozero_mainland_gal_filter$residuals), breaks = 20)
# 
# ### Filter Tip distance mainland gal ####
# # Raw data works best
# #### Raw data ####
# meri_tip_distance_mainland_gal_m7 <- lmer(tip_distance ~ mainland_island +
#                                year_collected +
#                                (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)
# #### Log transformed data ####
# meri_tip_distance_mainland_gal_m8 <- lmer(log(tip_distance) ~ mainland_island +
#                                year_collected +
#                                (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)
# #### Square root data ####
# meri_tip_distance_mainland_gal_m9 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                year_collected +
#                                (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance_wozero_mainland_gal_filter,REML=F)
# 
# 
# 
# 
# #### ANOVA Type II test ####
# Anova(meri_tip_distance_mainland_gal_m7)
# 
# #### Model Diagnostics ####
# # diagnostic(resid(meri_tip_distance_mainland_gal_m7))
# # diagnostic(resid(meri_tip_distance_mainland_gal_m8))
# # diagnostic(resid(meri_tip_distance_mainland_gal_m9))
# 
# # DHARMa
# # testResiduals(meri_tip_distance_mainland_gal_m7)
# 
# ## Emmeans estimates: Spine tip distance ####
# # Zero filter data
# EM_tip_dist_mainland_gal <- emmeans(meri_tip_distance_mainland_gal_m7, ~ mainland_island)
# ### Emmean plot: Spine tip distance ####
# plot(EM_tip_dist_mainland_gal, comparisons = T) + labs(title = "Mericarp Tip distance")
# pwpp(EM_tip_dist_mainland_gal)
# ### Percentage difference ####
# ((9/8.43 - 1)*100) # Mericarps ~ 6.66% more separated on islands
# 
# ## Lower spines ####
# meri_lower_spines_mainland_gal_glmm <- glmmTMB(factor(lower_spines) ~ mainland_island +
#                                                        (1|ID),
#                                                data = meri_lower_spines_mainland_gal,
#                                                family = binomial)
# ### ANOVA Type II test ####
# # Anova(meri_lower_spines_mainland_gal_glmm)
# 
# ## Emmeans estimates: Lower Spines ####
# #Glmm
# EM_lower_mainland_gal <- emmeans(meri_lower_spines_mainland_gal_glmm, ~ mainland_island, type = "response")
# ### Emmean plot: Lower spines ####
# plot(EM_lower_mainland_gal, comparisons = T) + labs(title = "Mericarp Lower Spines")
# pwpp(EM_lower_mainland_gal)
# ### Percentage difference ####
# ((0.407/0.998 - 1)*100)
