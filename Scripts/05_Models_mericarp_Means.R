# Script 19. Mericarp means loading and models per trait ####
# By: Daniel Reyes Corral

# This script is used to load the mericarp mean dataset. I will start with mericarp ind
# Then separate the dataset into individual traits, and use that for the linear models
# Then use emmeans to compare model results and test the same questions as the original analysis

# 19_01 Load the dataset mericarp ind. ####
# See script 02 for definition
# The main caveat of this dataset is that I removed lower spines I could use proportion
# but decided to test the other variables.
# Mericarp ind is the datset that uses the means for Galapagos and Florida with
# individual data from the other locations that have less samples.

mericarp_ind <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/mericarp_ind.csv")
mericarp_ind <- mericarp_ind %>% mutate_at(vars(ID,
                                        Herbarium,
                                        continent,
                                        country,
                                        island_group,
                                        mainland_island,
                                        galapagos_other,
                                        island_group,
                                        galapagos_island,
                                        finch_beak,
                                        ), list(factor))
str(mericarp_ind)

# 19_02 Trait datasets #####
# Separate data per trait filter NAs #

### Length ####
meri_length_ind <- dplyr::select(mericarp_ind, ID:location, length, Temp:varP)
meri_length_ind <- filter(meri_length_ind, !is.na(length))
meri_length_ind <- filter(meri_length_ind, !is.na(Temp_S))

###  Width ####
meri_width_ind <- dplyr::select(mericarp_ind, ID:location, width, Temp:varP)
meri_width_ind <- filter(meri_width_ind, !is.na(width))
meri_width_ind <- filter(meri_width_ind, !is.na(Temp_S))

### Depth ####
meri_depth_ind <- dplyr::select(mericarp_ind, ID:location, depth, Temp:varP)
meri_depth_ind <- filter(meri_depth_ind, !is.na(depth))
meri_depth_ind <- filter(meri_depth_ind, !is.na(Temp_S))

### Spine tip distance ####
meri_tip_distance_ind <- dplyr::select(mericarp_ind, ID:location, tip_distance, Temp:varP) #Has zeroes in the data
meri_tip_distance_ind <- filter(meri_tip_distance_ind, !is.na(tip_distance))
meri_tip_distance_ind <- filter(meri_tip_distance_ind, !is.na(Temp_S))
### Spine tip distance without zero #####
# We removed mericarps without upper spines from analysis.These mericarps had a tip distance of 0.
meri_tip_distance0_ind <- dplyr::filter(meri_tip_distance_ind, !tip_distance == 0)

# 19_03 Models per trait ####
# Similar to the original analysis I will use linear models per trait
# to see the effect of the trait on mainland and island populations
# year and bioclim variables are covariates, ID is still a random effect

## Length ####
#### Raw data ####
ind_length_m1 <- lmer(length ~ mainland_island +
                        year_collected + 
                        Herbarium +
                        Temp +
                        Temp_S +
                        Prec +
                        varP +
                        (1|ID),
                      data = meri_length_ind,
                      REML = F)
### Anova (car package) ####
Anova(ind_length_m1)

# The results are differnt this time, mainland island is barely significant
# Temp_S is significant, year too.

### Model diagnostics (DHARMA) ####
#testResiduals(ind_length_m1)

## Emmeans estimates: Length ####
EM_length_ind <- emmeans(ind_length_m1, ~ mainland_island)

### Emmean plot: Length ####
plot(EM_length_ind, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length_ind)

## Width ####
# For width, squared transformed seemed the best
#### Raw data ####
# ind_width_m1 <- lmer(width ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data = meri_width_ind, 
#                       REML = F)
#### Log transformed ####
# ind_width_m2 <- lmer(log(width) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data = meri_width_ind, 
#                       REML = F)
#### Square-root transformed ####
ind_width_m3 <- lmer(sqrt(width) ~ mainland_island +
                        year_collected +
                       Herbarium +
                        Temp +
                        Temp_S +
                        Prec +
                        varP +
                        (1|ID),
                      data = meri_width_ind, 
                      REML = F)
### Anova (car package) ####
Anova(ind_width_m3)

# Mainland_island is not significant
# year and varP are significant

 ### Model diagnostics (DHARMA) ####
# testResiduals(ind_width_m1)
# testResiduals(ind_width_m2)
# testResiduals(ind_width_m3)
# 
# par(mfrow = c(1, 3))
#  diagnostic(resid(ind_width_m1))
#  diagnostic(resid(ind_width_m2))
#  diagnostic(resid(ind_width_m3))
#  
#  hist(resid(ind_width_m3), breaks = 20)

## Emmeans estimates: Width ####
EM_width_ind <- emmeans(ind_width_m3, ~ mainland_island)

### Emmean plot: Width ####
plot(EM_width_ind, comparisons = TRUE) + labs(title = "Mericarp Width")
pwpp(EM_width_ind)

## Depth ####
#### Raw data ####
ind_depth_m1 <- lmer(depth ~ mainland_island +
                        year_collected +
                       Herbarium +
                        Temp +
                        Temp_S +
                        Prec +
                        varP +
                        (1|ID),
                      data=meri_depth_ind, 
                      REML = F)
#### Log transformed data ####
# ind_depth_m2 <- lmer(log(depth) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data=meri_depth_ind, 
#                       REML = F)

#### Square-root transformed data ####
# ind_depth_m3 <- lmer(sqrt(depth) ~ mainland_island +
#                         year_collected +
#                         Temp +
#                         Temp_S +
#                         Prec +
#                         varP +
#                         (1|ID),
#                       data=meri_depth_ind, 
#                       REML = F)

### Anova (car package) ####
Anova(ind_depth_m1)

# The effect of mainland/island is significant
# year and varP too

# #### Model Diagnostics ####
# hist(resid(ind_depth_m1), breaks = 20)
# hist(resid(ind_depth_m2), breaks = 20)
# hist(resid(ind_depth_m3), breaks = 20)
# 
# # # Residual histograms
# diagnostic(resid(ind_depth_m1))
# diagnostic(resid(ind_depth_m2))
# diagnostic(resid(ind_depth_m3))
# 
# 
# 
# # # DHARMa
# testResiduals(ind_depth_m1)
# testResiduals(ind_depth_m2)
# testResiduals(ind_depth_m3)

## Emmeans estimates: Depth ####
EM_depth_ind <- emmeans(ind_depth_m1, ~ mainland_island)
### Emmeans plot: Depth ####
plot(EM_depth_ind, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth_ind)

## Tip distance ####
# Filter tip distance without zero
###### Raw data ####
ind_tip_distance_m1 <- lmer(tip_distance ~ mainland_island +
                               year_collected +
                              Herbarium +
                               Temp +
                               Temp_S +
                               Prec +
                               varP +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance0_ind,REML=F)
###### Log transformed data ####
# ind_tip_distance_m2 <- lmer(log(tip_distance) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance0_ind,REML=F)
###### Squared-root data ####
# ind_tip_distance_m3 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                year_collected +
#                                Temp +
#                                Temp_S +
#                                Prec +
#                                varP +
#                                (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance0_ind,REML=F)


# Diagnostic
# diagnostic(resid(ind_tip_distance_m1))
# diagnostic(resid(ind_tip_distance_m2))
# diagnostic(resid(ind_tip_distance_m3))
# 
# hist(resid(ind_tip_distance_m3), breaks = 20)

### Anova (car package) ####
Anova(ind_tip_distance_m1)

# Tip distance is not significant for any variable.

### Model diagnostics DHARMa ####
# testResiduals(ind_tip_distance_m1)
# testResiduals(ind_tip_distance_m2)
# testResiduals(ind_tip_distance_m3)

## Emmeans estimates: Spine tip distance ####
# Zero filter data
EM_tip_dist_ind <- emmeans(ind_tip_distance_m1, ~ mainland_island)
### Emmeans plot: Spine tip distance ####
plot(EM_tip_dist_ind, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist_ind)

# PLOTS ####
### Length ####
#### Model plot ####
EM_length_ind
plot_length <- plot(EM_length_ind, comparisons = T, plotit = F)

ind_ggplot_length <- ggplot(plot_length, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) + 
  labs(title = expression(paste("Length (P = 0.06015)"))) +
  labs(x = "Population", y = "Mean Length (mm)")

### Width ####
#### Model plot ####
EM_width_ind
plot_width <- plot(EM_width_ind, comparisons = T, plotit = F)

ind_ggplot_width <- ggplot(plot_width, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = expression(paste("Width (P = 0.09625)"))) +
  labs(x = "Population", y = "Mean Width (mm)")


### Depth ####

#### Model plot ####
plot_depth <- plot(EM_depth_ind, comparisons = T, plotit = F)

ind_ggplot_depth <- ggplot(plot_depth, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) + 
  labs(title = expression(paste("Depth (P = 0.01462)"))) +
  labs(x = "Population", y = "Mean Depth (mm)")


### Tip distance ####

#### Model plot ####
plot_spine <- plot(EM_tip_dist_ind, comparisons = T, plotit = F)

ind_ggplot_spine <- ggplot(plot_spine, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = expression(paste("Tip distance (P = 0.68236)"))) +
  labs(x = "Population", y = "Spine Tip Distance (mm)")

## Individual traits figure ####
# Supplemental figure with individual mericarp traits. From the PCA.
figure_mericarp_ind_traits <- ggarrange(ind_ggplot_length,
                                        ind_ggplot_width,
                                        ind_ggplot_depth,
                                        ind_ggplot_spine,
                                        ggplot_PC1_mean,
                                        labels = c("A", "B", "C", "D", "E"),
                                        ncol = 3,
                                        nrow = 2) + 
  theme(text = element_text(family = "Noto Sans"))

