# Leaf dataset and analysis Script 14 ##
# By: Daniel Reyes Corral
# Date: 12/08/2021

# 14_01 Leaf data ####
# Dataset preparation for Tribulus leaves. This cleans and prepares leaf data for models
# I separate each trait: leaf lenght, leaflet lenght and leaf number into individual datasets.

leaf <- read_csv("~/R/02. Thesis/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_leaves_data_plus CDRS herbarium_clean.csv")
leaf <- leaf %>% mutate_at(vars(ID,
                                herbarium,
                                continent,
                                country,
                                island_group,
                                mainland_island,
                                galapagos_other,
                                island_group,
                                galapagos_island,
                                finch_beak), list(factor))


str(leaf)

## Leaf length ####
leaf_length <- filter(leaf, !is.na(leaf_length))

## Leaflet length ####
leaflet_length <- filter(leaf, !is.na(leaflet_length))

## Galapagos only leaf length dataset ####
gal_leaf_length <- filter(leaf_length, galapagos_other == "Galapagos")

## Galapagos only leaflet length dataset ####
gal_leaflet_length <- filter(leaflet_length, galapagos_other == "Galapagos")

# 14_02 Model 1: mainland - island ####
## Leaf length ####
# square root transformed data works best
### Raw data ####
leaf_length_m1 <- lmer(leaf_length ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
### Log transformed data ####
leaf_length_m2 <- lmer(log(leaf_length) ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
### Square root data ####
leaf_length_m3 <- lmer(sqrt(leaf_length) ~ mainland_island +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
### ANOVA Type II test ####
# Anova(leaf_length_m3)
# 
### Model Diagnostics ####
# # Residual histograms
# diagnostic(resid(leaf_length_m1))
# diagnostic(resid(leaf_length_m2))
#diagnostic(resid(leaf_length_m3))
# 
# # DHARMa
# testResiduals(leaf_length_m1)
# testResiduals(leaf_length_m2)
# testResiduals(leaf_length_m3)

## Leaflet length ####
# Log transformed data works best
### Raw data ####
leaflet_length_m1 <- lmer(leaflet_length ~ mainland_island +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
### Log transformed ####
leaflet_length_m2 <- lmer(log(leaflet_length) ~ mainland_island +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
### Squared root transformed ####
leaflet_length_m3 <- lmer(sqrt(leaflet_length) ~ mainland_island +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
### ANOVA Type II test ####
# Anova(leaflet_length_m2)

### Model Diagnostics ####
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

## Leaflet number ####
leaflet_num_m5 <- glmmTMB(number_of_leaflets ~ mainland_island +
                            (1|ID), data = leaf_length,
                          family = nbinom1())

### ANOVA Type II test ####
# Anova(leaflet_num_m5)

### Model Diagnostics ####
# # Residual histograms
#diagnostic(resid(leaflet_num_m5))
# 
# # DHARMa
# testResiduals(leaflet_num_m1)

# Emmean estimates and plots ####
## Leaf length ####
EM_leaf <- emmeans(leaf_length_m3, ~ mainland_island, type = "response")
plot(EM_leaf, comparisons = T) + labs(title = "Leaf Length")
pwpp(EM_leaf)
((30.4/25.2 - 1)*100) # Leafs on islands are ~ 20% longer than mainland

## Leaflet length ####
EM_leaflet <- emmeans(leaflet_length_m2, ~ mainland_island, type = "response")
plot(EM_leaflet, comparisons = T) + labs(title = "Leaflet Length")
pwpp(EM_leaflet)
((8.23/7.65 - 1)*100) # Leaflets are ~ 7.5% larger on islands

## Leaflet Number ####
EM_leaflet_num <- emmeans(leaflet_num_m5, ~ mainland_island)
plot(EM_leaflet_num, comparisons = T) + labs(title = "Leaflet Number")
pwpp(EM_leaflet_num)
((14.2/12.8 - 1)*100) # Leaflet number are ~11% higher on islands

# 14_03 Model 2: Galapagos - Other Islands ####
## Leaf length ####
# Log transformed data works best
### Raw data ####
leaf_length_m4 <- lmer(leaf_length ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
### Log transformed ####
leaf_length_m5 <- lmer(log(leaf_length) ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
### Square-root transformed ####
leaf_length_m6 <- lmer(sqrt(leaf_length) ~ galapagos_other +
                         year_collected +
                         (1|ID),data=leaf_length,REML=F)
### ANOVA Type II test ####
# Anova(leaf_length_m5)

### Model Diagnostics ####
# # Residual histograms
# diagnostic(resid(leaf_length_m4))
#diagnostic(resid(leaf_length_m5))
# diagnostic(resid(leaf_length_m6))
# 
# # DHARMa
# testResiduals(leaf_length_m4)
# testResiduals(leaf_length_m5)
# testResiduals(leaf_length_m6)

## Leaflet length ####
# Log transformed data works best
### Raw dataset ####
leaflet_length_m4 <- lmer(leaflet_length ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
### Log Transformed ####
leaflet_length_m5 <- lmer(log(leaflet_length) ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
### Square-root transformed ####
leaflet_length_m6 <- lmer(sqrt(leaflet_length) ~ galapagos_other +
                            year_collected +
                            (1|ID),
                          data=leaflet_length,
                          REML = F)
### ANOVA Type II test ####
#Anova(leaflet_length_m5)
 
### Model Diagnostics ####
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

## Leaflet number ####
### Raw datasets ####
leaflet_num_m6 <- glm(number_of_leaflets ~ galapagos_other +
                        year_collected,
                      family = poisson,
                      data=leaf_length)
### Log transformed ####
leaflet_num_m7 <- glm(number_of_leaflets ~ galapagos_other +
                        year_collected,
                      family = gaussian,
                      data=leaf_length)
### Square-root transformed ####
leaflet_num_m8 <- glmmTMB(number_of_leaflets ~ galapagos_other +
                            (1|ID), data = leaf_length,
                          family = nbinom1())

### ANOVA type II test ####
# Anova(leaflet_num_m8)

# Diagnostic
# # Residual histograms
# diagnostic(resid(leaflet_num_m8))
# DHARMa
# testResiduals(leaflet_num_m2)

## Emmean estimates Model 2 ####
# Seems that in Galapagos the flowers and the leaves are smaller compared to other islands_

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

## Leaves ####
### Leaf length ####
plot_leaf <- plot(EM_leaf, comparisons = T, plotit = F)

ggplot_leaf <- ggplot(plot_leaf, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaf Length
         
         (χ2 = 21.86, P = <0.001)
         ", x = "Population", 
       y = "Leaf Length (mm)")
### Leaflet length ####
plot_leaflet <- plot(EM_leaflet, comparisons = T, plotit = F)

ggplot_leaflet <- ggplot(plot_leaflet, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaflet Length
         
         (χ2 = 4.86, P = 0.027)
         ", x = "Population", 
       y = "Leaflet Length (mm)")
### Leaflet number ####
plot_leaflet_num <- plot(EM_leaflet_num, comparisons = T, plotit = F)

ggplot_leaflet_num <- ggplot(plot_leaflet_num, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaflet Number
         
         (χ2 = 39.61, P = <0.001)
         ", x = "Population", 
       y = "Leaflet Number") 

## Leaf ####
### Leaf Length ####
plot_leaf2 <- plot(EM_leaf2, comparisons = T, plotit = F)

ggplot_leaf2 <- ggplot(plot_leaf2, aes(x = galapagos_other, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaf Length
         
         (χ2 = 20.39, P = <0.001)
         ", x = "Population", 
       y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet2 <- plot(EM_leaflet2, comparisons = T, plotit = F)

ggplot_leaflet2 <- ggplot(plot_leaflet2, aes(x = galapagos_other, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaflet Length
         
         (χ2 = 17.20, P = <0.001)
         ", x = "Population", 
       y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num2 <- plot(EM_leaflet_num2, comparisons = T, plotit = F)

ggplot_leaflet_num2 <- ggplot(plot_leaflet_num2, aes(x = galapagos_other, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaflet Number
         
         (χ2 = 1.8645, P = 0.1721)
         ", x = "Population", 
       y = "Leaflet Number")

## Leaves ####
### Leaf length ####
plot_leaf3 <- plot(EM_leaf3, comparisons = T, plotit = F)

ggplot_leaf3 <- ggplot(plot_leaf3, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaf Length
         
         (χ2 = 1.8787, P = 0.1704)
         ", x = "Absence        Presence", 
       y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet3 <- plot(EM_leaflet3, comparisons = T, plotit = F)

ggplot_leaflet3 <- ggplot(plot_leaflet3, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaflet Length
         
         (χ2 = 2.0280, P = 0.1544)
         ", x = "Absence        Presence", 
       y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num3 <- plot(EM_leaflet_num3, comparisons = T, plotit = F)

ggplot_leaflet_num3 <- ggplot(plot_leaflet_num3, aes(x = finch_beak, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) +
  labs(title = "Leaflet Number
         
         (χ2 = 0.1347, P = 0.7136)
         ", x = "Absence        Presence", 
       y = "Leaflet Number") 

## Leaves ####

# Preparing the data summary by ID and mainland/island column
leaf_summary <- leaf %>% group_by(ID, mainland_island) %>% summarize(mean_leaf_length = mean(leaf_length),
                                                                     mean_leaflet_length = mean(leaflet_length),
                                                                     mean_leaflet_number = mean(number_of_leaflets))
leaf_summary <- leaf_summary %>% column_to_rownames("ID")
leaf_summary_traits <- dplyr::select(leaf_summary, !mainland_island)

### PCA ####
# leaves:
leaf_pca <- prcomp(leaf_summary_traits, scale = T)

# Visualize eigenvalues (scree plot):
fviz_eig(leaf_pca)

# Graph of individuals. Individuals with similar profile are grouped together
fviz_pca_ind(leaf_pca, repel = T, geom = c("point"), habillage = leaf_summary$mainland_island, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
# Variables
fviz_pca_var(leaf_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
#Biplot
fviz_pca_biplot(leaf_pca, repel = T,
                geom = c("point"),
                habillage = leaf_summary$mainland_island,
                col.var = "black",
                addEllipses = T
)

# Model 2: Galapagos Other ####

## Leaves ####
# Preparing the data summary by ID and mainland/island column
leaf_summary_model2 <- leaf_islands %>% group_by(ID, galapagos_other) %>% summarize(mean_leaf_length = mean(leaf_length),
                                                                                    mean_leaflet_length = mean(leaflet_length),
                                                                                    mean_leaflet_number = mean(number_of_leaflets))
leaf_summary_model2 <- leaf_summary_model2 %>% column_to_rownames("ID")
leaf_summary_traits_model2 <- dplyr::select(leaf_summary_model2, !galapagos_other)

### PCA ####
# leaves:
leaf_pca_model2 <- prcomp(leaf_summary_traits_model2, scale = T)

# Visualize eigenvalues (scree plot):
fviz_eig(leaf_pca_model2)

# Graph of individuals. Individuals with similar profile are grouped together
fviz_pca_ind(leaf_pca_model2, repel = T, geom = c("point"), habillage = leaf_summary_model2$galapagos_other, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
# Variables
fviz_pca_var(leaf_pca_model2,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
#Biplot
fviz_pca_biplot(leaf_pca_model2, repel = T,
                geom = c("point"),
                habillage = leaf_summary_model2$galapagos_other,
                col.var = "black",
                addEllipses = T
)

## Leaves ####
# Preparing the data summary by ID and mainland/island column
leaf_summary_model3 <- leaf_gal %>% group_by(ID, finch_beak) %>% summarize(mean_leaf_length = mean(leaf_length),
                                                                           mean_leaflet_length = mean(leaflet_length),
                                                                           mean_leaflet_number = mean(number_of_leaflets))
leaf_summary_model3 <- leaf_summary_model3 %>% column_to_rownames("ID")
leaf_summary_traits_model3 <- dplyr::select(leaf_summary_model3, !finch_beak)

### PCA ####
# leaves:
leaf_pca_model3 <- prcomp(leaf_summary_traits_model3, scale = T)

# Visualize eigenvalues (scree plot):
fviz_eig(leaf_pca_model3)

# Graph of individuals. Individuals with similar profile are grouped together
fviz_pca_ind(leaf_pca_model3, repel = T, geom = c("point"), habillage = leaf_summary_model3$finch_beak, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
# Variables
fviz_pca_var(leaf_pca_model3,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
#Biplot
fviz_pca_biplot(leaf_pca_model3, repel = T,
                geom = c("point"),
                habillage = leaf_summary_model3$finch_beak,
                col.var = "black",
                addEllipses = T
)

