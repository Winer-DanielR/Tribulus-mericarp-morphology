# Script 11 Multivariate analysis ####

# I am using the mericarp dataset without NAs. I am also using lower spines as a numerical factor
# to be able to scale it with the other traits. The PCA is only for the mericarp dataset.
# First, I am creating a mericarp trait dataset, which selects for length, width,
# depth, tip distance and lower spines. This dataset also selects for ind_num.
# I did the PCA on the means per trait grouped by IDs to better vizualize the plot
# I scaled the traits first then did the summary per ID.


# 11_01 Create PCA mericarp database ####
# Check and remove NAs
mericarp_NA <- filter(mericarp, !is.na(length))
mericarp_NA <- filter(mericarp_NA, !is.na(depth))
mericarp_NA <- filter(mericarp_NA, !is.na(tip_distance))
mericarp_NA <- dplyr::filter(mericarp_NA, !tip_distance == 0)
# Check mericarp data for NAs per trait:
colSums(is.na(mericarp_NA))

# 11_02 Scale traits used for PCA ####
# Select the traits for the PCA
mericarp_traits <- dplyr::select(mericarp_NA, 1, 14:16,18)
# Filter upper spines remove zeroes
mericarp_traits <- dplyr::filter(mericarp_traits, !tip_distance == 0)

# Excelent way to determine which rows are duplicated
#mericarp_traits[duplicated(mericarp_traits) | duplicated(mericarp_traits, fromLast=TRUE), ]

mericarp_traits <- mericarp_traits %>% column_to_rownames(var = "ind_num")
str(mericarp_traits)
# Scaled all mericarp traits first
mericarp_traits <- scale(mericarp_traits)
mericarp_traits <- as_tibble(mericarp_traits)


# 11_03 Mericarp dataset MEANS by ID ####
# Using mericarp ind since this dataset uses the mean of Galapagos and
# Florida
mean_mericarp_NA <- filter(mericarp_ind, !is.na(length))
mean_mericarp_NA <- filter(mean_mericarp_NA, !is.na(depth))
mean_mericarp_NA <- filter(mean_mericarp_NA, !is.na(tip_distance))
mean_mericarp_NA <- filter(mean_mericarp_NA, !is.na(Temp_S))
mean_mericarp_NA <- filter(mean_mericarp_NA, !tip_distance == 0)
# Check mericarp data for NAs per trait:
colSums(is.na(mean_mericarp_NA))

## Scale traits used for PCA ####
# Select the traits for the PCA
mean_mericarp_traits <- dplyr::select(mean_mericarp_NA, 1, 14:17)
# Filter upper spines remove zeroes
mean_mericarp_traits <- dplyr::filter(mean_mericarp_traits, !tip_distance == 0)

mean_mericarp_traits <- select(mean_mericarp_traits, !c(ID))

# Scaled all mericarp traits first
mean_mericarp_traits <- scale(mean_mericarp_traits)
mean_mericarp_traits <- as_tibble(mean_mericarp_traits)

# mericarp_scaled <- cbind(mericarp_NA, mericarp_traits)
# # Select scaled traits
# mericarp_scaled <- dplyr::select(mericarp_scaled, c(ID:mericarp_num, 28:35))
# mericarp_summary <- mericarp_scaled %>% group_by(ID,mainland_island,island_group,year_collected,Herbarium) %>% summarize(Length = mean(length),
#                                                  Width = mean(width),
#                                                  Depth = mean(depth),
#                                                  Spine_tip_distance = mean(tip_distance),
#                                                  Temp = mean(Temp),
#                                                  Temp_S = mean(Temp_S),
#                                                  Prec = mean(Prec),
#                                                  varP = mean(varP)
#                                                  )
#mericarp_summary <- filter(mericarp_summary, !is.na(Spine_length))
# This is the dataframe I will use for the PCA it contains the means per ID:
# mericarp_traits_summary <- dplyr::select(mericarp_summary, Length,
#                                                            Width,
#                                                            Depth,
#                                                            Spine_tip_distance
#                                                            )
# mericarp_traits_summary <- mericarp_traits_summary %>% column_to_rownames("ID")
#Remove NAs and mainland island column
# mericarp_traits_summary <- select(mericarp_traits_summary, !c(mainland_island, year_collected, island_group))
#mericarp_traits_summary <- filter(mericarp_traits_summary, !is.na(Spine_length))

# 11_04 PCA mericarp MEANS ####
## PCA ####
mean_mericarp_pca <- prcomp(mean_mericarp_traits, scale = T)

## Visualize eigenvalues (scree plot) ####
fviz_eig(mean_mericarp_pca)

## MEAN Biplot ####
# Individuals with similar profile are grouped together
fviz_pca_ind(mean_mericarp_pca, repel = T, geom = c("point"), habillage = mean_mericarp_NA$mainland_island, palette = "Dark",
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
fviz_pca_var(mean_mericarp_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
             )
fviz_pca_biplot(mean_mericarp_pca, repel = T,
                geom = c("point"),
                habillage = mean_mericarp_NA$mainland_island,
                col.var = "black",
                addEllipses = T
                )

# 11_05 MEAN PCA plot ####
##  Theme summary Biplot ####
biplot1 <- fviz_pca_biplot(mean_mericarp_pca,
                        # Fill individuals by groups
                        title = "Mean Mericarps
                           ",
                        geom.ind = "point",
                        pointshape = c(21),
                        pointsize = 3,
                        stroke = 1.5,
                        fill.ind = mean_mericarp_NA$mainland_island,
                        col.ind = "black",
                        # Color variable by groups
                        legend.title = "Population",
                        repel = T,
                        col.var = "black",
                        labelsize = 5,
                        addEllipses = T,
                        palette = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73"),
                        
) + theme_transparent() + 
  scale_color_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73")) +
  # PCA theme, adds custom font and sizes that matches the other plots    
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        text = element_text(family = "Noto Sans"),
        legend.text = element_text(size = 12, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(fill = NA, size = 0))

biplot1

## Theme summary plots ####

var1 <- fviz_pca_var(mean_mericarp_pca,
                     col.var = "contrib",
                     title = "Variables contribution
                           ",
                     gradient.cols = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73"),
                     repel = TRUE,
                     legend.title = "Contribution"
) +
  theme_transparent() +
  # PCA theme, adds custom font and sizes that matches the other plots    
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        text = element_text(family = "Noto Sans"),
        legend.text = element_text(size = 12, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = NA, size = 0))
var1


# 11_06 Individual PCA - Test modeling ####
# Length, depth and width of mericarps all differ between island and mainland samples. 
# The three dimensions probably covary strongly. This should be mentioned and correlation coefficients supplied. 
# I recommend performing PCA on these three dimensions and using PC1 as a size factor to document the difference between Galapagos and mainland. 
# The results of analyzing the dimensions separately could be put in the Supplement

## INDIVIDUAL PCA ####

#### Mericarp size PCA excludes lower spines and only uses length, width, depth
# and spine tip distance.
# Mericarp scaled removed upper spines zeroes:
mericarp_NA_wozero <- dplyr::filter(mericarp_NA, !tip_distance == 0)
mericarp_NA <- filter(mericarp_NA, !is.na(spine_length))
mericarp_size_pca <- prcomp(dplyr::select(mericarp_traits, c(1:4)), scale = T)

#### Mericarp individual data. PCA ####
# Remove NAs, from spine length
#mericarp_traits <- filter(mericarp_traits, !is.na(spine_length))
mericarp_ind_pca <- prcomp(mericarp_traits, scale = T)

# PC1 Models ####
# Adding PC scores into MEAN mericarp dataset



# Adding PC scores into individual mericarp dataset
mericarp_scaled_PC <- cbind(mericarp_NA_wozero, mericarp_ind_pca$x)
mericarp_mean_scaled_PC <- cbind(mean_mericarp_NA, mean_mericarp_pca$x)

# 11_07 Model testing using the PC1 axis ####
## MEAN Model mainland island ####
meri_PC1_mean <- lmer(PC1 ~ mainland_island +
                        year_collected +
                        Herbarium +
                        (1|ID),
                      data = mericarp_mean_scaled_PC,
                      REML = F)

meri_PC1_mean_bioclim <- lmer(PC1 ~ mainland_island +
                          year_collected +
                          Herbarium +
                          Temp +
                          Temp_S +
                          Prec +
                          varP +
                          (1|ID),
                        data = mericarp_mean_scaled_PC,
                        REML = F)

## INDIVIDUAL Model mainland island ####
meri_PC1_m1<- lmer(PC1 ~ mainland_island +
                        year_collected +
                     Herbarium +
                        (1|ID),
                      data = mericarp_scaled_PC,
                      REML = F)
## Bioclimate model ####
meri_PC1_bioclim <- lmer(PC1 ~ mainland_island +
                     year_collected +
                     Herbarium +
                     Temp +
                     Temp_S +
                     Prec +
                     varP +
                     (1|ID),
                   data = mericarp_scaled_PC,
                   REML = F)

## ANOVA type II test ####
# Use the Anova function from the car package
Anova(meri_PC1_mean)
Anova(meri_PC1_mean_bioclim)
Anova(meri_PC1_m1)
Anova(meri_PC1_bioclim)

#summary(meri_PC1_m1)
#summary(meri_PC1_m2)

# Diagnostic custom function
# par(mfrow = c(1, 3))
# Residual histograms distributions
# diagnostic(resid(meri_PC1_m1))

# Diagnostics with DHARMA
#testResiduals(meri_PC1_m1)
#testResiduals(meri_PC1_m2)

## Emmeans estimates: Length ####
EM_PC1 <- emmeans(meri_PC1_m1, ~ mainland_island)
EM_PC1_bioclim <- emmeans(meri_PC1_bioclim, ~ mainland_island)

### Emmean plot: Length ####
plot(EM_PC1, comparisons = TRUE) + labs(title = "Mericarp Size")
pwpp(EM_PC1)

plot(EM_PC1_bioclim, comparisons = TRUE) + labs(title = "Mericarp Size")
pwpp(EM_PC1_bioclim)
