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
mericarp_traits <- dplyr::select(mericarp_NA, 1,12:14,16)
# Filter upper spines remove zeroes
mericarp_traits <- dplyr::filter(mericarp_traits, !tip_distance == 0)
mericarp_traits <- mericarp_traits %>% column_to_rownames("ind_num")
str(mericarp_traits)
# Scaled all mericarp traits first
mericarp_traits <- scale(mericarp_traits)
mericarp_traits <- as.tibble(mericarp_traits)




# 11_03 Mericarp dataset summarized by ID ####
# Created a new dataset with all parameters and scaled traits
# Combine scaled traits with original dataset without NAs
# This is the dataset that I am using for ploting the PCA
mericarp_scaled <- cbind(mericarp_NA, mericarp_traits)
# Select scaled traits
mericarp_scaled <- dplyr::select(mericarp_scaled, c(ID:mericarp_num, 23:26))
mericarp_summary <- mericarp_scaled %>% group_by(ID,mainland_island) %>% summarize(Length = mean(length),
                                                 Width = mean(width),
                                                 Depth = mean(depth),
                                                 Spine.Tip.Distance = mean(tip_distance))

# This is the dataframe I will use for the PCA it contains the means per ID:
mericarp_traits_summary <- dplyr::select(mericarp_summary, Length,
                                                           Width,
                                                           Depth,
                                                           Spine.Tip.Distance)
mericarp_traits_summary <- mericarp_traits_summary %>% column_to_rownames("ID")


# 11_04 PCA mericarp summary ####
## PCA ####
mericarp_pca <- prcomp(mericarp_traits_summary, scale = T)

## Visualize eigenvalues (scree plot) ####
fviz_eig(mericarp_pca)

## Biplot ####
# Individuals with similar profile are grouped together
fviz_pca_ind(mericarp_pca, repel = T, geom = c("point"), habillage = mericarp_summary$mainland_island, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
fviz_pca_var(mericarp_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
             )
fviz_pca_biplot(mericarp_pca, repel = T,
                geom = c("point"),
                habillage = mericarp_summary$mainland_island,
                col.var = "black",
                addEllipses = T
                )

# 11_05 Summary PCA plot ####
##  Theme summary Biplot ####
biplot1 <- fviz_pca_ind(mericarp_pca,
                        # Fill individuals by groups
                        title = "Mericarps
                           ",
                        geom.ind = "point",
                        pointshape = c(21),
                        pointsize = 3,
                        stroke = 1.5,
                        fill.ind = mericarp_summary$mainland_island,
                        col.ind = "black",
                        # Color variable by groups
                        legend.title = "Lower Spines",
                        repel = T,
                        col.var = "black",
                        labelsize = 5,
                        addEllipses = T,
                        palette = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73"),
                        
) + theme_transparent() + 
  # scale_color_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73")) +
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

var1 <- fviz_pca_var(mericarp_pca,
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

## PCA ####

#### Mericarp size PCA excludes lower spines and only uses length, width, depth
# and spine tip distance.
# Mericarp scaled removed upper spines zeroes:
mericarp_NA_wozero <- dplyr::filter(mericarp_NA, !tip_distance == 0)

mericarp_size_pca <- prcomp(dplyr::select(mericarp_traits, c(1:4)), scale = T)

#### Mericarp individual data. PCA ####
mericarp_ind_pca <- prcomp(mericarp_traits, scale = T)

## Visualize eigenvalues (scree plot) ####
### Size PCA ####
fviz_eig(mericarp_size_pca)
### Individual PCA ####
fviz_eig(mericarp_ind_pca)

## Biplot ####
### Size PCA ####
fviz_pca_ind(mericarp_size_pca, repel = T, geom = c("point"), habillage = mericarp_NA_wozero$mainland_island, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
fviz_pca_var(mericarp_size_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
fviz_pca_biplot(mericarp_size_pca, repel = T,
                geom = c("point"),
                habillage = mericarp_NA_wozero$lower_spines,
                col.var = "black",
                addEllipses = T
)

### Individual PCA ####
# It uses mericarp_NA as habillage because lower spines there is a factor.
fviz_pca_ind(mericarp_ind_pca, repel = T, geom = c("point"), habillage = mericarp_NA$lower_spines, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
fviz_pca_var(mericarp_ind_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
fviz_pca_biplot(mericarp_ind_pca, repel = T,
                geom = c("point"),
                habillage = mericarp_NA$mainland_island,
                col.var = "black",
                addEllipses = T
)

## Theme individual Biplot ####
### Mainland island comparison ####
biplot2 <- fviz_pca_biplot(mericarp_size_pca,
                        # Fill individuals by groups
                        title = "Mericarps
                           ",
                        geom.ind = "point",
                        pointshape = c(21),
                        pointsize = 4,
                        stroke = 0.5,
                        fill.ind = mericarp_NA_wozero$mainland_island,
                        col.ind = "black",
                        # Color variable by groups
                        legend.title = "Population",
                        repel = T,
                        col.var = "black", 
                        labelsize = 5,
                        addEllipses = T,
                        palette = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73"),
                        
) + theme_transparent() + 
  scale_color_manual(values = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73")) +
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

biplot2

## Theme individual Variable plots ####

var2 <- fviz_pca_var(mericarp_ind_pca,
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
var2


## Extract PC values ####
mericarp_scaled_PC <- cbind(mericarp_NA_wozero, mericarp_size_pca$x)

# 11_07 Model testing using the PC1 axis ####
## Model mainland island ####
meri_PC1_m1<- lmer(PC1 ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = mericarp_scaled_PC,
                      REML = F)

## ANOVA type II test ####
# Use the Anova function from the car package
 Anova(meri_PC1_m1)
# summary(meri_PC1_m1)

# Diagnostic custom function
# par(mfrow = c(1, 3))
# Residual histograms distributions
# diagnostic(resid(meri_PC1_m1))

# Diagnostics with DHARMA
# testResiduals(meri_PC1_m1)

## Emmeans estimates: Length ####
EM_PC1 <- emmeans(meri_PC1_m1, ~ mainland_island)

### Emmean plot: Length ####
plot(EM_PC1, comparisons = TRUE) + labs(title = "Mericarp Size")
pwpp(EM_PC1)

## Summary plots ####
#### Mainland Galapos plot ####
EM_PC1
plot_PC1 <- plot(EM_PC1, comparisons = T, plotit = F)

ggplot_PC1 <- ggplot(plot_PC1, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) + 
  labs(title = expression(paste("Mericarp Size (P = <0.001)"))) +
  labs(x = "Population", y = "Mericarp Size (PC1)") 


#### Violing plot ####
PC1_violin <- ggplot(mericarp_scaled_PC, aes(x = mainland_island, y = PC1, fill = mainland_island)) + 
  geom_violin(size = 1.5, trim = T) +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73")) +
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) + 
  labs(x = "Population", y = "Mericarp Size (PC1)", title = "Mericarp Size")

# 11_08 Mainland Island plots summary ####
## Mericarps ####
# Used in the main text
figure_mericarp_PC1_lower <- ggarrange(ggplot_PC1,
                                    ggplot_lower,
                                    labels = c("A", "B"),
                                    ncol = 2,
                                    nrow = 1) + 
  theme(text = element_text(family = "Noto Sans"))
## Flowers ####
# Flowers have model 1 and 2
figure_flower <- ggarrange(ggplot_flower,
                           ggplot_flower2,
                                    labels = c("A", "B"),
                                    ncol = 2,
                                    nrow = 1)


## Individual traits figure ####
# Supplemental figure with individual mericarp traits. From the PCA.
figure_mericarp_ind_traits <- ggarrange(ggplot_length,
                                    ggplot_width,
                                    ggplot_depth,
                                    ggplot_spine,
                                    labels = c("A", "B", "C", "D"),
                                    ncol = 3,
                                    nrow = 2) + 
  theme(text = element_text(family = "Noto Sans"))

# 11_09 Mainland Galapagos Island plots summary ####
figure_mericarp_mainland_gal <- ggarrange(ggplot_length_mainland_gal,
                                          ggplot_width_mainland_gal,
                                          ggplot_depth_mainland_gal,
                                          ggplot_spine_mainland_gal,
                                          ggplot_lower_mainland_gal,
                                          labels = c("A", "B", "C", "D","E"),
                                          ncol = 3,
                                          nrow = 2) + 
  theme(text = element_text(family = "Noto Sans"))

# 11_10 Violin plots summary ####
# Supplmental figure with data distribution of all traits.
violin_plots <- ggarrange(length_violin,
                          width_violin,
                          depth_violin,
                          tip_distance_violin,
                          #lower_spines_violin,
                          flower_violin,
                          flower_violin2,
                          labels = c("A", "B", "C", "D","E", "F", "G"),
                          ncol = 3,
                          nrow = 2) + 
  theme(text = element_text(family = "Noto Sans"))



