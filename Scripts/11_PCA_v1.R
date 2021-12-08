# PCA packages:
library(RColorBrewer)
library(ellipse)
library(rgl)
library(factoextra)
library(FactoMineR)

# Model 1: mainland/island traits PCA ####
# Datasets used after I ran the RDA code since it removes NAs: mericarp, flower and leaves

### Mericarps ####
# Scaled all mericarp traits first
mericarp_traits <- scale(mericarp_traits)

# Mericarp datset summarized by ID
# Created a new dataset with all parameters and scaled traits
mericarp_mainland <- cbind(mericarp, mericarp_traits)
mericarp_mainland <- dplyr::select(mericarp_mainland, c(ID:mericarp_num, 23:27))
mericarp_summary <- mericarp_mainland %>% group_by(ID,mainland_island) %>% summarize(Length = mean(length),
                                                 Width = mean(width),
                                                 Depth = mean(depth),
                                                 Spine.Tip.Distance = mean(tip_distance),
                                                 Lower.Spines = mean(lower_spines))

# This is the dataframe I will use for the PCA it contains the means per ID:
mericarp_traits_summary <- dplyr::select(mericarp_summary, Length,
                                                           Width,
                                                           Depth,
                                                           Spine.Tip.Distance,
                                                           Lower.Spines)
mericarp_traits_summary <- mericarp_traits_summary %>% column_to_rownames("ID")

### PCA of mericarp traits ####
# Mericarps:
mericarp_pca <- prcomp(mericarp_traits_summary, scale = T)

# Visualize eigenvalues (scree plot):
fviz_eig(mericarp_pca)

# Graph of individuas. Individuals with similar profile are grouped together
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
