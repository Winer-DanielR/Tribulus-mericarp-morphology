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

## Emmeans estimates: PCA  ####
EM_PC1 <- emmeans(meri_PC1_m1, ~ mainland_island)
EM_PC1_bioclim <- emmeans(meri_PC1_bioclim, ~ mainland_island)

### Emmean plot: PCA ####
plot(EM_PC1, comparisons = TRUE) + labs(title = "Mericarp Size")
pwpp(EM_PC1)

plot(EM_PC1_bioclim, comparisons = TRUE) + labs(title = "Mericarp Size")
pwpp(EM_PC1_bioclim)
