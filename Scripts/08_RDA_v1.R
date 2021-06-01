# Load the required packages
library(ade4)
library(adegraphics)
library(adespatial)
library(vegan)
library(vegan3d)
library(MASS)
library(rrcov)

# Model 1: Mainland/Island ####
## Mericarps #####
# Check mericarp data for NAs per trait:
colSums(is.na(mericarp)) #It seems that tip distance and depth have NAs. 286 and 346
### Remove NAs of traits ####
# Remove tip distance NAs:
mericarp <- filter(mericarp, !is.na(tip_distance))
# Remove depth NAs after removing tip distance:
mericarp <- filter(mericarp, !is.na(depth))

### Create mericarp traits matrix ####
mericarp_traits <- dplyr::select(mericarp, 1,12:14,16,19)
mericarp_traits <- mericarp_traits %>% column_to_rownames("ind_num")
#mericarp_traits <- mericarp_traits %>% mutate_at(vars(lower_spines), list(factor))
# [mericarp_traits] is the response matrix, 
# [mericarp] has the the explanatory variables

## RDA Mericarp Mainland Island ####
meri_RDA_mainland <- rda(scale(mericarp_traits) ~ mainland_island + Condition(year_collected), data = mericarp)

summary(meri_RDA_mainland)

### RDA mericarp ANOVA Global test ####
anova(meri_RDA_mainland, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA_mainland, by = "terms", permutations = how(nperm = 999))

## RDA mericarp. Variance partition ####
# Generate matrix per variables
mericarp_exp_matrix_abc <- dplyr::select(mericarp, year_collected, mainland_island)
mainland_island_matrix_a <- dplyr::select(mericarp, mainland_island)
year_collected_matrix_c <- dplyr::select(mericarp, year_collected)

# Variance partition
mericarp.varpart <- varpart(scale(mericarp_traits), mainland_island_matrix_a, year_collected_matrix_c)

### Variance partition plot ####
plot(mericarp.varpart, 
     digits = 2, 
     bg = c("cadetblue4", "chartreuse4"),
     Xnames = c("Population", "Year"),
     id.size = 1
)

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(mericarp_traits), mainland_island_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(mericarp_traits), year_collected_matrix_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(mericarp_traits), mericarp_exp_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(mericarp_traits), mainland_island_matrix_a, year_collected_matrix_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(mericarp_traits), year_collected_matrix_c, mainland_island_matrix_a), 
      permutations = how(nperm = 999))

## Flowers ####
### Create flower datasets ####
# Check flower data for NAs per trait:
colSums(is.na(flower)) # It seems there are no NAs for petal length, but there are NAs on year collected
# Remove year collected NAs:
flower <- filter(flower, !is.na(year_collected))

### Flower traits matrix. Petal length ####
flower_traits <- dplyr::select(flower, petal_length)

## RDA flower Mainland island comparison ####
flower_RDA_mainland <- rda(scale(flower_traits) ~ mainland_island
    + Condition(year_collected), data = flower)

### RDA flower ANOVA ####
anova(flower_RDA_mainland, permutations = how(nperm = 999))
# Test of all canonical axes
anova(flower_RDA_mainland, by = "terms", permutations = how(nperm = 999))

# Flower Triplot Mainland Island ###
# 
# ### Scaling 1 ###
# plot(flower_RDA_mainland,
#      scaling = 1,
#      display = c(#"sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Flower traits ~ Mainland/Island - scaling 1 - lc scores"
# )
# spe.sc1 <- scores(flower_RDA_mainland,
#                   choices = 1:2,
#                   scaling = 1,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
### Scaling 2 ###
# plot(flower_RDA_mainland,
#      display = c(#"sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Flower traits ~ Mainland/Island - scaling 2 - lc scores"
# )
# spe.sc1 <- scores(flower_RDA_mainland,
#                   choices = 1:2,
#                   scaling = 2,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )


## RDA Flower. Variance partition ####
# Generate matrix per variables
flower_exp_matrix_abc <- dplyr::select(flower, year_collected, mainland_island)
flower_mainland_island_matrix_a <- dplyr::select(flower, mainland_island)
flower_year_collected_matrix_c <- dplyr::select(flower, year_collected)

# Variance partition
flower.varpart <- varpart(scale(flower_traits), flower_mainland_island_matrix_a, flower_year_collected_matrix_c)

### Variance partition plot ####
plot(flower.varpart, 
     digits = 2, 
     bg = c("cadetblue4", "chartreuse4"),
     Xnames = c("Population", "Year"),
     id.size = 1
)

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(flower_traits), flower_mainland_island_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(flower_traits), flower_year_collected_matrix_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(flower_traits), flower_exp_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(flower_traits), flower_mainland_island_matrix_a, flower_year_collected_matrix_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(flower_traits), flower_year_collected_matrix_c, flower_mainland_island_matrix_a), 
      permutations = how(nperm = 999))

## Leaves ####
### Create leaf datasets ####

# Check leaf data for NAs per trait:

colSums(is.na(leaf)) # It seems that number of leaflets has 17 NAs
                     # It seems that year collected also has 17 NAs
# Remove year collected NAs:

leaf <- filter(leaf, !is.na(number_of_leaflets))
leaf <- filter(leaf, !is.na(year_collected))
leaf <- filter(leaf, !is.na(leaf_length))
leaf <- filter(leaf, !is.na(leaflet_length))

### Leaf traits matrix. ####
leaf_traits <- dplyr::select(leaf, leaf_length,
                                   leaflet_length,
                                   number_of_leaflets)


## RDA leaf Mainland island comparison ####
leaf_RDA_mainland <- rda(scale(leaf_traits) ~ mainland_island
                           + Condition(year_collected), data = leaf)

### RDA leaves ANOVA ####
anova(leaf_RDA_mainland, permutations = how(nperm = 999))
# Test of all canonical axes
anova(leaf_RDA_mainland, by = "terms", permutations = how(nperm = 999))

# ## Leaf Triplot Mainland Island ###
# 
# ### Scaling 1 ###
# plot(leaf_RDA_mainland,
#      scaling = 1,
#      display = c(#"sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Leaf traits ~ Mainland/Island - scaling 1 - lc scores"
# )
# spe.sc1 <- scores(leaf_RDA_mainland,
#                   choices = 1:2,
#                   scaling = 1,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
# ### Scaling 2 ###
# plot(leaf_RDA_mainland,
#      display = c(#"sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Leaf traits ~ Mainland/Island - scaling 2 - lc scores"
# )
# spe.sc1 <- scores(flower_RDA_mainland,
#                   choices = 1:2,
#                   scaling = 2,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
# 

## RDA Leaves Variance partition ####
# Generate matrix per variables
leaf_exp_matrix_abc <- dplyr::select(leaf, year_collected, mainland_island)
leaf_mainland_island_matrix_a <- dplyr::select(leaf, mainland_island)
leaf_year_collected_matrix_c <- dplyr::select(leaf, year_collected)

# Variance partition
leaf.varpart <- varpart(scale(leaf_traits), leaf_mainland_island_matrix_a, leaf_year_collected_matrix_c)

### Variance partition plot ####
plot(leaf.varpart, 
     digits = 2, 
     bg = c("cadetblue4", "chartreuse4"),
     Xnames = c("Population", "Year"),
     id.size = 1,
     )

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(leaf_traits), leaf_mainland_island_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(leaf_traits), leaf_year_collected_matrix_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(leaf_traits), leaf_exp_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(leaf_traits), leaf_mainland_island_matrix_a, leaf_year_collected_matrix_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(leaf_traits), leaf_year_collected_matrix_c, leaf_mainland_island_matrix_a), 
      permutations = how(nperm = 999))



# Model 2: Galapagos and other islands ####

## Flower ####
# Filter database for islands only
flower_islands <- filter(flower, mainland_island == "island")

# Filter traits for islands:
flower_islands_traits <- dplyr::select(flower_islands, petal_length)

## RDA Flower Galapagos and other islands ####
flower_RDA_Galapagos <- rda(flower_islands_traits ~ galapagos_other
                            + Condition(year_collected), data = flower_islands)

### Global test of the RDA results ####
anova(flower_RDA_Galapagos, permutations = how(nperm = 999))
# Test of all canonical axes
anova(flower_RDA_Galapagos, by = "terms", permutations = how(nperm = 999))

## Flower Triplot Galapagos other Island ###

### Scaling 1 ###
# plot(flower_RDA_Galapagos,
#      scaling = 1,
#      display = c("sp",
#                  #"lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Flower traits ~ Galapagos/Other Islands - scaling 1 - lc scores"
# )
# spe.sc1 <- scores(flower_RDA_Galapagos,
#                   choices = 1:2,
#                   scaling = 1,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
# ### Scaling 2 ###
# plot(flower_RDA_Galapagos,
#      display = c("sp",
#                  #"lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Flowers traits ~ Galapagos/Other Islands - scaling 2 - lc scores"
# )
# spe.sc1 <- scores(flower_RDA_Galapagos,
#                   choices = 1:2,
#                   scaling = 2,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
# 

## RDA flowers 2 Variance partition ####
# Generate matrix per variables
flower_islands_matrix_abc <- dplyr::select(flower_islands, year_collected, galapagos_other)
flower_galapagos_other_matrix_a <- dplyr::select(flower_islands, galapagos_other)
flower_year_collected_matrix2_c <- dplyr::select(flower_islands, year_collected)

# Variance partition
flower.varpart.islands <- varpart(scale(flower_islands_traits), flower_galapagos_other_matrix_a, flower_year_collected_matrix2_c)

### Variance partition plot ####
plot(flower.varpart.islands, 
     digits = 2, 
     bg = c("darksalmon", "blue"),
     Xnames = c("Island", "Year"),
     id.size = 1,
)

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(flower_islands_traits), flower_galapagos_other_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(flower_islands_traits), flower_year_collected_matrix2_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(flower_islands_traits), flower_islands_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(flower_islands_traits), flower_galapagos_other_matrix_a, flower_year_collected_matrix2_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(flower_islands_traits), flower_year_collected_matrix2_c, flower_galapagos_other_matrix_a), 
      permutations = how(nperm = 999))


## Leaves ####
# Filter database for islands only
leaf_islands <- filter(leaf, mainland_island == "island")

# Filter traits for islands:
leaf_islands_traits <- dplyr::select(leaf_islands, leaf_length,
                                                   leaflet_length,
                                                   number_of_leaflets)
## RDA Leaf Galapagos and other islands ####
leaf_RDA_Galapagos <- rda(scale(leaf_islands_traits) ~ galapagos_other
                            + Condition(year_collected), data = leaf_islands)

### Global test of the RDA results ####
anova(leaf_RDA_Galapagos, permutations = how(nperm = 999))
# Test of all canonical axes
anova(leaf_RDA_Galapagos, by = "terms", permutations = how(nperm = 999))

## Leaf Triplot Galapagos other Island ###

# ### Scaling 1 ###
# plot(leaf_RDA_Galapagos,
#      scaling = 1,
#      display = c("sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Leaf traits ~ Galapagos/Other Islands - scaling 1 - lc scores"
# )
# spe.sc1 <- scores(leaf_RDA_Galapagos,
#                   choices = 1:2,
#                   scaling = 1,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )

# ### Scaling 2 ###
# plot(leaf_RDA_Galapagos,
#      display = c("sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Mericarp traits ~ Galapagos/Other Islands - scaling 2 - lc scores"
# )
# spe.sc1 <- scores(leaf_RDA_Galapagos,
#                   choices = 1:2,
#                   scaling = 2,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )


## RDA leaves 2 Variance partition ####
# Generate matrix per variables
leaf_islands_matrix_abc <- dplyr::select(leaf_islands, year_collected, galapagos_other)
leaf_galapagos_other_matrix_a <- dplyr::select(leaf_islands, galapagos_other)
leaf_year_collected_matrix2_c <- dplyr::select(leaf_islands, year_collected)

# Variance partition
leaf.varpart.islands <- varpart(scale(leaf_islands_traits), leaf_galapagos_other_matrix_a, leaf_year_collected_matrix2_c)

### Variance partition plot ####
plot(leaf.varpart.islands, 
     digits = 2, 
     bg = c("darksalmon", "blue"),
     Xnames = c("Island", "Year"),
     id.size = 1,
)

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(leaf_islands_traits), leaf_galapagos_other_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(leaf_islands_traits), leaf_year_collected_matrix2_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(leaf_islands_traits), leaf_islands_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(leaf_islands_traits), leaf_galapagos_other_matrix_a, leaf_year_collected_matrix2_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(leaf_islands_traits), leaf_year_collected_matrix2_c, leaf_galapagos_other_matrix_a), 
      permutations = how(nperm = 999))


# Model 3: Finch beak Presence/Absence of large beak finches ####
## Mericarps ####

### Filter Galapagos dataset ####
mericarp_gal <- filter(mericarp, galapagos_other == "Galapagos")

mericarp_traits_gal <- dplyr::select(mericarp_gal, 1,12:14,16,19)
mericarp_traits_gal <- mericarp_traits_gal %>% column_to_rownames("ind_num")

## Mericarp finch beak RDA ####
meri_RDA_beak <- rda(scale(mericarp_traits_gal) ~ finch_beak 
                 + Condition(year_collected)
                 , data = mericarp_gal)

summary(meri_RDA_beak)

## Global test of the RDA results ####
anova(meri_RDA_beak, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA_beak, by = "terms", permutations = how(nperm = 999))

### Scaling 1 ###
# plot(meri_RDA_beak,
#      scaling = 1,
#      display = c("sp",
#                  #"lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Mericarp traits ~ Finch beak - scaling 1 - lc scores"
# )
# spe.sc1 <- scores(meri_RDA_beak,
#                   choices = 1:2,
#                   scaling = 1,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
### Scaling 2 ###
# plot(meri_RDA_beak,
#      display = c(#"sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Mericarp traits ~ Finch Beak - scaling 2 - lc scores"
# )
# spe.sc1 <- scores(meri_RDA_beak,
#                   choices = 1:2,
#                   scaling = 2,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )


## RDA Mericarps 3 Variance partition ####
# Generate matrix per variables
mericarp_beak_matrix_abc <- dplyr::select(mericarp_gal, year_collected, finch_beak)
mericarp_finch_matrix_a <- dplyr::select(mericarp_gal, finch_beak)
mericarp_year_collected_matrix3_c <- dplyr::select(mericarp_gal, year_collected)

# Variance partition
mericarp.varpart.finch <- varpart(scale(mericarp_traits_gal), mericarp_finch_matrix_a, mericarp_year_collected_matrix3_c)

### Variance partition plot ####
plot(mericarp.varpart.finch, 
     digits = 2, 
     bg = c("darkcyan", "darkgoldenrod3"),
     Xnames = c("Finch Beak", "Year"),
     id.size = 1,
)

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(mericarp_traits_gal), mericarp_finch_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(mericarp_traits_gal), mericarp_year_collected_matrix3_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(mericarp_traits_gal), mericarp_beak_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(mericarp_traits_gal), mericarp_finch_matrix_a, mericarp_year_collected_matrix3_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(mericarp_traits_gal), mericarp_year_collected_matrix3_c, mericarp_finch_matrix_a), 
      permutations = how(nperm = 999))

## Flower ####
### Filter Beak dataset ####
flower_gal <- filter(flower, galapagos_other == "Galapagos")
flower_traits_gal <- dplyr::select(flower_gal, petal_length)

## Flower finch beak RDA ####
flower_RDA_beak <- rda(scale(flower_traits_gal) ~ finch_beak 
                     + Condition(year_collected)
                     , data = flower_gal)

summary(flower_RDA_beak)

## Global test of the RDA results ####
anova(flower_RDA_beak, permutations = how(nperm = 999))
# Test of all canonical axes
anova(flower_RDA_beak, by = "terms", permutations = how(nperm = 999))


## Triplot Flower Finch Beak ###

# ### Scaling 1 ###
# plot(flower_RDA_beak,
#      scaling = 1,
#      display = c("sp",
#                  #"lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Flower traits ~ Finch beak - scaling 1 - lc scores"
# )
# spe.sc1 <- scores(flower_RDA_beak,
#                   choices = 1:2,
#                   scaling = 1,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
# ### Scaling 2 ###
# plot(flower_RDA_beak,
#      display = c(#"sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Flower traits ~ Finch Beak - scaling 2 - lc scores"
# )
# spe.sc1 <- scores(flower_RDA_beak,
#                   choices = 1:2,
#                   scaling = 2,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )

## RDA Flowers 3 Variance partition ####
# Generate matrix per variables
flower_beak_matrix_abc <- dplyr::select(flower_gal, year_collected, finch_beak)
flower_finch_matrix_a <- dplyr::select(flower_gal, finch_beak)
flower_year_collected_matrix3_c <- dplyr::select(flower_gal, year_collected)

# Variance partition
flower.varpart.finch <- varpart(scale(flower_traits_gal), flower_finch_matrix_a, flower_year_collected_matrix3_c)

### Variance partition plot ####
plot(flower.varpart.finch, 
     digits = 2, 
     bg = c("darkcyan", "darkgoldenrod3"),
     Xnames = c("Finch Beak", "Year"),
     id.size = 1,
)

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(flower_traits_gal), flower_finch_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(flower_traits_gal), flower_year_collected_matrix3_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(flower_traits_gal), flower_beak_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(flower_traits_gal), flower_finch_matrix_a, flower_year_collected_matrix3_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(flower_traits_gal), flower_year_collected_matrix3_c, flower_finch_matrix_a), 
      permutations = how(nperm = 999))


## Leaves ####

### Filter Galapagos dataset ####
leaf_gal <- filter(leaf, galapagos_other == "Galapagos")

# Check leaf data for NAs per trait:
colSums(is.na(leaf_gal)) # It seems that number of leaflets has 17 NAs

### Leaf traits matrix. ####
leaf_traits_gal <- dplyr::select(leaf_gal, leaf_length,
                             leaflet_length,
                             number_of_leaflets)


## RDA leaf Mainland island comparison ####
leaf_RDA_beak <- rda(scale(leaf_traits_gal) ~ finch_beak
                         + Condition(year_collected), data = leaf_gal)

### RDA leaves ANOVA ####
anova(leaf_RDA_beak, permutations = how(nperm = 999))
# Test of all canonical axes
anova(leaf_RDA_beak, by = "terms", permutations = how(nperm = 999))

## Triplot Mericarp Finch Beak ###
# source("triplot_RDA_Borcard2018.R")
# This function can improve the look of the triplots

# ### Scaling 1 ###
# plot(leaf_RDA_beak,
#      scaling = 1,
#      display = c("sp",
#                  "lc",
#                  "cn"
#      ),
#      type = "text",
#      main = "Triplot RDA Leaves traits ~ Finch beak - scaling 1 - lc scores"
# )
# spe.sc1 <- scores(leaf_RDA_beak,
#                   choices = 1:2,
#                   scaling = 1,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# 
### Scaling 2 ###
plot(leaf_RDA_beak,
     display = c(#"sp",
                 "lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Finch Beak - scaling 2 - lc scores"
)
# spe.sc1 <- scores(leaf_RDA_beak,
#                   choices = 1:2,
#                   scaling = 2,
#                   display = "sp"
# )
# arrows(0,0,
#        spe.sc1[,1] * 0.92,
#        spe.sc1[,2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )

## RDA Leaves 3 Variance partition ####
# Generate matrix per variables
leaf_beak_matrix_abc <- dplyr::select(leaf_gal, year_collected, finch_beak)
leaf_finch_matrix_a <- dplyr::select(leaf_gal, finch_beak)
leaf_year_collected_matrix3_c <- dplyr::select(leaf_gal, year_collected)

# Variance partition
leaf.varpart.finch <- varpart(scale(leaf_traits_gal), leaf_finch_matrix_a, leaf_year_collected_matrix3_c)

### Variance partition plot ####
plot(flower.varpart.finch, 
     digits = 2, 
     bg = c("darkcyan", "darkgoldenrod3"),
     Xnames = c("Finch Beak", "Year"),
     id.size = 1,
)

### Tests of all testable fractions ####
# Test of fraction [a+b]
anova(rda(scale(flower_traits_gal), flower_finch_matrix_a), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(scale(flower_traits_gal), flower_year_collected_matrix3_c), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
anova(rda(scale(flower_traits_gal), flower_beak_matrix_abc), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(scale(flower_traits_gal), flower_finch_matrix_a, flower_year_collected_matrix3_c), 
      permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(scale(flower_traits_gal), flower_year_collected_matrix3_c, flower_finch_matrix_a), 
      permutations = how(nperm = 999))




