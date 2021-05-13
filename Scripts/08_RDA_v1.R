# Load the required packages
library(ade4)
library(adegraphics)
library(adespatial)
library(vegan)
library(vegan3d)
library(MASS)
library(ellipse)
library(FactoMineR)
library(factoextra)
library(rrcov)
library(easyCODA)

# Mainland Island ####
## Mericarps #####
# Check mericarp data for NAs per trait:

colSums(is.na(mericarp)) #It seems that tip distance and depth have NAs. 286 and 346

# Remove tip distance NAs:
mericarp <- filter(mericarp, !is.na(tip_distance))

# Remove depth NAs after removing tip distance:
mericarp <- filter(mericarp, !is.na(depth))

### Create mericarp traits matrix ####
mericarp_traits <- dplyr::select(mericarp, 1,12:14,16,19)
mericarp_traits <- mericarp_traits %>% column_to_rownames("ind_num")
# mericarp_traits <- mericarp_traits %>% mutate_at(vars(lower_spines), list(factor))

### Transform the data Z-scores estimates:####
mericarp_traits_z <- sapply(mericarp_traits, 
                            function(mericarp_traits)(mericarp_traits-mean(mericarp_traits)/sd(mericarp_traits)))
# Edit the table to be ID same as mericarp1
mericarp_traits_z <- as.tibble(mericarp_traits_z)
mericarp_traits_z <- bind_cols(mericarp, mericarp_traits_z)
mericarp_traits_z <- dplyr::select(mericarp_traits_z, 1,23:27)
str(mericarp_traits_z)

mericarp_traits_z <- rename(mericarp_traits_z, length = length...23,
                            width = width...24,
                            depth = depth...25,
                            tip_distance = tip_distance...26,
                            lower_spines = lower_spines...27
                            )

mericarp_traits_z <- mericarp_traits_z %>% column_to_rownames("ind_num")

### Explanatory matrix, mainland/island and year ####
mericarp1 <- dplyr::select(mericarp, 1,4,6)
mericarp1 <- mericarp1 %>% column_to_rownames("ind_num")

# Mericarp traits is the response matrix, mericarp1 is the explanatory variables
# Separate variables from explanatory variables.

## RDA Mericarp Mainland Island ####

meri_RDA_mainland <- rda(mericarp_traits ~ mainland_island
                 + Condition(year_collected), data = mericarp1)
summary(meri_RDA_mainland)

### Global test of the RDA results ####
anova(meri_RDA_mainland, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA_mainland, by = "terms", permutations = how(nperm = 999))

## Mericarp Triplot Mainland Island ####
# source("triplot_RDA_Borcard2018.R") # This function can improve the look of the triplots

#biplot(meri_RDA_mainland)

### Scaling 1 ####
plot(meri_RDA_mainland,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
                 ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Mainland/Island - scaling 1 - lc scores"
     )
spe.sc1 <- scores(meri_RDA_mainland,
                  choices = 1:2,
                  scaling = 1,
                  display = "sp"
                  )
arrows(0,0,
      spe.sc1[,1] * 0.92,
      spe.sc1[,2] * 0.92,
      length = 0,
      lty = 1,
      col = "red"
      )

### Scaling 2 ####
plot(meri_RDA_mainland,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Mainland/Island - scaling 2 - lc scores"
)
spe.sc1 <- scores(meri_RDA_mainland,
                  choices = 1:2,
                  scaling = 2,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

# triplot.rda(mainland_island_rda,
#             scaling = 1,
#             ax1 = 1,
#             ax2 = 2,
#             plot.sites = F,
#             arrows.only = T,
#             silent = F)
# 
# triplot.rda(mainland_island_rda,
#             site.sc = "lc",
#             scaling = 1,
#             cex.char2 = 0.7,
#             pos.env = 3,
#             pos.centr = 1,
#             mult.arrow = 1.1,
#             mar.percent = 0.05,
#             #select.spe = sel_sp,
#             plot.sites = T,
#             arrows.only = F,
#             label.sites = F,
#             label.centr = F
# )
# triplot.rda(mainland_island_rda,
#             site.sc = "lc",
#             scaling = 2,
#             cex.char2 = 0.7,
#             pos.env = 3,
#             pos.centr = 1,
#             mult.arrow = 1.1,
#             mar.percent = 0.05,
#             #select.spe = sel_sp,
#             plot.sites = T,
#             arrows.only = F,
#             label.sites = F,
#             label.centr = F
# )


## Flowers ####
### Create flower datasets ####

# Check flower data for NAs per trait:

colSums(is.na(flower)) # It seems there are no NAs for petal length, but there are NAs on year collected

# Remove year collected NAs:

flower <- filter(flower, !is.na(year_collected))

### Flower traits matrix. Petal length ####
flower_traits <- dplyr::select(flower, petal_length)

## RDA flower Mainland island comparison ####
flower_RDA_mainland <- rda(flower_traits ~ mainland_island
    + Condition(year_collected), data = flower)

### Global test of the RDA results ####
anova(flower_RDA_mainland, permutations = how(nperm = 999))
# Test of all canonical axes
anova(flower_RDA_mainland, by = "terms", permutations = how(nperm = 999))

## Flower Triplot Mainland Island ####
source("triplot_RDA_Borcard2018.R")
# This function can improve the look of the triplots

### Scaling 1 ####
plot(flower_RDA_mainland,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Flower traits ~ Mainland/Island - scaling 1 - lc scores"
)
spe.sc1 <- scores(flower_RDA_mainland,
                  choices = 1:2,
                  scaling = 1,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

### Scaling 2 ####
plot(flower_RDA_mainland,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Flower traits ~ Mainland/Island - scaling 2 - lc scores"
)
spe.sc1 <- scores(flower_RDA_mainland,
                  choices = 1:2,
                  scaling = 2,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)


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


### Leaf traits z scoring. ####
leaf_traits_z <- sapply(leaf_traits, 
                            function(leaf_traits)(leaf_traits-mean(leaf_traits)/sd(leaf_traits)))
leaf_traits_z <- as.tibble(leaf_traits_z)

## RDA leaf Mainland island comparison ####
leaf_RDA_mainland <- rda(leaf_traits_z ~ mainland_island
                           + Condition(year_collected), data = leaf)

### Global test of the RDA results ####
anova(leaf_RDA_mainland, permutations = how(nperm = 999))
# Test of all canonical axes
anova(leaf_RDA_mainland, by = "terms", permutations = how(nperm = 999))

## Leaf Triplot Mainland Island ####
# source("triplot_RDA_Borcard2018.R") # This function can improve the look of the triplots

PLOT.RDA(leaf_RDA_mainland,
         map = "symmetric",
         main = "Triplot RDA Leaf traits ~ Mainland/Island - scaling 1 - lc scores",
         )



### Scaling 1 ####
plot(leaf_RDA_mainland,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Leaf traits ~ Mainland/Island - scaling 1 - lc scores"
)
spe.sc1 <- scores(leaf_RDA_mainland,
                  choices = 1:2,
                  scaling = 1,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

### Scaling 2 ####
plot(leaf_RDA_mainland,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Leaf traits ~ Mainland/Island - scaling 2 - lc scores"
)
spe.sc1 <- scores(flower_RDA_mainland,
                  choices = 1:2,
                  scaling = 2,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)


# Galapagos and other islands ####

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

## Flower Triplot Galapagos other Island ####

### Scaling 1 ####
plot(flower_RDA_Galapagos,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Flower traits ~ Galapagos/Other Islands - scaling 1 - lc scores"
)
spe.sc1 <- scores(flower_RDA_Galapagos,
                  choices = 1:2,
                  scaling = 1,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

### Scaling 2 ####
plot(flower_RDA_Galapagos,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Flowers traits ~ Galapagos/Other Islands - scaling 2 - lc scores"
)
spe.sc1 <- scores(flower_RDA_Galapagos,
                  choices = 1:2,
                  scaling = 2,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)



## Leaves ####
# Filter database for islands only
leaf_islands <- filter(leaf, mainland_island == "island")

# Filter traits for islands:
leaf_islands_traits <- dplyr::select(leaf_islands, leaf_length,
                                                   leaflet_length,
                                                   number_of_leaflets)
### Transformation of Leaf data: z-scoring ####
leaf_islands_traits_z <- sapply(leaf_islands_traits, 
                        function(leaf_islands_traits)(leaf_islands_traits-mean(leaf_islands_traits)/sd(leaf_islands_traits)))
leaf_islands_traits_z <- as.tibble(leaf_islands_traits_z)

## RDA Leaf Galapagos and other islands ####
leaf_RDA_Galapagos <- rda(leaf_islands_traits_z ~ galapagos_other
                            + Condition(year_collected), data = leaf_islands)

### Global test of the RDA results ####
anova(leaf_RDA_Galapagos, permutations = how(nperm = 999))
# Test of all canonical axes
anova(leaf_RDA_Galapagos, by = "terms", permutations = how(nperm = 999))

## Leaf Triplot Galapagos other Island ####

### Scaling 1 ####
plot(leaf_RDA_Galapagos,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Leaf traits ~ Galapagos/Other Islands - scaling 1 - lc scores"
)
spe.sc1 <- scores(leaf_RDA_Galapagos,
                  choices = 1:2,
                  scaling = 1,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

### Scaling 2 ####
plot(leaf_RDA_Galapagos,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Galapagos/Other Islands - scaling 2 - lc scores"
)
spe.sc1 <- scores(leaf_RDA_Galapagos,
                  choices = 1:2,
                  scaling = 2,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)




# Finch beak ####
## Mericarps ####

### Filter Galapagos dataset ####
mericarp_gal <- filter(mericarp, galapagos_other == "Galapagos")
mericarp_traits_gal <- dplyr::select(mericarp_gal, 1,12:14,16,19)

### Transform the data Z-scores estimates:####
mericarp_traits_gal_z <- sapply(mericarp_traits_gal, 
                            function(mericarp_traits_gal)(mericarp_traits_gal-mean(mericarp_traits_gal)/sd(mericarp_traits_gal)))
# Edit the table to be ID same as mericarp1
mericarp_traits_gal_z <- as.tibble(mericarp_traits_gal_z)
mericarp_traits_gal_z <- bind_cols(mericarp_gal, mericarp_traits_gal_z)
mericarp_traits_gal_z <- dplyr::select(mericarp_traits_gal_z, 1,24:28)
str(mericarp_traits_gal_z)

mericarp_traits_gal_z <- rename(mericarp_traits_gal_z, 
                            ind_num = ind_num...1,
                            length = length...24,
                            width = width...25,
                            depth = depth...26,
                            tip_distance = tip_distance...27,
                            lower_spines = lower_spines...28
)

mericarp_traits_gal_z <- mericarp_traits_gal_z %>% column_to_rownames("ind_num")

### Explanatory matrix: Finch beak and year ####
mericarp1_gal <- dplyr::select(mericarp_gal, 1:10)
mericarp1_gal <- mericarp1_gal %>% column_to_rownames("ind_num")

## Mericarp finch beak RDA ####
meri_RDA_beak <- rda(mericarp_traits_gal_z ~ finch_beak 
                 + Condition(year_collected)
                 , data = mericarp1_gal)

summary(meri_RDA_beak)

## Global test of the RDA results ####
anova(meri_RDA_beak, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA_beak, by = "terms", permutations = how(nperm = 999))


## Triplot Mericarp Finch Beak ####
source("triplot_RDA_Borcard2018.R")
# This function can improve the look of the triplots

### Scaling 1 ####
plot(meri_RDA_beak,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Finch beak - scaling 1 - lc scores"
)
spe.sc1 <- scores(meri_RDA_beak,
                  choices = 1:2,
                  scaling = 1,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

### Scaling 2 ####
plot(meri_RDA_beak,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Finch Beak - scaling 2 - lc scores"
)
spe.sc1 <- scores(meri_RDA_beak,
                  choices = 1:2,
                  scaling = 2,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)


## Flower ####
### Filter Beak dataset ####
flower_gal <- filter(flower, galapagos_other == "Galapagos")
flower_traits_gal <- dplyr::select(flower_gal, petal_length)

## Mericarp finch beak RDA ####
flower_RDA_beak <- rda(flower_traits_gal ~ finch_beak 
                     + Condition(year_collected)
                     , data = flower_gal)

summary(flower_RDA_beak)

## Global test of the RDA results ####
anova(flower_RDA_beak, permutations = how(nperm = 999))
# Test of all canonical axes
anova(flower_RDA_beak, by = "terms", permutations = how(nperm = 999))


## Triplot Mericarp Finch Beak ####
source("triplot_RDA_Borcard2018.R")
# This function can improve the look of the triplots

### Scaling 1 ####
plot(flower_RDA_beak,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Finch beak - scaling 1 - lc scores"
)
spe.sc1 <- scores(flower_RDA_beak,
                  choices = 1:2,
                  scaling = 1,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

### Scaling 2 ####
plot(flower_RDA_beak,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Finch Beak - scaling 2 - lc scores"
)
spe.sc1 <- scores(flower_RDA_beak,
                  choices = 1:2,
                  scaling = 2,
                  display = "sp"
)
arrows(0,0,
       spe.sc1[,1] * 0.92,
       spe.sc1[,2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)






