# Load the required packages
library(ade4)
library(adegraphics)
library(adespatial)
library(vegan)
library(vegan3d)
library(MASS)
library(rrcov)

# # Model 1: Mainland/Island ####
## RDA Mericarp Mainland Island ####
meri_RDA_mainland <- rda(scale(dplyr::select(mericarp_traits, c(1:4))) ~ mainland_island + Condition(year_collected), data = mericarp_NA_wozero)

summary(meri_RDA_mainland)

### RDA mericarp ANOVA Global test ####
anova(meri_RDA_mainland, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA_mainland, by = "terms", permutations = how(nperm = 999))

## RDA mericarp. Variance partition ####
# Generate matrix per variables
mericarp_exp_matrix_abc <- dplyr::select(mericarp_NA_wozero, year_collected, mainland_island)
mainland_island_matrix_a <- dplyr::select(mericarp_NA_wozero, mainland_island)
year_collected_matrix_c <- dplyr::select(mericarp_NA_wozero, year_collected)

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