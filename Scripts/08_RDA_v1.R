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

# Load mericarp data


##### All mericarps traits RDA ####
# Create mericarp traits data
mericarp_traits <- select(mericarp1, 1,12:18)
mericarp_traits <- mericarp_traits %>% column_to_rownames("ind_num")

# Mericarp traits is the response matrix, mericarp1 is the explanatory variables
# Separate variables from explanatory variables. Explanatory variables are
# mainland/island, galapagos/other

meri_RDA1 <- rda(mericarp_traits ~ mainland_island 
                 + year_collected
                 + continent
                 , data = mericarp1)
summary(meri_RDA1)

# Global test of the RDA results
anova(meri_RDA1, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA1, by = "axis", permutations = how(nperm = 999))

### Mericarp data withouth spine length ####

mericarp_traits1 <- select(mericarp2, 1,12:17)
mericarp_traits1 <- mericarp_traits1 %>% column_to_rownames("ind_num")

# Mericarp traits is the response matrix, mericarp1 is the explanatory variables
# Separate variables from explanatory variables. Explanatory variables are
# mainland/island, galapagos/other

meri_RDA2 <- rda(mericarp_traits1 ~ mainland_island 
                 + year_collected
                 , data = mericarp2)
summary(meri_RDA2)

# Global test of the RDA results
anova(meri_RDA1, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA1, by = "axis", permutations = how(nperm = 999))

#### Finch beak mericarp RDA ####
# All traits dataset #
mericarp1_gal <- filter(mericarp1, galapagos_other == "Galapagos")
mericarp_traits_gal <- select(mericarp1_gal, 1,12:18)
mericarp_traits_gal <- mericarp_traits_gal %>% column_to_rownames("ind_num")

mericarp_RDA_gal <- rda(mericarp_traits_gal ~ finch_beak 
                 + year_collected
                 , data = mericarp1_gal)

summary(mericarp_RDA_gal)

# Global test of the RDA results
anova(meri_RDA1, permutations = how(nperm = 999))
# Test of all canonical axes
anova(meri_RDA1, by = "axis", permutations = how(nperm = 999))


# RDA plot
# Alternate code using plot.cca -----------------------------
# Triplot using lc (model) site scores and scaling 2
dev.new(
  title = "RDA w. one 2nd-degree variable - scaling 2", 
  noRStudioGD = TRUE
)
plot(meri_RDA2,
     type = "text",
     scaling = "symmetric", 
     display = c("sp", 
                 #"lc", 
                 "cn"), 
     main = "Triplot RDA spe ~ dfs+dfs2 - scaling 1 - lc scores")
spe6.sc <- 
  scores(meri_RDA2, 
         choices = 1:2, 
         scaling = 1, 
         display = "sp")
arrows(0, 0, 
       spe6.sc[, 1] * 0.9, 
       spe6.sc[, 2] * 0.9, 
       length = 0, 
       lty = 1, 
       col = "red"
)
# End Alternate code using plot.cca --------------------------

# Lower spines are numbers not factors. How should I include them but as factors
# Should I include other information?


