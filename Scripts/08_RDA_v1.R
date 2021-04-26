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

# Load mericarp data ####
# Check mericarp data for NAs per trait:

colSums(is.na(mericarp)) #It seems that tip distance and depth have NAs. 286 and 346

# Remove tip distance NAs:
mericarp <- filter(mericarp, !is.na(tip_distance))

# Remove depth NAs after removing tip distance:
mericarp <- filter(mericarp, !is.na(depth))

## All mericarps traits RDA ####
### Create mericarp traits data ####
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


# Create mericarp explanatory matrix
mericarp1 <- dplyr::select(mericarp, 1,4,6)
mericarp1 <- mericarp1 %>% column_to_rownames("ind_num")

# Mericarp traits is the response matrix, mericarp1 is the explanatory variables
# Separate variables from explanatory variables.
# Mainland Island RDA ####

mainland_island_rda <- rda(mericarp_traits ~ mainland_island
                 + Condition(year_collected), data = mericarp1)
summary(mainland_island_rda)

## Global test of the RDA results ####
anova(mainland_island_rda, permutations = how(nperm = 999))
# Test of all canonical axes
anova(mainland_island_rda, by = "terms", permutations = how(nperm = 999))

## Triplot Mainland Island ####
source("triplot_RDA_Borcard2018.R")
# This function can improve the look of the triplots

### Scaling 1 ####
plot(mainland_island_rda,
     scaling = 1,
     display = c("sp",
                 #"lc",
                 "cn"
                 ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Mainland/Island - scaling 1 - lc scores"
     )
spe.sc1 <- scores(mainland_island_rda,
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
plot(mainland_island_rda,
     display = c("sp",
                 #"lc",
                 "cn"
     ),
     type = "text",
     main = "Triplot RDA Mericarp traits ~ Mainland/Island - scaling 2 - lc scores"
)
spe.sc1 <- scores(mainland_island_rda,
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

# Finch beak mericarp RDA ####
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


