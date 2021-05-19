
library(ggplot2)
library(ggvegan)

# Univariate plots ####
# Model 1: Mainland/Island comparison ####
## Mericarps ####
### Length ####
EM_length
plot_length <- plot(EM_length, comparisons = T, plotit = F)

ggplot_length <- ggplot(plot_length, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Mericarp: Length Means", x = "Population", 
         y = "Mean Length (mm)")
### Width ####
EM_width
plot_width <- plot(EM_width, comparisons = T, plotit = F)

ggplot_width <- ggplot(plot_width, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Mericarp: Width Means", x = "Population", 
                                          y = "Mean Width (mm)")
### Depth ####
plot_depth <- plot(EM_depth, comparisons = T, plotit = F)

ggplot_depth <- ggplot(plot_depth, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Mericarp: Depth Means", x = "Population", 
         y = "Mean Depth (mm)")
### Spine length ####
plot_spine <- plot(EM_spine, comparisons = T, plotit = F)

ggplot_spine <- ggplot(plot_spine, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Mericarp: Spine Length Means", x = "Population", 
         y = "Mean Spine Length (mm)")
### Lower spines ####
plot_lower <- plot(EM_lower1, comparisons = T, plotit = F)

ggplot_lower <- ggplot(plot_lower, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Mericarp: Lower Spines", x = "Population", 
         y = "Lower spines")
## Flower ####
plot_flower <- plot(EM_flower, comparisons = T, plotit = F)

ggplot_flower <- ggplot(plot_flower, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Flower: Petal length", x = "Population", 
         y = "Petal Legnth (mm)")
## Leaves ####
### Leaf length ####
plot_leaf <- plot(EM_leaf, comparisons = T, plotit = F)

ggplot_leaf <- ggplot(plot_leaf, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Leaf: Leaf Length", x = "Population", 
         y = "Leaf Length (mm)")
### Leaflet length ####
plot_leaflet <- plot(EM_leaflet, comparisons = T, plotit = F)

ggplot_leaflet <- ggplot(plot_leaflet, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Leaf: Leaflet Length", x = "Population", 
         y = "Leaflet Length (mm)")
### Leaflet number ####
plot_leaflet_num <- plot(EM_leaflet_num, comparisons = T, plotit = F)

ggplot_leaflet_num <- ggplot(plot_leaflet_num, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) + 
    labs(title = "Leaf: Leaflet Number", x = "Population", 
                                                      y = "Leaflet Number") 

# Model 2: Galapagos and other islands ####
## Flower ####
plot_flower2 <- plot(EM_flower2, comparisons = T, plotit = F)

ggplot_flower2 <- ggplot(plot_flower2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Flower: Petal Length", x = "Population", 
                                         y = "Petal Length (mm)") 

## Leaf ####
### Leaf Length ####
plot_leaf2 <- plot(EM_leaf2, comparisons = T, plotit = F)

ggplot_leaf2 <- ggplot(plot_leaf2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf: Leaf Length", x = "Population", 
                                      y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet2 <- plot(EM_leaflet2, comparisons = T, plotit = F)

ggplot_leaflet2 <- ggplot(plot_leaflet2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf: Leaflet Length", x = "Population", 
                                         y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num2 <- plot(EM_leaflet_num2, comparisons = T, plotit = F)

ggplot_leaflet_num2 <- ggplot(plot_leaflet_num2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf: Leaflet Number", x = "Population", 
                                         y = "Leaflet Number")

# Model 3: Finch communities within Galapagos ####
## Mericarps ####
### Length ####
EM_length
plot_length2 <- plot(EM_length3, comparisons = T, plotit = F)

ggplot_length2 <- ggplot(plot_length2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Mericarp: Length Means", x = "No Large Beak Finch        Large Beak Finch", 
                                           y = "Mean Length (mm)") 

### Width ####
plot_width2 <- plot(EM_width3, comparisons = T, plotit = F)

ggplot_width2 <- ggplot(plot_width2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Mericarp: Width Means", x = "No Large Beak Finch        Large Beak Finch", 
                                          y = "Mean Width (mm)") 
### Depth ####
plot_depth2 <- plot(EM_depth3, comparisons = T, plotit = F)

ggplot_depth2 <- ggplot(plot_depth2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +    labs(title = "Mericarp: Depth Means", x = "No Large Beak Finch        Large Beak Finch", 
                                                      y = "Mean Depth (mm)") 

### Spine length ####
plot_spine2 <- plot(EM_spine3, comparisons = T, plotit = F)

ggplot_spine2 <- ggplot(plot_spine2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Mericarp: Spine Length Means", x = "No Large Beak Finch        Large Beak Finch", 
                                                      y = "Mean Spine Length (mm)") 

### Lower spines ####
plot_lower2 <- plot(EM_lower4, comparisons = T, plotit = F)

ggplot_lower2 <- ggplot(plot_lower2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Mericarp: Lower Spine", x = "No Large Beak Finch        Large Beak Finch", 
                                                      y = "Lower spine Presence/Absence") 

## Flower ####
plot_flower3 <- plot(EM_flower3, comparisons = T, plotit = F)

ggplot_flower3 <- ggplot(plot_flower3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Flower: Petal Length", x = "No Large Beak Finch        Large Beak Finch", 
                                                      y = "Petal Length (mm)") 

## Leaves ####
### Leaf length ####
plot_leaf3 <- plot(EM_leaf3, comparisons = T, plotit = F)

ggplot_leaf3 <- ggplot(plot_leaf3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf: Leaf Length", x = "No Large Beak Finch        Large Beak Finch", 
                                                      y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet3 <- plot(EM_leaflet3, comparisons = T, plotit = F)

ggplot_leaflet3 <- ggplot(plot_leaflet3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf: Leaflet Length", x = "No Large Beak Finch        Large Beak Finch", 
                                                      y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num3 <- plot(EM_leaflet_num3, comparisons = T, plotit = F)

ggplot_leaflet_num3 <- ggplot(plot_leaflet_num3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 10) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 17), 
          axis.text.x = element_text(size = 17), 
          plot.title = element_text(size = 20), 
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf: Leaflet Number", x = "No Large Beak Finch        Large Beak Finch", 
                                                      y = "Leaflet Number") 
# RDA plots ####
## Model 1: Mainland/Island + year ####
scl <- 2
colvec <- c("cadetblue4", "chartreuse4")

### Mericarps ####
####Biplot with vegan ####
# Define factors mainland/island:
with(mericarp, levels(mainland_island))

#Blank plot
plot(meri_RDA_mainland, 
     type = "n",
     scaling = scl,
     main = "Triplot RDA Mericarp traits ~ Mainland/Island - scaling 2 - lc scores",
     # xlim = c(-0.3, 0.3),
     # ylim = c(-1,1),
     font.lab = 2,
     cex.axis = 1.5,
     cex.lab = 1.5,
     cex.main = 1.8,
     frame.plot = F
     )
#Add points with colored factors
with(mericarp, points(meri_RDA_mainland, display = c("lc"), col = "black", lwd = 2,
                       scaling = scl, pch = 21, cex = 1.9, bg = colvec[mainland_island]))
with(meri_RDA_mainland, legend("topright", legend = with(mericarp, levels(mainland_island)), bty = "n",
                               col = "black", lwd = 2, pch = 21, pt.bg = colvec, cex = 1.5))
#Add species
text(meri_RDA_mainland, display = "species", scaling = scl, cex = 0.8, col = "darkcyan")

# #Ggplot RDA
# scores(meri_RDA_mainland)
# plot_scores <- scores(meri_RDA_mainland, display = c("lc"))
# plot_scores <- as.data.frame(plot_scores)
# ggplot(plot_scores) +
#  aes(x = RDA1, y = PC1) +
#  geom_point(size = 2)

#GGvegan
#ggvegan::valid_layers(meri_RDA_mainland)
# autoplot(x, axes = c(1,2), layers = c("RDA1", "PC1"))
# x <- fortify(meri_RDA_mainland, display = c("lc", "bp"))
# ggplot(x, aes(RDA1, PC1)) +
# geom_point()

### Flowers ####
####Biplot with vegan ####
# Define factors mainland/island:
with(flower, levels(mainland_island))

#Blank plot
plot(flower_RDA_mainland, 
     type = "n",
     scaling = 2,
     main = "Triplot RDA Flower trait ~ Mainland/Island - scaling 2 - lc scores",
     xlim = c(-0.5, 0.5),
     ylim = c(-0.8,0.8),
     font.lab = 2,
     cex.axis = 1.5,
     cex.lab = 1.5,
     cex.main = 1.8,
     frame.plot = F
)
#Add points with colored factors
with(flower, points(flower_RDA_mainland, display = c("lc"), col = "black", lwd = 2,
                      scaling = 2, pch = 21, cex = 1.9, bg = colvec[mainland_island]))
with(flower_RDA_mainland, legend("topright", legend = with(flower, levels(mainland_island)), bty = "n",
                               col = "black", lwd = 2, pch = 21, pt.bg = colvec, cex = 1.5))
#Add species
text(flower_RDA_mainland, display = "species", scaling = 2, cex = 0.8, col = "darkcyan")

### Leaves ####
####Biplot with vegan ####
# Define factors mainland/island:
with(leaf, levels(mainland_island))

#Blank plot
plot(leaf_RDA_mainland, 
     type = "n",
     scaling = 2,
     main = "Triplot RDA Leaves traits ~ Mainland/Island - scaling 2 - lc scores",
     # xlim = c(-0.5, 0.5),
     # ylim = c(-1.2,1.2),
     font.lab = 2,
     cex.axis = 1.5,
     cex.lab = 1.5,
     cex.main = 1.8,
     frame.plot = F
)
#Add points with colored factors
with(leaf, points(leaf_RDA_mainland, display = c("lc"), col = "black", lwd = 2,
                    scaling = 2, pch = 21, cex = 1.9, bg = colvec[mainland_island]))
with(leaf_RDA_mainland, legend("topright", legend = with(leaf, levels(mainland_island)), bty = "n",
                                 col = "black", lwd = 2, pch = 21, pt.bg = colvec, cex = 1.5))
#Add species
text(leaf_RDA_mainland, display = "species", scaling = 2, cex = 0.8, col = "darkcyan")

# Model 2: Galapagos/Other ####
scl <- 2
colvec2 <- c("darkorchid3", "darksalmon")

### Flowers ####
####Biplot with vegan ####
# Define factors mainland/island:
with(flower_islands, levels(galapagos_other))

#Blank plot
plot(flower_RDA_Galapagos, 
     type = "n",
     scaling = 2,
     main = "Triplot RDA Flower: trait ~ Galapagos/Other - scaling 2 - lc scores",
     xlim = c(-0.5, 0.5),
     ylim = c(-2,2),
     font.lab = 2,
     cex.axis = 1.5,
     cex.lab = 1.5,
     cex.main = 1.8,
     frame.plot = F
)
#Add points with colored factors
with(flower_islands, points(flower_RDA_Galapagos, display = c("lc"), col = "black", lwd = 2,
                  scaling = 2, pch = 21, cex = 1.9, bg = colvec2[galapagos_other]))
with(flower_RDA_Galapagos, legend("topright", legend = with(flower_islands, levels(galapagos_other)), bty = "n",
                               col = "black", lwd = 2, pch = 21, pt.bg = colvec2, cex = 1.5))
#Add species
text(flower_RDA_Galapagos, display = "species", scaling = 2, cex = 0.8, col = "darkcyan")

### Leaves ####
####Biplot with vegan ####
# Define factors mainland/island:
with(leaf_islands, levels(galapagos_other))

#Blank plot
plot(leaf_RDA_Galapagos, 
     type = "n",
     scaling = 2,
     main = "Triplot RDA Leaves: trait ~ Galapagos/Other - scaling 2 - lc scores",
     xlim = c(-0.5, 0.5),
     ylim = c(-3.5,2),
     font.lab = 2,
     cex.axis = 1.5,
     cex.lab = 1.5,
     cex.main = 1.8,
     frame.plot = F
)
#Add points with colored factors
with(leaf_islands, points(leaf_RDA_Galapagos, display = c("lc"), col = "black", lwd = 2,
                            scaling = 2, pch = 21, cex = 1.9, bg = colvec2[galapagos_other]))
with(leaf_RDA_Galapagos, legend("topright", legend = with(leaf_islands, levels(galapagos_other)), bty = "n",
                                  col = "black", lwd = 2, pch = 21, pt.bg = colvec2, cex = 1.5))
#Add species
text(leaf_RDA_Galapagos, display = "species", scaling = 2, cex = 0.8, col = "darkcyan")

