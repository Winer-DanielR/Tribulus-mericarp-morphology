
library(ggplot2)
library(ggvegan)
library(extrafont)
# Univariate plots ####
# Model 1: Mainland/Island comparison ####
## Mericarps ####
### Length ####
EM_length
plot_length <- plot(EM_length, comparisons = T, plotit = F)

ggplot_length <- ggplot(plot_length, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) + 
    labs(title = expression(paste("Length (", chi,"2 = 14.10, P = <0.001)"))) +
    labs(x = "Population", y = "Mean Length (mm)") 
                            
### Width ####
EM_width
plot_width <- plot(EM_width, comparisons = T, plotit = F)

ggplot_width <- ggplot(plot_width, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = expression(paste("Width (", chi,"2 = 12.43, P = <0.001)"))) +
    labs(x = "Population", y = "Mean Width (mm)")
### Depth ####
plot_depth <- plot(EM_depth, comparisons = T, plotit = F)

ggplot_depth <- ggplot(plot_depth, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) + 
    labs(title = expression(paste("Depth (", chi,"2 = 52.33, P = <0.001)"))) +
    labs(x = "Population", y = "Mean Depth (mm)")
### Tip distance ####
plot_spine <- plot(EM_tip_dist, comparisons = T, plotit = F)

ggplot_spine <- ggplot(plot_spine, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = expression(paste("Tip distance (", chi,"2 = 5.85, P = 0.015)"))) +
    labs(x = "Population", y = "Spine Tip Distance (mm)")
### Lower spines ####
plot_lower <- plot(EM_lower, comparisons = T, plotit = F)

ggplot_lower <- ggplot(plot_lower, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 13, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = expression(paste("Lower spines (", chi,"2 = 76.74, P = 0.001)"))) +
    labs(x = "Population", y = "Lower spines")
## Flower ####
plot_flower <- plot(EM_flower, comparisons = T, plotit = F)

ggplot_flower <- ggplot(plot_flower, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) + 
    labs(title = expression(paste("Model 1 (", chi,"= 1.0773, P = 0.2993"))) +
    labs(x = "Population", y = "Petal Legnth (mm)")
## Leaves ####
### Leaf length ####
plot_leaf <- plot(EM_leaf, comparisons = T, plotit = F)

ggplot_leaf <- ggplot(plot_leaf, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf Length
         
         (χ2 = 21.86, P = <0.001)
         ", x = "Population", 
         y = "Leaf Length (mm)")
### Leaflet length ####
plot_leaflet <- plot(EM_leaflet, comparisons = T, plotit = F)

ggplot_leaflet <- ggplot(plot_leaflet, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaflet Length
         
         (χ2 = 4.86, P = 0.027)
         ", x = "Population", 
         y = "Leaflet Length (mm)")
### Leaflet number ####
plot_leaflet_num <- plot(EM_leaflet_num, comparisons = T, plotit = F)

ggplot_leaflet_num <- ggplot(plot_leaflet_num, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaflet Number
         
         (χ2 = 39.61, P = <0.001)
         ", x = "Population", 
                                                      y = "Leaflet Number") 

# Model 2: Galapagos and other islands ####
## Flower ####
plot_flower2 <- plot(EM_flower2, comparisons = T, plotit = F)

ggplot_flower2 <- ggplot(plot_flower2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = expression(paste("Model 2 (", chi,"2 = 156.39, P = <0.001)"))) +
    labs(x = "Population", y = "Petal length (mm)")
    
## Leaf ####
### Leaf Length ####
plot_leaf2 <- plot(EM_leaf2, comparisons = T, plotit = F)

ggplot_leaf2 <- ggplot(plot_leaf2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf Length
         
         (χ2 = 20.39, P = <0.001)
         ", x = "Population", 
                                      y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet2 <- plot(EM_leaflet2, comparisons = T, plotit = F)

ggplot_leaflet2 <- ggplot(plot_leaflet2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaflet Length
         
         (χ2 = 17.20, P = <0.001)
         ", x = "Population", 
                                         y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num2 <- plot(EM_leaflet_num2, comparisons = T, plotit = F)

ggplot_leaflet_num2 <- ggplot(plot_leaflet_num2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaflet Number
         
         (χ2 = 1.8645, P = 0.1721)
         ", x = "Population", 
                                         y = "Leaflet Number")

# Model 3: Finch communities within Galapagos ####
## Mericarps ####
### Length ####
EM_length
plot_length2 <- plot(EM_length3, comparisons = T, plotit = F)

ggplot_length2 <- ggplot(plot_length2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Length
         
         (χ2 = 3.0175, P = 0.08237)
         ", x = "Absence        Presence", 
                                           y = "Mean Length (mm)") 

### Width ####
plot_width2 <- plot(EM_width3, comparisons = T, plotit = F)

ggplot_width2 <- ggplot(plot_width2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Width
         
         (χ2 = 0.2936, P = 0.5879)
         ", x = "Absence        Presence", 
                                          y = "Mean Width (mm)") 
### Depth ####
plot_depth2 <- plot(EM_depth3, comparisons = T, plotit = F)

ggplot_depth2 <- ggplot(plot_depth2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Depth
         
         (χ2 = 0.7951, P = 0.3726)
         ", x = "Absence        Presence", 
                                                      y = "Mean Depth (mm)") 

### Tip distance ####
plot_spine2 <- plot(EM_tip_dist3, comparisons = T, plotit = F)

ggplot_spine2 <- ggplot(plot_spine2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Tip Distance
         
         (χ2 = 1.3133, P = 0.2518)
         ", x = "Absence        Presence", 
                                                      y = "Spine Tip Distance (mm)") 

### Lower spines ####
plot_lower2 <- plot(EM_lower3, comparisons = T, plotit = F)

ggplot_lower2 <- ggplot(plot_lower2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Lower Spine
         
         (χ2 = 2.4872, P = 0.1148)
         ", x = "Absence        Presence", 
                                                      y = "Lower spines") 

## Flower ####
plot_flower3 <- plot(EM_flower3, comparisons = T, plotit = F)

ggplot_flower3 <- ggplot(plot_flower3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Petal Length
         
         (χ2 = 0.3459, P = 0.5564)
         ", x = "Absence        Presence", 
                                                      y = "Petal Length (mm)") 

## Leaves ####
### Leaf length ####
plot_leaf3 <- plot(EM_leaf3, comparisons = T, plotit = F)

ggplot_leaf3 <- ggplot(plot_leaf3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaf Length
         
         (χ2 = 1.8787, P = 0.1704)
         ", x = "Absence        Presence", 
                                                      y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet3 <- plot(EM_leaflet3, comparisons = T, plotit = F)

ggplot_leaflet3 <- ggplot(plot_leaflet3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaflet Length
         
         (χ2 = 2.0280, P = 0.1544)
         ", x = "Absence        Presence", 
                                                      y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num3 <- plot(EM_leaflet_num3, comparisons = T, plotit = F)

ggplot_leaflet_num3 <- ggplot(plot_leaflet_num3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = "Leaflet Number
         
         (χ2 = 0.1347, P = 0.7136)
         ", x = "Absence        Presence", 
                                                      y = "Leaflet Number") 
# # RDA plots ####
# # Model 1: Mainland/Island + year ####
# scl <- 2
# colvec <- c("cadetblue4", "chartreuse4")
# 
# ### Mericarps ####
# ####Biplot with vegan ####
# # Define factors mainland/island:
# with(mericarp, levels(mainland_island))
# 
# #Blank plot
# plot(meri_RDA_mainland,
#      type = "n",
#      scaling = scl,
#      main = "Triplot RDA Mericarp traits ~ Mainland/Island - scaling 2 - lc scores",
#      # xlim = c(-0.3, 0.3),
#      # ylim = c(-1,1),
#      font.lab = 2,
#      cex.axis = 1.5,
#      cex.lab = 1.5,
#      cex.main = 1.3,
#      frame.plot = F
#      )
# #Add points with colored factors
# with(mericarp, points(meri_RDA_mainland, display = c("lc"), col = "black", lwd = 2,
#                        scaling = scl, pch = 21, cex = 1.9, bg = colvec[mainland_island]))
# with(meri_RDA_mainland, legend("topright", legend = with(mericarp, levels(mainland_island)), bty = "n",
#                                col = "black", lwd = 2, pch = 21, pt.bg = colvec, cex = 1.5))
# #Add species
# text(meri_RDA_mainland, display = "species", scaling = scl, cex = 0.8, col = "darkcyan")
# # 
# # # #Ggplot RDA
# # # scores(meri_RDA_mainland)
# # # plot_scores <- scores(meri_RDA_mainland, display = c("lc"))
# # # plot_scores <- as.data.frame(plot_scores)
# # # ggplot(plot_scores) +
# # #  aes(x = RDA1, y = PC1) +
# # #  geom_point(size = 2)
# # 
# # #GGvegan
# # #ggvegan::valid_layers(meri_RDA_mainland)
# # # autoplot(x, axes = c(1,2), layers = c("RDA1", "PC1"))
# # # x <- fortify(meri_RDA_mainland, display = c("lc", "bp"))
# # # ggplot(x, aes(RDA1, PC1)) +
# # # geom_point()
# # 
# # ### Leaves ####
# ####Biplot with vegan ####
# # Define factors mainland/island:
# with(leaf_islands, levels(galapagos_other))
# 
# #Blank plot
# plot(leaf_RDA_mainland,
#      type = "n",
#      scaling = 2,
#      main = "Triplot RDA Leaves: trait ~ Galapagos/Other - scaling 2 - lc scores",
#      #xlim = c(-0.5, 0.5),
#      #ylim = c(-3.5,2),
#      font.lab = 2,
#      cex.axis = 1.5,
#      cex.lab = 1.5,
#      cex.main = 1.3,
#      frame.plot = F
# )
# #Add points with colored factors
# with(leaf, points(leaf_RDA_Galapagos, display = c("lc"), col = "black", lwd = 2,
#                             scaling = 2, pch = 21, cex = 1.9, bg = colvec[mainland_island]))
# with(leaf_RDA_mainland, legend("topright", legend = with(leaf, levels(mainland_island)), bty = "n",
#                                   col = "black", lwd = 2, pch = 21, pt.bg = colvec, cex = 1.5))
# #Add species
# text(leaf_RDA_mainland, display = "species", scaling = 2, cex = 0.8, col = "darkcyan")
# # 
# 
# ## Model 3: Finch Beak + year ####
# scl <- 2
# colvec3 <- c("darkcyan", "darkgoldenrod3")
# 
# ### Mericarps ####
# ####Biplot with vegan ####
# # Define factors mainland/island:
# with(mericarp_gal, levels(finch_beak))
# 
# #Blank plot
# plot(meri_RDA_beak, 
#      type = "n",
#      scaling = scl,
#      main = "Triplot RDA Mericarp traits ~ Finch Beak - scaling 2 - lc scores",
#      xlim = c(-0.3, 0.3),
#      #ylim = c(-1,1),
#      font.lab = 2,
#      cex.axis = 1.5,
#      cex.lab = 1.5,
#      cex.main = 1.3,
#      frame.plot = F
# )
# #Add points with colored factors
# with(mericarp_gal, points(meri_RDA_beak, display = c("lc"), col = "black", lwd = 2,
#                       scaling = scl, pch = 21, cex = 1.9, bg = colvec3[finch_beak]))
# with(meri_RDA_beak, legend("topright", legend = with(mericarp_gal, levels(finch_beak)), bty = "n",
#                                col = "black", lwd = 2, pch = 21, pt.bg = colvec3, cex = 1.5))
# #Add species
# text(meri_RDA_beak, display = "species", scaling = scl, cex = 0.8, col = "darkcyan")
# 
# ### Leaves ####
# ####Biplot with vegan ####
# # Define factors mainland/island:
# with(leaf_gal, levels(finch_beak))
# 
# #Blank plot
# plot(leaf_RDA_beak, 
#      type = "n",
#      scaling = 2,
#      main = "Triplot RDA Leaves traits ~ Finch Beak - scaling 2 - lc scores",
#      xlim = c(-0.3, 0.3),
#      ylim = c(-1,1),
#      font.lab = 2,
#      cex.axis = 1.5,
#      cex.lab = 1.5,
#      cex.main = 1.3,
#      frame.plot = F
# )
# #Add points with colored factors
# with(leaf_gal, points(flower_RDA_beak, display = c("lc"), col = "black", lwd = 2,
#                         scaling = 2, pch = 21, cex = 1.9, bg = colvec3[finch_beak]))
# with(leaf_RDA_beak, legend("topright", legend = with(leaf_gal, levels(finch_beak)), bty = "n",
#                              col = "black", lwd = 2, pch = 21, pt.bg = colvec3, cex = 1.5))
# #Add species
# text(leaf_RDA_beak, display = "species", scaling = 2, cex = 0.8, col = "darkcyan")
# 
# 
