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
    labs(title = expression(paste("Length (P = <0.001)"))) +
    labs(x = "Population", y = "Mean Length (mm)")

EM_length_mainland_gal
plot_length_mainland_gal <- plot(EM_length_mainland_gal, comparisons = T, plotit = F)

ggplot_length_mainland_gal <- ggplot(plot_length_mainland_gal, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) + 
    labs(title = expression(paste("Length (P = <0.001)"))) +
    labs(x = "Population", y = "Mean Length (mm)") 

length_violin <- ggplot(meri_length, aes(x = mainland_island, y = length, fill = mainland_island)) + 
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
          labs(x = "Population", y = "Length (mm)", title = "Length")
                            
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
    labs(title = expression(paste("Width (P = <0.001)"))) +
    labs(x = "Population", y = "Mean Width (mm)")

EM_width_mainland_gal
plot_width_mainland_gal <- plot(EM_width_mainland_gal, comparisons = T, plotit = F)

ggplot_width_mainland_gal <- ggplot(plot_width_mainland_gal, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = expression(paste("Width (P = <0.001)"))) +
    labs(x = "Population", y = "Mean Width (mm)")

width_violin <- ggplot(meri_width, aes(x = mainland_island, y = width, fill = mainland_island)) + 
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
    labs(x = "Population", y = "Width (mm)", title = "Width")

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
    labs(title = expression(paste("Depth (P = <0.001)"))) +
    labs(x = "Population", y = "Mean Depth (mm)")

plot_depth_mainland_gal <- plot(EM_depth_mainland_gal, comparisons = T, plotit = F)

ggplot_depth_mainland_gal <- ggplot(plot_depth_mainland_gal, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) + 
    labs(title = expression(paste("Depth (P = <0.001)"))) +
    labs(x = "Population", y = "Mean Depth (mm)")

depth_violin <- ggplot(meri_depth, aes(x = mainland_island, y = depth, fill = mainland_island)) + 
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
    labs(x = "Population", y = "Depth (mm)", title = "Depth")

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
    labs(title = expression(paste("Tip distance (P = 0.015)"))) +
    labs(x = "Population", y = "Spine Tip Distance (mm)")

plot_spine_mainland_gal <- plot(EM_tip_dist_mainland_gal, comparisons = T, plotit = F)

ggplot_spine_mainland_gal <- ggplot(plot_spine_mainland_gal, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = expression(paste("Tip distance (P = 0.0270)"))) +
    labs(x = "Population", y = "Spine Tip Distance (mm)")

tip_distance_violin <- ggplot(meri_tip_distance, aes(x = mainland_island, y = tip_distance, fill = mainland_island)) + 
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
    labs(x = "Population", y = "Spinte tip distance (mm)", title = "Spine Tip Distance")

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
    labs(title = expression(paste("Lower spines (P = 0.001)"))) +
    labs(x = "Population", y = "Lower spines")

plot_lower_mainland_gal <- plot(EM_lower_mainland_gal, comparisons = T, plotit = F)

ggplot_lower_mainland_gal <- ggplot(plot_lower_mainland_gal, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) + 
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 13, face = "bold"),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = expression(paste("Lower spines (P = 0.001)"))) +
    labs(x = "Population", y = "Lower spines")

lower_spines_violin <- ggplot(meri_lower_spines, aes(x = mainland_island, y = lower_spines, fill = mainland_island)) + 
    geom_violin(size = 1.5, trim = F) +
    scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73")) +
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          legend.position = "none",
          panel.background = element_rect(fill = NA)) + 
    labs(x = "Population", y = "Lower spines", title = "Lower Spines")

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
    labs(title = expression(paste("Model 1 (P = 0.2993)"))) +
    labs(x = "Population", y = "Petal Legnth (mm)")

flower_violin <- ggplot(flower, aes(x = mainland_island, y = petal_length, fill = mainland_island)) + 
    geom_violin(size = 1.5, trim = F) +
    scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73")) +
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          legend.position = "none",
          panel.background = element_rect(fill = NA)) + 
    labs(x = "Population", y = "Petal Length (mm)", title = "Petal Length")

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
    labs(title = expression(paste("Model 2 (P = <0.001)"))) +
    labs(x = "Population", y = "Petal length (mm)")

flower_violin2 <- ggplot(flower_galapagos_other, aes(x = galapagos_other, y = petal_length, fill = galapagos_other)) + 
    geom_violin(size = 1.5, trim = F) +
    scale_fill_manual(values = c("#85c0f9", "#009e73")) +
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10), 
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold"),
          text = element_text(family = "Noto Sans"),
          legend.position = "none",
          panel.background = element_rect(fill = NA)) + 
    labs(x = "Population", y = "Petal Length (mm)", title = "Petal Length")
    
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
violin_plots <- ggarrange(length_violin,
                          width_violin,
                          depth_violin,
                          tip_distance_violin,
                          lower_spines_violin,
                          flower_violin,
                          flower_violin2,
                          labels = c("A", "B", "C", "D","E", "F", "G"),
                          ncol = 3,
                          nrow = 3) + 
    theme(text = element_text(family = "Noto Sans"))


