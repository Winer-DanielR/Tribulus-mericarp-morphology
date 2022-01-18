# Script 5. Summary plots from the mean estimated for models ####

# 05_01 Univariate plots ####
# This scripts uses the emmean outputs from the univariate models.
# It also includes violin plots of the distributions of the data for each trait.

# Model 1: Mainland/Island comparison ####
## 05_01_01 Mericarps ####
### Length ####
#### Model plot ####
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

#### Mainland Galapos plot ####
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


#### Violing plot ####
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
#### Model plot ####
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

#### Mainland Galapagos plot ####
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

#### Violin plot ####
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

#### Model plot ####
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

#### Mainland Galapagos plot ####
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

#### Violin plot ####
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

#### Model plot ####
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

#### Mainland Galapagos plot ####
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

#### Violin plot ####
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
#### Model plot ####
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

#### Mainland Galapagos plot ####
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

#### Violin plot ####
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

## 05_01_02 Flower ####
### Petal length ####
#### Model plot ####
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

#### Violin plot ####
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

# 05_02 Model 2: Galapagos and other islands ####
## 05_02_01 Flower ####
### Petal length ####
#### Model plot ####
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

#### Violin plot ####
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

# 05_03 Mainland Island plots summary ####
figure_mericarp_model1 <- ggarrange(ggplot_length,
                                    ggplot_width,
                                    ggplot_depth,
                                    ggplot_spine,
                                    ggplot_lower,
                                    labels = c("A", "B", "C", "D","E"),
                                    ncol = 3,
                                    nrow = 2) + 
    theme(text = element_text(family = "Noto Sans"))

# 05_04 Mainland Galapagos Island plots summary ####
figure_mericarp_mainland_gal <- ggarrange(ggplot_length_mainland_gal,
                                    ggplot_width_mainland_gal,
                                    ggplot_depth_mainland_gal,
                                    ggplot_spine_mainland_gal,
                                    ggplot_lower_mainland_gal,
                                    labels = c("A", "B", "C", "D","E"),
                                    ncol = 3,
                                    nrow = 2) + 
    theme(text = element_text(family = "Noto Sans"))

# 05_03 Violin plots summary ####
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


