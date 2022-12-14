# Script 6. Summary plots from the mean estimated for models ####

# 06_01 Univariate plots ####
# This scripts uses the emmean outputs from the univariate models.
# It also includes violin plots of the distributions of the data for each trait.
# I created a function for the plot appereance 
## Plot function for emmean summary ####

my_emmean_barplot <- function(emmean_plot, x, title, lab_x, lab_y){
  ggplot(emmean_plot, aes(x = x, y = the.emmean)) + 
    geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 6) +
    #geom_line(aes(group= "paired"), size = 1.5) +
    theme(axis.line = element_line(linetype = "solid", size = 1.5), 
          axis.title = element_text(size = 12, face = "bold"), 
          axis.text = element_text(size = 10),
          axis.text.x = element_text(size = 11), 
          plot.title = element_text(size = 12),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = title) + labs(x = lab_x, y = lab_y)
}
# For lower spines and flowers the emman is asigned as upperCL or lower CL
my_emmean_barplot2 <- function(emmean_plot, x, title, lab_x, lab_y){
  ggplot(emmean_plot, aes(x = x, y = the.emmean)) +
    geom_errorbar(size = 1.5, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 6) +
    scale_y_continuous(breaks = seq(-20,20,1/4)) +
    scale_x_discrete(name = " ") +
    theme(axis.line = element_line(linetype = "solid", size = 1.5),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(size = 11),
          plot.title = element_text(size = 12),
          text = element_text(family = "Noto Sans"),
          panel.background = element_rect(fill = NA)) +
    labs(title = title) + labs(x = lab_x, y = lab_y)
}


# Model 1: Mainland/Island comparison ####
## 06_01_01 Mericarps ####
### Length ####
#### No bioclimate variables ####
EM_length
plot_length <- plot(EM_length, comparisons = T, plotit = F)

ggplot_length <- my_emmean_barplot2(plot_length, plot_length$mainland_island,
                  "Length (P = <0.001)",
                  "Population",
                  "Length (mm)")

#### Including bioclimate variables ####
EM_length_bioclim
plot_length_bioclim <- plot(EM_length_bioclim, comparisons = T, plotit = F)

ggplot_length_bioclim <- my_emmean_barplot2(plot_length_bioclim, plot_length_bioclim$mainland_island,
                                   "Length (P = 0.060)",
                                   "Population",
                                   "Length (mm)")

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

#### No bioclimate variables ####
ggplot_width <- my_emmean_barplot2(plot_width, plot_width$mainland_island,
                                   "Width (P = 0.002)",
                                   "Population",
                                   "Width (mm)")

#### Including bioclimate variables ####
EM_width_bioclim
plot_width_bioclim <- plot(EM_width_bioclim, comparisons = T, plotit = F)

ggplot_width_bioclim <- my_emmean_barplot2(plot_width_bioclim, plot_width_bioclim$mainland_island,
                                           "Width (P = 0.096)",
                                           "Population",
                                           "Width (mm)")
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
#### No bioclimate variables ####
EM_depth
plot_depth <- plot(EM_depth, comparisons = T, plotit = F)

ggplot_depth <- my_emmean_barplot2(plot_depth, plot_depth$mainland_island,
                                  "Depth (P = <0.001)",
                                  "Population",
                                  "Depth (mm)")

#### Including bioclimate variables ####
EM_depth_bioclim
plot_depth_bioclim <- plot(EM_depth_bioclim, comparisons = T, plotit = F)

ggplot_depth_bioclim <- my_emmean_barplot2(plot_depth_bioclim, plot_depth_bioclim$mainland_island,
                                  "Depth (P = 0.014)",
                                  "Population",
                                  "Depth (mm)")

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
#### No bioclimate variables ####
EM_tip_dist
plot_spine <- plot(EM_tip_dist, comparisons = T, plotit = F)

ggplot_spine <- my_emmean_barplot2(plot_spine, plot_spine$mainland_island,
                                  "Spine Tip Distance (P = 0.447)",
                                  "Population",
                                  "Tip Distance (mm)")

#### Including bioclimate variables ####
EM_tip_dist_bioclim
plot_spine_bioclim <- plot(EM_tip_dist_bioclim, comparisons = T, plotit = F)

ggplot_spine_bioclim <- my_emmean_barplot2(plot_spine_bioclim, plot_spine_bioclim$mainland_island,
                                  "Spine Tip Distance (P = 0.682)",
                                  "Population",
                                  "Tip Distance (mm)")

# 06_05 PCA model plots ####
## No bioclimate variables ####
EM_PC1_mean
plot_PC1 <- plot(EM_PC1_mean, comparisons = T, plotit = F)

ggplot_PC1 <- my_emmean_barplot2(plot_PC1, plot_PC1$mainland_island,
                                    "Mericarp Size (P = <0.001)",
                                    "Population",
                                    "PC1 Scores")

## Mean bioclimate variables ####
EM_PC1_mean_bioclim
plot_PC1_mean_bioclim <- plot(EM_PC1_mean_bioclim, comparisons = T,
                              plotit = F)

ggplot_PC1_bioclim <- my_emmean_barplot2(plot_PC1_mean_bioclim, plot_PC1_mean_bioclim$mainland_island,
                                      "Mericarp Size (P = 0.231)",
                                      "Population",
                                      "PC1 Scores")

# 06_06 Mainland Island plots summary ####
## Mericarps ####
# Used in the main text
figure_mericarp_PC1 <- ggarrange(ggplot_PC1,
                                       ggplot_PC1_bioclim,
                                       labels = c("A", "B"),
                                       ncol = 2,
                                       nrow = 1) + 
  theme(text = element_text(family = "Noto Sans"))

figure_mericarp_PC1 <- annotate_figure(figure_mericarp_PC1,
                                 top = text_grob("Continent-Island Analysis     Bioclimate Factors Analysis",
                                                 color = "black", face = "bold", size = 16))

## Individual traits figure ####
# Supplemental figure with individual mericarp traits. From the PCA.
mericarp_ind_plot <- ggarrange(ggplot_length,
                                        ggplot_width,
                                        ggplot_depth,
                                        ggplot_spine,
                                        labels = c("A", "B", "C", "D"),
                                        ncol = 2,
                                        nrow = 2) + 
  theme(text = element_text(family = "Noto Sans"))

mericarp_ind_plot <- annotate_figure(mericarp_ind_plot,
                                 top = text_grob("Continent-Island Analysis",
                                                 color = "black", face = "bold", size = 16))

## Individual traits figure bioclimate ####
mericarp_ind_plot_bioclim <- ggarrange(ggplot_length_bioclim,
                               ggplot_width_bioclim,
                               ggplot_depth_bioclim,
                               ggplot_spine_bioclim,
                               labels = c("A", "B", "C", "D"),
                               ncol = 2,
                               nrow = 2) + 
  theme(text = element_text(family = "Noto Sans"))

mericarp_ind_plot_bioclim <- annotate_figure(mericarp_ind_plot_bioclim,
                                     top = text_grob("Bioclimate Factor Analysis",
                                                     color = "black", face = "bold", size = 16))


