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

ggplot_length <- my_emmean_barplot(plot_length, plot_length$mainland_island,
                  "Length (P = <0.001)",
                  "Population",
                  "Length (mm)")

#### Including bioclimate variables ####
EM_length_bioclim
plot_length_bioclim <- plot(EM_length_bioclim, comparisons = T, plotit = F)

ggplot_length_bioclim <- my_emmean_barplot(plot_length_bioclim, plot_length_bioclim$mainland_island,
                                   "Length (P = 0.414)",
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
ggplot_width <- my_emmean_barplot(plot_width, plot_width$mainland_island,
                                   "Width (P = <0.001)",
                                   "Population",
                                   "Width (mm)")

#### Including bioclimate variables ####
EM_width_bioclim
plot_width_bioclim <- plot(EM_width_bioclim, comparisons = T, plotit = F)

ggplot_width_bioclim <- my_emmean_barplot(plot_width_bioclim, plot_width_bioclim$mainland_island,
                                           "Width (P = 0.137)",
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

ggplot_depth <- my_emmean_barplot(plot_depth, plot_depth$mainland_island,
                                  "Depth (P = <0.001)",
                                  "Population",
                                  "Depth (mm)")

#### Including bioclimate variables ####
EM_depth_bioclim
plot_depth_bioclim <- plot(EM_depth_bioclim, comparisons = T, plotit = F)

ggplot_depth_bioclim <- my_emmean_barplot(plot_depth_bioclim, plot_depth_bioclim$mainland_island,
                                  "Depth (P = 0.017)",
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

ggplot_spine <- my_emmean_barplot(plot_spine, plot_spine$mainland_island,
                                  "Spine Tip Distance (P = 0.015)",
                                  "Population",
                                  "Tip Distance (mm)")

#### Including bioclimate variables ####
EM_tip_dist_bioclim
plot_spine_bioclim <- plot(EM_tip_dist_bioclim, comparisons = T, plotit = F)

ggplot_spine_bioclim <- my_emmean_barplot(plot_spine_bioclim, plot_spine_bioclim$mainland_island,
                                  "Spine Tip Distance (P = 0.439)",
                                  "Population",
                                  "Tip Distance (mm)")

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

#### Tip distance Lower spines comparison ####
##### No bioclimate variables #####
EM_tip_dist_lower
plot_tip_lower <- plot(EM_tip_dist_lower, comparisons = T, plotit = F)

ggplot_spine_lower <- my_emmean_barplot(plot_tip_lower, plot_tip_lower$lower_spines,
                                          "Spine Tip Distance (P = <0.001)",
                                          "Lower Spines",
                                          "Tip Distance (mm)")

##### Bioclimate Variables #####
EM_tip_dist_lower_bioclim
plot_tip_lower_bioclim <- plot(EM_tip_dist_lower_bioclim, comparisons = T, plotit = F)

ggplot_spine_lower_bioclim <- my_emmean_barplot(plot_tip_lower_bioclim, plot_tip_lower_bioclim$lower_spines,
                                        "Spine Tip Distance (P = <0.001)",
                                        "Lower Spines",
                                        "Tip Distance (mm)")

### Lower spines ####
#### No bioclimate variables ####
EM_lower
plot_lower <- plot(EM_lower, comparisons = T, plotit = F)

ggplot_lower <- my_emmean_barplot2(plot_lower, plot_lower$mainland_island,
                                   "Lower Spines (P = <0.001)",
                                   "Population",
                                   "Lower Spines")

#### Including bioclimate variables #####
EM_lower_bioclim
plot_lower_bioclim <- plot(EM_lower_bioclim, comparisons = T, plotit = F)

ggplot_lower_bioclim <- my_emmean_barplot2(plot_lower_bioclim, plot_lower_bioclim$mainland_island,
                                   "Lower Spines (P = 0.216)",
                                   "Population",
                                   "Lower Spines")

## 06_01_02 Flower ####
### Petal length ####
#### Model plot ####
EM_flower
plot_flower <- plot(EM_flower, comparisons = T, plotit = F)

ggplot_flower <- my_emmean_barplot2(plot_flower, plot_flower$mainland_island,
                                    "Model 1 (P = 0.239)",
                                    "Population",
                                    "Petal Length (mm)")

#### Including Bioclimate variables #####
EM_flower_bioclim
plot_flower_bioclim <- plot(EM_flower_bioclim, comparisons = T, plotit = F)

ggplot_flower_bioclim <- my_emmean_barplot2(plot_flower_bioclim, plot_flower_bioclim$mainland_island,
                                    "Model 1 (P = 0.001)",
                                    "Population",
                                    "Petal Length (mm)")
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

# 06_02 Model 2: Galapagos and other islands ####
## 06_02_01 Flower ####
### Petal length ####
#### No bioclimate variables ####
EM_flower2
plot_flower2 <- plot(EM_flower2, comparisons = T, plotit = F)
ggplot_flower2 <- my_emmean_barplot2(plot_flower2, plot_flower2$galapagos_other,
                                    "Model 2 (P = <0.001)",
                                    "Population",
                                    "Petal Length (mm)")

#### Including bioclimate variables ####
EM_flower2_bioclim
plot_flower2_bioclim <- plot(EM_flower2_bioclim, comparisons = T, plotit = F)
ggplot_flower2_bioclim <- my_emmean_barplot2(plot_flower2_bioclim, plot_flower2_bioclim$galapagos_other,
                                     "Model 2 (P = <0.001)",
                                     "Population",
                                     "Petal Length (mm)")
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


# 06_03 Model PCA plots ####

## Summary plots ####
#### Mainland Galapos plot ####
EM_PC1
plot_PC1 <- plot(EM_PC1, comparisons = T, plotit = F)

ggplot_PC1 <- ggplot(plot_PC1, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) + 
  labs(title = expression(paste("Mericarp Size (P = <0.001)"))) +
  labs(x = "Population", y = "Mericarp Size (PC1)") 

EM_PC1
plot_PC2 <- plot(EM_PC_climate, comparisons = T, plotit = F)

ggplot_PC2 <- ggplot(plot_PC2, aes(x = mainland_island, y = the.emmean)) + 
  geom_errorbar(size = 1.5, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
  geom_point(size = 6) + 
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA)) + 
  labs(title = expression(paste("Mericarp Size (P = <0.001)"))) +
  labs(x = "Population", y = "Mericarp Size (PC1)") 

#### Violing plot ####
PC1_violin <- ggplot(mericarp_scaled_PC, aes(x = mainland_island, y = PC1, fill = mainland_island)) + 
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
  labs(x = "Population", y = "Mericarp Size (PC1)", title = "Mericarp Size")

# 06_04 PCA plots ####
### Eigenvalues plot ####
fviz_eig(mericarp_ind_pca)

### Individual PCA ####
# It uses mericarp_NA as habillage because lower spines there is a factor.
fviz_pca_ind(mericarp_ind_pca, repel = T, geom = c("point"), habillage = mericarp_NA$mainland_island, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)
fviz_pca_var(mericarp_ind_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
fviz_pca_biplot(mericarp_ind_pca, repel = T,
                geom = c("point"),
                habillage = mericarp_NA$mainland_island,
                col.var = "black",
                addEllipses = T
)

## Theme individual Biplot ####
### Mainland island comparison ####
biplot2 <- fviz_pca_biplot(mericarp_size_pca,
                           # Fill individuals by groups
                           title = "Mericarps
                           ",
                           geom.ind = "point",
                           pointshape = c(21),
                           pointsize = 4,
                           stroke = 0.5,
                           fill.ind = mericarp_NA_wozero$mainland_island,
                           col.ind = "black",
                           # Color variable by groups
                           legend.title = "Islands",
                           repel = T,
                           col.var = "black", 
                           labelsize = 5,
                           addEllipses = T,
                           palette = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73"),
                           
) + theme_transparent() + 
  scale_color_manual(values = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73")) +
  # PCA theme, adds custom font and sizes that matches the other plots    
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        text = element_text(family = "Noto Sans"),
        legend.text = element_text(size = 12, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(fill = NA, size = 0))

biplot2

## Theme individual Variable plots ####

var2 <- fviz_pca_var(mericarp_ind_pca,
                     col.var = "contrib",
                     title = "Variables contribution
                           ",
                     gradient.cols = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73"),
                     repel = TRUE,
                     legend.title = "Contribution"
) +
  theme_transparent() +
  # PCA theme, adds custom font and sizes that matches the other plots    
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        text = element_text(family = "Noto Sans"),
        legend.text = element_text(size = 12, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = NA, size = 0))
var2


# 06_05 PCA model plots ####
## No bioclimate variables ####
EM_PC1
plot_PC1 <- plot(EM_PC1, comparisons = T, plotit = F)

ggplot_PC1 <- my_emmean_barplot(plot_PC1, plot_PC1$mainland_island,
                                    "Mericarp Size (P = <0.001)",
                                    "Population",
                                    "PC1 Scores")

## Mean bioclimate variables ####
EM_PC1_mean_bioclim
plot_PC1_mean_bioclim <- plot(EM_PC1_mean_bioclim, comparisons = T,
                              plotit = F)

ggplot_PC1_mean <- my_emmean_barplot2(plot_PC1_mean_bioclim, plot_PC1_mean_bioclim$mainland_island,
                                      "Mericarp Size (P = 0.231)",
                                      "Population",
                                      "PC1 Scores")

## Including bioclimate variables #####
EM_PC1_bioclim
plot_PC1_bioclim <- plot(EM_PC1_bioclim, comparisons = T, plotit = F)

ggplot_PC1_bioclim <- my_emmean_barplot(plot_PC1_bioclim, plot_PC1_bioclim$mainland_island,
                                "Mericarp Size (P = 0.222)",
                                "Population",
                                "PC1 Scores")



# 06_06 Mainland Island plots summary ####
## Mericarps ####
# Used in the main text
figure_mericarp_PC1_lower <- ggarrange(ggplot_PC1,
                                       ggplot_lower,
                                       labels = c("A", "B"),
                                       ncol = 2,
                                       nrow = 1) + 
  theme(text = element_text(family = "Noto Sans"))

figure_mericarp_PC1_lower <- annotate_figure(figure_mericarp_PC1_lower,
                                 top = text_grob("Continent-Island Analysis",
                                                 color = "black", face = "bold", size = 16))

bioclim_mericarp_PC1_lower <- ggarrange(ggplot_PC1_bioclim,
                                       ggplot_lower_bioclim,
                                       labels = c("C", "D"),
                                       ncol = 2,
                                       nrow = 1) + 
  theme(text = element_text(family = "Noto Sans"))

bioclim_mericarp_PC1_lower <- annotate_figure(bioclim_mericarp_PC1_lower, 
                                         top = text_grob("Plus Bioclimate Factors Analysis",
                                                         color = "black", face = "bold", size = 16))

figure_PC1_lower_complete <- ggarrange(figure_mericarp_PC1_lower,
                                       bioclim_mericarp_PC1_lower,
                                    ncol = 1,
                                    nrow = 2)

## Flowers ####
# Flowers have model 1 and 2
figure_flower <- ggarrange(ggplot_flower,
                           ggplot_flower2,
                           #ggplot_flower_bioclim,
                           #ggplot_flower2_bioclim,
                           labels = c("A", "B"),
                           ncol = 2,
                           nrow = 1)

figure_flower <- annotate_figure(figure_flower,
                                 top = text_grob("Continent-Island Analysis",
                                                 color = "black", face = "bold", size = 16))

figure_flower_bioclim <- ggarrange(ggplot_flower_bioclim,
                           ggplot_flower2_bioclim,
                           labels = c("C", "D"),
                           ncol = 2,
                           nrow = 1)

figure_flower_bioclim <- annotate_figure(figure_flower_bioclim, 
                                 top = text_grob("Plus Bioclimate Factors Analysis",
                                                 color = "black", face = "bold", size = 16))

figure_flower_complete <- ggarrange(figure_flower,
                                    figure_flower_bioclim,
                                    ncol = 1,
                                    nrow = 2)

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


# 11_10 Violin plots summary ####
# Supplmental figure with data distribution of all traits.
violin_plots <- ggarrange(length_violin,
                          width_violin,
                          depth_violin,
                          tip_distance_violin,
                          #lower_spines_violin,
                          flower_violin,
                          flower_violin2,
                          labels = c("A", "B", "C", "D","E", "F", "G"),
                          ncol = 3,
                          nrow = 2) + 
  theme(text = element_text(family = "Noto Sans"))
