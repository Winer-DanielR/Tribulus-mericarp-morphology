
library(ggplot2)

# Univariate plots ####
# Model 1: Mainland/Island comparison ####
## Mericarps ####
### Length ####
EM_length
plot_length <- plot(EM_length, comparisons = T, plotit = F)

ggplot_length <- ggplot(plot_length, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
    axis.text = element_text(size = 14), 
    plot.title = element_text(size = 16)) +labs(title = "Mericarp: Length Means", x = "Population", 
    y = "Mean Length (mm)") 

### Width ####
EM_width
plot_width <- plot(EM_width, comparisons = T, plotit = F)

ggplot_width <- ggplot(plot_width, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Width Means", x = "Population", 
                                                      y = "Mean Width (mm)") 

### Depth ####
plot_depth <- plot(EM_depth, comparisons = T, plotit = F)

ggplot_depth <- ggplot(plot_depth, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Depth Means", x = "Population", 
                                                      y = "Mean Depth (mm)") 

### Spine length ####
plot_spine <- plot(EM_spine, comparisons = T, plotit = F)

ggplot_spine <- ggplot(plot_spine, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Spine Length Means", x = "Population", 
                                                      y = "Mean Spine Length (mm)") 

### Lower spines ####
plot_lower <- plot(EM_lower1, comparisons = T, plotit = F)

ggplot_lower <- ggplot(plot_lower, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Lower Spine", x = "Population", 
                                                      y = "Lower spine Presence/Absence") 

## Flower ####
plot_flower <- plot(EM_flower, comparisons = T, plotit = F)

ggplot_flower <- ggplot(plot_flower, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Flower: Petal Length", x = "Population", 
                                                      y = "Petal Length (mm)") 

## Leaves ####
### Leaf length ####
plot_leaf <- plot(EM_leaf, comparisons = T, plotit = F)

ggplot_leaf <- ggplot(plot_leaf, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaf Length", x = "Population", 
                                                      y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet <- plot(EM_leaflet, comparisons = T, plotit = F)

ggplot_leaflet <- ggplot(plot_leaflet, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaflet Length", x = "Population", 
                                                      y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num <- plot(EM_leaflet_num, comparisons = T, plotit = F)

ggplot_leaflet_num <- ggplot(plot_leaflet_num, aes(x = mainland_island, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaflet Number", x = "Population", 
                                                      y = "Leaflet Number") 

# Model 2: Galapagos and other islands ####
## Flower ####
plot_flower2 <- plot(EM_flower2, comparisons = T, plotit = F)

ggplot_flower2 <- ggplot(plot_flower2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Flower: Petal Length", x = "Population", 
                                                      y = "Petal Length (mm)") 

## Leaf ####
### Leaf Length ####
plot_leaf2 <- plot(EM_leaf2, comparisons = T, plotit = F)

ggplot_leaf2 <- ggplot(plot_leaf2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaf Length", x = "Population", 
                                                      y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet2 <- plot(EM_leaflet2, comparisons = T, plotit = F)

ggplot_leaflet2 <- ggplot(plot_leaflet2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaflet Length", x = "Population", 
                                                      y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num2 <- plot(EM_leaflet_num2, comparisons = T, plotit = F)

ggplot_leaflet_num2 <- ggplot(plot_leaflet_num2, aes(x = galapagos_other, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaflet Number", x = "Population", 
                                                      y = "Leaflet Number")

# Model 3: Finch communities within Galapagos ####
## Mericarps ####
### Length ####
EM_length
plot_length2 <- plot(EM_length3, comparisons = T, plotit = F)

ggplot_length2 <- ggplot(plot_length2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Length Means", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Mean Length (mm)") 

### Width ####
plot_width2 <- plot(EM_width3, comparisons = T, plotit = F)

ggplot_width2 <- ggplot(plot_width2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Width Means", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Mean Width (mm)") 
### Depth ####
plot_depth2 <- plot(EM_depth3, comparisons = T, plotit = F)

ggplot_depth2 <- ggplot(plot_depth2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Depth Means", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Mean Depth (mm)") 

### Spine length ####
plot_spine2 <- plot(EM_spine3, comparisons = T, plotit = F)

ggplot_spine2 <- ggplot(plot_spine2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Spine Length Means", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Mean Spine Length (mm)") 

### Lower spines ####
plot_lower2 <- plot(EM_lower4, comparisons = T, plotit = F)

ggplot_lower2 <- ggplot(plot_lower2, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Mericarp: Lower Spine", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Lower spine Presence/Absence") 

## Flower ####
plot_flower3 <- plot(EM_flower3, comparisons = T, plotit = F)

ggplot_flower3 <- ggplot(plot_flower3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Flower: Petal Length", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Petal Length (mm)") 

## Leaves ####
### Leaf length ####
plot_leaf3 <- plot(EM_leaf3, comparisons = T, plotit = F)

ggplot_leaf3 <- ggplot(plot_leaf3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaf Length", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Leaf Length (mm)") 

### Leaflet length ####
plot_leaflet3 <- plot(EM_leaflet3, comparisons = T, plotit = F)

ggplot_leaflet3 <- ggplot(plot_leaflet3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = upper.CL, ymin = lower.CL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaflet Length", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Leaflet Length (mm)") 

### Leaflet number ####
plot_leaflet_num3 <- plot(EM_leaflet_num3, comparisons = T, plotit = F)

ggplot_leaflet_num3 <- ggplot(plot_leaflet_num3, aes(x = finch_beak, y = the.emmean)) + 
    geom_errorbar(size = 0.8, aes(ymax = asymp.UCL, ymin = asymp.LCL, width = 0.2)) +
    geom_point(size = 5) + 
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          plot.title = element_text(size = 16)) +labs(title = "Leaf: Leaflet Number", x = "Absence Large Beak Finch        Prescence Large Beak Finch", 
                                                      y = "Leaflet Number") 
