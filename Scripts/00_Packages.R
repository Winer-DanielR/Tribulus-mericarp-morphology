
#################################################################
################Tribulus Herbarium Samples#######################
#################################################################

### The main goal is to use methods to identify the samples collected on the Galapagos expedition in 2017
### Using the Herbarium samples from the CDRS in collaboration with Patricia
### Expedition samples are from 10 islands and mostly from 1 population per island.
### By Daniel Reyes Corral
### 

### Package script
library(tidyverse) #Data wrangling and plots
library(lme4) #For linear models
library(glmmTMB)
library(readr) #For uploading tables
library(emmeans) #For estimating emmeans
library(car) #For model analysis
library(DHARMa) #For testing models
library(ggplot2)
library(ggvegan)
library(extrafont)
library(ggpubr)

# PCA packages:
library(RColorBrewer)
library(ellipse)
library(rgl)
library(factoextra)
library(FactoMineR)

# # Results editing
library(xtable) #For making ANOVA outcomes dataframes and tables
library(broom) #For making other statistical objects into Tidy Tibbles
library(ggpubr) #For arrage ggplots into a single plot
library(knitr)
library(kableExtra)

