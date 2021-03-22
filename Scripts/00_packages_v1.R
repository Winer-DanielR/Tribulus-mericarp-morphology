
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
library(emmeans) #For testing models
library(car) #For model analysis
library(DHARMa)

