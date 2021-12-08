# Leaf dataset and analysis Script 14 ##
# By: Daniel Reyes Corral
# Date: 12/08/2021

# 14_01 Leaf data ####
# Dataset preparation for Tribulus leaves. This cleans and prepares leaf data for models
# I separate each trait: leaf lenght, leaflet lenght and leaf number into individual datasets.

leaf <- read_csv("~/R/02. Thesis/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Processed/Tribulus_leaves_data_plus CDRS herbarium_clean.csv")
leaf <- leaf %>% mutate_at(vars(ID,
                                herbarium,
                                continent,
                                country,
                                island_group,
                                mainland_island,
                                galapagos_other,
                                island_group,
                                galapagos_island,
                                finch_beak), list(factor))


str(leaf)

## 14_01_01 Leaf length ####
leaf_length <- filter(leaf, !is.na(leaf_length))


## 14_01_02 Leaflet length ####
leaflet_length <- filter(leaf, !is.na(leaflet_length))

## 14_01_03 Galapagos only leaf length dataset ####
gal_leaf_length <- filter(leaf_length, galapagos_other == "Galapagos")

## 14_01_04 Galapagos only leaflet length dataset ####
gal_leaflet_length <- filter(leaflet_length, galapagos_other == "Galapagos")
