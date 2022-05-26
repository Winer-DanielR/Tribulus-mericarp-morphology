# NOTE:
# This script is outdated I already collected the data using QGIS.

# World climate dataset preparation

# This script is to load the climate variables from worldclim and add them to the
# mericarp and flower dataset.

# Load flower dataset
flower <- flower <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/22 Chapter. Tribulus phenotypic variation in mainland and island populations/22.03 R code/Tribulus-mericarp-morphology/Data/Processed/Tribulus_flower_data_clean.csv")
flower <- flower %>% mutate_at(vars(ID,
                                    continent,
                                    country,
                                    island_group,
                                    mainland_island,
                                    galapagos_other,
                                    island_group,
                                    galapagos_island,
                                    finch_beak), list(factor))
str(flower)

# # Load worldclim dataset (raster package)
# clim <- raster::getData("worldclim", var = "bio", res = 2.5)
# climate_variables <- clim[[c(1,4,12,15)]]

# Coodinates
coordinates <- select(flower, latitude, longitude)


# points <- SpatialPoints(coordinates, proj4string = climate_variables@crs)
# 
# values <- raster::extract(climate_variables,points)
# 
# flower_climate <- cbind.data.frame(coordinates(points),values)
# 
# flower <- left_join(flower, flower_climate, by = NULL, copy = F, suffix=c(".x",".y"))

# I was able to download the bio variables at 30s resulutions. I load them here.
# I am following this tutorial: https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html

bio_1 <- raster("wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
bio_4 <- raster("wc2.1_30s_bio/wc2.1_30s_bio_4.tif")
bio_12 <- raster("wc2.1_30s_bio/wc2.1_30s_bio_12.tif")
bio_15 <- raster("wc2.1_30s_bio/wc2.1_30s_bio_15.tif")

# Extract data from WorldCLim for your sites
plot(bio_1)
plot(bio_4)

bio_dataset <- coordinates
bio_dataset$bio_1 <- raster::extract(bio_1, coordinates)
bio_dataset$bio_4 <- raster::extract(bio_4, coordinates)
bio_dataset$bio_12 <- raster::extract(bio_12, coordinates)
bio_dataset$bio_15 <- raster::extract(bio_15, coordinates)

flower <- cbind(flower, bio_dataset)
flower <- select(flower, !c(25,26))

