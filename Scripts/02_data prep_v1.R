### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

### Script for modifying data and get general data ready for analysis ###


tribulus_mericarp <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus mericarp morphology/Tribulus-mericarp-morphology/Data/Raw/Cleaned data mericarp morphology Tribulus.csv")
tribulus_mericarp <- as_tibble(tribulus_mericarp)
names(tribulus_mericarp)

tribulus_mericarp <- tribulus_mericarp %>% mutate_at(vars(ID,
                                                          Herbarium,
                                                          continent,
                                                          island_group,
                                                          mainland_island,
                                                          location,
                                                          Galapagos_island,
                                                          finch_beak,
                                                          site, individual_sample,
                                                          spine_num,
                                                          lower_spines), list(factor))

str(tribulus_mericarp)
head(tribulus_mericarp)

#Year is a number instead of a factor.

