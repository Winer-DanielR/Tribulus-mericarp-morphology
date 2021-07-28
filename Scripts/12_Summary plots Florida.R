# I decided to split the data to explore more that variation that was mentioned in the
# meeting, about the individual differences between specific groups. So I separated the
# datasets into continents and I can explore the univariate models this way.
# However, this may be a confounding factor that we can include on the initial general model.
# This was also mentioned by the Grants.

continent <- mericarp %>% group_split(continent, keep = T)

africa <- as.tibble(continent[[1]])
africa <- africa %>% mutate_at(vars(ID,
                                              Herbarium,
                                              continent,
                                              island_group,
                                              mainland_island,
                                              galapagos_other,
                                              island_group,
                                              galapagos_island,
                                              finch_beak), list(factor))
str(africa)


north_america <- as.tibble(continent[[2]])
north_america <- north_america %>% mutate_at(vars(ID,
                                    Herbarium,
                                    continent,
                                    island_group,
                                    mainland_island,
                                    galapagos_other,
                                    island_group,
                                    galapagos_island,
                                    finch_beak), list(factor))

south_america <- as.tibble(continent[[3]])
south_america <- south_america %>% mutate_at(vars(ID,
                                                  Herbarium,
                                                  continent,
                                                  island_group,
                                                  mainland_island,
                                                  galapagos_other,
                                                  island_group,
                                                  galapagos_island,
                                                  finch_beak), list(factor))

# Africa mericarp mainland/island
africa_length <- lmer(length ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = africa,
                      REML = F)
Anova(africa_length)

##### Emmean Length ####
africa_EM_length <- emmeans(africa_length, ~ mainland_island)
plot(africa_EM_length, comparisons = TRUE) + labs(title = "Africa Mericarp Length")
pwpp(africa_EM_length)

# North America mericarp mainland/island
north_america_length <- lmer(length ~ mainland_island +
                        year_collected +
                        (1|ID),
                      data = north_america,
                      REML = F)
Anova(north_america_length)

##### Emmean Length ####
north_america_EM_length <- emmeans(north_america_length, ~ mainland_island)
plot(north_america_EM_length, comparisons = TRUE) + labs(title = "North America Mericarp Length")
pwpp(north_america_EM_length)

# South America mericarp mainland/island
south_america_length <- lmer(length ~ mainland_island +
                               year_collected +
                               (1|ID),
                             data = south_america,
                             REML = F)
Anova(south_america_length)

##### Emmean Length ####
south_america_EM_length <- emmeans(south_america_length, ~ mainland_island)
plot(south_america_EM_length, comparisons = TRUE) + labs(title = "South America Mericarp Length")
pwpp(south_america_EM_length)

