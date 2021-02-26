### Analysis of mericarp data collected for the Antagonistic and Mutualistic interactions paper ####
#By: Daniel Reyes Corral
#Version_1

#### Linear models per trait ####

#### Model: trait ~ mainland + year. ####
# Is there and effect between island and mainland populations? #

# Mericarp data ####
# run simple linear mixed effect model, with ID as random effect per trait

# Length ####
names(mericarp)
mericarp_length_m1<-lmer(length ~ mainland_island +
                       year_collected +
                       (1|ID),data=mericarp,REML=F)
# type III test
Anova(mericarp_length_m1)
hist(residuals(mericarp_length_m1),breaks = 15 )



# Width ####
mericarp_width_m1<-lmer(width ~ mainland_island +
                           year_collected +
                           (1|ID),data=mericarp,REML=F)
# type III test
Anova(mericarp_width_m1)

# Depth ####
mericarp_depth_m1<-lmer(depth ~ mainland_island +
                           year_collected +
                           (1|ID),data=mericarp,REML=F)
# type III test
Anova(mericarp_depth_m1)

# Spine length ####
mericarp_spine.length_m1<-lmer(spine_length ~ mainland_island +
                           year_collected +
                           (1|ID),data=mericarp,REML=F)
# type III test
Anova(mericarp_spine.length_m1)

# Tip distance ####
mericarp_tip.distance_m1<-lmer(tip_distance ~ mainland_island +
                                 year_collected +
                                 (1|ID),data=mericarp,REML=F)
# type III test
Anova(mericarp_tip.distance_m1)

# Spine number ####
mericarp_spine.number_m1 <- glm(spine_num ~ mainland_island + 
                                  year_collected, 
                                data = mericarp,
                                family = "binomial")

Anova(mericarp_spine.number_m1)

#### Lower spines ####
mericarp_lower.spines_m1 <- glm(lower_spines ~ mainland_island + 
                                  year_collected, 
                                data = mericarp, family = "binomial")
Anova(mericarp_lower.spines_m1)

# Flower data ####
#run simple linear mixed effect model, treating ID as random effect to account for multiple measurement per plant
flower_m1 <- lmer(petal_length ~ mainland_island +
               year_collected +
               (1|ID),data=flower,REML=F)
#type III test using lmertest
anova(flower_m1)
#same test in CAR
Anova(flower_m1)

# Leaves data ####
# run simple linear mixed effects models on each leaf treat

# Leaf length ####
leaf_length_m1<-lmer(leaf_length ~ mainland_island +
                       year_collected +
                       (1|ID),data=leaf,REML=F)
Anova(leaf_length_m1)

# Leaflet length ####
leaflet_length_m1<-lmer(leaflet_length ~ mainland_island +
                          year_collected +
                          (1|ID),data=leaf,REML=F)
anova(leaflet_length_m1)

# Leaflet number ####
leaflet_num_m1<-lmer(number_of_leaflets ~ mainland_island +
                       year_collected + 
                       (1|ID),data=leaf,REML=F)
anova(leaflet_num_m1)


# Estimates of LS means ####

# mainland/island effect. Model 1 ####

#the following estimates LS means for the mainland_island effect for each response

emm_options(opt.digits=F)

# Mericarp ####
emmeans(mericarp_length_m1, ~mainland_island)
emmeans(mericarp_width_m1, ~mainland_island)
emmeans(mericarp_depth_m1, ~mainland_island)
emmeans(mericarp_spine.length_m1, ~mainland_island)
emmeans(mericarp_tip.distance_m1, ~mainland_island)
emmeans(mericarp_spine.number_m1, ~mainland_island)
emmeans(mericarp_lower.spines_m1, ~mainland_island)

# Flower ####
emmeans(flower_m1,~mainland_island)

# Leaf ####
emmeans(leaf_length_m1,~mainland_island)
emmeans(leaflet_length_m1,~mainland_island)
emmeans(leaflet_num_m1,~mainland_island)




#### Filter datasets for Galapagos only data ####
gal_length <- filter(mericarp, galapagos_other == "Galapagos")
gal_width <- filter(mericarp, galapagos_other == "Galapagos")
gal_depth <- filter(mericarp, location == "Galapagos")
gal_spine_length <- filter(trib_spine_length, location == "Galapagos")
gal_tip_distance <- filter(trib_tip_distance, location == "Galapagos")
gal_spine_number <- filter(trib_spine_num, location == "Galapagos")
gal_lower_spines <- filter(trib_lower_spines, location == "Galapagos")


#### Length ####
# Raw data
mericarp_length_m2<- lm(length ~ finch_beak + year_collected, data = gal_length) #Full model
Anova(mericarp_length_m2)


#### Width ####
# Raw data
width_beak_raw <- lm(width ~ finch_beak + year_collected, data = gal_width) #Full model
Anova(width_beak_raw)

#### Depth ####
# Raw data
depth_beak_raw <- lm(depth ~ finch_beak + year_collected, data = gal_depth) #Full model
Anova(depth_beak_raw)

#### Spine Length ####
# Raw data
spine_beak_raw <- lm(spine_length ~ finch_beak + year_collected, data = gal_spine_length) #Full model
Anova(spine_beak_raw)

#### Tip distance ####
# Raw data
tip_dist_beak_raw <- lm(tip_distance ~ finch_beak + year_collected, data = gal_tip_distance) #Full model
Anova(tip_dist_beak_raw)

#### Spine number ####
spine_number_beak <- glm(spine_num ~ finch_beak + year_collected, data = gal_spine_number, family = "binomial")
Anova(spine_number_beak)

#### Lower spines ####
lower_spines_beak <- glm(lower_spines ~ finch_beak + year_collected, data = gal_lower_spines, family = "binomial")
Anova(lower_spines_beak)



# Diagnostic for models ######
# Model 1 mainland/island ####
attach(mericarp)
attach(flower)
attach(leaf)
hist(length)
hist(petal_length)
hist(leaf_length)

diagnostic(length)
plot(mericarp_length_m1)

diagnostic(width)
diagnostic(depth)

diagnostic(petal_length)

diagnostic(leaf_length)
diagnostic(leaflet_length)
diagnostic(leaf_num)






