rm(list=ls())
getwd()
setwd("~/Dropbox/Marc/PROJECTS/Reyes/Herbarium samples/Herbarium measurements_flowers/Harrison_Harvard/merge files")
list.files()

#old code used to merge the data files
harvard_flws<-read.csv("Harvard_flowers.csv")
attach(harvard_flws)
names(harvard_flws)

harvard_lvs<-read.csv("Harvard_leaves.csv")
attach(harvard_lvs)
names(harvard_lvs)

harvard_meta<-read.csv("Harvard_meta data.csv")
attach(harvard_meta)
names(harvard_meta)

HUH_flws_merge<-merge(harvard_flws, harvard_meta, by = "ID")
HUH_lvs_merge<-merge(harvard_lvs,harvard_meta,by="ID")

write.csv(HUH_flws_merge,file="HUH_flws_merge_2021.2.1.csv",row.names=FALSE)
write.csv(HUH_lvs_merge,file="HUH_lvs_merge_2021.2.1.csv",row.names=FALSE)
#END of old merger scripts

setwd("~/Dropbox/Marc/PROJECTS/Reyes/Herbarium samples/Herbarium measurements_flowers/Data_GOOD")
list.files()
flw_dat<-read.csv("herbarium_flws_concat_2021.2.3.csv")
lvs_dat<-read.csv("herbarium_lvs_concat_2021.2.3.csv")
str(flw_dat)
str(lvs_dat)
head(flw_dat)
head(lvs_dat)
attach(flower)
attach(leaf)
names(flw_dat)
hist(petal_length) #nicely normally distributed, no clear outliers
names(lvs_dat) 
hist(leaf_length) #right skewed, a few large leaves, but not clearly any errors
hist(leaflet_length) #right skewed, a few large leaves, but not clearly any errors
hist(number_of_leaflets,breaks=10)#nice distribution, slight left skew

library(lme4)
library(lmerTest)
library(car)

#Flower data
#run simple linear mixed effect model, treating ID as random effect to account for multiple measurement per plant
flw_m1<-lmer(petal_length~mainland_island+(1|ID),data=flw_dat,REML=F)
#type III test using lmertest
anova(flw_m1)
#same test in CAR
Anova(flw_m1)

#Leaves data
#run simple linear mixed effects models on each leaf treat
names(lvs_dat)
lvsL_m1<-lmer(leaf.length~mainland_island+(1|ID),data=lvs_dat,REML=F)
anova(lvsL_m1)

lfletL_m1<-lmer(leaflet.length~mainland_island+(1|ID),data=lvs_dat,REML=F)
anova(lfletL_m1)

lflet.no_m1<-lmer(number.of.leaflets~mainland_island+(1|ID),data=lvs_dat,REML=F)
anova(lflet.no_m1)

library(emmeans)
#the following estimates LS means for the mainland_island effect for each response
emm_options(opt.digits=F)
emmeans(flw_m1,~mainland_island)
emmeans(lvsL_m1,~mainland_island)
emmeans(lfletL_m1,~mainland_island)
emmeans(lflet.no_m1,~mainland_island)
