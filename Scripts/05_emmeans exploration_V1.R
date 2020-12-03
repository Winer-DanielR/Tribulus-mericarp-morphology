# Emmeans exploration

length_full_model <- lm(log(length) ~ mainland_island + year_collected 
                 + mainland_island:year_collected, data = tribulus_mericarp)
emm1 <- emmeans(length_full_model, specs = pairwise ~ mainland_island:year_collected, type = "response",
                adjust = "none")
emm1$contrasts %>%
  confint() #Shows confidence intervals

emm1$contrasts %>%
  summary(infer = T) #Shows P and confidence intervals

emm1_contrasts <- emm1$contrasts %>%
  confint() %>%
  as.data.frame() #Put comparisons as data frames

emm2 <- emmeans(length_full_model, specs = pairwise ~ mainland_island|year_collected, type = "response")

emm2$contrasts %>%
  rbind()

emmeans(length_full_model, specs = pairwise ~ mainland_island)

?"contrast-methods"
