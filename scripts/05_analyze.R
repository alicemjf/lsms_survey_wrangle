library(sjPlot)
library(lme4)

rur <- hh_all_cntry %>%
  filter(relbl_urb_rur == "RURAL")

table(mod_df$region_unq)

# create model df

mod_df <- rur %>%
  mutate(fw = case_when(cooking_fuel_prm_grp == "FIREWOOD (PURCHASED / COLLECTED)" ~ 1,
                                                cooking_fuel_prm_grp != "FIREWOOD (PURCHASED / COLLECTED)" ~ 0)) %>%
  mutate(mdrn_fossil = case_when(cooking_fuel_prm_grp == "MODERN (FOSSIL FUEL)" ~ 1,
                        cooking_fuel_prm_grp != "MODERN (FOSSIL FUEL)" ~ 0)) %>%
  mutate(ch = case_when(cooking_fuel_prm_grp == "CHARCOAL" ~ 1,
                        cooking_fuel_prm_grp != "CHARCOAL" ~ 0)) %>%
  dplyr::select(country, year, relbl_region, fw, ch, mdrn_fossil) %>%
  filter(! is.na(fw), ! is.na(relbl_region)) %>%
  mutate(year.sc = scale(year)) %>%
  mutate(year.since.2000 = year -2000) %>%
  mutate(region_unq = as.factor(paste0(country,"_", relbl_region))) %>%
  filter(! region_unq %in% c("NER_OTHER", "NER_NIAMEY")) # filter out these reigons as they only have 2018 data

hist(mod_df$year.sc)
str(mod_df)


######################################################## REGIONAL FW CH TRANSITIONS PER COUNTRY ###################################

# fw with regional interaction
S <- split(mod_df, mod_df$country)

lapply(S, function(x){
  m <- glm(fw ~ year.since.2000 * region_unq, family = binomial(link = "logit"), data = x)
  plot_model(m, type = "int")
}
)

# ch with regional interactions
lapply(S, function(x){
  m <- glm(ch ~ year.since.2000 * region_unq, family = binomial(link = "logit"), data = x)
  plot_model(m, type = "int")
}
)

# TZA DES shoot up trend might be due to fact there are only rural DES hhs in 2010 & 2014, so trend is an extrapolation
# these hh's probably got reclassifed to urban in later years as city expanded

# NER regional charcoal graphs all over the place because very low % using ch < 1%
# NGA regional charcoal graphs are all over the place as only 0% ch in first 3 years then 1% in final 3 years. 
# NGA instead has increasing modern fossil fuels:

mod_nga_fossil <- glm(mdrn_fossil ~ year.since.2000 * region_unq, data = mod_df, subset = country == "NGA")
plot_model(mod_nga_fossil, type = "int")


################################################# FIREWOOD MODELS ##############################################

# Model 1
## Simplest model - does year effect the likelihood of firewood being the primary cooking fuel? 
mod1 <- glm(fw ~ year.since.2000 + country, family = binomial(link = "logit"), data = mod_df) # - year 2000 to decrease intercept (INTERCEPT IS NOW LOG ODDS IN YEAR 2000)
summary(mod1)            
plot(mod1)

# Model 2 with country interaction

mod2 <- glm(fw ~ year.since.2000 * country, family = binomial(link = "logit"), 
              data = mod_df) # - year 2000 to decrease intercept (INTERCEPT IS NOW LOG ODDS IN YEAR 2000)
summary(mod2) # effect of year vary by country. INTERCEPT AND SLOPE VARY BY COUNTRY

plot_model(mod2, type = "int", transform = NULL)

# Model 3 with region interaction
mod3 <- glm(fw ~ year.since.2000 * region_unq, family = binomial(link = "logit"), data = mod_df) # - year 2000 to decrease intercept (INTERCEPT IS NOW LOG ODDS IN YEAR 2000)
summary(mod3) # effect of year vary by country. INTERCEPT AND SLOPE VARY BY COUNTRY

plot_model(mod3)


################################################ CHARCOAL MODELS ######################################################

# Model 1
## Simplest model - does year effect the likelihood of firewood being the primary cooking fuel? 
mod1ch <- glm(ch ~ year.since.2000 + country, family = binomial(link = "logit"), data = mod_df) # - year 2000 to decrease intercept (INTERCEPT IS NOW LOG ODDS IN YEAR 2000)
summary(mod1ch)            
plot(mod1ch)

# Model 2 with country interaction
mod2ch <- glm(ch ~ year.since.2000 * country, family = binomial(link = "logit"), 
            data = mod_df) # - year 2000 to decrease intercept (INTERCEPT IS NOW LOG ODDS IN YEAR 2000)
summary(mod2ch) 
plot_model(mod2ch, type = "int", transform = NULL)

# Model 3 with region interaction
mod3ch <- glm(ch ~ year.since.2000 * region_unq, family = binomial(link = "logit"), data = mod_df) # - year 2000 to decrease intercept (INTERCEPT IS NOW LOG ODDS IN YEAR 2000)
summary(mod3ch) 
plot_model(mod3ch, type = "int", transform = NULL)










################################################### MIXED EFFECTS MODELS (NOT BEING USED IN CURRENT ANALYSIS) #############
## Include country as a random effect (fixed slope)
mod4 <- glmer(fw ~ year.sc + (1 | country), data = mod_df, family = "binomial", control=glmerControl(optimizer="bobyqa"))
summary(mod4) # gives a warning message 'model unidentifiable'
?lmer

# Model 3 - random effect of country, random slope

mod5 <- glmer(fw ~ year.sc + (1 + year.sc | country), data = mod_df, family = "binomial", control=glmerControl(optimizer="bobyqa"))
summary(mod5)


table(mod_df$country, mod_df$year)
