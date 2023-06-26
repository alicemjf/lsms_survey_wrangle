library(sjPlot)
library(lme4)

# ANALYSIS FOR PGR CONFERENCE POSTER
# 10/05/2023
library(dplyr)

rur <- hh_all_cntry %>%
  filter(relbl_urb_rur == "RURAL")

# create model df

mod_df <- rur %>%
  mutate(fw = case_when(cooking_fuel_prm_grp == "FIREWOOD (PURCHASED / COLLECTED)" ~ 1,
                        cooking_fuel_prm_grp != "FIREWOOD (PURCHASED / COLLECTED)" ~ 0)) %>%
  mutate(mdrn_fossil = case_when(cooking_fuel_prm_grp == "MODERN (FOSSIL FUEL)" ~ 1,
                                 cooking_fuel_prm_grp != "MODERN (FOSSIL FUEL)" ~ 0)) %>%
  mutate(ch = case_when(cooking_fuel_prm_grp == "CHARCOAL" ~ 1,
                        cooking_fuel_prm_grp != "CHARCOAL" ~ 0)) %>%
  dplyr::select(country, year, relbl_strata1, fw, ch, mdrn_fossil, hh_weight) %>%
  filter(! is.na(fw), ! is.na(relbl_strata1), ! is.na(hh_weight)) %>%
  mutate(year.sc = scale(year)) %>%
  mutate(year.since.2000 = year -2000) %>%
  mutate(region_unq = as.factor(paste0(country,"_", relbl_strata1))) 

summary(mod_df)


################### FULL DATA SET MODELS (UN-WEIGHTED) ####################

# make filter dataset that excludes NGA as NGA has no charcoal
mod_df_filt <- mod_df %>%
  filter(country != "NGA")


# ch with country interactions
ch_mod <- glm(ch ~ year.since.2000 * country, family = binomial(link = "logit"), data = mod_df_filt) # run with df that excludes NGA as NGA has no charcoal some years
summary(ch_mod)
plot_model(ch_mod, type = "int")

chp <- plot_model(ch_mod, type=("pred"),
                         terms=c("year.since.2000","country"), colors = "flat")+ # to change colors on chart
theme(text = element_text(size = 40))



# WIP
mod_df$ch <- as.factor(mod_df$ch)
mod_df$fw <- as.factor(mod_df$fw)
S <- split(mod_df, mod_df$country)
summary(mod_df)

lapply(S, function(x){
  m <- glm(ch ~ year.since.2000 * country, family = binomial(link = "logit"), data = x)
}
)
summary(mod_df_filt)

### running by country because lapply on list not working

eth <- filter(mod_df, country == "ETH")
eth_mod <- glm(ch ~ year.since.2000, family = binomial(link = "logit"), data = eth)
mwi <- filter(mod_df, country == "MWI")
mwi_mod <- glm(ch ~ year.since.2000, family = binomial(link = "logit"), data = mwi)
ner <- filter(mod_df, country == "NER")
ner_mod <- glm(ch ~ year.since.2000, family = binomial(link = "logit"), data = ner)
tza <- filter(mod_df, country == "TZA")
tza_mod <- glm(ch ~ year.since.2000, family = binomial(link = "logit"), data = tza)
uga <- filter(mod_df, country == "UGA")
uga_mod <- glm(ch ~ year.since.2000, family = binomial(link = "logit"), data = uga)

summary(eth_mod)
summary(mwi_mod)
summary(ner_mod)
summary(tza_mod)
summary(uga_mod)

# fw with country interactions
fw_mod <- glm(fw ~ year.since.2000 * country, family = binomial(link = "logit"), data = mod_df)
summary(fw_mod)

plot_model(fw_mod, type = "int", show.p = FALSE)


fwp <- plot_model(fw_mod, type=("pred"),
          terms=c("year.since.2000","country"), colors = "flat")+ # to change colors on chart
  theme(text = element_text(size = 40))


lapply(S, function(x){
  m <- glm(fw ~ year.since.2000 * country, family = binomial(link = "logit"), data = x)
}
)


#### per country model

eth <- filter(mod_df, country == "ETH")
eth_modfw <- glm(fw ~ year.since.2000, family = binomial(link = "logit"), data = eth)
mwi <- filter(mod_df, country == "MWI")
mwi_modfw <- glm(fw ~ year.since.2000, family = binomial(link = "logit"), data = mwi)
ner <- filter(mod_df, country == "NER")
ner_modfw <- glm(fw ~ year.since.2000, family = binomial(link = "logit"), data = ner)
tza <- filter(mod_df, country == "TZA")
tza_modfw <- glm(fw ~ year.since.2000, family = binomial(link = "logit"), data = tza)
uga <- filter(mod_df, country == "UGA")
uga_modfw <- glm(fw ~ year.since.2000, family = binomial(link = "logit"), data = uga)
nga_modfw <- glm(fw ~ year.since.2000, family = binomial(link = "logit"), data = nga)

summary(eth_modfw)
summary(mwi_modfw)
summary(ner_modfw)
summary(tza_modfw)
summary(uga_modfw)
summary(nga_modfw)

# fossil with country interactions
fossil_mod <- glm(mdrn_fossil ~ year.since.2000 * country, family = binomial(link = "logit"), data = mod_df)
summary(fossil_mod)
plot_model(fossil_mod, type = "int")


fossilp <- plot_model(fossil_mod, type=("pred"),
                  terms=c("year.since.2000","country"), colors = "flat")+ # to change colors on chart
                  theme(text = element_text(size = 40))



library(gridExtra)




                        