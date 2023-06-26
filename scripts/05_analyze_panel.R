# create model df
rur_p <- filter(hh_all_cntry_panels, relbl_urb_rur == "RURAL", bp == 1, orig_hh == 1)

mod_pdf <- rur_p %>% 
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
  mutate(region_unq = as.factor(paste0(country,"_", relbl_region)))

str(mod_pdf)


# Model 2 (charcoal) with country interaction
mod2ch_p <- glm(ch ~ year.since.2000 * country, family = binomial(link = "logit"), 
              data = mod_pdf) # - year 2000 to decrease intercept (INTERCEPT IS NOW LOG ODDS IN YEAR 2000)
summary(mod2ch_p) 
plot_model(mod2ch_p, type = "int", transform = NULL)

# balance panel of original households show same trends of increasing charcoal as x-sec data, but less steep increase

# fw with regional interaction
Sp <- split(mod_pdf, mod_pdf$country)

lapply(Sp, function(x){
  m <- glm(fw ~ year.since.2000 * region_unq, family = binomial(link = "logit"), data = x)
  plot_model(m, type = "int")
}
)

# ch with regional interactions
lapply(Sp, function(x){
  m <- glm(ch ~ year.since.2000 * region_unq, family = binomial(link = "logit"), data = x)
  plot_model(m, type = "int")
}
)

