

sampling_props <- read.csv('./lookup/sampling_props.csv') %>%
  dplyr::select(- source) # don't need the source column for this purpose

sampling_props$relbl_strata1 <- trimws(toupper(sampling_props$strata1))


######################## sum weights for each country, each year

# by strata 1
all_strata1 <- hh_all_cntry %>%
  group_by(country, year, relbl_strata1) %>%
  summarise(w = sum(hh_weight, na.rm = TRUE)) %>%
  mutate(pct = round(w/sum(w)*100, digits =2)) 

# join with the reported sampling props
strata1_join <- full_join(all_strata1, sampling_props, by = c("country", "year", "relbl_strata1"))

# for the countires we have sampling props for, do a check by filter
eth <- filter(strata1_join, country == "ETH")
ner <- filter(strata1_join, country == "NER")
mwi <- filter(strata1_join, country == "MWI") # LIKOMA, NENO, RUMPHI missing for MWI from some years. This is compatable with the response for these districts on the LSMS website
nga <- filter(strata1_join, country == "NGA")

strata1_join <- strata1_join %>%
  filter(! country %in% c("TZA", "UGA")) %>%
  dplyr::select(- strata1) %>%
  mutate(pct_dif = pct - ref_pct)

# output to look at weight comparisons
write.csv(strata1_join, './outputs/misc/strata1_wgts_comparison.csv', row.names= FALSE)

#### exploring some issues (weighted share a lot bigger or less than reference share)
niamey <- filter(hh_all_cntry, country == "NER", year == "2011", relbl_strata1 == "NIAMEY")
sum(niamey$hh_weight)
ner11 <- filter(hh_all_cntry, country == "NER", year == "2011")

ner11_cov <- read_dta('./data_raw/NER/ner11ecvmamen_p1.dta')

ner11_cov <- ner11_cov %>%
  filter(strate == 81)
sum(ner11_cov$hhweight)


# by rural / urban

hh_all_cntry %>%
  group_by(country, year, relbl_urb_rur) %>%
  summarise(w = sum(hh_weight, na.rm = TRUE)) %>%
  mutate(pct = round(w/sum(w)*100, digits =2)) %>%
  view()

# sum across country
hh_all_cntry %>%
  group_by(country, year) %>%
  summarise(w = sum(hh_weight, na.rm = TRUE)) %>%
  view()

ner11 <- filter(hh_all_cntry, country == "NER", year == "2011")


## join for comparison between weighted and unweighted 

eth11_comp <- full_join(eth11_prop, eth11_weight)

write.csv(eth11_comp, "./outputs/misc/eth11_wgts_comparison.csv", row.names = FALSE)


############################################# CALCULATE WEIGHTED FUEL SHARE FOR ALL COUNTIRES ###################################
# sum of weights for all rural households per country per year
rur_wgts <- hh_all_cntry %>% 
  filter(relbl_urb_rur == "RURAL") %>%
  group_by(country, year) %>%
  summarise(ttl_wgt = sum(hh_weight, na.rm = TRUE)) 

# sum of weights per fuel, per country, per year
fuel_wgts <- hh_all_cntry %>% 
  filter(relbl_urb_rur == "RURAL") %>%
  group_by(country, year, cooking_fuel_prm_grp) %>%
  summarise(sum_wgts = sum(hh_weight, na.rm = TRUE)) 


# join and clauclate pct share
wgt_fuel_share_RUR <- left_join(fuel_wgts, rur_wgts, by = c("country", "year")) %>%
  mutate(wgt_pct = sum_wgts / ttl_wgt * 100)

# caulcate unweighted share and join to weighted df
unwgt_fuel_share_RUR <- hh_all_cntry %>%
  filter(relbl_urb_rur == "RURAL") %>%
  group_by(country, year, cooking_fuel_prm_grp) %>% 
  count() %>% # add count of fuel per country per year
  ungroup() %>% 
  group_by(country, year) %>% 
  mutate(ttl = sum(n),# count per year
         unwgt_pct = (n / ttl)*100) 

fuel_share_RUR <-full_join(wgt_fuel_share_RUR, unwgt_fuel_share_RUR, by = c("country", "year", "cooking_fuel_prm_grp"))

fuel_share_RUR %>%
  filter(cooking_fuel_prm_grp == "CHARCOAL") %>%
  view()

########################################## ETH only ######################
# ETH specific code used as first attempt at using weights


filter(hh_all_cntry, country == "ETH", year == 2011) %>%
  group_by(relbl_region) %>%
  summarise(w = sum(hh_weight)) %>%
  mutate(d = w/sum(w)*100) %>%
  view()

###### unweighted fuel share ###

eth11_prop <- hh_all_cntry %>%
  filter(country == "ETH", year == "2011", relbl_urb_rur == "RURAL") %>%
  group_by(cooking_fuel_prm_grp) %>% 
  count() %>% # add count of fuel per country per year
  ungroup() %>% 
  mutate(ttl = sum(n),# count per year
         pct = round((n / ttl)*100, digits = 3)) 

##### weighted fuel share ETH

eth11 <- filter(hh_all_cntry, country == "ETH", year == "2011", relbl_urb_rur == "RURAL")

eth11_weight <- data.frame(tapply(eth11$hh_weight, eth11$cooking_fuel_prm_grp, sum))
eth11_weight$cooking_fuel_prm_grp <- rownames(eth11_weight)
row.names(eth11_weight) <- NULL
eth11_weight$n_wgt <- eth11_weight$tapply.eth11.hh_weight..eth11.cooking_fuel_prm_grp..sum.

eth11_weight <- eth11_weight %>%
  dplyr::select(- tapply.eth11.hh_weight..eth11.cooking_fuel_prm_grp..sum.) %>%
  mutate(ttl_wgt = sum(n_wgt)) %>%
  mutate(pct_wgt = round(n_wgt / sum(n_wgt)*100, digits = 3)) 