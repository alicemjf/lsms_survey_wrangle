library(ggplot2)

hh_all_cntry %>%  as_tibble() %>% 
  group_by(country, year, cooking_fuel_prm_grp) %>% 
  count() %>% # add count of fuel per country per year
  ungroup() %>% 
  group_by(country, year) %>% 
  mutate(ttl = sum(n),# count per year
         pct_ttl = (n / ttl)*100) %>% # calculate prop of hh using fuel in country per year
  ggplot(aes(year, pct_ttl, color = cooking_fuel_prm_grp, group = cooking_fuel_prm_grp)) +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("All HHs (urb + rur")+
facet_wrap(~country)

####
hh_all_cntry %>%  as_tibble() %>% 
  filter(relbl_urb_rur == "URBAN") %>%
  group_by(country, year, cooking_fuel_prm_grp) %>% 
  count() %>% # add count of fuel per country per year
  ungroup() %>% 
  group_by(country, year) %>% 
  mutate(ttl = sum(n),# count per year
         pct_ttl = (n / ttl)*100) %>% # calculate prop of hh using fuel in country per year
  ggplot(aes(year, pct_ttl, color = cooking_fuel_prm_grp, group = cooking_fuel_prm_grp)) +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Urban HHs")+
  facet_wrap(~country)  


#### LINE PLOT OF X-SECTION DATA - rural
# (INCLDUING THE UP_ID SEQUENCED DATA INTRODUCES DUPLICATES DUE TO SPLIT OFF HHs - so plotting X sec for overview)
hh_all_cntry %>%  as_tibble() %>% 
  filter(relbl_urb_rur == "RURAL") %>%
  group_by(country, year, cooking_fuel_prm_grp) %>% 
  count() %>% # add count of fuel per country per year
  ungroup() %>% 
  group_by(country, year) %>% 
  mutate(ttl = sum(n),# count per year
         pct_ttl = (n / ttl)*100) %>% # calculate prop of hh using fuel in country per year
  ggplot(aes(year, pct_ttl, color = cooking_fuel_prm_grp, group = cooking_fuel_prm_grp)) +
  geom_point() + 
  geom_line() +
  geom_smooth(method = "lm", colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Rural HHs")+
  facet_wrap(~country)


