library(survey)


hh_all_cntry <- filter(hh_all_cntry, ! is.na(hh_weight)) %>%
  mutate(ea_new = paste0(country, "_", EA)) # new EA var that includes country code


all_cntry_svy <- svydesign(ids = ~ea_new, strata = hh_all_cntry$relbl_strata1, 
                           weights = hh_all_cntry$hh_weight, data = hh_all_cntry, nest= TRUE)


# need to apply these options to work around some strata only having one cluster (PSU)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

svyby( ~ dist_road , ~ country , all_cntry_svy , svymean, na.rm = TRUE)
weights_sum <- svyby( ~ hh_weight, ~ country+year, all_cntry_svy , svytotal, na.rm = TRUE)
#write.csv(weights_sum, './outputs/misc/weights_sum.csv', row.names = FALSE)

fuels_total <- svyby( ~ cooking_fuel_prm_grp, ~ country+year+relbl_urb_rur, all_cntry_svy , svytotal, na.rm = TRUE)

fuels_total2 <- data.frame(fuels_total) # make into a df

# pivot into correct format 
fuels_total2 <- fuels_total2 %>%
  pivot_longer(cols = c(starts_with("cooking"), starts_with("se")),
               names_to = c('.value', 'fuel'),
               names_pattern = '(.*?)_(.*)') %>% 
  rename(ttl = cooking, se = se.cooking)   


fuels_total2$fuel <- gsub("^.{0,12}", "", fuels_total2$fuel) # get rid of the 'cooking_fuel_prm_grp' prefix

#write.csv(fuels_total2, './outputs/misc/fuels_totals.csv', row.names = FALSE)

################ BAR PLOT #############
rur_fuels2 <- filter(fuels_total2, relbl_urb_rur == "RURAL")
rur_ch <- filter(rur_fuels2, fuel == "CHARCOAL")
rur_fw <- filter(rur_fuels2, fuel == "FIREWOOD..PURCHASED...COLLECTED.")

ggplot(rur_ch, aes(x = year, y = ttl, fill = fuel)) +
  geom_col() +
  facet_wrap(~ country)
  
ggplot(rur_fw, aes(x = year, y = ttl, fill = fuel)) +
  geom_col() +
  facet_wrap(~ country)

library(RColorBrewer)

ggplot(rur_fuels2, aes(x = year, y = ttl, fill = fuel)) +
  geom_bar(position="fill", stat="identity", color = "black") +
  facet_wrap(~ country)+ 
  scale_fill_brewer(palette = "Reds")

install.packages("paletteer")
library(paletteer)
p + scale_color_paletteer_d(nord, aurora)

# add trend line and error bars!

############## PREPARE DF FOR REGRESSION ########################

all_cntry_svy <- update(all_cntry_svy, ch = ifelse(cooking_fuel_prm_grp == "CHARCOAL", 1, 0))
all_cntry_svy <- update(all_cntry_svy, fw = ifelse(cooking_fuel_prm_grp == "FIREWOOD", 1, 0))
all_cntry_svy <- update(all_cntry_svy, fossil = ifelse(cooking_fuel_prm_grp == "MODERN FOSSIL FUEL", 1, 0))
all_cntry_svy <- update(all_cntry_svy,year_since_2000 = year - 2000)
all_cntry_svy_mod <- subset(all_cntry_svy, country != "NGA", relbl_urb_rur == "RURAL")

ch0 <- svyglm(ch ~ year_since_2000 * country, design = all_cntry_svy_mod, family = quasibinomial)
summary(ch0)  
library(sjPlot)  
plot_model(ch0, type = "int")  
  


fw0 <- svyglm(fw ~ year_since_2000 * country, design = all_cntry_svy_mod, family = quasibinomial)
summary(fw0)  
library(sjPlot)  
plot_model(fw0, type = "int")  
