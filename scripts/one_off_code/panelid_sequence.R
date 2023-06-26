### PANEL ID SEQUENCE
# done country by country because each country requires different processing 

library(dplyr)
library(data.table)
library(haven)
library(labelled)
library(tidyverse)


###################################################################################################################### ETH #####

ethy1 <- read_dta('./data_raw/ETH/eth11sect_cover_hh_w1.dta')
ethy2 <- read_dta('./data_raw/ETH/eth13sect_cover_hh_w2.dta')
ethy3 <- read_dta('./data_raw/ETH/eth15sect_cover_hh_w3.dta')
ethy4 <- read_dta('./data_raw/ETH/eth18sect_cover_hh_w4.dta')

# ethy4 is a new panel. No shared hh with earlier surveys
# check if there is overlap on hhids
ethy4[ethy4$household_id %in% ethy1$household_id == TRUE,] # 0
ethy4[ethy4$household_id %in% ethy2$household_id == TRUE,] # 0
ethy4[ethy4$household_id %in% ethy3$household_id == TRUE,] # 0

ethy1 <- ethy1 %>% 
  mutate(y1_hhid = household_id) %>%
  dplyr::select(y1_hhid, household_id)


ethy2 <- ethy2 %>% 
  mutate( y2_hhid = household_id2) %>%
  dplyr::select(y2_hhid, household_id, household_id2)
  
ethy3 <- ethy3 %>%
  mutate(y3_hhid = household_id2)%>%
  dplyr::select(y3_hhid, household_id2)

ethy4 <- ethy4 %>%
  mutate(y4_hhid = household_id)%>%
  dplyr::select(y4_hhid, household_id)

# need to use full join so that every household that took part in any year is included in mapping
eth12 <- full_join(ethy1, ethy2, by = "household_id")

eth123 <- full_join(eth12, ethy3, by = "household_id2") 

eth1234 <- full_join(eth123, ethy4) %>%
  dplyr::select(-c(household_id, household_id2)) %>%
  mutate(country = "ETH") %>%
  mutate(survey_hhid_y1 = ifelse(!is.na(y1_hhid), paste0(country, "_", y1_hhid, "_2011"), as.numeric(NA))) %>%
  mutate(survey_hhid_y2 = ifelse(!is.na(y2_hhid), paste0(country, "_", y2_hhid, "_2013"), as.numeric(NA)))  %>%
  mutate(survey_hhid_y3 = ifelse(!is.na(y3_hhid), paste0(country, "_", y3_hhid, "_2015"), as.numeric(NA)))  %>%
  mutate(survey_hhid_y4 = ifelse(!is.na(y4_hhid), paste0(country, "_", y4_hhid, "_2018"), as.numeric(NA))) %>%
  mutate(up_id =  paste0(country,"_",row_number()))%>%
  dplyr::select(-c(y1_hhid, y2_hhid, y3_hhid, y4_hhid)) %>%
  dplyr::select(country, up_id, everything()) 
  

eth_panelids <- melt(setDT(eth1234), id.vars = c("country","up_id"), variable.name = "survey", value.name = "survey_hhid")

# add flag for balance panel

eth_panelids <- eth_panelids %>%
  group_by(up_id) %>%
  mutate(tmp = sum(!is.na(survey_hhid))) %>%
  mutate(bp = ifelse(tmp == 3, 1, 0)) %>% # although a 4th year is added keep the panel check to 3 because y4 is not part of the panel. It is a Xsec
  dplyr::select(-tmp) %>%
  ungroup() %>%
  mutate(orig_hh = case_when(survey == "survey_hhid_y4" ~ 0, # add orig_hh flag. ETH survey does not track split off hhs, so all hh are orginal apart from y4 which is a new survey
                             survey != "survey_hhid_y4" ~ 1)) %>%
  filter(! is.na(survey_hhid)) # filter out the rows y4 rows which have been created for y1:3 households

length(unique(eth_panelids$up_id[eth_panelids$bp == 1])) # number of full panel HH is 3639

ethy4_check <- eth_panelids %>% filter(survey == "survey_hhid_y4",
                                      ! is.na(survey_hhid)) # all are flagged as bp = 0 which is correct

testeth <- filter(eth_panelids, bp == 1, orig_hh == 1)

###################################################################################################################### MWI #####

mwiy1 <- read_dta('./data_raw/MWI/mwihh_mod_a_filt_10.dta')
mwiy2 <- read_dta('./data_raw/MWI/mwihh_mod_a_filt_13.dta')
mwiy3 <- read_dta('./data_raw/MWI/mwihh_mod_a_filt_16.dta')
mwiy4 <- read_dta('./data_raw/MWI/mwihh_mod_a_filt_19.dta')


mwiy1 <- mwiy1 %>%
  mutate(y1_hhid = case_id) %>%
  dplyr::select(case_id, y1_hhid)


mwiy2 <- mwiy2 %>%
  dplyr::select(y2_hhid, case_id)


mwiy3 <- mwiy3 %>%
  dplyr::select(y3_hhid, y2_hhid)

mwiy4 <- mwiy4 %>%
  dplyr::select(y4_hhid, y3_hhid)


mwi12 <- full_join(mwiy1, mwiy2, by = "case_id")

mwi123 <- full_join(mwi12, mwiy3, by = "y2_hhid")

mwi1234 <- full_join(mwi123, mwiy4, by = "y3_hhid")

unique(mwi1234)

mwi1234 <- mwi1234 %>%
  mutate(country = "MWI") %>%
  mutate(survey_hhid_y1 = ifelse(!is.na(y1_hhid), paste0(country, "_", y1_hhid, "_2010"), as.numeric(NA))) %>%
  mutate(survey_hhid_y2 = ifelse(!is.na(y2_hhid), paste0(country, "_", y2_hhid, "_2013"), as.numeric(NA))) %>%
  mutate(survey_hhid_y3 = ifelse(!is.na(y3_hhid), paste0(country, "_", y3_hhid, "_2016"), as.numeric(NA))) %>%
  mutate(survey_hhid_y4 = ifelse(!is.na(y4_hhid), paste0(country, "_", y4_hhid, "_2019"), as.numeric(NA))) %>%
  mutate(up_id =  paste0(country,"_",row_number())) %>% # add a universal panel id that is the country_ sequence of number.
  dplyr::select(-c(y1_hhid, y2_hhid, y3_hhid, y4_hhid, case_id)) %>%
  dplyr::select(country,up_id, everything()) 



mwi_panelids <- melt(setDT(mwi1234), id.vars = c("country","up_id"), variable.name = "survey", value.name = "survey_hhid")


mwi_panelids <- mwi_panelids %>%
  group_by(up_id) %>%
  mutate(tmp = sum(!is.na(survey_hhid))) %>%
  mutate(bp = ifelse(tmp == 4, 1, 0)) %>%
  dplyr::select(-tmp) %>%
  ungroup() %>%
  mutate(orig_hh = mwi1234$orig_hh[match(mwi_panelids$up_id, mwi1234$up_id)])


length(unique(mwi_panelids$up_id[mwi_panelids$bp == 1])) # number of full panel HH is 3178

# add orig_hh flag

mwi_orig <- mwi1234 %>%
  group_by(survey_hhid_y1) %>%  
  mutate(count = seq(n())) %>%
  mutate(orig_hh = case_when(count == 1 ~ 1,
                             count != 1 ~ 0)) %>%
  filter(orig_hh == 1)

# lookup orig flag
mwi_panelids <- mwi_panelids %>%
  mutate(orig_hh = case_when(up_id %in% mwi_orig$up_id ~ 1,
                             ! up_id %in% mwi_orig$up_id ~ 0))


    
###################################################################################################################### NER #####

neryr1 <- read_dta('./data_raw/NER/ner11ecvmamen_p1.dta')
neryr2 <- read_dta('./data_raw/NER/ner14ECVMA2_MS00P1.dta')
neryr3 <- read_dta('./data_raw/NER/ner18s00_me_ner2018.dta')


neryr1 <- neryr1 %>%
  # NER needs mapping vairbale to be created
  # from basic info document 2014 p7: "The GRAPPE and MENAGE identifiers of the households in 2014 are identical with the grappe and menage identifiers in 2011."
  mutate(mapping = paste0(grappe, menage)) %>% 
  dplyr::select(mapping, hid) %>%
  mutate(survey_hhid_y1 = paste0("NER_", hid, "_2011"))

neryr2 <- neryr2 %>%
  # NER needs mapping vairbale to be created
  # from basic info document 2014 p7: "The GRAPPE and MENAGE identifiers of the households in 2014 are identical with the grappe and menage identifiers in 2011."
  mutate(mapping = paste0(GRAPPE, MENAGE)) %>% 
  # for 2014 also need to create the unique hhid from 3 variables (which is used in my survey id key)
  mutate(survey_hhid_y2 = paste0("NER_", GRAPPE, MENAGE, EXTENSION,"_2014")) %>%
  dplyr::select(mapping, survey_hhid_y2)

neryr3 <- neryr3 %>%
  mutate(mapping = paste0(grappe, menage)) %>%
  dplyr::select(mapping) %>%
  mutate(survey_hhid_y3 = paste0("NER_", mapping, "_2018"))


# nery3 is a new panel. No shared hh with earlier surveys
# check if there is overlap on hhids
neryr3[neryr3$mapping %in% neryr2$mapping == TRUE,] # there is overlap
neryr3[neryr3$mapping %in% neryr1$mapping == TRUE,] # there is overlap

# prefix neryr3 hhids with 3_ to avoid incorrectly seuqencing a panel with yr3 (yr3 is independent)
neryr3$mapping <- paste0("3_", neryr3$mapping)

ner_12 <- full_join(neryr1, neryr2, by = "mapping")

ner_123 <- full_join(ner_12, neryr3, by = "mapping") 

ner_123 <- ner_123 %>%
  mutate(country = "NER") %>%
  mutate(up_id =  paste0(country,"_",row_number())) %>%
  dplyr::select(-c(hid, mapping)) %>%
  dplyr::select(country, up_id, everything())
  

ner_panelids <- melt(setDT(ner_123), id.vars = c("country","up_id"), variable.name = "survey", value.name = "survey_hhid")

# add flag for balance panel

ner_panelids <- ner_panelids %>%
  group_by(up_id) %>%
  mutate(tmp = sum(!is.na(survey_hhid))) %>%
  mutate(bp = ifelse(tmp == 2, 1, 0)) %>%
  dplyr::select(-tmp)

neryr3_check <- filter(ner_panelids, survey == "survey_hhid_y3", ! is.na(survey_hhid)) # all are flagged as bp = 0 which is correct

### 

ner_orig <- ner_123 %>%
  group_by(survey_hhid_y1) %>%  
  mutate(count = seq(n())) %>%
  mutate(orig_hh = case_when(count == 1 ~ 1,
                             count != 1 ~ 0)) %>%
  ungroup() %>%
  filter(orig_hh == 1)

ner_panelids <- ner_panelids %>%
  mutate(orig_hh = case_when(up_id %in% ner_orig$up_id ~ 1,
                             ! up_id %in% ner_orig$up_id ~ 0)) %>%
  filter(! is.na(survey_hhid))

nertest <- filter(ner_panelids, bp == 1, orig_hh == 1)

###################################################################################################################### NGA #####

ngayr1 <- read_dta('./data_raw/NGA/nga10sect8_harvestw1.dta')
ngayr2 <- read_dta('./data_raw/NGA/nga12sect8_harvestw2.dta')
ngayr3 <- read_dta('./data_raw/NGA/nga15sect11_plantingw3.dta')
ngayr4 <- read_dta('./data_raw/NGA/nga18sect11_plantingw4.dta')

ngayr1 <- ngayr1 %>%
  mutate(y1_hhid = hhid) %>%
  dplyr::select(y1_hhid, hhid)

ngayr2 <- ngayr2 %>%
  mutate(y2_hhid = hhid) %>%
  dplyr::select(y2_hhid, hhid)

ngayr3 <- ngayr3 %>%
  mutate(y3_hhid = hhid) %>%
  dplyr::select(y3_hhid, hhid)


ngayr4 <- ngayr4 %>%
  mutate(y4_hhid = hhid) %>%
  dplyr::select(y4_hhid, hhid)


nga12 <- full_join(ngayr1, ngayr2, by = "hhid")

nga123 <- full_join(nga12, ngayr3, by = "hhid")

nga1234 <- full_join(nga123, ngayr4, by = "hhid")

nga1234 <- nga1234 %>%
  mutate(country = "NGA") %>%   
  mutate(survey_hhid_y1 = ifelse(!is.na(y1_hhid), paste0(country, "_", y1_hhid, "_2010"), as.numeric(NA))) %>%
  mutate(survey_hhid_y2 = ifelse(!is.na(y2_hhid), paste0(country, "_", y2_hhid, "_2012"), as.numeric(NA))) %>%
  mutate(survey_hhid_y3 = ifelse(!is.na(y3_hhid), paste0(country, "_", y3_hhid, "_2015"), as.numeric(NA))) %>%
  mutate(survey_hhid_y4 = ifelse(!is.na(y4_hhid), paste0(country, "_", y4_hhid, "_2018"), as.numeric(NA))) %>%
  mutate(up_id =  paste0(country,"_",row_number())) %>%
  dplyr::select(-c(hhid, y1_hhid, y2_hhid, y3_hhid, y4_hhid)) %>%
  dplyr::select(country, up_id, everything())

nga_panelids <- melt(setDT(nga1234), id.vars = c("country","up_id"), variable.name = "survey", value.name = "survey_hhid")


# add flag for balance panel

nga_panelids <- nga_panelids %>%
  group_by(up_id) %>%
  mutate(tmp = sum(!is.na(survey_hhid))) %>%
  mutate(bp = ifelse(tmp == 4, 1, 0)) %>%
  dplyr::select(-tmp) %>%
  ungroup() %>%
  mutate(orig_hh = 1) # add original ousehold flag - split offs not followed so flag all as orginal

length(unique(nga_panelids$up_id[nga_panelids$bp == 1])) # number of full panel HH is 1407



###################################################################################################################### TZA #####

tza_panel <- read_dta('./data_raw/TZA/tzaupd4_hh_a.dta')
tzayr5 <- read_dta('./data_raw/TZA/tza19HH_SEC_A.dta')


tza_panel <- haven::as_factor(tza_panel)
tza_panel <- remove_val_labels(tza_panel)

tza_panel <- tza_panel %>%
  dplyr::select(UPHI, round, r_hhid)


tzaw <- spread(tza_panel, key = round, value = r_hhid)

tzaw <- tzaw %>%
  mutate(y1_hhid = `1`) %>%
  mutate(y2_hhid = `2`) %>%
  mutate(y3_hhid = `3`) %>%
  mutate(y4_hhid = `4`) %>%
  dplyr::select(-c(`1`, `2`,`3`, `4`, UPHI))
  
tzayr5 <- tzayr5 %>%
  mutate(y5_hhid = sdd_hhid) %>%
  dplyr::select(y5_hhid, y4_hhid)

tza12345 <- full_join(tzaw, tzayr5, by = "y4_hhid")
  


tza12345 <- tza12345 %>%
  mutate(country = "TZA") %>%
  mutate(survey_hhid_y1 = ifelse(!is.na(y1_hhid), paste0(country, "_", y1_hhid, "_2008"), as.numeric(NA))) %>%
  mutate(survey_hhid_y2 = ifelse(!is.na(y2_hhid), paste0(country, "_", y2_hhid, "_2010"), as.numeric(NA))) %>%
  mutate(survey_hhid_y3 = ifelse(!is.na(y3_hhid), paste0(country, "_", y3_hhid, "_2012"), as.numeric(NA))) %>%
  mutate(survey_hhid_y4 = ifelse(!is.na(y4_hhid), paste0(country, "_", y4_hhid, "_2014"), as.numeric(NA))) %>%
  mutate(survey_hhid_y5 = ifelse(!is.na(y5_hhid), paste0(country, "_", y5_hhid, "_2019"), as.numeric(NA))) %>%
  mutate(up_id =  paste0(country,"_",row_number())) %>%
  dplyr::select(-c(y1_hhid, y2_hhid, y3_hhid, y4_hhid, y5_hhid)) %>%
  dplyr::select(country, up_id, everything()) 


tza_panelids <- melt(setDT(tza12345), id.vars = c("country","up_id"), variable.name = "survey", value.name = "survey_hhid")

# add flag for balance panel

tza_panelids <- tza_panelids %>%
  group_by(up_id) %>%
  mutate(tmp = sum(!is.na(survey_hhid))) %>%
  mutate(bp = ifelse(tmp == 5, 1, 0)) %>%
  dplyr::select(-tmp) %>%
  ungroup() 


length(unique(tza_panelids$up_id[tza_panelids$bp == 1])) # number of full panel HH is 958

### orig hh flags
# create flag
tza_orig <- tza12345 %>%
  group_by(survey_hhid_y1) %>%  
  mutate(count = seq(n())) %>%
  mutate(orig_hh = case_when(count == 1 ~ 1,
                             count != 1 ~ 0)) %>%
  ungroup() %>%
  filter(orig_hh == 1)

# look up
tza_panelids <- tza_panelids %>%
  mutate(orig_hh = case_when(up_id %in% tza_orig$up_id ~ 1,
                             ! up_id %in% tza_orig$up_id ~ 0))


###################################################################################################################### UGA #####

ugayr1 <-  read_dta('./data_raw/UGA/uga10GSEC1.dta')
ugayr2 <-  read_dta('./data_raw/UGA/uga11GSEC1.dta')
ugayr3 <-  read_dta('./data_raw/UGA/uga13GSEC1.dta')
ugayr4 <-  read_dta('./data_raw/UGA/uga15GSEC1.dta')
ugayr5 <-  read_dta('./data_raw/UGA/uga18GSEC1.dta')
ugayr6 <-  read_dta('./data_raw/UGA/uga19GSEC1.dta')

# Uganda yrs 1:3 is a panel 

ugayr1 <- ugayr1 %>%
  mutate(y1_hhid = HHID) %>%
  dplyr::select(y1_hhid, HHID)

ugayr2 <- ugayr2 %>%
  mutate(y2_hhid = HHID) %>%
  dplyr::select(y2_hhid, HHID)

ugayr3 <- ugayr3 %>%
  mutate(y3_hhid = HHID) %>% 
  dplyr::select(y3_hhid, HHID_old) %>%
  mutate(HHID = as.character(HHID_old)) %>%
  dplyr::select(y3_hhid, HHID) 
  
uga12 <- full_join(ugayr1, ugayr2, by = "HHID")

uga123 <- full_join(uga12, ugayr3, by = "HHID")

# Uganda yrs 4:6 is a new panel
ugayr4 <- ugayr4 %>%
  mutate(y4_hhid = HHID) %>%
  dplyr::select(y4_hhid, HHID)

ugayr5 <- ugayr5 %>%
  mutate(y5_hhid = hhid) %>%
  dplyr::select(y5_hhid, t0_hhid) %>%
  mutate(HHID = t0_hhid) %>%
  dplyr::select(y5_hhid, HHID) 

ugayr6 <- ugayr6 %>%
  mutate(y6_hhid = hhid) %>%
  mutate(y5_hhid = hhidold) %>%
  dplyr::select(y6_hhid, y5_hhid)

uga45 <- full_join(ugayr4, ugayr5, by = "HHID")

uga456 <- full_join(uga45, ugayr6, by = "y5_hhid")


#### add survey hhids ect..


uga123 <- uga123 %>%
  mutate(country = "UGA") %>%
  dplyr::select(country, HHID, everything()) %>%   
  mutate(survey_hhid_y1 = ifelse(!is.na(y1_hhid), paste0(country, "_", y1_hhid, "_2010"), as.numeric(NA))) %>% 
  mutate(survey_hhid_y2 = ifelse(!is.na(y2_hhid), paste0(country, "_", y2_hhid, "_2011"), as.numeric(NA))) %>%
  mutate(survey_hhid_y3 = ifelse(!is.na(y3_hhid), paste0(country, "_", y3_hhid, "_2013"), as.numeric(NA))) %>%
  dplyr::select(-c(HHID, y1_hhid, y2_hhid, y3_hhid)) %>%
  mutate(up_id =  paste0(country,"_w123_",row_number())) # include waves of relevant panel in up_id
  

uga456 <- uga456 %>%
  mutate(country = "UGA") %>%
  dplyr::select(country, HHID, everything()) %>%
  mutate(survey_hhid_y4 = ifelse(!is.na(y4_hhid), paste0(country, "_", y4_hhid, "_2015"), as.numeric(NA))) %>% 
  mutate(survey_hhid_y5 = ifelse(!is.na(y5_hhid), paste0(country, "_", y5_hhid, "_2018"), as.numeric(NA))) %>%
  mutate(survey_hhid_y6 = ifelse(!is.na(y6_hhid), paste0(country, "_", y6_hhid, "_2019"), as.numeric(NA))) %>%
  dplyr::select(-c(HHID, y4_hhid, y5_hhid, y6_hhid)) %>%
  mutate(up_id =  paste0(country,"_w456_",row_number())) # include waves of relevant panel in up_id

uga123l <- melt(setDT(uga123), id.vars = c("country","up_id"), variable.name = "survey", value.name = "survey_hhid")  
uga456l <- melt(setDT(uga456), id.vars = c("country","up_id"), variable.name = "survey", value.name = "survey_hhid")
uga456l$survey_hhid <- toupper(uga456$survey_hhid) # convert to upper to match the processed survey files




uga_panelids <- rbind(uga123l, uga456l)



# add flag for balance panel

uga_panelids <- uga_panelids %>%
  group_by(up_id) %>%
  mutate(tmp = sum(!is.na(survey_hhid))) %>%
  mutate(bp = ifelse(tmp == 3, 1, 0)) %>%
  dplyr::select(-tmp) %>%
  ungroup()

# orig_hh flag

uga123_orig <- uga123 %>%
  group_by(survey_hhid_y1) %>%  
  mutate(count = seq(n())) %>%
  mutate(orig_hh = case_when(count == 1 ~ 1,
                             count != 1 ~ 0)) %>%
  ungroup() %>% 
  filter(orig_hh == 1) %>%
  dplyr::select(up_id)

uga456_orig <- uga456 %>%
  group_by(survey_hhid_y4) %>%  
  mutate(count = seq(n())) %>%
  mutate(orig_hh = case_when(count == 1 ~ 1,
                             count != 1 ~ 0)) %>%
  ungroup() %>%
  filter(orig_hh == 1) %>%
  dplyr::select(up_id)

uga_orig <- rbind(uga123_orig, uga456_orig)

# lookup orig panel flag

uga_panelids <- uga_panelids %>%
  mutate(orig_hh = case_when(up_id %in% uga_orig$up_id ~1,
                             ! up_id %in% uga_orig$up_id ~ 0))

ugatest <- filter(uga_panelids, bp == 1, orig_hh == 1)

########################################################################################### PANEL SUMMARY ALL COUNTIRES ######

panel_ids <- rbind(eth_panelids, mwi_panelids, ner_panelids, nga_panelids, tza_panelids, uga_panelids)
panel_ids <- panel_ids %>%
  dplyr::select(-survey)
write.csv(panel_ids, './lookup/up_ids.csv', row.names = FALSE)



# BALANCED PANELS
bps <- c(length(unique(eth_panelids$up_id[eth_panelids$bp == 1])), length(unique(mwi_panelids$up_id[mwi_panelids$bp == 1])),
         length(unique(nga_panelids$up_id[nga_panelids$bp == 1])), length(unique(ner_panelids$up_id[ner_panelids$bp == 1])),
         length(unique(tza_panelids$up_id[tza_panelids$bp == 1])), length(unique(uga_panelids$up_id[uga_panelids$bp == 1])))
countries <- c("ETH", "MWI", "NGA", "NER", "TZA", "UGA")

bps_summary <- data.frame(countries, bps)

bps_summary
