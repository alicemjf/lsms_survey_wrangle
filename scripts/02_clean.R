
######################################################################################### 1. MERGE INTO DATAFRAMES (HH & INDIV) 

data_levels <- unique(col_lookup$data_level, na.rm = TRUE) # vector unique options for data level

# 3a. Create lists of hh and indiv data
IN <- lapply(L, function(y) y[str_subset(names(y), "indiv")])
HH <- lapply(L, function(y) y[str_subset(names(y), "hh")])

# 3b. Merge hh data for all countries, and merge indiv data for all countries
hh_all_cntry <- all_cntry_dfs(L = HH, hh_or_indiv = "hh")
indiv_all_cntry <- all_cntry_dfs(L = IN, hh_or_indiv = "indiv")


##################################################################################### 2. SUMMARIZE FROM INDIV TO HH LEVEL 

# 2a. Function for pre-summary tidy

pre_sum_tidy(indiv_all_cntry)

# 2b. Function for summary 

summary_fun(indiv_all_cntry)

###################################################################################################### 3. RE-LABELLING

# 5a. convert all character vectors in df to upper case (helps with labels)
hh_all_cntry <- data.frame(lapply(hh_all_cntry, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


# 5b. export a csv of the unique values per column (values need to be manually reviewed & re-labelled)

unique_vals_csv_fun(hh_all_cntry)
unique_vals_csv_fun(indiv_all_cntry)
unique_vals_csv_fun(hh_summary)


# 5c. Re-label function -  read in csv in the relabel folder and update columns accordingly

relabel(hh_all_cntry)
# manual hack for a strata name issue
hh_all_cntry <- hh_all_cntry %>%
  mutate(relbl_strata1 = if_else(country == "MWI"& year == "2016"& strata1 == "LILONGEWE", "LILONGWE CITY", relbl_strata1))
hh_all_cntry$relbl_strata1 <- trimws(hh_all_cntry$relbl_strata1)

relabel(indiv_all_cntry)
relabel(hh_summary)



############################################################################################# 4. LOOK UP UP_IDs

up_ids <- read.csv('./lookup/up_ids.csv') # read in matching file
# do with left join instead of match[]

# join to create a separate df. Using universal panel ids results in duplicate rows for some households in a given survey year 
# this is due to split off hh i.e. if hh1 has 5 split off hhs in round 4, there will be 5 rows for hh1 in round 1 
# because each of the 5 split hhs will have their own up_id and hh1 is round 1 is included in each as the base

# ** N>B !!!
# in the all_cntry_panel datasets the level of obs is a panel sequence (grouped by up_id). Don't work with survey_hhid in this df

hh_all_cntry_panels <- left_join(hh_all_cntry, up_ids)%>%
  dplyr::select(up_id, survey_hhid, everything()) %>%
  filter(! is.na(up_id))


indiv_all_cntry_panels <- left_join(indiv_all_cntry, up_ids) %>%
  dplyr::select(up_id, survey_hhid, everything()) %>%
  filter(! is.na(up_id))

hh_summary_panels <- left_join(hh_summary, up_ids) %>%
  dplyr::select(up_id, survey_hhid, everything())%>%
  filter(! is.na(up_id))



####################################################################################### 5. OUTPUTS CLEANED FILES

write.csv(hh_all_cntry, './outputs/cleaned_data/1_hh_all_cntry_xsec.csv', row.names = FALSE)
write.csv(hh_all_cntry_panels, './outputs/cleaned_data/2_hh_all_cntry_panels.csv', row.names = FALSE)
write.csv(indiv_all_cntry, './outputs/cleaned_data/3_indiv_all_cntry_xsec.csv', row.names = FALSE)
write.csv(indiv_all_cntry_panels, './outputs/cleaned_data/4_indiv_all_cntry_panels.csv', row.names = FALSE)
write.csv(hh_summary, './outputs/cleaned_data/5_hh_summary_xsec.csv', row.names = FALSE)
write.csv(hh_summary_panels, './outputs/cleaned_data/6_hh_summary_panels.csv', row.names = FALSE)


######################################################################################## 6. APPLY CORRECT VECTOR TYPES TO VARIABLES

# create vector of unique variable names in all dfs of interest
all_vars <- unique(c(names(hh_all_cntry), names(hh_all_cntry_panels), names(hh_summary), names(hh_summary_panels), names(indiv_all_cntry), names(indiv_all_cntry_panels)))
#write.csv(all_vars, './lookup/tmp/vars.csv', row.names = FALSE)

# read in edited csv which includes flag for variable type
var_classes <- read.csv('./lookup/vars2.csv')

# For each of the dfs of interest, change character class based on the class column of var_classes$class column #######
# code solution to do this from here: https://stackoverflow.com/questions/65541605/writing-a-custom-function-to-convert-class-of-variables-in-a-dataframe-based-on
hh_all_cntry <- hh_all_cntry %>% 
  mutate(across(any_of(var_classes$var_name), 
                ~get(paste0("as.",var_classes[var_classes$var_name == cur_column(),"class"]))(.x)))

hh_all_cntry_panels <- hh_all_cntry_panels %>% 
  mutate(across(any_of(var_classes$var_name), 
                ~get(paste0("as.",var_classes[var_classes$var_name == cur_column(),"class"]))(.x)))


hh_summary <- hh_summary %>% 
  mutate(across(any_of(var_classes$var_name), 
                ~get(paste0("as.",var_classes[var_classes$var_name == cur_column(),"class"]))(.x)))

hh_summary_panels <- hh_summary_panels %>% 
  mutate(across(any_of(var_classes$var_name), 
                ~get(paste0("as.",var_classes[var_classes$var_name == cur_column(),"class"]))(.x)))

indiv_all_cntry <- indiv_all_cntry %>% 
  mutate(across(any_of(var_classes$var_name), 
                ~get(paste0("as.",var_classes[var_classes$var_name == cur_column(),"class"]))(.x)))


indiv_all_cntry_panels <- indiv_all_cntry_panels %>% 
  mutate(across(any_of(var_classes$var_name), 
                ~get(paste0("as.",var_classes[var_classes$var_name == cur_column(),"class"]))(.x)))





