# WIP file to do V1 but in a more structured, country level approach

####### This code has been split in 01_read and 02_clean scirpts!

################################################### 0. LOAD LIBS & SOURCE FUNCTIONS ###################################################

library(haven)
library(purrr)
library(zonator)
library(labelled)
library(tidyverse)
library(sjlabelled)
library(dplyr)
library(parallel)

# source functions file
source('./scripts/functions.R')
  
############################################################################################### 1. READ IN THE LOOKUP FILES

col_lookup <- read_lookup_tbl("./lookup/col_lookup_panel.csv") 
# N.B - any variable you want to work with MUST be in the lookup file
# only variables listed in the lookup file get read in!

######################################################################################################## 2. READ IN SURVEYS

countries <- unique(col_lookup$country) # create vectors of unique countries

L <- sapply(countries, read_surveys, lu_df = col_lookup, USE.NAMES = TRUE) # using sapply to enable naming of list elements with country vector


######################################################################################### 3. MERGE INTO DATAFRAMES (HH & INDIV) 

data_levels <- unique(col_lookup$data_level, na.rm = TRUE) # vector unique options for data level

# 3a. Create lists of hh and indiv data
IN <- lapply(L, function(y) y[str_subset(names(y), "indiv")])
HH <- lapply(L, function(y) y[str_subset(names(y), "hh")])

# 3b. Merge hh data for all countries, and merge indiv data for all countries
hh_all_cntry <- all_cntry_dfs(L = HH, hh_or_indiv = "hh")
indiv_all_cntry <- all_cntry_dfs(L = IN, hh_or_indiv = "indiv")


##################################################################################### 4. SUMMARIZE FROM INDIV TO HH LEVEL 

# 4a. Function for pre-summary tidy

pre_sum_tidy(indiv_all_cntry)

# 4b. Function for summary 

summary_fun(indiv_all_cntry)

###################################################################################################### 5. RE-LABELLING

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
relabel(indiv_all_cntry)
relabel(hh_summary)

########################################################################################### 6. TESTING DATA IS PRESENT

#> Create test functions to make sure all expected data is there (every expected country, every expected year)
#> Test that for a given var, not all values are NA (e.g. check if all UGA 2010 cooking fuel is NA - this means something has gone wrong..)
#> Other tests? 


############################################################################################# 7. LOOK UP UP_IDs

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


