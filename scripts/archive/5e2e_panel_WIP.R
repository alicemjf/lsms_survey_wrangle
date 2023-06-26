# this script is a WIP for the end-to-end (e2e) automated wrangling & cleaning for household survey data.
# this is for 6 countries, for MULTIPLE yearA (TZA LSMS 2008, MWI LSMS 2010, ETH LSMS, NGA LSMS, NER LSMS, UGA LSMS)
# this builds on the script 4.e2eWIP which works for multiple country, ONE year
# this file will be a pre-cursor step to wrangling panel ids 
# everything in this file is applied in batch to all countries, all years

library(haven)
library(purrr)
library(zonator)
library(labelled)
library(tidyverse)
library(sjlabelled)
library(dplyr)

# source functions file
source('./wrangle_scripts/functions.R')


##################################################### 1. READ IN THE LOOKUP FILES ##############################################  
col_lookup <- read.csv('./lookup/col_lookup_panel.csv', na.strings = "")

# create a column showing the file path in col lookup
# and create a column with the object name for the list- country &  file name (w/o file extension)
col_lookup <- col_lookup %>% 
  mutate(raw_name_ext = paste0("data_raw/",country,"/" , raw_name)) %>%
  mutate(name4list = paste0(country,"_",sans_ext,"_",year))

# create a vector of the file names which are at the hh level
hh_level <- col_lookup$name4list[col_lookup$data_level == "hh"]
hh_level <- unique(hh_level)

# create a vector of the file names which are at the indiv level
indiv_level <- col_lookup$name4list[col_lookup$data_level == "indiv"]
indiv_level <- unique(indiv_level)

################################################## 2. READ IN THE RAW SURVEY DATA #############################################

# create a vector of dataframes in the directory that end with .dta 
temp <- list.files(pattern="*.dta", recursive = TRUE)

# make another vector that only includes the files paths listed in col lookup
# this avoids reading in unnecessary data files
temp2 <-  temp[temp %in% c(col_lookup$raw_name_ext)]

# create a vector of the file names (without extension)
temp3 <- 
  temp2 %>%
  basename %>%
  file_path_sans_ext

# include country and year in the name in the file name
country <- col_lookup$country[match(temp3, col_lookup$sans_ext)]
year <- col_lookup$year[match(temp3, col_lookup$sans_ext)]
object_names <- paste0(country, "_", temp3, "_",year)

# read in all the files from the file name vector (temp 2) 
# assign the file name as the name in the list
myfiles <- lapply(temp2, 
                  read_dta) %>%
  setNames(object_names) 

# add a column to each df which includes the file name 
myfiles <- mapply(cbind, myfiles, "file_name"= object_names, SIMPLIFY=F)

# replace values with haven labels (converts any haven labelled object to a factor e.g. replaces 1 and 2 with "male", "female")
myfiles <- lapply(myfiles, haven::as_factor)

##################################################### 3. CHECK ALL FILES READ IN ############################################################# 
 col_lookup <- col_lookup %>%
   mutate(tmp = paste0(country,"_", sans_ext, "_", year)) %>%
   mutate(check = if_else(tmp %in% object_names, "Y", "N"))

# APPLY FUNCTION TO CHECK IF FILES READ IN CORRECTLY, IF FILE IS MISSING ITS NAME WILL BE PRINTED
missing_files_fun(col_lookup)

###################################################. 4. ADD META DATA TO EACH FILE ##########################################################

# add a column to each df in the list contains the country code
myfiles <- mapply(cbind, myfiles, "country"=substr(names(myfiles),0,3), SIMPLIFY=F) # country code is taken from the first 3 letters of the object name

# add a column to each df in the list contains the survey year
myfiles <- mapply(cbind, myfiles, "af_year"=str_sub(names(myfiles),-4,-1), SIMPLIFY=F) # year is taken from the last 4 letters of the object name - called af_year because some files already have "year" columns in them

 ################################################## 5. STANDARDIZE COLUMN NAMES ########################################################

# Column names are updated per country, to avoid clashes in column names across country surveys
country_vctr <- unique(country) # make vector of list of countries in file

myfiles2 <- list() # make empty list to add new data to

for(id in country_vctr){
  filt <- col_lookup %>% filter(country == id) # filter look up table for ech country id
  
  l <- myfiles[substr(names(myfiles),0,3) == (id)] # filter myfiles for each country id
  
  # now for each list of l, look up the name column names from the col lookup df, filtered by country id
  l <- lapply(l, function(x) { 
    mm <- match(names(x), filt$orig_col)
    names(x)[!is.na(mm)] <- as.character(filt$new_col[na.omit(mm)])
    x
  })
  # assign to the new list
  myfiles2[[id]] <- l
}

# output of FOR loop is nested lists - applying unlist to reduce nesting by one level
myfiles2 <- unlist(unname(myfiles2), recursive = FALSE) # unname prevents nested list name been appended to the unlisted lists

 
################################################### 6.NER 2014 id IISUE FIX ###################################################################

# NER datafiles do not come with a unique hhid, they need to be created through the concatenation of 3 columns
#paste0(GRAPPE, MENAGE, EXTENSION)
myfiles2 <- lapply(myfiles2, transform, hhid2=ifelse(country == "NER" & af_year == "2014", paste0(GRAPPE, MENAGE, EXTENSION), hhid))

# add a column that is country_hhid to create a new hhid 
myfiles2 <- lapply(myfiles2, transform, af_hhid = paste0(country, "_", hhid2))

################################################## 7. CONVERT ALL VARS TO CHARACTER ################################################# 

# this prevents "can't combine errors" when mergings dfs 
myfiles2 <- lapply(myfiles2, function(x) {x <- lapply(x, as.character);x})


############################################## 8.SEPERATE 'MYFILES' LIST BASED ON LEVEL OF DATA COLLECTION - HH LEVEL OF INDIVIDUAL LEVEL #######################

####### HH level ############
# make a new list from myfiles
# elements in the new list have names that appears in the hh_level vector
# i.e. this list is for the dataframes where data is collected at the level of the household

by_hhid <- myfiles2[hh_level]

# join the hhid elements by row to make one big dataframe for all countries
by_hhid_join <- bind_rows(by_hhid) 

# select my vars - df is too big to work with all columns (processing time is slow)
# read in file of list of variables of interest
vars_df <- read.csv('./lookup/vars.csv') 
# convert to vector
vars <- vars_df$vars
# create vector for just the hh variables
tmp_vars <- filter(vars_df, level != "indiv") 
hh_vars <- tmp_vars$vars  

# select the columns from the joined file that are in the hh_vars list
by_hhid_join <- by_hhid_join[,which(colnames(by_hhid_join) %in% hh_vars)]  

# reshape df so there is one row per observation (an observation = a household)
by_hhid_join <- by_hhid_join %>%
  group_by(af_year, af_hhid) %>% # NEED TO GROUP BY YEAR TO AVOID GROUPING HH'S ACROSS YEARS
  summarize(across(everything(), ~ first(na.omit(.))))


####### Indiv level ############
# do the same for the indiv
by_indiv<- myfiles2[indiv_level]

by_indiv_join <- bind_rows(by_indiv)
by_indiv_join <- by_indiv_join[,which(colnames(by_indiv_join) %in% vars)] 


############################################ 9. SUMMARISE FROM INDIV TABLE TO HH LEVEL TABLE ############################################

### First need to add column that shows hh head sex (for one hh the value will be repeated over several rows)

# read in lookup table to replace any foreign language labels
labels <- read.csv('./lookup/labels.csv')
by_indiv_join <- by_indiv_join %>%
  mutate(tmp = case_when(relation_head %in% labels$label ~ labels$new_label[match(by_indiv_join$relation_head, labels$label)],
                         TRUE ~ relation_head)) # create a temp column, if the label is in the lookup table, return the new lableL, otherwise leave as is
by_indiv_join <- by_indiv_join %>%
  mutate(relation_head = tmp) %>% # replace relation_head with the temp column (which includes the replaced foreign values for Head)
  dplyr::select(-tmp)


# convert all characters to upper case to have same labels across countries
by_indiv_join$relation_head <- toupper(by_indiv_join$relation_head) 


# create a temporary df of hh's that have either 0 or more than 1 reported HH head (these hh's break the code)
tmp <- by_indiv_join %>%
  mutate(tmp = case_when(relation_head == "HEAD" ~ "1",
                         relation_head != "HEAD" ~ "0")) %>%
  mutate(tmp = as.numeric(tmp)) %>%
  group_by(af_year, af_hhid) %>%
  summarise(head_count = sum(tmp, na.rm = TRUE)) %>%
  filter(head_count != 1)


# Create the column showing sex of hh head
by_indiv_join <- by_indiv_join %>%
  filter(! af_hhid %in%  tmp$af_hhid) %>% # filter out hh's that appear in the tmp file (i.e. have 0 or more than one HEAD)
  filter(complete.cases(af_hhid, relation_head)) %>% # filter for complete cases (no NAs in the relation to head or sex file)
  group_by(af_year, af_hhid) %>%
  mutate(hh_head_sex = if_else(relation_head == "HEAD", sex, sex[relation_head == "HEAD"])) %>% # create a column that shows sex of head of household
  ungroup()


###### Second create columns showing number of dependents (0-14, > 65 years old) and non dependents (15-64 years old)

by_indiv_join <- by_indiv_join %>%
  group_by(af_year, af_hhid) %>%
  mutate(young_dependents = sum(age %in% 0:14)) %>%
  mutate(old_dependents = sum(age > 64)) %>%
  mutate(non_dependents = sum(age %in% 15:64)) %>%
  mutate(depend_ratio = ((young_dependents + old_dependents)/non_dependents))



# !!!!!!!! what to do when dependence ratio = Inf? (i.e. bottom of eq is 0)??

######## Next make summary table
summary <- by_indiv_join %>% 
  group_by(af_hhid, af_year) %>% # group by id
  dplyr::summarize(hh_size = n(),# hh size is the number of times each hhid appears
                   hh_head_sex = hh_head_sex, # hh_head_sex is taken from the indiv id table (created in lines 85&86)
                   dpnd_ratio = depend_ratio) %>% # depend ratio taken from the indiv table (create in line 136-141)
  unique() # return unique rows (without this we get many duplicate rows)

############################################################ 10.CREATE FINAL DF ###########################################################
# Join the summary table with the hhid table
mydf <- left_join(by_hhid_join, summary, by = c("af_hhid", "af_year"))

# re-order columns
mydf <- dplyr::select(mydf, af_hhid, country, af_year, urb_rur, hh_head_sex, hh_size, dpnd_ratio, dist_road, cooking_fuel_prm) 



###################################################### STANDARDIZE LABELS ###############################################################################

# FUEL LABELS

# convert fuel & sex labels to capital letters
mydf$cooking_fuel_prm <- toupper(mydf$cooking_fuel_prm)
mydf$hh_head_sex <- toupper(mydf$hh_head_sex)
mydf$urb_rur <- toupper(mydf$urb_rur)

# make df of all fuel labels & export to create a new lookup table
fuels <- data.frame(unique(mydf$cooking_fuel_prm))
write.csv(fuels, "./lookup/fuels.csv", row.names = FALSE) # export file 
# manual editing is required on the exported lookup file, need to decide on re-labelling for fuels  

# read in edited lookup tables for fuels
fuelslookup <- read.csv("./lookup/fuels2.csv", na.strings = "")

mydf <- mydf %>%
  ungroup() %>%
  mutate(tmpfuel = fuelslookup$relabel[match(mydf$cooking_fuel_prm, fuelslookup$unique.mydf.cooking_fuel_prm.)]) %>%
  mutate(cooking_fuel_prm_grp = fuelslookup$group[match(mydf$cooking_fuel_prm, fuelslookup$unique.mydf.cooking_fuel_prm.)])# look up grouping of cooking fuels (have create grouping for ease of anaylsis)
mydf <- mydf %>%
  mutate(cooking_fuel_prm = tmpfuel) %>% # replace primary fuel column with the new labels
  dplyr::select(- tmpfuel) # get rid of temp column


# STILL NEED TO STANDARDIZE OTHER LABELS - SEX, URB/RUR ECT..

