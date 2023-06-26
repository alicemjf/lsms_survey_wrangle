# this script is a WIP for the end-to-end (e2e) automated wrangling & cleaning for household survey data.
# this is for a single country, for a single year.
# the script 4.e2eWIP tests out using the code for 2 countries

library(haven)
library(purrr)
library(zonator)
library(labelled)
library(tidyverse)
library(sjlabelled)

##################################################### 1. READ IN THE LOOKUP FILES ##############################################  
col_lookup <- read.csv('./lookup/col_lookup08.csv')

# create a column showing the file path in col lookup
col_lookup <- col_lookup %>% 
  mutate(raw_name_ext = paste0("data_raw/", raw_name))

# create a vector of the file names which are at the hh level
hh_level <- col_lookup$sans_ext[col_lookup$data_level == "hh"]
hh_level <- unique(hh_level)
# create a vector of the file names which are at the indiv level
indiv_level <- col_lookup$sans_ext[col_lookup$data_level == "indiv"]
indiv_level <- unique(indiv_level)

################################################## 2. READ IN THE RAW SURVEY DATA #############################################

# create a vector of dataframes in the directory that end with .dta 
temp <- list.files(pattern="*.dta", recursive = TRUE)

# make another vector that only includes the files paths listed in col lookup
# this avoids reading in unnecessary data files
temp2 <-  temp[temp %in% c(col_lookup$raw_name_ext)]

# create a vector of the file names (without extension)
object_names <- 
  temp2 %>%
  basename %>%
  file_path_sans_ext


# read in all the files from the file name vector (temp 2) 
# assign the file name as the name in the list
myfiles <- lapply(temp2, 
                  read_dta) %>%
  setNames(object_names) 

# add a column to each df which includes the file name 
# this is used later for lookups
myfiles <- mapply(cbind, myfiles, "file_name"= object_names, SIMPLIFY=F)

# replace values with haven labels (converts any haven labelled object to a factor e.g. replaces 1 and 2 with "male", "female")
myfiles <- lapply(myfiles, haven::as_factor)

################################################## 3. RENAME COLUMN NAMES ########################################################

# need to rename columns before joining dfs so that hhid label is standardized
# use lapply to update any column names that match with the column names in the look up table.

myfiles <- lapply(myfiles, function(x) {
  mm <- match(names(x), col_lookup$orig_col)
  names(x)[!is.na(mm)] <- as.character(col_lookup$new_col[na.omit(mm)])
  x
})

############################################## 4.SEPERATE 'MYFILES' LIST BASED ON LEVEL OF DATA COLLECTION - HH LEVEL OF INDIVIDUAL LEVEL #######################

# make a new list from myfiles
# elements in the new list have names that appears in the hh_level vector
# i.e. this list is for the dataframes where data is collected at the level of the household
by_hhid <- myfiles[hh_level]

# join the elements of by_hhid list by hhid
by_hhid_join <- by_hhid %>% reduce(left_join, by = "af_hhid") # this returns a dataframe
# test <- list(by_hhid_join) if you want to convert back to a list

# do the same for the indiv
by_indiv<- myfiles[indiv_level]
# don't need to join for indiv in this file

by_indiv_join <- by_indiv[[1]] # making a df for the sake of look ups (normally this would be the result of the join function)

############################################ 5. SUMMARISE FROM INDIV TABLE TO HH LEVEL TABLE ############################################

# First add column for hh head sex
by_indiv_join <- by_indiv_join %>%
  group_by(af_hhid) %>%
  mutate(hh_head_sex = case_when(relation_head == "HEAD" ~ sex, 
                                 relation_head != "HEAD" ~ sex[relation_head == "HEAD"])) %>%
  ungroup()

# Next make summary table
summary <- by_indiv_join %>% 
  group_by(af_hhid) %>% # group by id
  dplyr::summarize(hh_size = n(),# hh size is the number of times each hhid appears
                   hh_head_sex = hh_head_sex) %>% # hh_head_sex is taken from the indiv id table (created in lines 85&86)
  # this step is where we could inlucde e.g. the dependency section
  unique() # return unique rows (without this we get many duplicate rows)
  

# Join the summary table with the hhid table
by_hhid_join <- left_join(by_hhid_join, summary, by = "af_hhid")

by_hhid_join$hh_head_sex

# select the vars I want
vars <- read.csv('./lookup/vars.csv') # read in file of list of variables
vars <- vars$vars # convert to vector

myvars <- by_hhid_join %>% dplyr::select(vars) # select the columns from the joined file that are in the vars list


# checks
#hist(myvars$hh_head_sex)
#max(myvars$hh_size)
#check <- filter(myvars, hh_size > 15) # some households are very large in size

#check2 <- filter(by_indiv_join, af_hhid %in% "14020040030072") # this hh has 46 rows - and it has 46 rows in the member roster so not a mistake?

#myvars$hh_head_sex <- as.factor(myvars$hh_head_sex)
#ggplot(myvars, aes(x=hh_head_sex)) + geom_bar()
#ggplot(myvars, aes(x=urb_rur)) + geom_bar()
#ggplot(myvars, aes(x=cooking_fuel_prm)) + geom_bar()
