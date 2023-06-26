library(haven)
library(purrr)
library(zonator)
library(labelled)
library(tidyverse)

####### 1. READ IN THE LOOKUP FILES #######  
col_lookup <- read.csv('./lookup/col_lookup.csv')
label_lookup <- read.csv('./lookup/label_lookup.csv')

# create a column showing the file path in col lookup
col_lookup <- col_lookup %>% 
  mutate(raw_name_ext = paste0("data_raw/", raw_name))

# create unique id for each label in label lookup 

label_lookup <- label_lookup %>% mutate(concat = paste0(raw_name, col,orig_label))

####### 2. READ IN THE RAW SURVEY DATA ################

# create a vector of dataframes in the directory that end with .dta 
temp <- list.files(pattern="*.dta", recursive = TRUE)

# make another vector that only includes the files paths listed in col lookup, to avoid reading in unnecessary data files
temp2 <-  temp[temp %in% c(col_lookup$raw_name_ext)]

# create a vector of the file names (without extension)
object_names <- 
  temp2 %>%
  basename %>%
  file_path_sans_ext


# read in all the files from the file name vector and assign the file name as the name in the list
myfiles <- lapply(temp2, 
       read_dta) %>%
  setNames(object_names) 

# add a column to each df which includes the file name (used later for lookups)
myfiles <- mapply(cbind, myfiles, "file_name"= object_names, SIMPLIFY=F)


####### 3. LOOKUP STANDARDIZED COLUMN NAMES #######

# use lapply to update any column nanms that match with the column names in the look up table.
myfiles <- lapply(myfiles, function(x) {
  mm <- match(names(x), col_lookup$orig_col)
  names(x)[!is.na(mm)] <- as.character(col_lookup$new_col[na.omit(mm)])
  x
})


###### 4. RE-LABEL FOR STANDARDIZED LABELS ACROSS DFS #######
# convert dfs in list to long format for looking up in vector format
myfiles2 <- lapply(myfiles, function(x) pivot_longer(x, -c(hhid, file_name), names_to = "col", values_to = "value",
                                                   values_transform = list(value = as.character)))

# add the unique label id to each df in the list (unique label id = file name,column name, value)
myfiles2 <- lapply(myfiles2, transform, concat = paste0(file_name,col,value))


# if the unique id is in the look up table then return the new label based on the matching ids
myfiles2 <- lapply(myfiles2, function(x) {
  mutate(x,
         new_value= case_when(concat %in% label_lookup$concat ~ label_lookup$new_label[match(x$concat, label_lookup$concat)],
                              TRUE ~ value))})
# check it worked for 2019 data 
check <- myfiles2[[1]]
check2 <- myfiles2[[6]]

#########################  NEED TO RE-DO PIVOT LONGER SO THAT THERE IS ONE ROW PER ROUND IN THE UPD FILE!

####### FINAL STEP? SELECT ONLY RELEVANT COLUMNS #######

keep <- unique(col_lookup$new_col) # make a vector of the new column names from the look up table (this should cover all columns you are interested in)
myfiles3 <- lapply(myfiles2, function(x) subset(x, select = intersect(keep, colnames(x)))) # select columns that intersect with the column names in 'keep'
