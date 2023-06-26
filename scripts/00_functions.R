## Functions


# 1. Function to read in lookup table, and create and necessary concatendated columns for lookups ############
read_lookup_tbl <- function(filename) {
  df <- read.csv(file = filename, na.strings = "")
  df$raw_name_ext <- paste0("data_raw/",df$country,"/" , df$raw_name)
  df$name4list <- paste0(df$country, "_" , df$module, "_" , df$data_level, "_" , df$year)
  df$orig_col_id <- paste0(df$year, "_", df$orig_col) # prefix each column name with year so that column name look ups are year specific
  return(df)
  #assign("col_lookup", col_lookup, envir = .GlobalEnv) better practice to assign within the script, not in the function
  
}


# 2. Function to read in as a list the .dta files in country directory that are in the lookup table (i.e. only required files) ###########
# function updates list element names to module names & removes Haven labels
# function also updates column names according to the name in the lookup table
# function also creates a unique household survey id (survey_hhid) which is unique to each household in a survey year (this is the key to use for merging)
# output of this is a list per country with the relevant dataframes, with all cars as characters


read_surveys <- function(country, lu_df){
  library(dplyr)
  library(haven)
  library(stringr)
  # filter out NAs in lu_df raw names
  lu_df <- lu_df[! is.na(lu_df$raw_name),]
  # generate correctly formatted list of file names (incl file path)
  temp <- list.files(path = paste0('./data_raw/', country), pattern = "*.dta") # list of all dta files in country folder
  filenames <-  temp[temp %in% c(lu_df$raw_name)] # subset of files - only those in the col_look up table
  objectnames <- lu_df$name4list[match(filenames, lu_df$raw_name)] # set object names (these will be given to the df when read in)
  filenames <- paste0('./data_raw/', country, "/" , filenames) # modify the list of file names to include the path 'data_raw/country...so that they are found by the read_dta function
  
  # identify which columns are needed for each country
  tmp <- lu_df[lu_df$country == country,]
  vars <- unique(tmp$orig_col)
  
  # read in the list of files and set object names
  myfiles <- lapply(filenames, 
                    read_dta, col_select = any_of(vars)) %>% # read in the data for any variables in vars (skip anything else - avoids reading in millions of columns)
    setNames(objectnames)
  
  # remove Haven labels
  myfiles <- lapply(myfiles, haven::as_factor)
  
  # add year as column to each df (based on last 4 letters of object name which are the year)
  myfiles <- mapply(cbind, myfiles, "yr"=str_sub(names(myfiles),-4,-1), SIMPLIFY=F) 
  
  # add year as a prefix to each column name (this is used to look up the correct column for the correct year)
  for(i in 1:length(myfiles)) {
    prefix <- unique(myfiles[[i]]$yr)
    
    myfiles[[i]] <- myfiles[[i]] %>% 
      rename_with( ~ paste(prefix, ., sep = "_"))
  }
  
  # filter lookup table by country
  filt <- lu_df[lu_df$country == country,] 
  
  # look up the name column names from the col lookup df, filtered by country id
  myfiles <- lapply(myfiles, function(x) { 
    mm <- match(names(x), filt$orig_col_id)
    names(x)[!is.na(mm)] <- as.character(filt$new_col[na.omit(mm)])
    x
  })
  
  # add year column again to make column name for year generic 
  # (due to earlier steps current year columns are prefixed with the year e.g. 2018_year)
  myfiles <- mapply(cbind, myfiles, "year"=str_sub(names(myfiles),-4,-1), SIMPLIFY=F) 
  
  # add country as a column 
  myfiles <- mapply(cbind, myfiles, "country" = country, SIMPLIFY=F)
  
  # NER 2014 & 2018 id issue fix (using a nested if)
  myfiles <- lapply(myfiles, transform, hhid2=ifelse(country == "NER" & year == "2014", paste0(`EA`, `MENAGE`, `EXTENSION`), # grappe gets renamed to EA in ealrier rows
                                                     ifelse(country == "NER" & year == "2018", paste0(`EA`, `menage`), hhid)))
  
  # create unique household survey id key
  myfiles <- lapply(myfiles, transform, survey_hhid = paste0(country, "_", hhid2, "_", year)) 
  
  # convert all vars to characters for now to prevent issues when merging files
  myfiles<- lapply(myfiles, function(x) {x <- lapply(x, as.character);x})
  
  # assign to environment
  return(myfiles)
  #assign(country, myfiles, envir = .GlobalEnv) # assign to global env, using character string of argument as name
  
}


# 3. Functions for creating merged dfs at hh and indiv level #############################################

# 3b. Function to create 2 dfs - one for all individual level data & one for household level data (for all countires)

# v3
all_cntry_dfs <- function(L, hh_or_indiv){
  # bindrows of list at level [[1]]
  tmpL <- lapply(L, function(y) bind_rows(y))
  # bind rows again to create a df
  tmp_df <- bind_rows(tmpL)
  
  # read in variables look up table
  vars_file <- paste0('./lookup/vars_', hh_or_indiv, '.csv')
  vars_df <- read.csv(vars_file) 
  # convert to vector
  vars <- vars_df$vars
  
  # select the columns from the joined file that are in the hh_vars list
  df <- tmp_df[,which(colnames(tmp_df) %in% vars)]  
  # if data is at hh level, reshape df so there is one row per observation (an observation = a household), otherwise leave as is
  ifelse(hh_or_indiv == "hh", df <- df %>%
           group_by(survey_hhid) %>%
           summarise(country = country,
                     year = year,
                     strata1 = ifelse(all(is.na(strata1)), as.character(NA), first(na.omit(strata1))),
                     dist_road = ifelse(all(is.na(dist_road)), as.character(NA), first(na.omit(dist_road))),
                     urb_rur = ifelse(all(is.na(urb_rur)), as.character(NA), first(na.omit(urb_rur))),
                     cooking_fuel_prm = ifelse(all(is.na(cooking_fuel_prm)), as.character(NA), first(na.omit(cooking_fuel_prm))),
                     lat_mod = ifelse(all(is.na(lat_mod)), as.character(NA), first(na.omit(lat_mod))),
                     lon_mod = ifelse(all(is.na(lon_mod)), as.character(NA), first(na.omit(lon_mod))),
                     EA = ifelse(all(is.na(EA)), as.character(NA), first(na.omit(EA))),
                     hh_weight = ifelse(all(is.na(hh_weight)), as.character(NA), first(na.omit(hh_weight))),
                     hh_weight_p = ifelse(all(is.na(hh_weight_p)), as.character(NA), first(na.omit(hh_weight_p)))) %>%
           unique(), df)
  
  # assign to global env
  return(df)
  #assign(paste0(hh_or_indiv,"_all_cntry_bind"), df, envir = .GlobalEnv)
}


# 4. Function to summarize vars from individual level to household level #################################

# 4a. Before summarizing, some tidying of the indiv data is needed
# set up to work without dplyr because dplyr is complicated within functions

pre_sum_tidy <- function(df){
  
  # tidy household head labels (replace french labels with english)
  labels <- read.csv('./lookup/labels.csv')
  labels$label <- toupper(labels$label)
  labels$new_label <- toupper(labels$new_label)
  df$relation_head <- toupper(df$relation_head)
  df$tmp <- ifelse(df$relation_head %in% labels$label, labels$new_label[match(df$relation_head, labels$label)], df$relation_head)
  df$relation_head <- df$tmp
  df <- df[,! names(df) %in% c("tmp")]
  
  
  # get rid of households with no household head or more than one household head listed
  tmp_df <- df
  tmp_df$tmp <- ifelse(tmp_df$relation_head == "HEAD", 1,0) # flag for head y or n
  tmp_df$tmp <- as.numeric(tmp_df$tmp)
  agg_tmp <- aggregate(tmp ~ survey_hhid , data=tmp_df, FUN=sum) # count of number of heads per hh
  agg_tmp <- agg_tmp[agg_tmp$tmp != 1, ] # subset where head is 0 or more than 1 head
  
  df <- df[! df$survey_hhid %in% agg_tmp$survey_hhid,] # filter out hhs that have more than one or 0 household head
  df <- df[complete.cases(df$relation_head, df$survey_hhid),]
  #filter(complete.cases(survey_hhid, relation_head))
  
  # convert all characters to upper case to have same labels across countries
  df$sex <- toupper(df$sex)
  df$age <- as.numeric(df$age)
  df <- as.data.frame(df)
  
  # assign to global env
  assign("indiv_all_cntry", df, envir = .GlobalEnv)
}



# 4b. Summary function 
# this function creates a summary table showing dependency ratio, sex ratio, sex of hh head

summary_fun <- function(df) {
  hh_summary <- df %>%
    group_by(survey_hhid) %>%
    dplyr::summarize(country = country,
                     young_dependents = sum(age %in% 0:14, na.rm = TRUE),
                     old_dependents = sum(age > 64, na.rm = TRUE),
                     non_dependents = sum(age %in% 15:64, na.rm = TRUE),
                     depend_ratio= young_dependents + old_dependents/non_dependents,
                     count_f = sum((substr(sex,0,1)) == "F", na.rm = TRUE),
                     count_m = sum((substr(sex,0,1)) == "M", na.rm = TRUE),
                     sex_ratio = (count_m/count_f)*100,
                     hh_head_sex = sex[relation_head == "HEAD"],
                     hh_size = n()) %>%
    unique()
  hh_summary <- as.data.frame(hh_summary) # stop it from defaulting to tibble
  assign("hh_summary", hh_summary, envir = .GlobalEnv)
}


# 5. Relabel functions ################################################

# 5b - functions to writse csv for unique columns of each vlaue in a df 
# outputs are manually edited into look up tables

unique_vals_csv_fun <- function(df){
  
  cols <- colnames(df)
  
  for (col in cols) {
    unq <- unique(df[,col])
    write.csv(unq, paste0('./lookup/tmp/', col, '.csv'), row.names = FALSE) 
  }
}

# 5c. Re-label function
# read in csv in the relabel folder and update columns accordingly (files in relabel folder have been manually edited to show correct new labels)

relabel <- function(df) {
  cols <- colnames(df)
  files <- list.files(path = './lookup/relabel', pattern = ".csv")
  name <- deparse(substitute(df)) # for output df
  
  
  for (col in cols) {
    tmp <- paste0(col,'2.csv') # paste 2 to col name is this is how the files are named in the subdirectory
    
    # if the column isn't in the list of files in relabel directory, skip to next iteration
    if(! tmp %in% files) next
    
    #read in relabel csv for the column
    nm <- paste0('./lookup/relabel/', col, '2.csv')
    lbl_df <- read.csv(nm, na.strings = "")
    
    # lookup new labels
    df[,paste0("relbl_",col)] <- lbl_df$relabel[match(df[,col], lbl_df$x)]
    
    # look up fuel group labels for primary cooking fuel
    if(col != "cooking_fuel_prm") next # if the column is not primary cooking fuel, skip to next iteration
    df$cooking_fuel_prm_grp <- lbl_df$group[match(df[,col], lbl_df$x)] # if the column is primary cooking fuel, look up the group label 
  }
  
  
  assign(name, df, envir = .GlobalEnv)
}





# Function to check if all files listed in the col_lookup table have been read in successfully ####################################################




# unused in current version

missing_files_fun <- function(df){
  p <-  filter(df, check == "N" & ! is.na(sans_ext))
  ifelse(nrow(p) >= 1, print(unique(p$raw_name)), "all files read in successfully") # if a file has not been read it, its name will be printed
}






