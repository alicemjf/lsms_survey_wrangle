# cut functions
# not in use but saving incase useful snippets of code within them

# previous for loop (won't work over L)
for(country in countries){ # loop over each country 
  for(data_level in data_levels){ # & each data level (hh or indiv)
    data_level_subsets(country_id = country, hh_or_indiv = data_level) # this creates a list per country per dataframe
  }
}


## using lapply 
L2 <- lapply(countries, read_surveys, lu_df = col_lookup) # list elements are names as [1], [2] ect...

## using parallel processing (not working currently)
cl <- makeCluster(getOption("cl.cores", 6))
L3 <- parLapply(cl, countries, read_surveys, col_lookup = col_lookup)
stopCluster(cl)



# 3b. Function to create 2 dfs - one for all individual level data & one for household level data (for all countires)
all_cntry_dfs1 <- function(hh_or_indiv){
  # create vector of df names with the pattern (data level)
  tmp <- ls(pattern = hh_or_indiv, envir = .GlobalEnv)
  # use the character vector of df names in evaluation to bind_rows of dfs with those names
  tmp2 <- tmp %>%
    str_split(",", simplify = TRUE) %>%
    map(~eval(parse(text = .))) %>%
    bind_rows()
  
  # read in variables look up table
  vars_file <- paste0('./lookup/vars_', hh_or_indiv, '.csv')
  vars_df <- read.csv(vars_file) 
  # convert to vector
  vars <- vars_df$vars
  
  # select the columns from the joined file that are in the hh_vars list
  df <- tmp2[,which(colnames(tmp2) %in% vars)]  
  # if dtaa is at hh level, reshape df so there is one row per observation (an observation = a household), otherwise leave as is
  ifelse(hh_or_indiv == "hh", df <- df %>%
           group_by(survey_hhid) %>% # NEED TO GROUP BY YEAR TO AVOID GROUPING HH'S ACROSS YEARS
           summarize(across(everything(), ~ first(na.omit(.)))), df)
  
  # assign to global env
  assign(paste0(hh_or_indiv,"_all_cntry_bind"), df, envir = .GlobalEnv)
}

# 3a. Function to create sublists based on the level te survey data is collected at (individual or housrhold level)

data_level_subsets <- function(country_id, hh_or_indiv){
  # create subset of col lookup based on country and data level
  flt <- col_lookup %>% 
    filter(country == country_id & data_level == hh_or_indiv) %>%
    filter(orig_col != "no_var")
  unq_name4list <- unique(flt$name4list)
  # create name for output
  name <- paste0(country_id, "_", hh_or_indiv)
  # get the list corresponding to country name form the global env
  l <- get(country_id, envir = .GlobalEnv)
  # subset the list based on the elements that occur for that country at that data level (e.g. all TZA hh files)
  sublist <- l[unq_name4list] # could output from here if you want output as lists (next step turns to dataframes)
  df <- bind_rows(sublist) # bindrows of each sublist
  assign(name, df, envir = .GlobalEnv) # output to environment
}



## loop to subset into df levels

for(country in countries){ # loop over each country 
  for(data_level in data_levels){ # & each data level (hh or indiv)
    data_level_subsets(country_id = country, hh_or_indiv = data_level) # this creates a list per country per dataframe
  }
}



# make a df for each round in TZA UPD
for (i in 1:length(tza_rounds)) {
  assign(paste0("tzayr",i), subset(tza_panel, round == tza_rounds[[i]]))
}
