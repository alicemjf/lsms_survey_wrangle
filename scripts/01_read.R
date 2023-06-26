
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
source('./scripts/00_functions.R')

############################################################################################### 1. READ IN THE LOOKUP FILES

col_lookup <- read_lookup_tbl("./lookup/col_lookup_panel.csv") 
# N.B - any variable you want to work with MUST be in the lookup file
# only variables listed in the lookup file get read in!

######################################################################################################## 2. READ IN SURVEYS

countries <- unique(col_lookup$country) # create vectors of unique countries

L <- sapply(countries, read_surveys, lu_df = col_lookup, USE.NAMES = TRUE) # using sapply to enable naming of list elements with country vector

