a <- read_dta("data_raw/HH_SEC_A.dta")
mm <- match(names(a), col_lookup$orig_col)
names(a)[!is.na(mm)] <- as.character(col_lookup$new_col[na.omit(mm)])

a <- remove_labels(a)
a_lng <- a %>% pivot_longer(-c(interview__key, hhid, hh_a06), names_to = "col", values_to = "value",
                               values_transform = list(value = as.character)) # put survey data in longer format for working with the lookup table


a_lng <- a_lng %>% mutate(concat = paste0(col,value)) # CREATE unique id for each col name + label
label_lookup <- label_lookup %>% mutate(concat = paste0(col,orig_label)) # CREATE unique id for each col name + label

a_lng <- a_lng %>%
  mutate(value = dplyr::case_when(concat %in% label_lookup$concat ~ label_lookup$new_label[match(big_test$concat, label_lookup$concat)],
                           TRUE ~ value)) # if the unique id is in the look up table then return the new label based on the matching ids
  
### to make this work across a list of dfs need to include the file name in the concat_id..to make sure it is the
### label for that file
##########################################################################################################################

# IGNORE BELOW HERE...THIS WAS FIGURING OUT THE APPRAOCH










 #mutate(value = label_lookup$new_label[match(big_test$concat, label_lookup$concat)])

#label_lookup2 <- label_lookup %>% pivot_longer(c(orig_label, new_label), names_to = "type", values_to = "label") # label needs to be in long format (I think)
#############

lbl <- match(names(a), label_lookup$col) # which column names of the df are in the col column of the lookup table
tmp <- a[!is.na(lbl)] # columns in df which are in the label_lookup table i.e. these columns need re labeling


#test <- match(names(a)[!is.na(lbl)], label_lookup$col) # index of the rows where columns in a match column names in label look up
#label_lookup$new_label[test]

tmp <- remove_val_labels(tmp)

urb_indx <- tmp$urb_rur %in% label_lookup$orig_label # index of the points where the label is the type that needs changing
tmp$urb_rur[urb_indx] <- as.numeric(label_lookup$new_label[match(tmp$urb_rur, label_lookup$orig_label)[label_lookup$col %in% "urb_rur"]])
 
merge(tmp, label_lookup, by.x = "urb_rur", by.y = "col", all.x = TRUE)
?merge

test <- replace(tmp$urb_rur, tmp$urb_rur[urb_indx], label_lookup$new_label[match(tmp$urb_rur, label_lookup$orig_label)])

### trying with case when. When column name of A is in the lookup table column.. do something (doesn't work though)

b <- a %>% test = dplyr::case_when(names(a) %in% label_lookup$col ~ label_lookup$new_label[match(a[!is.na(lbl)], label_lookup$orig_label)],
                                   TRUE ~ 0)