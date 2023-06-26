# sankey function
library(networkD3)

nga <- filter(hh_all_cntry_panels, country == "NGA", relbl_urb_rur == "RURAL", bp == 1)
nga <- dplyr::select(nga, up_id, year, cooking_fuel_prm_grp)
nga_w <- reshape(nga, idvar = "up_id", timevar = "year", direction = "wide")

nga_w <- nga_w %>% 
  filter(complete.cases(.))


nga_w[] <- paste(str_sub(col(nga_w, TRUE), -4, -1), as.matrix(nga_w), sep = "_") # paste the last 4 characters of the column name (which will be the year) to each value 

nga_w <- dplyr::select(nga_w, -up_id) # get rid of id col

# rename cols to y20XX 
n <- unique(paste0("y", (str_sub(col(nga_w, TRUE), -4, -1))))
nga_w <- setNames(nga_w, n)



## set up Sankey dfs
# how to code this programtically?
trans1_2 <- nga_w %>% group_by(y2010, y2012) %>% summarise(sum=n())
trans2_3 <- nga_w %>% group_by(y2012, y2015) %>% summarise(sum=n())
trans3_4 <- nga_w %>% group_by(y2015, y2018) %>% summarise(sum=n())

### set up links and nodes dfs - needed for the sankey network

colnames(trans1_2)[1:2] <- colnames(trans2_3)[1:2] <- colnames(trans3_4)[1:2] <- c("source","target")

links <- rbind(as.data.frame(trans1_2), 
               as.data.frame(trans2_3), 
               as.data.frame(trans3_4))

nodes <- data.frame(name=unique(c(links$source, links$target)))
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "sum", NodeID = "name",
              fontSize = 12, nodeWidth = 30)



sn <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                    Target = "target", Value = "sum", NodeID = "name",
                    fontSize = 16)
sn