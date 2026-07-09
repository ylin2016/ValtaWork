#2026.6.25 Read json file for amentity

library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

# Read JSON
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-Valta AI/Amentity/")
listings <- fromJSON(
  "2026-06-22_all_listings_122.json",
  flatten = TRUE
)
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)

amenity_df <- listings %>%
  select(
    listing_id = `_id`,
    nickname,
    title,
    amenities
  ) %>%
  unnest_longer(amenities, values_to = "amenity") 

amenity_df = amenity_df %>% 
  filter(nickname %in% property$Listing[property$Status %in% "Active"])

#unique(amenity_df$amenity) %>% write.csv("tmp.csv")
amenity_grp = read.csv("./Amenity groups.csv")
groups = unique(amenity_grp$Group[!amenity_grp$Group %in% ""])
listings = unique(amenity_df$nickname)
amentity_map = vector('list',length(groups))
names(amentity_map) = groups

for(k in groups)
{
  tmp = amenity_df %>% 
    filter(amenity %in% amenity_grp$Amenity[amenity_grp$Group %in% k]) %>%
      select(nickname,amenity) %>%
      pivot_wider(names_from = amenity,values_from=amenity) 
  tmp[,-1] = apply(tmp[,-1,],2,function(x) ifelse(!is.na(x),"X",NA))  
  amentity_map[[k]] = rbind.fill(tmp,data.frame(nickname=setdiff(listings,tmp$nickname))) %>%
    arrange(nickname)
}
write.xlsx(amentity_map,"Guesty_amenities.xlsx",firstActiveRow = 2,withFilter = T)

                      