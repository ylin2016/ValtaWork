library(plyr)
library(dplyr)
library(openxlsx)
setwd("/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/")
cleaner = read.xlsx("./Working/Data/Cleaning Schedule and Price.xlsx",sheet='Sheet2')
data = read.csv(paste0('./Working/Data/2024/Guesty_booking_confirmed_2024-04.csv'))

property = data %>% 
        select(LISTING.S.NICKNAME,Cleaning.fee,LISTING.S.CITY,TYPE.OF.PROPERTY,
               TYPE.OF.LISTING, NUMBER.OF.BEDROOMS,NUMBER.OF.BEDS,NUMBER.OF.BATHROOMS) %>% 
        unique() %>% 
        arrange(LISTING.S.NICKNAME)
View(property)

cleaner = cleaner %>% filter(!is.na(Cleaning.fee)) %>% 
    mutate(remote = ifelse(Group %in% 'In town',"In town","Remote"))
summary(lm<-lm(Cleaning.fee ~ Bed + Bath,data=cleaner))
summary(lm<-lm(Cleaning.fee ~ Bed + Bath + Sq.Ft,data=cleaner))
summary(lm<-lm(Cleaning.fee ~ Bed + Bath + Sq.Ft + remote,data=cleaner))
summary(lm<-lm(Cleaning.fee ~ Bed + Bath + Sq.Ft + Cleaner.lead,data=cleaner))

## 5/11/2024 : 

listing_feature = read.xlsx("./Working/Data/Listing_property.xlsx")
cleaner = read.xlsx("./Working/Data/Cleaning Schedule and Price.xlsx",sheet='Sheet2')

listings = merge(listing_feature,cleaner,by.x='NICKNAME',by.y='Property',all.x=T) %>%
  filter(!NICKNAME %in% c('Keaau 15-1542','Ocean Spray Resort'))
View(listings)



