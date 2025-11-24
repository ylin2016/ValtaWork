library(readxl)
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost")
property = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
property.vrbo = vrbo %>%
  group_by(Property.ID,Unit.ID,Address) %>%
  reframe(reservation=n())

property.guesty = guesty %>%
  group_by(LISTING.S.NICKNAME, LISTING.S.CITY,listing.address.full) %>%
  reframe(reservation=n()) %>%
  mutate(address = sapply(listing.address.full,
                          function(x) unlist(strsplit(x,','),use.names = F)[1]))
names(property.guesty$address) = NULL

write.csv(property.vrbo,'property_vrbo.csv',row.names = F,na='')
## manually fill the listname since the address is so different between 2 forms

property.vrbo = read.csv('property_vrbo.csv')

# 2 vrbo properties were not on guesty #44406 SE 144th St #21833 8th Pl W

property.vrbo = merge(property.vrbo, property.guesty,
                      by.x='listname',by.y='LISTING.S.NICKNAME',all=T)
property = merge(property,property.guesty[,c('LISTING.S.NICKNAME', 'LISTING.S.CITY',
                                             'listing.address.full')],by.x='listname',by.y='LISTING.S.NICKNAME',all.x=T)
property = merge(property,property.vrbo[,c(1:3,5)],by='listname',all.x=T)
write.csv(property,'Property_Cohost.csv',row.names=F,na='')
