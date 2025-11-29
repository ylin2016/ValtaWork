## 2024
library(plyr)
library(dplyr)
library(openxlsx)
setwd("/Users/ylin/My Drive/Cohost")
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

############### 2025 ########################

cohost_raw = read_excel('Working/Guest Reviews.xlsx',sheet = 'Reviews by cohost')
cohost = cohost_raw %>% select(Property,Cohost) %>%
  unique()  %>%
  mutate(listname = sapply(Property,function(x) {
    y=unlist(strsplit(x,'-'));y[length(y)]})) %>%
  mutate(listname = sub('Poulso','Poulsbo',listname)) %>%
  mutate(listname = sub('SeaTac','Seatac',listname)) %>%
  
  mutate(listname = sub('Mercer Island','Mercer',listname)) %>%
  mutate(listname = sub('Long branch','Longbranch',listname)) %>%
  mutate(listname = ifelse(grepl('Microsoft',listname),
                           sapply(listname,function(x)
                           {y=unlist(strsplit(x,' '));
                           paste0(y[1],' ',y[2],'-',y[3])}),listname)) %>%
  filter(!(grepl('Longbranch',listname) & Cohost %in% 'Lucia'))

cohost = cohost %>%
  mutate(listname = sub('Main House + ADU','Main',listname,fixed=T)) %>%
  rbind(cohost %>%
          filter(grepl("Mercer 3627",listname)) %>%
          mutate(listname = sub('Main House + ADU','ADU',listname,fixed=T))) %>%
  rbind(cohost %>%
          filter(grepl("Mercer 3627",listname)) %>%
          mutate(listname = "Mercer 3627")) %>%
  rbind.fill(data.frame(Property = c("SF-Seattle 3617","SF-Bellevue 13020","Condo-Microsoft 14645-C19"),
                        Cohost =c('Zoey','Xu','Zoey'),
                        listname=c("Seattle 3617","Bellevue 13020","Microsoft 14645-C19"))) %>%
  arrange(listname)

write.xlsx(cohost,'./Working/Property_Cohost.xlsx')

## 10/28/2025: Update property data with master file
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(googlesheets4)

property = read.xlsx('/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')

STR = read.xlsx("/Users/ylin/My Drive/Cohost/** Properties ** -- Valta/Listings, Team & Vendor Master Sheet.xlsx")
googlesheets4::gs4_auth()
LTR.raw = read_sheet("https://docs.google.com/spreadsheets/d/1YZu5Ppx6vnMnFGaL3SmNwOu1vH81lIaSCYYQSHiynpk/edit?gid=1565986358#gid=1565986358",
                 sheet = "Priority",skip=3)
googlesheets4::gs4_deauth()

LTR = LTR.raw[,1:6] 
LTR = LTR %>% 
  mutate(Type="LTR",Status ="Active",
         Listing = sub("-"," ",sub(" -","",Properties)),
         Listing = sub("Beadchwood","Beachwood",Listing))

txt = c("Microsoft D303","Microsoft E205","Microsoft C19","Cottage tiny",
        paste0("14507 4 plex ",1:4))
chg = c("Microsoft 14615-D303","Microsoft 14620-E205","Microsoft 14645-C19",
        "Cottage 11 (tiny)",paste0("Bellevue 14507U",1:4))
for(k in 1:length(txt)) LTR$Listing[LTR$Listing %in% txt[k]] = chg[k]


STR = STR %>% 
  select(Listing=Property,Status,City,Sq.Feet,Bed.room,Bath.room,Max.guests,Pet,King,Queen,Twin,
          Full,Sofa.Bed,Trundle.bed,Air.Matress,`Bunk.beds.(twin)`,
          `Bunk.beds.(full)`) %>%
  filter(!is.na(Listing)) %>%
  mutate(Type="STR",Listing = sub("(Caregiver)","",Listing,fixed=T))
colnames(STR) = gsub("[()]","",colnames(STR))

PropertyStatus = merge(STR %>% select(Listing,Status,Type,
                      Bed.room,Bath.room,Max.guests,Pet,King,Queen,Twin,
                      Full,Sofa.Bed,Trundle.bed,Air.Matress,Bunk.beds.twin,
                      Bunk.beds.full),
               LTR %>% select(Listing,Status,Group,Type),
               by="Listing",all=T) %>%
         mutate(Status = ifelse(is.na(Status.x),Status.y,Status.x),
                Type = ifelse(is.na(Type.x),Type.y,
                          ifelse(is.na(Type.y),Type.x,"Both")),
                furnished = ifelse(Type %in% "LTR",'No',"Yes")) %>%
         select(-c(Status.x,Status.y,Type.x,Type.y))

PropertyStatus %>% 
  group_by(Status,Type,furnished) %>% 
  reframe(nListing=n())

report = read.xlsx("/Users/ylin/My Drive/Cohost/Data and Reporting/03-Revenue & Pricing/Analytics/RevenueReport.xlsx",
                   startRow = 5,sheet = "Revenue") %>% select(Listing,Status)
# 
# property$rnk = 1:nrow(property)
# property_upd = merge(report,property,by="Listing",all.y=T) %>%
#   mutate(Type = ifelse(Status.x %in% c('STR',"LTR"),Status.x,Type),
#          Status = ifelse(Status.x %in% c('STR',"LTR"),"Active",
#                     ifelse(Status.x %in% c("Active","Inactive"),Status.x,Status.y))) %>%
#   arrange(rnk)
# 
# property_upd %>% select(Listing,Type,Status,Status.x,Status.y) %>% View()

# both = merge(property,PropertyStatus,by="Listing",all=T)
# write.csv(property_upd,"both_property.csv",row.names=F,na="")

PropertyStatus = property %>% 
  filter(!Listing %in% c("Beachwood","OSBR","Bellevue 14507","Seattle 906","Burien 14407")) %>%
    select(Listing,Type,Status,furnished) 
  
  
PropertyStatus %>% 
  group_by(Status,Type,furnished) %>% 
  reframe(nListing=n())

setdiff(property$Listing[property$Status %in% "Active"],report$Listing[report$Status %in% "Active"])
setdiff(property$Listing[property$Status %in% "Inactive"],report$Listing[report$Status %in% "Inactive"])
setdiff(property$Listing[property$Type %in% "LTR"],report$Listing[report$Status %in% "LTR"])

wts = read.xlsx("/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx",
                    sheet = "LaundryWeights")
property_upd = property %>% 
  select(Listing, OCCUPANCY, BEDROOMS, BEDS, BATHROOMS, Nking,Nqueen, Nfull,
                Ntwin,Sofa.Bed,Trundle.bed,Air.Matress) %>%
  replace(is.na(.), 0) %>%
  mutate(sheets.wt = (wts$Sheets[wts$Type %in% 'King'] * Nking +
                        wts$Sheets[wts$Type %in% 'Queen'] * Nqueen +
                        wts$Sheets[wts$Type %in% 'Full'] * Nfull +
      wts$Sheets[wts$Type %in% 'Twin'] * (Ntwin +Sofa.Bed+Trundle.bed)) / BEDS,
         blanket.wt = wts$quilt[wts$Type %in% 'King'] * Nking +
           wts$quilt[wts$Type %in% 'Queen'] * Nqueen +
           wts$quilt[wts$Type %in% 'Full'] * Nfull +
           wts$quilt[wts$Type %in% 'Twin'] * (Ntwin +Sofa.Bed+Trundle.bed))

## 2025.11.21 Update quilt and sheet weights based on the updated room numbers
##            check cleaning fee to Guesty value
propertyfile = '/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx'
property = read.xlsx(propertyfile)
ListingStatus = read.xlsx("/Users/ylin/My Drive/Cohost/Data and Reporting/03-Revenue & Pricing/Analytics/RevenueReport.xlsx",  
                          sheet = "Listing_Status") 
wts = read.xlsx(propertyfile,sheet = "LaundryWeights")
cleaning = read.xlsx(propertyfile,sheet = "Cleaning")

both = merge(cleaning[,c("Listing","Cleaning.fee")],
             ListingStatus[,c("Listing","Cleaning.fee(Guesty)")],by="Listing",all=T)
colnames(both)[3] = "cleaning_guesty"

both %>% filter(Cleaning.fee !=cleaning_guesty)

property_upd = read.csv("/Users/ylin/Downloads/property_info_update.csv")
property_upd$NICKNAME = trimws(property_upd$NICKNAME)
Property = merge(property,property_upd,by.x="Listing",by.y='NICKNAME',all=T)

Property %>% select(Listing,BEDROOMS,NUMBER.OF.BEDROOMS) %>%
  filter(BEDROOMS!=NUMBER.OF.BEDROOMS |(is.na(BEDROOMS) &!is.na(NUMBER.OF.BEDROOMS)))

Property %>% select(Listing,NUMBER.OF.BEDROOMS,TYPE.OF.BEDS) %>%
  write.csv("/Users/ylin/Downloads/beds.csv",row.names=F,na="")

## after update beds, recalculate weights:
property = read.xlsx(propertyfile)
property_upd = property %>% 
  select(Listing, BEDS, King,Queen, Full,
         Twin,Sofa.Bed,Trundle.bed,Air.Matress) %>%
  replace(is.na(.), 0) %>%
  mutate(sheets.wt = (wts$Sheets[wts$Type %in% 'King'] * King +
                        wts$Sheets[wts$Type %in% 'Queen'] * Queen +
                        wts$Sheets[wts$Type %in% 'Full'] * Full +
                        wts$Sheets[wts$Type %in% 'Twin'] * (Twin +Sofa.Bed+Trundle.bed)) / BEDS,
         blanket.wt = wts$quilt[wts$Type %in% 'King'] * King +
           wts$quilt[wts$Type %in% 'Queen'] * Queen +
           wts$quilt[wts$Type %in% 'Full'] * Full +
           wts$quilt[wts$Type %in% 'Twin'] * (Twin +Sofa.Bed+Trundle.bed))

property_upd %>% select(Listing,sheets.wt,blanket.wt) %>%
  write.csv("/Users/ylin/Downloads/laundry_wt.csv",row.names=F,na="")

## 2025.11.28: add Set 
property = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')

sets = read.xlsx("./Data and Reporting/03-Revenue & Pricing/Analytics/RevenueReport.xlsx",
                 sheet="Listing_Status")
property = merge(property,sets %>% select(Listing,Set),by="Listing",all.x=T)
property %>% select(Listing,Set) %>% write.csv("property_temp.csv",row.names=F,na="")
