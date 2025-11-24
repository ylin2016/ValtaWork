library(dplyr)
library(plyr)
library(openxlsx)
library(reshape2)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Data and Reporting/03-RevenueProjection/")
format_reservation <- function(data,startdate,enddate){
  data$CheckIn = as.Date(data$CHECK.IN)
  data$CheckOut = as.Date(data$CHECK.OUT)
  data = data[data$CheckOut>=startdate & data$CheckOut<=enddate,]
  data$Earnings = as.numeric(ifelse(!is.na(data$TOTAL.PAID),data$TOTAL.PAID,data$TOTAL.PAYOUT))
  data = data[,c('LISTING.S.NICKNAME','CONFIRMATION.CODE','status','GUEST.S.NAME','NUMBER.OF.GUESTS',
                 'CheckIn','CheckOut','NUMBER.OF.NIGHTS','Earnings','SOURCE')]
  colnames(data) = c('Listing','Confirmation.Code','Status','GuestName','Guests','CheckIn',
                     'CheckOut','Nights','Earnings','Source')
  data$Comment = data$Cleaner = data$CohostPayOut = data$Backup = NA
  data
}

guesty23 = read.csv('../Data/2023/Guesty_PastBooking_12312023.csv',
                     stringsAsFactors = F,na.strings = c(NA,"")) %>%
           filter(CHECK.OUT>='2023-01-01')
guesty24 = read.csv('../Data/2024/Guesty_Booking_2024_20240601.csv')
guesty = rbind(guesty23,guesty24)
colnames(guesty)[c(8,7,12)] = c("Guests",'Nights',"Listing")
guesty$Platform = guesty$SOURCE
guesty$Platform[guesty$SOURCE %in% c('BE-API','manual','website','expediaIntegrated')] = 'Direct'
guesty$Platform[guesty$SOURCE %in% c('HomeAway CA','HomeAway DE','HomeAway UK',
                                     'Vrbo','Vrbo Canada')] = 'VRBO'
guesty$Platform[guesty$SOURCE %in% c('Booking Engine')] = 'Booking.com'
guesty$Platform[guesty$SOURCE %in% 'homesVillasByMarriott']='Marriott'

nonguesty = read.xlsx('./2024 Reservation data.xlsx',sheet="Not Guesty",startRow = 2) %>% 
  mutate(CHECK.IN = as.Date(From.date,origin= '1899-12-30'),
         CHECK.OUT = as.Date(as.integer(To.date),origin= '1899-12-30'),
         Platform ="NotGuesty")
colnames(nonguesty)[c(7,12,11)] = c("Nights","TOTAL.PAYOUT",'Cleaning.fee')
reference = read.xlsx('./2024 Reservation data.xlsx',sheet="Reference") 
varname = c("Platform","Listing","CHECK.IN","CHECK.OUT",'Cleaning.fee',
            "TOTAL.PAYOUT","Guests",'Nights')

dat = rbind.fill(guesty[,varname],nonguesty[,intersect(colnames(nonguesty),varname)]) %>%
       mutate(Quarter = quarters(as.Date(CHECK.OUT)),Year=substr(CHECK.OUT,1,4),
              Listing = sub("Seattle 3617 Origin","Seattle 3617",Listing))
table1 = dat %>% group_by(Listing,Platform,Year) %>% 
  reframe(Revenue=round(sum(TOTAL.PAYOUT,na.rm=T))) %>% 
  dcast(Year+Listing ~ Platform)

YearTotal = merge(table1 %>% filter(Year==2023) %>% mutate(Year=NULL),
      table1 %>% filter(Year==2024) %>% mutate(Year=NULL),
      by="Listing",all=T,suffixes = c(".2023",".2024"))

table2024 = merge(YearTotal, reference[,c("Property","Airdna.revenue","Airdna.occupancy",
                                "Airdna.nightly.rate","Zillow.estimate")],
               by.x='Listing',by.y='Property',all.x=T)
View(table2024)

tableQ = dat %>% group_by(Listing,Year,Quarter) %>% 
  reframe(NightRate = round(mean((TOTAL.PAYOUT-Cleaning.fee)/Nights,na.rm=T))) %>% 
  dcast(Year+Listing ~ Quarter)


table2024 = merge(table2024,tableQ %>% filter(Year==2024) %>% mutate(Year=NULL),
                  by="Listing",all.x=T)

table2024 = merge(table2024,tableQ %>% filter(Year==2023) %>% mutate(Year=NULL),by="Listing",
                  all.x=T,suffixes = c(".2024",".2023"))
write.csv(table2024,'./RevenueSummary2024.csv',row.names=F,na='')
