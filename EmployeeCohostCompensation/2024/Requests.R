## from 202406 add duplicated records
library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
library(lubridate)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/")
format_reservation <- function(data,startdate,enddate){
  colnames(data) = toupper(colnames(data))
  data$CheckIn = as.Date(data$CHECK.IN)
  data$CheckOut = as.Date(data$CHECK.OUT)
  data = data[data$CheckOut>=startdate & data$CheckOut<=enddate,]
  data$Earnings = as.numeric(ifelse(!is.na(data$TOTAL.PAID) & data$TOTAL.PAID>0,data$TOTAL.PAID,data$TOTAL.PAYOUT))
  data = data[,c('LISTING.S.NICKNAME','CONFIRMATION.CODE','STATUS','GUEST.S.NAME','NUMBER.OF.GUESTS',
                 'CheckIn','CheckOut','NUMBER.OF.NIGHTS','Earnings','SOURCE')]
  colnames(data) = c('Listing','Confirmation.Code','Status','GuestName','Guests','CheckIn',
                     'CheckOut','Nights','Earnings','Source')
  data$Comment = data$Cleaner = data$CohostPayOut = data$Backup = NA
  data
}

data = NULL
for(k in c(paste0('2024-0',5:9),paste0('2024-',10:12),paste0('2025-0',1:5)))
    {
      startdate = paste0(k,"-01") 
      enddate = as.character(as.Date(startdate)+days_in_month(startdate)-1)
      fileloc = paste0('./Working/Data/',substr(k,1,4),'/')
      confirmed = read.csv(paste0(fileloc,'Guesty_booking_confirmed_',k,'.csv'))
      data = rbind(data,data.frame(Yearmonth =k,
                                   format_reservation(confirmed,startdate,enddate)))
}    
sum(dup<-duplicated(paste(data$Listing,data$GuestName))) 

data[paste(data$Listing,data$GuestName) %in% paste(data$Listing,data$GuestName)[dup],] %>%
  arrange(Listing,CheckIn) %>%
  write.csv("Duplicated.csv",row.names=F)
