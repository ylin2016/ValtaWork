## Format VRBO records to guesty records for input of PowerBi
## Format CH & VRBO records (from RevenueProjection 2022, 2023) to guesty records for input of PowerBi
## 10/18/2023: add Sept/Oct booking from guesty adj by airbnb
## 11/08/2023: Using all past booking data pulled today + airbnb_01_2023-12_2023.csv + airbnb_pending.csv
## 12/31/2023: Using all past booking data pulled today + airbnb_01_2023-12_2023.csv + airbnb_pending.csv

library(readxl)
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost")
property = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
                 
##===========Guesty record adjusted by airbnb payout ============================###
#guesty = read.csv('./Data and Reporting/Data/2024/Guesty_Booking_2024_20240302.csv')
#guesty = read.csv('./Data and Reporting/Data/2024/Guesty_Booking_2024_20240401.csv')
guesty = read.csv('./Data and Reporting/Data/2024/Guesty_Booking_2024_20250214.csv')#20240601.csv')
guesty$Earnings = ifelse(!guesty$TOTAL.PAID %in% c(NA,0), guesty$TOTAL.PAID,
                         guesty$TOTAL.PAYOUT)
write.csv(guesty,'Data and Reporting/Input_PowerBI/Guesty_Booking_2024_20250214.csv',
           row.names=F,na='')

## correct guesty record airbnb payout
#abpay = read.csv('./Data and Reporting/Data/2023.01.01 to 2023.12.31.csv')
# abpay1 = read.csv('./Data and Reporting/Data/airbnb_01_2023-12_2023.csv')
# abpay2 = read.csv('./Data and Reporting/Data/airbnb_pending.csv')
# abpay2022 = read.csv('./Data and Reporting/Data/2020.01 to 2022.12 Airbnb transactions.csv')
# abpay = rbind(abpay1,abpay2,abpay2022)
# abpay = abpay[!abpay$Confirmation.Code %in% c(NA,''),]
# dup = duplicated(abpay$Confirmation.Code)
# dups = abpay[abpay$Confirmation.Code %in% abpay$Confirmation.Code[dup],]
# dups = dups %>% arrange(Confirmation.Code)
# ab.payout = abpay %>% group_by(Confirmation.Code,Start.Date,Nights,Guest,Listing) %>% 
#   reframe(airbnb.payout = sum(Amount))
# abpay2022 = ab.payout %>% filter(Confirmation.Code %in% abpay2022$Confirmation.Code)
# 
# guesty = merge(guesty,ab.payout[,c('Confirmation.Code','airbnb.payout')], 
#                by.x = 'CONFIRMATION.CODE', by.y='Confirmation.Code',all.x=T)
# 
# diff = guesty %>% 
#   filter(SOURCE %in% c('Airbnb') & (abs(Earnings -airbnb.payout) > 1e-6)) %>%
#   select(CONFIRMATION.CODE,LISTING.S.NICKNAME,TOTAL.PAYOUT,Earnings,airbnb.payout)
# 
# idx =which(guesty$SOURCE %in% 'Airbnb' & 
#              abs(guesty$Earnings -guesty$airbnb.payout) > 1e-6) 
# guesty$Earnings[idx] = guesty$airbnb.payout[idx]
# guesty[,c('inquiryId','airbnb.payout')] = NULL
# guesty$LISTING.S.NICKNAME = sub("Seattle 3617 Origin","Seattle 3617",guesty$LISTING.S.NICKNAME)
# write.csv(guesty,'Data and Reporting/Input_PowerBI/Guesty_PastBooking_airbnb_adj_12312023.csv',
#           row.names=F,na='')


