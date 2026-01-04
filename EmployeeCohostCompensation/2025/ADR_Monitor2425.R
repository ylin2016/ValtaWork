## Monitor Average Daily Rates by property, compare same month on different years
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost")
property = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
guesty2024 = read.csv('./Data and Reporting/Input_PowerBI/Guesty_Booking_2024_20250214.csv')
sum24 = guesty2024 %>% 
  mutate(Year = substr(CHECK.OUT,1,4),Month = substr(CHECK.OUT,6,7)) %>% 
  filter(Year==2024) %>% 
  group_by(LISTING.S.NICKNAME,Year,Month) %>% 
  reframe(AvgDailyRate = mean(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          MaxDailyRate = max(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          Nights = sum(NUMBER.OF.NIGHTS))

g2501 = read.csv('./Cohost Cleaner Compensation/Working/Data/2025/Guesty_booking_confirmed_2025-01.csv')
sum25 = g2501 %>% 
  mutate(Year = substr(CHECK.OUT,1,4),Month = substr(CHECK.OUT,6,7),
         Earnings = ifelse(!is.na(TOTAL.PAID), TOTAL.PAID,TOTAL.PAYOUT)) %>% 
  filter(Year==2025 & Month=='01') %>% 
  group_by(LISTING.S.NICKNAME,Year,Month) %>% 
  reframe(AvgDailyRate = mean(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          MaxDailyRate = max(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          Nights = sum(NUMBER.OF.NIGHTS)) 

sum_all = merge(sum25,sum24, by=c('LISTING.S.NICKNAME','Month'),suffix = c("",".2024"),all.x=T)

sum_all = sum_all %>% 
  mutate(DailyRateChanges = ifelse(!is.na(AvgDailyRate.2024),paste0(round(100*
             (AvgDailyRate-AvgDailyRate.2024)/AvgDailyRate.2024,1),"%"),NA),
         Year.2024=NULL) %>%
  mutate_at(vars(AvgDailyRate,MaxDailyRate,AvgDailyRate.2024,MaxDailyRate.2024),round) 

write.xlsx(sum_all,"./Data and Reporting/03-Revenue & Pricing/Property_dailyrate_comparing_2024_2025-20250216.xlsx",
           firstActiveRow = 2,withFilter = T)

