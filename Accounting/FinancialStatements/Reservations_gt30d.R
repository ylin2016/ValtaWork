library(plyr)
library(dplyr)
library(lubridate)
library(openxlsx)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/04-Accounting/DeferRevenue/")
data = read.csv("./Data/Guesty_gt30d_reservation-20260202.csv")
data$From = as.Date(data$CHECK.IN)
data$To = as.Date(data$CHECK.OUT)

# months = data.frame(CHECK.IN = seq(as.Date("2024-01-01"),length=24,by="months"),
#                     CHECK.OUT= seq(as.Date("2024-02-01"),length=24,by="months")-1)

data = data %>% filter(data$CHECK.OUT>=as.Date("2025-12-01")) %>%
  mutate(CHECK.IN = substr(CHECK.IN,1,10),
         CHECK.OUT =substr(CHECK.OUT,1,10))
res = NULL
for(i in 1:nrow(data))
{
  From = data$From[i]
  To = data$To[i]
  breaks <- seq(floor_date(From, "month") + months(1),
                ceiling_date(To, "month"),
                by = "month")
  
  #days = To-From 
  #seqs =30*(0:floor(days/30))
  ranges = data.frame(CHECK.IN = c(From, breaks[-length(breaks)]),
                      CHECK.OUT = pmin(breaks, To))
  # ranges = months[months$CHECK.OUT>From & months$CHECK.IN<To,]
  # ranges[1,1] = From
  # ranges[nrow(ranges),2] = To
  ranges = ranges %>% mutate(Platform = data$SOURCE[i],
                             Type = "Payment",
                             Reservation = data$CONFIRMATION.CODE[i],
                             Name = data$GUEST.S.NAME[i],
                             Listing = data$LISTING.S.NICKNAME[i],
                             Nights= CHECK.OUT-CHECK.IN,
                             TotalPay = data$TOTAL.PAYOUT[i],
                             TotalNight = data$NUMBER.OF.NIGHTS[i])
  ranges$Nights[nrow(ranges)] = ranges$Nights[nrow(ranges)]
  
  ranges = ranges %>% 
            mutate(TOTAL.PAYOUT = round(as.numeric(Nights*TotalPay/TotalNight),2)) %>% 
            mutate(TotalPay=NULL,TotalNight=NULL,
                   YearMonth = substr(as.character(CHECK.IN),1,7))
  temp = rbind.fill(data.frame(Platform = data$SOURCE[i],
                               Type = c("Invoice","Def Rev"),
                               Reservation = data$CONFIRMATION.CODE[i],
                               Name = data$GUEST.S.NAME[i],
                               Listing = data$LISTING.S.NICKNAME[i],
                               CHECK.IN = rep(data$CHECK.IN[i],2),
                               CHECK.OUT = c(data$CHECK.OUT[i],NA),
                               Nights= c(data$NUMBER.OF.NIGHTS[i],NA),
                               Cleaning = NA,
                               TOTAL.PAYOUT = c(data$TOTAL.PAYOUT[i],NA)),
                    ranges)
  res = rbind(res,temp[c(1,3,2,4:nrow(temp)),])
}
write.csv(res,'Reservation_gt30d_MonthlyPay_20260202.csv',row.names=F,na='')
