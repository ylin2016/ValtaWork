## Monitor Average Daily Rates by property, compare same month on different years
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost")
property = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
#guesty2024 = read.csv('./Data and Reporting/Input_PowerBI/Guesty_Booking2024_20240518.csv')
guesty2024 = read.csv('./Data and Reporting/Input_PowerBI/Guesty_Booking_2024_20250214.csv')
guesty2023 = read.csv('./Data and Reporting/Input_PowerBI/Guesty_PastBooking_airbnb_adj_12312023.csv')
CHs =  read.csv('./Data and Reporting/Input_PowerBI/Rev_CH_2023.csv')
Vrbo2023 = read.csv('./Data and Reporting/Input_PowerBI/VRBO_20200101-20231230.csv')
CH2023 = CHs %>% filter(substr(CHECK.OUT,1,4)==2023) 
dat2023 = rbind(guesty2023,CH2023,Vrbo2023)

sum23 = dat2023 %>% 
    mutate(Year = substr(CHECK.OUT,1,4),Month = substr(CHECK.OUT,6,7)) %>% 
    filter(Year==2023) %>% 
    group_by(LISTING.S.NICKNAME,Year,Month) %>% 
    reframe(AvgDailyRate = mean(Earnings/NUMBER.OF.NIGHTS),
            MedianDailyRate = median(Earnings/NUMBER.OF.NIGHTS),
            MinDailyRate = min(Earnings/NUMBER.OF.NIGHTS),
            MaxDailyRate = max(Earnings/NUMBER.OF.NIGHTS),
            Nights = sum(NUMBER.OF.NIGHTS))
sum24 = guesty2024 %>% 
  mutate(Year = substr(CHECK.OUT,1,4),Month = substr(CHECK.OUT,6,7)) %>% 
  filter(Year==2024) %>% 
  group_by(LISTING.S.NICKNAME,Year,Month) %>% 
  reframe(AvgDailyRate = mean(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          MedianDailyRate = median(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          MinDailyRate = min(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          MaxDailyRate = max(TOTAL.PAYOUT/NUMBER.OF.NIGHTS),
          Nights = sum(NUMBER.OF.NIGHTS))

sum_all = merge(sum24,sum23,
                by=c('LISTING.S.NICKNAME','Month'),all=T,suffix = c("",".2023"))
sum_all = sum_all %>% 
  mutate(Year=2024,
         DailyRateChanges = ifelse(!is.na(AvgDailyRate.2023),
                  (AvgDailyRate-AvgDailyRate.2023)/AvgDailyRate.2023,NA))
View(sum_all)
old = read.xlsx("./Data and Reporting/03-Revenue & Pricing/Property_dailyrate_comparing_2023_2024.xlsx") 
old$idx = 1:nrow(old)
both = merge(sum_all,old,by=c("LISTING.S.NICKNAME","Month","Year"),suffix=c('','.old'),all=T)
both = both %>% arrange(idx)
write.xlsx(both,"./Data and Reporting/03-Revenue & Pricing/Property_dailyrate_comparing_2023_2024-20250214.xlsx")
changed = sum_all %>% group_by(LISTING.S.NICKNAME) %>% 
  reframe(changes = sum(!is.na(DailyRateChanges))>0)
listing = changed$LISTING.S.NICKNAME[changed$changes]

library(ggplot2)
data = rbind(sum23,sum24) 
data = merge(data,sum_all[sum_all$Year==2024,c('LISTING.S.NICKNAME','Year','Month',"DailyRateChanges")],
             by=c('LISTING.S.NICKNAME','Year','Month'),all.x=T)
data$changes = ifelse(is.na(data$DailyRateChanges),'',
                      ifelse(data$DailyRateChanges>0,'',paste0(round(data$DailyRateChanges*100,1),"%")))

pdf("./Data and Reporting/RevenueProjection/AverageDailyRateChanges2024.pdf",width = 9,height=11)
npg = ceiling(length(listing)/6)
for(i in 1:npg)
{
  starts = (i-1)*6+1
  ends = ifelse(i*6>length(listing),length(listing),i*6)
  listing.sel = listing[starts:ends]
  print(ggplot(data %>% filter(LISTING.S.NICKNAME %in% listing.sel),
        aes(Month,AvgDailyRate,fill=Year,color=Year)) + 
        geom_bar(stat='identity',position=position_dodge2(0.7)) + 
        geom_text(aes(label=changes),color='black',size=3,
                  position=position_dodge2(0.9),vjust=0) +
  facet_wrap(~LISTING.S.NICKNAME,ncol=2,scale='free'))
}
dev.off()
