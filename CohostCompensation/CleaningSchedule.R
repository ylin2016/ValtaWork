## 2025.9.16: Align daily cleaning schedule, income, and 
#              Maria's payout and compare the cost
## - Guesty checkout
## - maria cleaning payout
## - maria car schedule 

library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(reshape2)
library(scales)

setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/05-Cleaning/")

###============= daily payout ============= 
payout = vector('list',12)
for(i in 1:10)
{
  tmp = read.xlsx("./Data/Maria cleaning payment process_copied20250916.xlsx",
                  sheet=paste0("2025-",i))
  idx = grep("Property name",tmp[,1])[1]+1
  idx = ifelse(is.na(idx),1,idx)
  indv = tmp[c(idx:(grep("Total payment",tmp[,1])[1]),
               grep("payout",tmp[,1]),grep("Due",tmp[,1])),1:5]
  colnames(indv) = c("Listing","Cleaning.fee","Times","Total","PayDate")
  payout[[i]] = indv
}

###=============cleanings per day by guesty ============= 

property = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx')
cleaner = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx',sheet='Cleaning') %>%
  mutate(Cleaning.fee = ifelse(is.na(New.cleaning.fee), 
                               Current.clg.fee,New.cleaning.fee))
cleanings = NULL
for(k in paste0(20250,1:8))
  cleanings = rbind.fill(cleanings,
            read.xlsx("../01- Compensation Calculation/Cohost's reservation sheets/CleaningSheet_PayOut.xlsx",sheet = k))
cleanings = cleanings %>% 
  mutate(CheckIn=as.Date(CheckIn,origin='1899-12-30'),
         CheckOut=as.Date(CheckOut,origin='1899-12-30'),
         Month = ifelse(Month==45658,"2025-01",Month))
cleanings = merge(cleanings, cleaner %>% select(Cleaner.lead,Listing,Maria.pay),
                  by=c('Cleaner.lead','Listing'),all.x=T)

##============= LRT cleaning ============= 

LRTs = read.xlsx("./Data/Valta Homes Residential Cleaning Schedule and Payment Record.xlsx") %>%
  select(Service.Date,Name,Address,`Maria.(.80.of.Column.K)`)
colnames(LRTs)= c("CheckOut","GuestName","Listing","Maria.pay")
LRTs = LRTs %>% 
  filter(!is.na(CheckOut) & !grepl("Total|TOTAL",CheckOut)) %>%
  mutate(CheckOut = as.Date(as.integer(CheckOut),origin= '1899-12-30'),
         Month = substr(CheckOut,1,7),Cleaner.lead="Maria",
         Cleaning.fee=Maria.pay/0.8)

allcleanings = rbind.fill(cleanings,LRTs)

## ==========================================
payCleaner = read.xlsx("./Data/Maria cleaning payment process_copied20250916.xlsx",
                       sheet="Payment",startRow = 3) %>%
  filter(!grepl("Maria",Payment.date)) 
payCleaner = payCleaner %>%
  mutate(Week=as.Date(`Week.(Fri)`,origin='1899-12-30'),
         Day=as.Date(Day,origin='1899-12-30'),
         PayDay = as.Date(as.integer(Payment.date),origin='1899-12-30'))
## ==========================================
daily = allcleanings %>% 
  filter(Cleaner.lead %in% "Maria" & CheckOut>='2025-06-01') %>% 
  group_by(CheckOut) %>%
  reframe(units = n(),
          units_residential = sum(is.na(Earnings)),
          CleaningFee=sum(Maria.pay,na.rm=T),
          CleaningFee.STR = sum(Maria.pay[!is.na(Earnings)],na.rm=T),
          CleaningFee.LTR = sum(Maria.pay[is.na(Earnings)],na.rm=T),
          renevne = sum(Cleaning.fee)) 

fridays = weekdays(daily$CheckOut)
fridays = daily$CheckOut[fridays %in% "Friday"]
fridays = c(fridays[1]-7,fridays,fridays[length(fridays)]+7)
daily$Week = cut(daily$CheckOut,breaks=fridays,right=T)
levels(daily$Week) = fridays[-1]

daily = merge(daily,payCleaner %>% 
                group_by(Week,Day) %>% 
                reframe(PayCleaner = sum(Rate)),
              by.x='CheckOut',by.y='Day',suffix=c("",".pay")) %>%
  mutate(Month = substr(CheckOut,1,7),
         PayCleaner_cumsum = unlist(tapply(PayCleaner,Month,cumsum)),
         CleaningFee_cumsum = unlist(tapply(CleaningFee,Month,cumsum)),
         CleaningFee_STR_cum = unlist(tapply(CleaningFee.STR,Month,cumsum)),
         CleaningFee_LTR_cum = unlist(tapply(CleaningFee.LTR,Month,cumsum)))

dataplot = daily %>% 
  select(CheckOut,CleaningFee,CleaningFee.STR,CleaningFee.LTR,PayCleaner,
         PayCleaner_cumsum,CleaningFee_cumsum,CleaningFee_STR_cum,CleaningFee_LTR_cum
         ) %>% 
  pivot_longer(!CheckOut,values_to = 'Amount') %>%
  mutate(checkout=as.POSIXct(CheckOut),
         Month=substr(CheckOut,1,7)) 

weekplot = daily %>% group_by(Week) %>%
  reframe(CleaningFee = sum(CleaningFee),
          CleaningFee.STR =sum(CleaningFee.STR,na.rm=T),
          CleaningFee.LTR =sum(CleaningFee.LTR,na.rm=T),
          PayCleaner=sum(PayCleaner,na.rm=T)) %>%
  pivot_longer(!Week,values_to = 'Amount') %>%
  mutate(Month=substr(Week,1,7),
         week = as.POSIXct(Week)) 


dataplot %>% filter(checkout<="2025-8-31" & name %in% c("CleaningFee","PayCleaner")) %>%
  ggplot(aes(checkout,Amount,group=name,color=name)) +
  geom_point(size=1) +
  geom_line() +
  facet_wrap(~Month,ncol=1,scale="free") +
  labs(x="Check Out Date",y="Amount",color="",title = "Daily Cleaning PayOut")+
  theme(legend.position = 'top')

dataplot %>% filter(checkout<="2025-8-31" & 
                      name %in% c("CleaningFee_cumsum","PayCleaner_cumsum")) %>%
  ggplot(aes(checkout,Amount,group=name,color=name)) +
  geom_point(size=1) +
  geom_line() +
  facet_wrap(~Month,ncol=1,scale="free") +
  labs(x="Check Out Date",y="Amount",color="",title = "Cumulative Daily Cleaning PayOut")+
  theme(legend.position = 'top')

weekplot %>% filter(week<="2025-8-31" & name %in% c("CleaningFee","PayCleaner")) %>%
  ggplot(aes(week,Amount,group=name,color=name)) +
  geom_point(size=1) +
  geom_line() +	
  scale_x_datetime(date_breaks ='7 day', labels = date_format("%m-%d")) +
#  facet_wrap(~Month,ncol=1,scale="free") +
  labs(x="Friday at week",y="Amount",color="",title = "Weekly Cleaning PayOut")+
  theme(legend.position = 'top')



##  check guesty checkout# with monthly maria payout checkout# 
monthly = allcleanings %>% 
  filter(Cleaner.lead %in% "Maria") %>% 
  group_by(Month,Listing) %>%
  reframe(Times=n())

maria.monthly = NULL
for(i in 5:8)
{
  tmp = payout[[i]] %>% filter(!is.na(Times)) %>% 
    mutate(Listing = sub("#|Island ",'',Listing)) %>%
    mutate(Listing = ifelse(grepl("Redmond Gull",Listing),
                            "Redmond 7579",Listing))
  monthly.adj = monthly %>% filter(Month %in% paste0("2025-0",i)) %>% 
    select(Month,Listing,Times)
  monthly.adj$Times[monthly.adj$Listing %in% 
                      c("Seatac 12834 Lower","Seatac 12834 Upper")] = 
    monthly.adj$Times[monthly.adj$Listing %in% 
                        c("Seatac 12834 Lower","Seatac 12834 Upper")] +
    monthly.adj$Times[monthly.adj$Listing %in% "Seatac 12834"]
  monthly.adj = monthly.adj %>% filter(!Listing %in% "Seatac 12834")
  tmp = merge(tmp[1:(grep("Total payment",tmp$Listing)-1),],monthly.adj,
              by='Listing',all=T,suffix = c('.maria','.guesty'))
  tmp$Month = paste0("2025-0",i)
  maria.monthly = rbind(maria.monthly,tmp)
}


##================ car schedules ================
car_arrange = NULL
for(i in 5:8)
{
  cars = read.xlsx("./Data/Maria cleaning team route_Revised.xlsx",
              sheet=paste0('2025-0',i),colNames = F)
  dat = cars[,c(1,4,7,10,13)]
  dat = dat %>% 
      mutate(Date = ifelse(grepl("Car",X1),NA,X1)) %>%
      mutate(Date = as.Date(as.integer(Date),origin= '1899-12-30'))%>%
      fill(Date) %>%
      filter(!is.na(X4))
  colnames(dat) = c("Car",paste0("prop",1:4),"Date")
  long_dat = dat[,-1] %>%  
    pivot_longer(!Date,values_to = "Unit")%>%
    filter(!is.na(Unit)) %>%
    arrange(Date) %>%
    mutate(Month = paste0('2025-0',i))
  car_arrange = rbind(car_arrange,long_dat)
}
car_arrange = car_arrange %>% 
  mutate(Unit = ifelse(grepl("Lower     Upper|Lower Upper|Upper Lower|Seatac 12834 Whole",Unit),
          "Seatac 12834",Unit))
car_arrange$nwords = unlist(sapply(car_arrange$Unit,function(x) 
                length(unlist(strsplit(x," ")))))
car_arrange = car_arrange %>% filter(nwords<=4)
car_arrange$Unit[car_arrange$Unit %in% c("Mercer 3627 ADU Main","Mercer 3627 Main ADU",
                     "Mercer 3627 Whole","Mercer 3627 WHOLE")] = "Mercer 3627"
car_arrange$Unit[car_arrange$Unit %in% "12520 Bellevue"] = "Bellevue 12520"
car_arrange$Unit[car_arrange$Unit %in% "Clyde hill 8830"] = "Clyde Hill 8830"
car_arrange$Unit[car_arrange$Unit %in% "Seattle 710 main"] = "Seattle 710"

car_arrange = rbind.fill(car_arrange %>% 
                           filter(!Unit %in% c("Seattle 710 Main Adu","Seatac 12834")),
                  data.frame(Unit = c("Seattle 710","Seattle 710 ADU"),
                      car_arrange %>% filter(Unit %in% "Seattle 710 Main Adu") %>% 
                      select(-Unit)),
                car_arrange %>% filter(Unit %in% "Seatac 12834") %>% 
                  mutate(Unit='Seatac 12834 Lower'),
                car_arrange %>% filter(Unit %in% "Seatac 12834") %>% 
                  mutate(Unit='Seatac 12834 Upper'))


monthly_car = car_arrange %>% group_by(Month,Unit) %>%
  reframe(Times.car=n())
#alldata = merge(maria.monthly,monthly_car,by.x=c("Month","Listing"),
 #               by.y=c("Month","Unit"),all=T) #need LRT property to align

##


