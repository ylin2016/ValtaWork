###################################################################
## Create Monthly cohost compensation sheet ##
###################################################################
##======================================================================
## source: - confirmed and cancelled booking records downloaded from 
##           Guesty queries on the last day of the month
##         - Use TOTAL.PAID, then TOTAL.payout
##         - AirBnb bookings need to use airbnb records for earnings
## Cohost payout: 
##  Confirmed booking 
##          - use the cohost rates to assign per confirmed booking
##          - >=30 day, use ceiling (nights/30) * cohost rate
##          - If earnings <100, no payout for cohost
##  Cancelled booking
##          - if we have earning>$100, cohost gets 50% of <30 rate
##  Trip  - use cohost trip rates
## Last Update: 10/01/2023
## 9/30/2023: Update 
##    for booking>=30 days, use floor(nights/30) * cohost rate 
##    instead of >=30 cohost rate
##    add trip compensations
## 4/30/2024 : Update
##    change ##  Cancelled booking
##          - if we have earning>$200, cohost gets 50% of <30 rate
## 5/2/2024: Update 
##        Limited trips at most 4 times (including bi-monthly) per property
## 7/26/2024:
##    change ##  Confirm booking
##          - if we have earning<$200, cohost no rate
##    change ##  Cancelled booking
##          - if we have earning<$200, cohost no rate
## Cottages All: count as 10 cottages for cohost
## 9/26/2024: 
##    change any cancellations for non-airbnb platform, set earning =0 and payout=0
## 10/1/2024: add Niya taking care of trash, $50/m
##======================================================================
## 1/2 change Seattle 1424c and 1430b to Zexi
## 1/25 change all Brian's to Rachel
## 1/1: Add Dajiang for Seattle 1117
##======================================================================
#library(readxl)
library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/")
startdate ='2024-10-01'
enddate = '2024-10-31'
filemonth = substr(enddate,1,7)
fileloc = './Working/Data/2024/'
##-----------------------------------------------------------------------------------
## Input files
##-----------------------------------------------------------------------------------
cohost = read.xlsx('./Working/Data/Property_Cohost.xlsx')
employee = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Employee')
employee.rates = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Rates',rows=1:4)
employee = employee %>% select(-(Comment)) %>% join(employee.rates)
cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
  arrange(Listing)
#View(cohost)
reviews = read.xlsx('./Working/Data/2024/guesty_reviews_202410.xlsx')
reviews = reviews[,!duplicated(colnames(reviews))]
##-----------------------------------------------------------------------------------
######## booking records ########
confirmed = read.csv(paste0(fileloc,'Guesty_booking_confirmed_',filemonth,'.csv'))
cancelled = read.csv(paste0(fileloc,'Guesty_booking_cancelled_',filemonth,'.csv'))
format_reservation <- function(data,startdate,enddate){
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
setdiff(cohost$Listing,confirmed$LISTING.S.NICKNAME)
setdiff(confirmed$LISTING.S.NICKNAME,cohost$Listing) 
setdiff(cancelled$LISTING.S.NICKNAME,cohost$Listing) 

confirmed = format_reservation(confirmed,startdate,enddate)
## Check same guest continued stay in 2 bookings 
sum(dup<-duplicated(paste(confirmed$Listing,confirmed$GuestName)))
confirmed[paste(confirmed$Listing,confirmed$GuestName) %in% 
            paste(confirmed$Listing,confirmed$GuestName)[dup],] %>% 
  arrange(GuestName,CheckIn)
combResvId = paste(confirmed$Listing,confirmed$GuestName)[dup]
combResv = confirmed %>%
  filter(paste(Listing,GuestName) %in% combResvId) %>%
  group_by(Listing,GuestName) %>%
  reframe(Confirmation.Code = paste(Confirmation.Code,collapse='/'),
          Status = unique(Status),
          GuestName = unique(GuestName),
          Guests = max(Guests),
          CheckIn = min(CheckIn),
          CheckOut = max(CheckOut),
          Nights=sum(Nights),
          Earnings = sum(Earnings),
          Source = paste(Source,collapse='/'))
confirmed = rbind.fill(confirmed %>%
             filter(!(paste(Listing,GuestName) %in% combResvId)),combResv)
cancelled = format_reservation(cancelled,startdate,enddate)
cancelled$Earnings[!cancelled$Source %in% 'Airbnb' & !is.na(cancelled$Earnings)] = 0
##-----------------------------------------------------------------------------------
######## add trip data ########
trips  = read.xlsx(paste0(fileloc,'TripLog_',filemonth,'.xlsx'))
inspections = read.xlsx(paste0(fileloc,'Bi_monthly_Inspection_',filemonth,'.xlsx'))
colnames(trips)[1:5] = c('Timestamp','CohostName','visiting.Date','Property','Comment')
colnames(inspections)[1:4] = c('Timestamp','CohostName','Property','visiting.Date')

trips = trips %>% filter(!Property %in% 'Other') %>%
  mutate(Month = substr(enddate,1,7),
         Cohost = sapply(CohostName,function(x) unlist(strsplit(x,' '))[1]),
         CheckIn = as.Date(visiting.Date,origin= '1899-12-30'),
         Status = ifelse(grepl('Bi-monthly inspection|Bio monthly inspection',Comment),'BimonInspection',
                    ifelse(grepl('Take care of trash|Monthly|garbage bins',Comment) & Cohost %in% c("Lucia"),
                           'MonthlyTrashCollection','Trip'))) %>%
  mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
  mutate(Cohost = sub('ChenHui','Sasha',Cohost)) %>% 
  mutate(Cohost = ifelse(CohostName %in% 'Jing Zhou','JingZhou',Cohost)) %>%
  join(employee %>% select(Nick.name,Trip.rate,`Bi-mo.inspection`,MonthlyTrashCollection) %>% 
         mutate(Cohost = Nick.name)) %>%
  mutate(CohostPayOut = ifelse(Status %in% 'BimonInspection',`Bi-mo.inspection`,
                               ifelse(Status %in% 'MonthlyTrashCollection',
                                      MonthlyTrashCollection,Trip.rate))) %>%
  arrange(Cohost,Property,CheckIn)
# add Niya's monthly trash collection $50/m
trips.trashes = trips[trips$Cohost %in% 'Niya',][1,]
trips.trashes[,c("CheckIn","Status",'MonthlyTrashCollection','CohostPayOut',"Comment")] = 
    c("2024-10-31","MonthlyTrashCollection",50,50,"")

trips = rbind(trips, trips.trashes)
trips = trips %>% filter(!Cohost %in% 'Eddie')
# check
table(trips$Cohost,exclude=NULL)
table(trips$Property)
table(trips$CheckIn)
table(trips$Status)

inspections = inspections %>% filter(!Property %in% 'Other') %>%
  mutate(Month = substr(enddate,1,7),
         Cohost = sapply(CohostName,function(x) unlist(strsplit(x,' '))[1]),
         CheckIn = as.Date(visiting.Date,origin= '1899-12-30'),
         Status = 'BimonInspection') %>% 
  mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
  mutate(Cohost = sub('ChenHui','Sasha',Cohost)) %>% 
  mutate(Cohost = ifelse(CohostName %in% 'Jing Zhou','JingZhou',Cohost)) %>%
  join(employee %>% select(Nick.name,`Bi-mo.inspection`) %>% 
         mutate(Cohost = Nick.name)) %>%
  mutate(CohostPayOut = `Bi-mo.inspection`) %>%
  arrange(Cohost,Property)

trip.var = c('Month','Cohost','Property','CheckIn','Status','CohostPayOut','Comment')
trips.all = rbind.fill(inspections,trips)[,trip.var] %>% 
  #filter(!duplicated(paste(Property,CheckIn))) %>% 
  mutate(Listing = Property) %>%
  arrange(Cohost,CheckIn)

##!!!! check trips <= 4 times in a month, if >4 times, ==>4
table(trips.all$Property)
View(trips.all) #26
##-----------------------------------------------------------------------------------
reservations = rbind(confirmed,cancelled)
reservations = merge(cohost %>% select(Listing,Property,Cohost,Regular.rate),reservations,by='Listing',all.y=T)
reservations = reservations %>% 
  mutate(Month = substr(enddate,1,7),
         CohostPayOut = ifelse(is.na(Earnings),0,
                          ifelse(Earnings<200,0,
                            ifelse(Status %in% 'canceled', Regular.rate/2,
                            ifelse(Status %in% 'confirmed',
                             ifelse(Nights<=30,Regular.rate,floor(Nights/30)*Regular.rate),NA))))) %>%
  mutate(CohostPayOut = ifelse(Listing %in% "Cottages All OSBR",CohostPayOut*10,CohostPayOut))
dim(reservations) #370
# #=================================================================================
# ## add backup 
# backup = read.xlsx(paste0(fileloc,'Cohost_Backup_',filemonth,'.xlsx'))
# colnames(backup)[-1] = c('Cohost.org','StartTime','EndTime','Backup','Property')
# 
# backup = backup[-6,] %>% 
#   mutate(StartTime = as.Date(StartTime,origin= '1899-12-30'),
#          EndTime = as.Date(EndTime,origin= '1899-12-30'),
#          Cohost.org = sapply(Cohost.org,function(x) unlist(strsplit(x," (",fixed=T))[1])) 
# 
# backup = merge(backup, employee[,c('CohostName','Nick.name')],
#                by.x='Cohost.org',by.y='CohostName',all.x=T)
# backup = merge(backup, employee[,c('CohostName','Nick.name')],by.x='Backup',
#                by.y='CohostName',suffix=c('.org','.backup'),all.x=T)
# 
# #View(backup)
# reservations_backup = NULL
# for(i in 1:nrow(backup))
# {
#   idx = reservations$CheckOut<=backup$EndTime[i] &
#     reservations$CheckOut>=backup$StartTime[i] &
#     reservations$Property %in% unlist(strsplit(backup$Property[i],', '))
#   if(sum(idx)>0){
#     reservations_backup = rbind(reservations_backup,reservations %>%
#                                   filter(CheckOut<=backup$EndTime[i] & CheckOut>=backup$StartTime[i] &
#                                            Property %in% unlist(strsplit(backup$Property[i],', '))) %>%
#                                   mutate(Cohost = backup$Nick.name.backup[i],
#                                          CohostPayOut = CohostPayOut/2,
#                                          Earnings = Earnings/2,
#                                          Comment = paste('Backup for',backup$Nick.name.org[i])))
#     reservations$CohostPayOut[idx] = reservations$CohostPayOut[idx]/2
#     reservations$Earnings[idx] = reservations$Earnings[idx]/2
#     reservations$Backup[idx] = backup$Nick.name.backup[i]
#   }
# }

##-----------------------------------------------------------------------------------
## Combine all 
##-----------------------------------------------------------------------------------
reservations = merge(reservations,
                     reviews %>% filter(!is.na(Reservation)) %>% 
                       select(Reservation,Cleanliness, Communication, Location, Value,Overall,
                              Booking.com.Rating),
                      by.x='Confirmation.Code',by.y='Reservation',all.x=T)

output.val = c('Month','Listing','Confirmation.Code','Status','GuestName','Guests','CheckIn',
               'CheckOut','Nights','Earnings','CohostPayOut','Backup','Comment')

reservations = rbind.fill(reservations,trips.all) #reservations_backup,trips.all)
reservations = reservations %>% filter(!Cohost %in% c("Yumiko"))
length(unique(reservations$Listing)) #59

file_loc = "./Cohost's reservation sheets/ReservationSheet_"
for(k in sort(unique(reservations$Cohost)))
 {
  print(k)
  temp <-reservations %>% 
    filter(Cohost %in% k) %>% 
    select(all_of(output.val)) %>% 
    arrange(Listing,CheckIn) 
  CohostName =  paste0(employee$First.Name[employee$Nick.name %in% k],'_',
                       employee$Last.Name[employee$Nick.name %in% k])
  # new cohost this month
  if(k %in% c("Alice"))
  { 
    write.xlsx(list("2024"=temp),paste0(file_loc,CohostName,'.xlsx'),
          na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
  }else{
    old = read.xlsx(paste0(file_loc,CohostName,'.xlsx'))
    old = old %>% filter(!Month %in% substr(enddate,1,7)) %>%
      mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
             CheckOut = as.Date(as.integer(CheckOut),origin= '1899-12-30'))
    colnames(old)[colnames(old) %in% 'Property'] = 'Listing'
    temp = rbind.fill(temp,old)
    if(k %in% c("Crystal","Yumiko","Eddie","Shaya","Felecia","Paul","Shaya","Anna",
                "Thao","Feifei","JingZhou","Lulu","Sasha","YongChao")) ## new this year
    { 
      write.xlsx(list("2024"=temp),paste0(file_loc,CohostName,'.xlsx'),
                 na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
    }else{
      old2023 = read.xlsx(paste0(file_loc,CohostName,'.xlsx'),sheet='2023')
      write.xlsx(list("2024"=temp,"2023"=old2023),paste0(file_loc,CohostName,'.xlsx'),
           na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
    }
  }
}
sum_property = reservations %>% 
  group_by(Cohost,Property) %>% 
  reframe(Month = substr(enddate,1,7),
          Reservations = sum(Status %in% c('confirmed','canceled')), 
          Nights = sum(Nights[Status %in% 'confirmed']),
          Confirmed = sum(Status %in% 'confirmed'),
          Canceled = sum(Status %in% 'canceled'),
          Trips = sum(Status %in% 'Trip'),
          Bimonthly= sum(Status %in% 'BimonInspection'),
          TrashMonthly = sum(Status %in% 'MonthlyTrashCollection'),
          CH_Lease = sum(Status %in% 'CH Lease'),
          Backup = sum(grepl('Backup for',Comment)),
          Earnings = round(sum(Earnings,na.rm=T),2),
          CohostPayOut = sum(as.numeric(CohostPayOut),na.rm=T)) %>%
  relocate(Month,.before=Cohost)

sum_cohost = sum_property %>% group_by(Cohost) %>% 
  reframe(Month = substr(enddate,1,7),
          Reservations = sum(Reservations), 
          Confirmed = sum(Confirmed),
          Canceled = sum(Canceled),
          Trips = sum(Trips),
          Bimonthly= sum(Bimonthly),
          TrashMonthly = sum(TrashMonthly),
          CH_Lease = sum(CH_Lease),
          Backup = sum(Backup),
          Earnings = round(sum(Earnings,na.rm=T),2),
          CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
  relocate(Month,.before=Cohost)
View(sum_cohost)
View(sum_property)

## with review
sum_rates = 
  reservations %>% group_by(Cohost) %>% 
  reframe(Nreviews = sum(!is.na(Overall)),
          AverageOverall = ifelse(sum(!is.na(Overall))==0,NA,round(mean(Overall,na.rm=T),2)),
          Rates = paste(paste0(names(table(Overall)),":(",table(Overall),")"),collapse = ';'),
          AverageCleanliness = ifelse(sum(!is.na(Cleanliness))==0,NA,round(mean(Cleanliness,na.rm=T),2)),
          AverageCommunication = ifelse(sum(!is.na(Communication))==0,NA,round(mean(Communication,na.rm=T),2)),
          AverageLocation = ifelse(sum(!is.na(Location ))==0,NA,round(mean(Location,na.rm=T),2)),
          AverageValue = ifelse(sum(!is.na(Value))==0,NA,round(mean(Value,na.rm=T),2))) %>%
  mutate(Overall_N = ifelse(is.na(AverageOverall),NA,Rates)) %>% 
         select(-Rates)
rate_airbnb = reservations %>% 
    filter(grepl('airbnb',Source)) %>% 
    group_by(Cohost) %>% 
    reframe(AverageOverall_airbnb = ifelse(sum(!is.na(Overall))==0,NA,round(mean(Overall,na.rm=T),2)))
rate_vrbo = reservations %>% 
  filter(grepl('VRBO',Source)) %>% 
  group_by(Cohost) %>% 
  reframe(AverageOverall_VRBO = ifelse(sum(!is.na(Overall))==0,NA,round(mean(Overall,na.rm=T),2)))
sum_cohost = merge(sum_cohost,sum_rates,by='Cohost',all.x=T)
sum_cohost = merge(sum_cohost,rate_airbnb,by='Cohost',all.x=T)
sum_cohost = merge(sum_cohost,rate_vrbo,by='Cohost',all.x=T)

View(sum_cohost)
View(sum_property)
sum_loc = "./Cohost's reservation sheets/"
sum_prop = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Property')
sum_coh  = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Cohost')
paiday = as.character(as.Date(as.integer(sum_coh$Paid.day[1:38]),origin= '1899-12-30'))
sum_coh$Paid.day[1:38] =  paiday
sum_property_all = rbind.fill(sum_property,
                              sum_prop %>% filter(!Month %in% substr(enddate,1,7))) %>%
                                  relocate(Bimonthly,TrashMonthly,.before = Earnings)
sum_cohost_all = rbind.fill(sum_cohost,
                            sum_coh %>% filter(!Month %in% substr(enddate,1,7))) %>% 
                                  relocate(Bimonthly,TrashMonthly,.before = Earnings)
output = list(Cohost=sum_cohost_all,Property = sum_property_all)
View(output$Cohost)

write.xlsx(output,"./Cohost's reservation sheets/Property_Cohost_Summary.xlsx",
           firstActiveRow = 2,withFilter = T)

write.xlsx(output$Cohost,"./Working/Property_Cohost_Summary.xlsx",
           firstActiveRow = 2,withFilter = T)

##!!!! STR table ##########################
org_table = read.xlsx("./Working/Data/2024/STR Properties by Month.xlsx") 
colnames(org_table)[-1] = c("d12/2023",paste0('d',1:12,'/2024'))
org_table$rank = 1:nrow(org_table)
STR_table = reservations %>% filter(Status %in% 'confirmed') %>%
  group_by(Property) %>% 
  reframe(STRs = sum(Nights<30)) %>%
  mutate(d202410 = ifelse(STRs>0,"STR",""))
STR_Table = merge(org_table[,c("Listing","rank")],STR_table[,c("Property",'d202410')],
                  by.x="Listing",by.y='Property',all=T) %>% arrange(rank)
View(STR_Table)
write.xlsx(STR_Table, "./Cohost's reservation sheets/STR_Table.xlsx",firstActiveRow = 2,withFilter = T)
##============================================================================
##============================================================================

## Cleaner payment sheet
cleaner = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Cleaning')
cleansheet = merge(confirmed[,c("Listing",'CheckIn','CheckOut','Guests','Nights',"GuestName","Earnings")],
                   cleaner[,c("Listing","Cleaner.lead",'Cleaning.fee')],
                   by="Listing",all.x=T) 
#Niya_cleaning = trips.all %>% filter(Cohost %in% 'Niya' & Comment %in% 'Cleaning')
cleansheet = cleansheet %>% 
  mutate(Month =substr(enddate,1,7)) %>% 
  relocate(Month,Listing) %>% 
  filter(Earnings>100)

summary_cleaning  = cleansheet %>% group_by(Cleaner.lead,Listing,Cleaning.fee) %>% 
  reframe(No_of_Cleaning = n(),Total_fee = sum(Cleaning.fee)) %>%
  mutate(Month =substr(enddate,1,7)) %>% 
  relocate(Month,Cleaner.lead)

PayOut= cleansheet %>% 
  group_by(Cleaner.lead) %>% 
  reframe(Reservations=n(),Payout = sum(Cleaning.fee)) %>% 
  mutate(Month =substr(enddate,1,7)) %>% 
  relocate(Month,Reservations)

filename ="./Working/CleaningSheet_PayOut_update.xlsx"
#summary_cleaning_old = read.xlsx(filename,sheet = 'CleaningPerProperty')
#summary_cleaning = rbind.fill(summary_cleaning_old,summary_cleaning)
#PayOut_old = read.xlsx(filename,sheet = 'PayOut')
#PayOut = rbind(PayOut_old,PayOut)
#Cleansheet202403 =  read.xlsx(filename,sheet = 'CleaningSheet')
cleaning.output = list(CleaningSheet =cleansheet,
                       CleaningPerProperty=summary_cleaning,
                       PayOut = PayOut)
write.xlsx(cleaning.output,filename,firstActiveRow = 2,withFilter = T)

##================= write seperate file for cleaner =================
cleaning_loc = "./Cohost's reservation sheets/CleaningSheet_"
for(k in unique(cleansheet$Cleaner.lead))
{
  print(k)
  old = read.xlsx(paste0(cleaning_loc,k,'.xlsx'))
  old = old %>% filter(!Month %in% substr(enddate,1,7)) %>%
    mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
           CheckOut = as.Date(as.integer(CheckOut),origin= '1899-12-30'))
  temp = cleansheet %>% filter(Cleaner.lead %in% k) %>% select(all_of(colnames(old)))
  temp = rbind.fill(temp,old)
  write.xlsx(list("2024"=temp),paste0(cleaning_loc,k,'.xlsx'),
             na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
}
#####===============================================================
# Maria's daily cost 

Maria = cleansheet %>% filter(Cleaner.lead %in% 'Maria') %>% 
  group_by(CheckOut) %>%
  reframe(Cost = sum(Cleaning.fee))

write.csv(Maria,"./Working/Maria_daily_cleaning.csv",row.names=F,na='')

# Maria team reviews 
cars = read.xlsx('Working/Data/2024/MariaCleaningCarArrange202410.xlsx')
cars = cars %>% mutate(Date = as.Date(Date,origin= '1899-12-30')) 
cars_long = cars %>% 
      select(Date,Car,Property.1,Property.2,Property.3,Property.4,Property.5,Property.6) %>%
      pivot_longer(
      cols = starts_with("Property."),
      names_to = "PropertyNo",
      names_prefix = "Property",
      values_to = "Listing_raw",
      values_drop_na = TRUE) %>% mutate(PropertyNo = as.numeric(sub('.','',PropertyNo)))
cars_long$Listing = cars_long$Listing_raw
cars_long$Listing = sub("SeaTac","Seatac",cars_long$Listing)
cars_long$Listing = sub("adu","ADU",cars_long$Listing)
cars_long$Listing = sub("Seattel","Seattle",cars_long$Listing)
cars_long$Listing = sub("1424c","1424C",cars_long$Listing)
cars_long$Listing[cars_long$Listing %in% c("Seatac 12834 whole","Seatach Whole","Seatac AB")]="Seatac 12834" 
cars_long$Listing[cars_long$Listing %in% c("Seatac 12834 upper","Seatac Upper 12834","Seatac upper")]="Seatac 12834 Upper"
cars_long$Listing[cars_long$Listing %in% c("Seatac lower")]="Seatac 12834 Lower"
cars_long$Listing[cars_long$Listing %in% "Microsoft D303"] = "Microsoft 14615-D303"
cars_long$Listing[cars_long$Listing %in% "Kirkland 10409"] = "Bellevue 10409"
setdiff(cars_long$Listing,cleansheet$Listing)

cars_long = merge(cars_long,cleansheet[,c('CheckOut',"Listing","Cleaner.lead","Cleanliness")],
                  by.x=c("Date","Listing"),by.y=c("CheckOut","Listing"),all.x=T)
View(cars_long)

cars_review = cars_long %>% select(Date,Car,PropertyNo,Cleanliness) %>% 
  arrange(Date,Car,PropertyNo) %>%
  pivot_wider(names_from = PropertyNo, values_from = Cleanliness)
colnames(cars_review)[-(1:2)] = paste0('Cleanliness.Property.',colnames(cars_review)[-(1:2)])
View(cars_review)
cars = merge(cars,cars_review,by=c("Date","Car"),all.x=T)
write.xlsx(cars,"Working/MariaCleaningCarArrange202410_ratings.xlsx",rowNames=F,na='')
#####===============================================================
sum_property = reservations %>% 
  filter(is.na(Backup)) %>%
  group_by(Property) %>% 
  reframe(Month = substr(enddate,1,7),
          Reservations = sum(Status %in% c('confirmed','canceled')), 
          Nights = sum(Nights[Status %in% 'confirmed']),
          Confirmed = sum(Status %in% 'confirmed'),
          Canceled = sum(Status %in% 'canceled'),
          Trips = sum(Status %in% 'Trip'),
          Bimonthly= sum(Status %in% 'BimonInspection'),
          TrashMonthly = sum(Status %in% 'MonthlyTrashCollection'),
          CH_Lease = sum(Status %in% 'CH Lease'),
          Backup = sum(grepl('Backup for',Comment)),
          Earnings = round(sum(Earnings,na.rm=T),2),
          CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
  relocate(Month,.before=Property)

summary_cleaning  = cleansheet %>% group_by(Property) %>% 
  reframe(No_of_Cleaning = n(),Total_fee = sum(Cleaning.fee))

tmp = merge(sum_property,summary_cleaning,by='Property',all.x=T)
tmp$pct = NULL
idx = which(tmp$Confirmed==tmp$No_of_Cleaning)
tmp$pct[idx] = paste0(round(tmp$CohostPayOut/(tmp$Earnings-tmp$Total_fee)*100,1),"%")[idx]
tmp = tmp %>% arrange(Property)
write.xlsx(tmp,"./Working/Property_Cohost_Summary_tmp.xlsx",
           firstActiveRow = 2,withFilter = T)
