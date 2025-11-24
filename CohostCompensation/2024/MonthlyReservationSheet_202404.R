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
##======================================================================
## 1/2 change Seattle 1424c and 1430b to Zexi
## 1/25 change all Brian's to Rachel
## 1/1: Add Dajiang for Seattle 1117
##======================================================================
#library(readxl)
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/")
startdate ='2024-04-01'
enddate = '2024-04-30'
filemonth = substr(enddate,1,7)
fileloc = './Working/Data/2024/'
##-----------------------------------------------------------------------------------
## Input files
##-----------------------------------------------------------------------------------
cohost = read.xlsx('./Working/Data/Property_Cohost.xlsx')
employee = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Employee')
employee.rates = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Rates')
employee = employee %>% select(-(Comment)) %>% join(employee.rates)
cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
  arrange(Listing)
#View(cohost)

##-----------------------------------------------------------------------------------
######## booking records ########
confirmed = read.csv(paste0(fileloc,'Guesty_booking_confirmed_',filemonth,'.csv'))
cancelled = read.csv(paste0(fileloc,'Guesty_booking_cancelled_',filemonth,'.csv'))
format_reservation <- function(data,startdate,enddate){
  data$CheckIn = as.Date(data$CHECK.IN)
  data$CheckOut = as.Date(data$CHECK.OUT)
  data = data[data$CheckOut>=startdate & data$CheckOut<=enddate,]
  data$Earnings = ifelse(!is.na(data$TOTAL.PAID),data$TOTAL.PAID,data$TOTAL.PAYOUT)
  data = data[,c('LISTING.S.NICKNAME','CONFIRMATION.CODE','status','GUEST.S.NAME','NUMBER.OF.GUESTS',
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
cancelled = format_reservation(cancelled,startdate,enddate)
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
                    ifelse(Comment %in% 'Take care of trash' & Cohost %in% 'Yi','MonthlyTrashCollection','Trip'))) %>%
  mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
  mutate(Cohost = ifelse(CohostName %in% 'Jing Zhou','JingZhou',Cohost)) %>%
  join(employee %>% select(Nick.name,Trip.rate,`Bi-mo.inspection`,MonthlyTrashCollection) %>% 
         mutate(Cohost = Nick.name)) %>%
  mutate(CohostPayOut = ifelse(Status %in% 'BimonInspection',`Bi-mo.inspection`,
                               ifelse(Status %in% 'MonthlyTrashCollection',
                                      MonthlyTrashCollection,Trip.rate))) %>%
  arrange(Cohost,Property,CheckIn)
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

# assigned cleaning fee for owner cleaned Seattle 1623
#trips.all$CohostPayOut[trips.all$Listing %in% 'Seattle 1623' & 
#                         trips.all$Comment %in% 'Cleaning by owner'] = 150
View(trips.all) #38
##-----------------------------------------------------------------------------------
reservations = rbind(confirmed,cancelled)
reservations = merge(cohost,reservations,by='Listing',all.y=T)
reservations = reservations %>% 
  mutate(Month = substr(enddate,1,7),
         CohostPayOut = ifelse(Status %in% 'cancelled' & (!is.na(Earnings) & Earnings) <200,0,
                          ifelse(!is.na(Earnings) & Earnings<100,0,
                               ifelse(Status %in% 'confirmed',
                                      ifelse(Nights<=30,Regular.rate,floor(Nights/30)*Regular.rate),
                                      ifelse(!is.na(Earnings),Regular.rate/2,0)))))
##-----------------------------------------------------------------------------------
## Combine all 
##-----------------------------------------------------------------------------------

output.val = c('Month','Listing','Confirmation.Code','Status','GuestName','Guests','CheckIn',
               'CheckOut','Nights','Earnings','CohostPayOut','Backup','Comment')

reservations = rbind.fill(reservations,trips.all)
length(unique(reservations$Listing)) #56

## add Niya's cleaning trips

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
  if(k %in% c("Lulu","ChenHui"))
  { 
    write.xlsx(list("2024"=temp),paste0(file_loc,CohostName,'.xlsx'),
          na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
  }else{
    old = read.xlsx(paste0(file_loc,CohostName,'.xlsx'))
    old = old %>% filter(!Month %in% substr(enddate,1,7)) %>%
      mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
             CheckOut = as.Date(as.integer(CheckOut),origin= '1899-12-30'))
    colnames(old)[colnames(old) %in% 'Property'] = 'Listing'
    temp = rbind.fill(old,temp)
    if(k %in% c("Anna","Thao","Feifei","JingZhou")) ## new this year
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
            Earnings = sum(Earnings,na.rm=T),
            CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
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
                Earnings = sum(Earnings,na.rm=T),
                CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
  relocate(Month,.before=Cohost)
View(sum_cohost)
View(sum_property)
sum_loc = "./Cohost's reservation sheets/"
sum_prop = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Property')
sum_coh  = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Cohost')
sum_property_all = rbind.fill(sum_prop %>% filter(!Month %in% substr(enddate,1,7)),
                              sum_property) %>% 
  relocate(Bimonthly,TrashMonthly,.before = Earnings)
sum_cohost_all = rbind.fill(sum_coh %>% filter(!Month %in% substr(enddate,1,7)),
                            sum_cohost) %>% 
  relocate(Bimonthly,TrashMonthly,.before = Earnings)
output = list(Cohost=sum_cohost_all,Property = sum_property_all)
View(output$Cohost)

write.xlsx(output,"./Cohost's reservation sheets/Property_Cohost_Summary.xlsx",
           firstActiveRow = 2,withFilter = T)

## Cleaner payment sheet
cleaner = read.xlsx("./Working/Data/Cleaning Schedule and Price.xlsx")
cleansheet = merge(confirmed[,c("Listing",'CheckIn','CheckOut','Guests','Nights')],
                   cleaner[,c("Property","Cleaner.lead",'Cleaning.fee')],
                   by.x="Listing",by.y='Property',all.x=T)
cleansheet = cleansheet %>% 
  mutate(Month =substr(enddate,1,7)) %>% 
  relocate(Month,Listing)

summary_cleaning  = cleansheet %>% group_by(Cleaner.lead,Listing,Cleaning.fee) %>% 
  reframe(No_of_Cleaning = n(),Total_fee = sum(Cleaning.fee)) %>%
  mutate(Month =substr(enddate,1,7)) %>% 
  relocate(Month,Cleaner.lead)

PayOut= cleansheet %>% 
  group_by(Cleaner.lead) %>% 
  reframe(Reservations=n(),Payout = sum(Cleaning.fee)) %>% 
  mutate(Month =substr(enddate,1,7)) %>% 
  relocate(Month,Reservations)

filename ="./Cohost's reservation sheets/CleaningSheet_PayOut.xlsx"
summary_cleaning_old = read.xlsx(filename,sheet = 'CleaningPerProperty')
summary_cleaning = rbind.fill(summary_cleaning_old,summary_cleaning)

PayOut_old = read.xlsx(filename,sheet = 'PayOut')
PayOut = rbind(PayOut_old,PayOut)

Cleansheet202403 =  read.xlsx(filename,sheet = 'CleaningSheet')
cleaning.output = list(CleaningSheet202404 =cleansheet,
                       CleaningPerProperty=summary_cleaning,
                       PayOut = PayOut,CleanSheet202403 = Cleansheet202403)
write.xlsx(cleaning.output,filename,firstActiveRow = 2,withFilter = T)
