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
##======================================================================
## 9/30/2023: Update 
##    for booking>=30 days, use floor(nights/30) * cohost rate 
##    instead of >=30 cohost rate
##    add trip compensations
## 10/1/2023: revise : check out after 9/12/2023, reservations for Hanz and Destenit belong to Brian
## 10/5/2023: revise : check out after 9/12/2023, reservations for Poulsbo 563 belong to Brian from Lucia
##            Niya turn to super-host rates
##======================================================================
#library(readxl)
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost")
#confirmed = read.csv('./Working/Data/monthly_confirmed_booking_Aug2023.csv')
#cancelled = read.csv('./Working/Data/monthly_cancelled_booking_Aug2023.csv')
#confirmed = read.csv('./Working/Data/Guesty_ooking_confirmed_Aug2023.csv')
#cancelled = read.csv('./Working/Data/Guesty_Booking_cancelled_Aug2023.csv')
confirmed = read.csv('./Cohost Cleaner Compensation/Working/Data/2023/Guesty_booking_confirmed_Sept2023.csv')
cancelled = read.csv('./Cohost Cleaner Compensation/Working/Data/2023/Guesty_booking_cancelled_Sept2023.csv')
startdate ='2023-09-01'
enddate = '2023-09-30'

cohost = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
employee = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx',sheet='Employee')
employee.rates = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx',sheet='Rates')
employee = employee %>% join(employee.rates)
cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
         arrange(listname)

#abpay = read.csv('./Cohost Cleaner Compensation/Working/Data/2023/Archived/2023.01.01 to 08.30 Airbnb transactions.csv')
abpay = read.csv('./Data and Reporting/Data/2023.01.01 to 2023.12.31.csv') #1273
abpay = abpay[!abpay$Confirmation.Code %in% c(NA,''),] #985 
dup = duplicated(abpay$Confirmation.Code)
dups = abpay[abpay$Confirmation.Code %in% abpay$Confirmation.Code[dup],]
dups = dups %>% arrange(Confirmation.Code)

reservation.ab = abpay %>% 
  group_by(Confirmation.Code) %>% reframe(payout = sum(Amount))
#View(cohost)
setdiff(cohost$listname,confirmed$LISTING.S.NICKNAME)
setdiff(confirmed$LISTING.S.NICKNAME,cohost$listname) 
setdiff(cancelled$LISTING.S.NICKNAME,cohost$listname) 

format_reservation <- function(data,startdate,enddate){
 data$CheckIn = as.Date(data$CHECK.IN)
 data$CheckOut = as.Date(data$CHECK.OUT)
 data = data[data$CheckOut>=startdate & data$CheckOut<=enddate,]
 data$Earnings = ifelse(!is.na(data$TOTAL.PAID),data$TOTAL.PAID,data$TOTAL.PAYOUT)
 data = data[,c('LISTING.S.NICKNAME','CONFIRMATION.CODE','status','GUEST.S.NAME','NUMBER.OF.GUESTS',
                'CheckIn','CheckOut','NUMBER.OF.NIGHTS','Earnings','SOURCE')]
 colnames(data) = c('Property','Confirmation.Code','Status','GuestName','Guests','CheckIn',
                    'CheckOut','Nights','Earnings','Source')
 data$Comment = data$Cleaner = data$CohostPayOut = data$Backup = NA
 data
}

confirmed = format_reservation(confirmed,startdate,enddate)
cancelled = format_reservation(cancelled,startdate,enddate)

reservations = rbind(confirmed,cancelled)
reservations = merge(cohost[,!grepl('Property',colnames(cohost))],
                     reservations,by.x='listname',by.y='Property',all.y=T)
colnames(reservations)[1] = 'Property'

##==================================
## adjust payout based on airbnb records
reservations = merge(reservations,reservation.ab,
                     by='Confirmation.Code',all.x=T)
reservations %>% 
  filter(Source %in% c('Airbnb') & (abs(Earnings -payout) > 1e-6 | 
        (is.na(Earnings) & payout ==0))) %>%
  select(Confirmation.Code,Property,GuestName,Earnings,payout)
idx =which(reservations$Source %in% 'Airbnb' & 
      (abs(reservations$Earnings -reservations$payout) > 1e-6 | 
        (is.na(reservations$Earnings) & reservations$payout %in% 0)))
reservations$Earnings[idx] = reservations$payout[idx]

dups = dups[dups$Confirmation.Code %in% reservations$Confirmation.Code,] %>% 
  group_by(Confirmation.Code) %>% reframe(payout=sum(Amount))
reservations[reservations$Confirmation.Code %in% dups$Confirmation.Code,]
##==================================

reservations = reservations %>% 
  mutate(Month = substr(enddate,1,7),
         CohostPayOut = ifelse(!is.na(Earnings) & Earnings<100,0,
                           ifelse(Status %in% 'confirmed',
                            ifelse(Nights<=30,Regular.rate,floor(Nights/30)*Regular.rate),
                              ifelse(!is.na(Earnings),Regular.rate/2,0))))

output.val = c('Month','Property','Confirmation.Code','Status','GuestName','Guests','CheckIn',
               'CheckOut','Nights','Earnings','CohostPayOut','Backup','Comment')

## revise : check out after 9/12/2023, reservations for Hanz and Destenit belong to Brian
idx = reservations$CheckOut>'2023-09-12' & reservations$Cohost %in% c('Hans','Destenit')
reservations$Cohost[idx] = 'Brian'
reservations$First.Name[idx] = 'Hong'
reservations$Last.Name[idx] = 'Hu'

## 10/5/2023: revise : check out after 9/12/2023, reservations for Poulsbo 563 belong to Brian from Lucia
idx = reservations$CheckOut>'2023-09-12' & reservations$Property %in% c('Poulsbo 563')
reservations$Cohost[idx] = 'Brian'
reservations$First.Name[idx] = 'Hong'
reservations$Last.Name[idx] = 'Hu'

## add trip data
trips  = read.xlsx('./Working/Data/2023/TripLog_Sept2023.xlsx')
colnames(trips)[1:5] = c('Timestamp','CohostName','visiting.Date','Property','Comment')

trips = trips %>% filter(!Property %in% 'Other') %>%
  mutate(Month = substr(enddate,1,7),
         Cohost = sapply(CohostName,function(x) unlist(strsplit(x,' '))[1]),
         CheckIn = as.Date(visiting.Date,origin= '1899-12-30'),
         Status = ifelse(grepl('Bi-monthly inspection',Comment),'BimonInspection','Trip')) %>%
  mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
  join(employee %>% select(Nick.name,Trip.rate,`Bi-mo.inspection`) %>% 
         mutate(Cohost = Nick.name)) %>%
  mutate(CohostPayOut = ifelse(Status %in% 'BimonInspection',`Bi-mo.inspection`,Trip.rate)) %>%
  arrange(Cohost,Property,CheckIn)
# check
unique(trips$Cohost)
unique(trips$Property)
table(trips$CheckIn)


trip.var = c('Month','Cohost','Property','CheckIn','Status','CohostPayOut','Comment')
reservations = rbind.fill(reservations,trips)
length(unique(reservations$Property)) #42

## add Yi's trash trips
Yitrip = data.frame(Property='Seattle 1512', Cohost='Yi',
                    Status ='Trip for trash collection',
                    Month=c('2023-08','2023-09'), 
                    CohostPayOut = 100)
reservations = rbind.fill(reservations,Yitrip)

file_loc = "./Cohost Cleaner Compensation/Cohost's reservation sheets/ReservationSheet_"
for(k in sort(unique(reservations$Cohost)))
 { 
  old = read.xlsx(paste0(file_loc,
                   employee$First.Name[employee$Nick.name %in% k],'_',
                   employee$Last.Name[employee$Nick.name %in% k],'.xlsx'))
  old = old %>% mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
                       CheckOut = as.Date(CheckOut,origin= '1899-12-30'),)
  temp <-reservations %>% 
  filter(Cohost %in% k) %>% 
  select(all_of(output.val)) %>% 
  arrange(Property,CheckIn) 
  temp = rbind.fill(old ,temp)
  #temp = rbind.fill(old %>% filter(Month %in% '2023-08'),temp)
  write.xlsx(list("2023"=temp),paste0(file_loc,
              employee$First.Name[employee$Nick.name %in% k],'_',
              employee$Last.Name[employee$Nick.name %in% k],'.xlsx'),
            na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
 }

sum_property = reservations %>% 
  group_by(Cohost,Property) %>% 
    reframe(Reservations = n(), Nights = sum(Nights[Status %in% 'confirmed']),
            confirmed = sum(Status %in% 'confirmed'),
            canceled = sum(Status %in% 'canceled'),
            trips = sum(Status %in% c('Trip','BimonInspection')),
            Earnings = sum(Earnings,na.rm=T),
            CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
     mutate(Reservations = Reservations-trips)
    

sum_cohost = sum_property %>% group_by(Cohost) %>% 
        reframe(Reservations = sum(Reservations), 
                confirmed = sum(confirmed),
                canceled = sum(canceled),
                trips = sum(trips),
                Earnings = sum(Earnings,na.rm=T),
                CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
        mutate(Reservations = Reservations-trips)

output = list(Cohost=sum_cohost,Property = sum_property)
write.xlsx(output,'./Cohost Cleaner Compensation/Working/SummaryReports/Property_Cohost_summary_Sept2023(1).xlsx')

