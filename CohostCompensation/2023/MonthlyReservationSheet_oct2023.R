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
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/")
confirmed = read.csv('./Working/Data/2023/Guesty_booking_confirmed_Oct2023.csv')
cancelled = read.csv('./Working/Data/2023/Guesty_booking_cancelled_Oct2023.csv')

startdate ='2023-10-01'
enddate = '2023-10-31'

cohost = read.xlsx('./Working/Data/Property_Cohost.xlsx')
employee = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Employee')
employee.rates = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Rates')
employee = employee %>% join(employee.rates)
cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
         arrange(listname)

# #abpay = read.csv('./Working/Data/2023/Archived/2023.01.01 to 08.30 Airbnb transactions.csv')
# abpay = read.csv('../Data and Reporting/Data/2023.01.01 to 2023.12.31.csv') #1273
# abpay = abpay[!abpay$Confirmation.Code %in% c(NA,''),] #985 
# dup = duplicated(abpay$Confirmation.Code)
# dups = abpay[abpay$Confirmation.Code %in% abpay$Confirmation.Code[dup],]
# dups = dups %>% arrange(Confirmation.Code)
# 
# reservation.ab = abpay %>% 
#   group_by(Confirmation.Code) %>% reframe(payout = sum(Amount))
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

# ##==================================
# ## adjust payout based on airbnb records
# reservations = merge(reservations,reservation.ab,
#                      by='Confirmation.Code',all.x=T)
# reservations %>% 
#   filter(Source %in% c('Airbnb') & (abs(Earnings -payout) > 1e-6 | 
#         (is.na(Earnings) & payout ==0))) %>%
#   select(Confirmation.Code,Property,GuestName,Earnings,payout)
# idx =which(reservations$Source %in% 'Airbnb' & 
#       (abs(reservations$Earnings -reservations$payout) > 1e-6 | 
#         (is.na(reservations$Earnings) & reservations$payout %in% 0)))
# reservations$Earnings[idx] = reservations$payout[idx]
# 
# dups = dups[dups$Confirmation.Code %in% reservations$Confirmation.Code,] %>% 
#   group_by(Confirmation.Code) %>% reframe(payout=sum(Amount))
# reservations[reservations$Confirmation.Code %in% dups$Confirmation.Code,]
# ##==================================

reservations = reservations %>% 
  mutate(Month = substr(enddate,1,7),
         CohostPayOut = ifelse(!is.na(Earnings) & Earnings<100,0,
                           ifelse(Status %in% 'confirmed',
                            ifelse(Nights<=30,Regular.rate,floor(Nights/30)*Regular.rate),
                              ifelse(!is.na(Earnings),Regular.rate/2,0))))

output.val = c('Month','Property','Confirmation.Code','Status','GuestName','Guests','CheckIn',
               'CheckOut','Nights','Earnings','CohostPayOut','Backup','Comment')
## add trip data
trips  = read.xlsx('./Working/Data/2023/TripLog_Oct2023.xlsx')
colnames(trips)[1:5] = c('Timestamp','CohostName','visiting.Date','Property','Comment')

trips = trips %>% filter(!Property %in% 'Other') %>%
  mutate(Month = substr(enddate,1,7),
         Cohost = sapply(CohostName,function(x) unlist(strsplit(x,' '))[1]),
         CheckIn = as.Date(visiting.Date,origin= '1899-12-30'),
         Status = ifelse(grepl('Bi-monthly inspection',Comment),'BimonInspection',
                         ifelse(Comment %in% 'Take care of trash','MonthlyTrashCollection','Trip'))) %>%
  mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
  mutate(Cohost = ifelse(CohostName %in% 'Jing Zhou','JingZhou',Cohost)) %>%
  join(employee %>% select(Nick.name,Trip.rate,`Bi-mo.inspection`,MonthlyTrashCollection) %>% 
         mutate(Cohost = Nick.name)) %>%
  mutate(CohostPayOut = ifelse(Status %in% 'BimonInspection',`Bi-mo.inspection`,
                         ifelse(Status %in% 'MonthlyTrashCollection',MonthlyTrashCollection,Trip.rate))) %>%
  arrange(Cohost,Property,CheckIn)
# check
table(trips$Cohost,exclude=NULL)
table(trips$Property)
table(trips$CheckIn)
table(trips$Status)

trip.var = c('Month','Cohost','Property','CheckIn','Status','CohostPayOut','Comment')
reservations = rbind.fill(reservations,trips)
length(unique(reservations$Property)) #41


file_loc = "./Cohost's reservation sheets/ReservationSheet_"
for(k in sort(unique(reservations$Cohost)))
 {
  temp <-reservations %>% 
    filter(Cohost %in% k) %>% 
    select(all_of(output.val)) %>% 
    arrange(Property,CheckIn) 
  if(!k %in% 'JingZhou'){
  old = read.xlsx(paste0(file_loc,
                   employee$First.Name[employee$Nick.name %in% k],'_',
                   employee$Last.Name[employee$Nick.name %in% k],'.xlsx'))
  old = old %>% mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
                       CheckOut = as.Date(CheckOut,origin= '1899-12-30'),)
  #temp = rbind.fill(old ,temp)
  temp = rbind.fill(old %>% filter(Month %in% c('2023-08','2023-09')),temp)
  }
  write.xlsx(list("2023"=temp),paste0(file_loc,
              employee$First.Name[employee$Nick.name %in% k],'_',
              employee$Last.Name[employee$Nick.name %in% k],'.xlsx'),
            na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
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
                Earnings = sum(Earnings,na.rm=T),
                CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
  relocate(Month,.before=Cohost)
sum_loc = "./Cohost's reservation sheets/"
sum_prop = read.xlsx(paste0(sum_loc,'Property_Cohost_summary_Sept2023.xlsx'),sheet = 'Property')
sum_coh  = read.xlsx(paste0(sum_loc,'Property_Cohost_summary_Sept2023.xlsx'),sheet = 'Cohost')

sum_prop = sum_prop %>% mutate(Month='2023-09') %>% relocate(Month,.before=Cohost)
sum_coh = sum_coh %>% mutate(Month='2023-09') %>% relocate(Month,.before=Cohost)

sum_property_all = rbind.fill(sum_prop,sum_property) %>% 
  relocate(Bimonthly,TrashMonthly,.before = Earnings)
sum_cohost_all = rbind.fill(sum_coh,sum_cohost) %>% 
  relocate(Bimonthly,TrashMonthly,.before = Earnings)
output = list(Cohost=sum_cohost_all,Property = sum_property_all)
write.xlsx(output,"./Cohost's reservation sheets/Property_Cohost_Summary.xlsx",
           firstActiveRow = 2,withFilter = T)

