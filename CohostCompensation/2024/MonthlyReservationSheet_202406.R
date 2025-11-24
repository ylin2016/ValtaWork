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
startdate ='2024-06-01'
enddate = '2024-06-30'
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
  data$Earnings = as.numeric(ifelse(!is.na(data$TOTAL.PAID),data$TOTAL.PAID,data$TOTAL.PAYOUT))
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
## manually add one record from May manual blocked:
confirmed = rbind.fill(confirmed,data.frame(Listing='Kirkland 11321',
              Confirmation.Code='HMTXAHNE88',Status='confirmed',
            GuestName='Lindy Arsenault',CheckIn = as.Date("2024-05-09"),
            CheckOut = as.Date('2024-05-12'),Nights=3,Earnings=737.20,Source='Direct'))
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
                    ifelse(grepl('Take care of trash',Comment) & Cohost %in% c('Yi',"Lucia"),
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

# assigned cleaning fee for owner cleaned Seattle 1623: Cleaning done by Maria
#cleaning.trip = confirmed[confirmed$Listing %in% 'Seattle 1623',] %>% 
#          filter(!duplicated(CheckIn)) %>% select(any_of(c('Listing',trip.var))) %>% 
#          mutate(Comment='Cleaning',Cohost='Niya',Property=Listing,Month='2024-06')
#trips.all = rbind.fill(trips.all,cleaning.trip)
#trips.all$CohostPayOut[trips.all$Listing %in% 'Seattle 1623' & 
#                         trips.all$Comment %in% 'Cleaning'] = 150
View(trips.all) #33
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

#=================================================================================
## add backup 
backup = read.xlsx(paste0(fileloc,'Cohost_Backup_',filemonth,'.xlsx'))

colnames(backup)[-1] = c('Cohost.org','StartTime','EndTime','Backup','Property')
backup = backup %>%
  mutate(StartTime = as.Date(StartTime,origin= '1899-12-30'),
         EndTime = as.Date(EndTime,origin= '1899-12-30'),
         Cohost.org = sapply(Cohost.org,function(x) unlist(strsplit(x," (",fixed=T))[1])) 

backup = merge(backup, employee[,c('CohostName','Nick.name')],
               by.x='Cohost.org',by.y='CohostName',all.x=T)
backup = merge(backup, employee[,c('CohostName','Nick.name')],by.x='Backup',
               by.y='CohostName',suffix=c('.org','.backup'),all.x=T)

#View(backup)
reservations_backup = NULL
for(i in 1:nrow(backup))
{
  idx = reservations$CheckOut<=backup$EndTime[i] &
    reservations$CheckOut>=backup$StartTime[i] &
    reservations$Property %in% unlist(strsplit(backup$Property[i],', '))
  if(sum(idx)>0){
    reservations_backup = rbind(reservations_backup,reservations %>%
                                  filter(CheckOut<=backup$EndTime[i] & CheckOut>=backup$StartTime[i] &
                                           Property %in% unlist(strsplit(backup$Property[i],', '))) %>%
                                  mutate(Cohost = backup$Nick.name.backup[i],
                                         CohostPayOut = CohostPayOut/2,
                                         Earnings = Earnings/2,
                                         Comment = paste('Backup for',backup$Nick.name.org[i])))
    reservations$CohostPayOut[idx] = reservations$CohostPayOut[idx]/2
    reservations$Earnings[idx] = reservations$Earnings[idx]/2
    reservations$Backup[idx] = backup$Nick.name.backup[i]
  }
}

##-----------------------------------------------------------------------------------
## Combine all 
##-----------------------------------------------------------------------------------
output.val = c('Month','Listing','Confirmation.Code','Status','GuestName','Guests','CheckIn',
               'CheckOut','Nights','Earnings','CohostPayOut','Backup','Comment')

reservations = rbind.fill(reservations,reservations_backup,trips.all)
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
  if(k %in% c("Paul","Shaya"))
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
    if(k %in% c("Anna","Thao","Feifei","JingZhou","Lulu","Sasha","YongChao")) ## new this year
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
                Earnings = round(sum(Earnings,na.rm=T),2),
                CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
  relocate(Month,.before=Cohost)
View(sum_cohost)
View(sum_property)
sum_loc = "./Cohost's reservation sheets/"
sum_prop = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Property')
sum_coh  = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Cohost')
#paiday = as.character(as.Date(as.integer(sum_coh$Paid.day[-(1:12)]),origin= '1899-12-30'))
#sum_coh$Paid.day[-(1:12)] =  paiday
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


## Cleaner payment sheet
cleaner = read.xlsx("./Working/Data/Cleaning Schedule and Price.xlsx")
cleansheet = merge(confirmed[,c("Listing",'CheckIn','CheckOut','Guests','Nights')],
                   cleaner[,c("Property","Listing","Cleaner.lead",'Cleaning.fee')],
                   by="Listing",all.x=T)

Niya_cleaning = trips.all %>% filter(Cohost %in% 'Niya' & Comment %in% 'Cleaning')

cleansheet = cleansheet %>% 
  mutate(Month =substr(enddate,1,7)) %>% 
  relocate(Month,Listing) %>%
  mutate(Cleaner.lead = ifelse(Listing %in% 'Seattle 1623',
         ifelse(!CheckOut %in% Niya_cleaning$CheckIn,"Maria",Cleaner.lead),Cleaner.lead))
  
cleansheet %>% filter(Listing %in% 'Seattle 1623')

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
cleaning.output = list(CleaningSheet202406 =cleansheet,
                       CleaningPerProperty=summary_cleaning,
                       PayOut = PayOut)
write.xlsx(cleaning.output,filename,firstActiveRow = 2,withFilter = T)

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
