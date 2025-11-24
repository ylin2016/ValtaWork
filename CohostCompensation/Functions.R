library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
library(lubridate)
##-----------------------------------------------------------------------------------
## Input files
##-----------------------------------------------------------------------------------
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
cohost_input = function(){
  cohost = read.xlsx('./Working/Data/Property_Cohost.xlsx')
  employee = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Employee')
  employee.rates = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Rates',rows=1:4)
  employee = employee %>% select(-(Comment)) %>% join(employee.rates)
  cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
    arrange(Listing)
  list(cohost=cohost,employee=employee)
}
confirmed_input = function(fileloc,filemonth){
  confirmed = read.csv(paste0(fileloc,'Guesty_booking_confirmed_',filemonth,'.csv'))
  #print(setdiff(cohost$Listing,confirmed$LISTING.S.NICKNAME))
  print("Property missed: ")
  print(setdiff(confirmed$LISTING.S.NICKNAME,cohost$Listing))
  confirmed = format_reservation(confirmed,startdate,enddate) 
  confirmed
}

cancelled_input = function(fileloc,filemonth){
  cancelled = read.csv(paste0(fileloc,'Guesty_booking_cancelled_',filemonth,'.csv'))
  cancelled = format_reservation(cancelled,startdate,enddate)
  cancelled$Earnings[!cancelled$Source %in% 'Airbnb' & !is.na(cancelled$Earnings)] = 0
  print("Property missed: ")
  print(setdiff(cancelled$LISTING.S.NICKNAME,cohost$Listing)) 
  cancelled
}
duplicated_reservations_thismonth = function(confirmed,prior1){
  ## Check same guest continued stay in 2 bookings 
  print(sum(dup<-duplicated(paste(confirmed$Listing,confirmed$GuestName))))
  combResvId = paste(confirmed$Listing,confirmed$GuestName)[dup]
  print("Potential duplicates: ")
  print(confirmed[paste(confirmed$Listing,confirmed$GuestName) %in% combResvId,] %>%
          arrange(GuestName,CheckIn))
  combResvId
}

duplicated_reservations_lastmonth = function(confirmed,prior1){
  prior2 = as.character(as.Date(prior1)+days_in_month(prior1)-1)
  confirmed0 = read.csv(paste0(fileloc,'Guesty_booking_confirmed_',
                               format(as.Date(prior2),"%Y-%m"),'.csv'))
  confirmed0 = format_reservation(confirmed0,prior1,prior2)
  dups = intersect(paste(confirmed$Listing,confirmed$GuestName),
                   paste(confirmed0$Listing,confirmed0$GuestName))
  
  print(rbind(confirmed[paste(confirmed$Listing,confirmed$GuestName) %in% dups,],
              confirmed0[paste(confirmed0$Listing,confirmed0$GuestName) %in% dups,]) %>%
          arrange(Listing,CheckIn))
  dups
} 
update_duplicated_confirmed = function(confirmed,combResvId){
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
}
trips_input = function(fileloc,filemonth,employee,LRTs=NULL){
  trips  = read.xlsx(paste0(fileloc,'TripLog_',filemonth,'.xlsx'))
  colnames(trips)[1:5] = c('Timestamp','CohostName','visiting.Date','Property','Comment')
  
  trips = trips %>% filter(!Property %in% 'Other') %>%
    mutate(Month = substr(enddate,1,7),
           Cohost = sapply(CohostName,function(x) unlist(strsplit(x,' '))[1]),
           CheckIn = as.Date(visiting.Date,origin= '1899-12-30'),
           Status = ifelse(grepl('Bi-monthly inspection|Bio monthly inspection',Comment),'BimonInspection',
                           ifelse(grepl('Move in/out Walkthrough',Comment),'Walkthrough',
                                  ifelse(grepl('Take care of trash|Monthly|garbage bins',Comment) & Cohost %in% c("Lucia"),
                                         'MonthlyTrashCollection','Trip')))) %>%
    mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
    mutate(Cohost = sub('ChenHui','Sasha',Cohost)) %>% 
    mutate(Cohost = ifelse(CohostName %in% 'Jing Zhou','JingZhou',Cohost)) %>%
    join(employee %>% select(Nick.name,Trip.rate,`Bi-mo.inspection`,MonthlyTrashCollection,Walkthrough) %>% 
           mutate(Cohost = Nick.name)) %>%
    join(cohost %>% select(Listing,BEDROOMS)) %>%
    mutate(Bimonthly = ifelse(BEDROOMS %in% 1,40,ifelse(BEDROOMS %in% 2,60,80))) %>%
    mutate(CohostPayOut = ifelse(Status %in% 'BimonInspection',Bimonthly,
                                 ifelse(Status %in% 'MonthlyTrashCollection',
                                        MonthlyTrashCollection,
                                        ifelse(Status %in% 'Walkthrough',Walkthrough,Trip.rate)))) %>%
    arrange(Cohost,Property,CheckIn)
  
  # add Niya's monthly trash collection $50/m
  trips.trashes = data.frame(Month =  substr(enddate,1,7),
                             Property= 'Seattle 1623',
                             Cohost='Niya',
                             CheckIn = as.Date(enddate),
                             Status = "MonthlyTrashCollection",
                             CohostPayOut = 50)
  ## change to VA from Eddies's Long term lease fee
  trips.LTR = data.frame(Month =  substr(enddate,1,7),
                         Property= paste('Beachwood',LRTs), # sept
                         Cohost='VA',
                         CheckIn = as.Date(enddate),
                         Status = "Long Term Leases",
                         CohostPayOut = 45)
  trips = rbind.fill(trips, trips.trashes,trips.LTR)
  
  trips[trips$Cohost %in% 'Destenit','Comment'] = 
    paste0("Flavio's trip: ",trips[trips$Cohost %in% 'Destenit','Comment'])
  trips[trips$Cohost %in% 'Destenit','CohostPayOut'] = 40
  trips
}

inspections_input= function(fileloc,filemonth,employee){
  inspections = read.xlsx(paste0(fileloc,'Bi_monthly_Inspection_',filemonth,'.xlsx'))
  colnames(inspections)[1:4] = c('Timestamp','CohostName','Property','visiting.Date')
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
    join(cohost %>% select(Property,BEDROOMS)) %>%
    mutate(Bimonthly = ifelse(BEDROOMS %in% 1,40,ifelse(BEDROOMS %in% 2,60,80))) %>%
    mutate(CohostPayOut = Bimonthly) %>%
    arrange(Cohost,Property)
  print(table(inspections$BEDROOMS,exclude=NULL))
  inspections
}

combine_trips = function(trips,inspections=NULL){
  trip.var = c('Month','Cohost','Property','CheckIn','Status','CohostPayOut','Comment')
  if(is.null(inspections)){
    trips.all = trips[,trip.var]
  }else{trips.all = rbind.fill(inspections,trips)[,trip.var]}

  trips.all  = trips.all %>% 
    filter(!duplicated(paste(Property,CheckIn))) %>% 
    mutate(Listing = Property) %>%
    arrange(Cohost,CheckIn)
  print("##!!!! check trips <= 4 times in a month, if >4 times, ==>4")
  print(sort(table(trips.all$Property)))
  trips.all
}
combine_reservations= function(confirmed,cancelled){
  reservations = rbind(confirmed,cancelled)
  reservations = merge(cohost %>% select(Listing,Property,Cohost,Regular.rate),reservations,by='Listing',all.y=T)
  reservations = reservations %>% 
    mutate(Month = substr(enddate,1,7),
           CohostPayOut = ifelse(is.na(Earnings),0,
             ifelse(Earnings<200,0,
              ifelse(Status %in% 'canceled', Regular.rate/2,
               ifelse(Status %in% 'confirmed',
                ifelse(Nights<=30,Regular.rate,floor(Nights/30)*Regular.rate),NA))))) %>%
    mutate(CohostPayOut = ifelse(Listing %in% "Cottages All OSBR",
                                 CohostPayOut*10,CohostPayOut))
  reservations
}
add_backups = function(fileloc, filemonth,reservations,employee) ## add backup
{
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
  reservations_backup
}

combine_reservations_trips_backup = function(reservations,trips.all,
                                             reservations_backup=NULL){
  
    output.val = c('Month','Listing','Confirmation.Code','Status','GuestName',
                   'Guests','CheckIn','CheckOut','Nights','Earnings',
                   'CohostPayOut','Backup','Comment')
    if(is.null(reservations_backup)){
      reservations = rbind.fill(reservations,trips.all)
    
    }else{
      reservations = rbind.fill(reservations,trips.all,reservations_backup)
    }
    reservations = reservations %>% filter(!Cohost %in% c("Yumiko"))
    reservations
}
cohost_sheets = function(reservations,employee){
  output.val = c('Month','Listing','Confirmation.Code','Status','GuestName',
                 'Guests','CheckIn','CheckOut','Nights','Earnings',
                 'CohostPayOut','Backup','Comment')
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
    if(k %in% c())  # new cohost this month
    { 
      write.xlsx(list("2025"=temp),paste0(file_loc,CohostName,'.xlsx'),
                 na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
    }else{
      old2023 = try(read.xlsx(paste0(file_loc,CohostName,'.xlsx'),sheet='2023'))
      old2024 = try(read.xlsx(paste0(file_loc,CohostName,'.xlsx'),sheet='2024'))
      old = read.xlsx(paste0(file_loc,CohostName,'.xlsx'))
      old = old %>% filter(!Month %in% substr(enddate,1,7)) %>%
        mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
               CheckOut = as.Date(as.integer(CheckOut),origin= '1899-12-30'))
      colnames(old)[colnames(old) %in% 'Property'] = 'Listing'
      temp = rbind.fill(temp,old)  
      if(k %in% c("Bri","VA","Destenit")) ## new this year
      { 
        write.xlsx(list("2025"=temp),paste0(file_loc,CohostName,'.xlsx'),
                   na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
      }else{
        if(class(old2023)=="try-error"){
          write.xlsx(list("2025"=temp,"2024"=old2024),
                     paste0(file_loc,CohostName,'.xlsx'),
                     na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
        }else{
          write.xlsx(list("2025"=temp,"2024"=old2024,"2023"=old2023),
                     paste0(file_loc,CohostName,'.xlsx'),
                     na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
        }
      }
    }
  }
}

summary_sheets = function(reservations){
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
            LT_Lease = sum(Status %in% 'Long Term Leases'),
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
            LT_Lease = sum(LT_Lease),
            Backup = sum(Backup),
            Earnings = round(sum(Earnings,na.rm=T),2),
            CohostPayOut = sum(CohostPayOut,na.rm=T)) %>%
    relocate(Month,.before=Cohost)
    output = list(Cohost=sum_cohost,Property = sum_property)
}

update_summarysheet = function(sum_cohost,sum_property){
  sum_loc = "./Cohost's reservation sheets/"
  sum_prop0 = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Property_2324')
  sum_coh0  = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Cohost_2324')
  sum_prop = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Property')
  sum_coh  = read.xlsx(paste0(sum_loc,'Property_Cohost_Summary.xlsx'),sheet = 'Cohost')
  
  sum_coh0$Paid.day = as.character(as.Date(as.integer(sum_coh0$Paid.day),origin= '1899-12-30'))
  sum_coh$Paid.day = as.character(as.Date(as.integer(sum_coh$Paid.day),origin= '1899-12-30'))
  
  sum_property_all = rbind.fill(sum_property,
                                sum_prop %>% filter(!Month %in% substr(enddate,1,7))) %>%
    relocate(Bimonthly,TrashMonthly,.before = Earnings)
  
  sum_cohost= sum_cohost %>% 
    mutate(Paid.amount = ifelse(Cohost %in% c("Bri","Feifei","Paul","VA"),NA,
        ifelse(Cohost %in% "Crystal",length(unique(sum_property$Property[Cohost %in% "Crystal"]))*150,
           ifelse(Cohost %in% "Shaya", 1600,CohostPayOut))))
  sum_cohost_all = rbind.fill(sum_cohost,
                              sum_coh %>% filter(!Month %in% substr(enddate,1,7))) %>%
    relocate(Bimonthly,TrashMonthly,.before = Earnings)
  
  output = list(Cohost=sum_cohost_all,Property = sum_property_all,
                Cohost_2324=sum_coh0,Property_2324=sum_prop0)
}
STR_calculate_table = function(reservations){
  org_table = read.xlsx("./Working/Data/2025/STR Properties by Month.xlsx") 
  colnames(org_table)[-1] = paste0('m',1:2,'/2025')
  org_table$rank = 1:nrow(org_table)
  STR_table = reservations %>% filter(Status %in% 'confirmed') %>%
    group_by(Listing) %>% 
    reframe(STRs = sum(Nights<30)) %>%
    mutate(current = ifelse(STRs>0,"STR",""))
  STR_Table = merge(org_table[,c("Listing","rank")],STR_table[,c("Listing",'current')],
                    by="Listing",all=T) %>% arrange(rank)
  write.xlsx(STR_Table, "./Cohost's reservation sheets/STR_Table.xlsx",
             firstActiveRow = 2,withFilter = T)
  STR_Table
}

cleaning_sheets_summary = function(reservations,enddate){
  ## Cleaner payment sheet
  cleaner = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Cleaning')
  cleansheet = merge(confirmed[,c("Listing","Confirmation.Code",'CheckIn','CheckOut','Guests','Nights',"GuestName","Earnings")],
                     cleaner[,c("Listing","Cleaner.lead",'Cleaning.fee',"Maria.pay")],
                     by="Listing",all.x=T) 
  cleansheet = cleansheet %>% 
    mutate(Month =substr(enddate,1,7)) %>% # 2025.10 changed
         #  Cleaning.fee = ifelse(is.na(New.cleaning.fee),  ## 2025.03 changed
         #                       Current.clg.fee,New.cleaning.fee)) %>% 
    relocate(Month,Listing) %>% 
    filter(Earnings>100) # %>%
   # mutate(New.cleaning.fee=NULL,Current.clg.fee=NULL)
  
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
  cleaning.output
}
cleaner_sheets = function(cleansheet){
  cleaning_loc = "./Cohost's reservation sheets/CleaningSheet_"
  for(k in unique(cleansheet$Cleaner.lead))
  {
    print(k)
    old2024 = read.xlsx(paste0(cleaning_loc,k,'.xlsx'),sheet='2024')
    old = read.xlsx(paste0(cleaning_loc,k,'.xlsx'),sheet='2025')
    old = old %>% filter(!Month %in% substr(enddate,1,7)) %>%
      mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
             CheckOut = as.Date(as.integer(CheckOut),origin= '1899-12-30'))
    temp = cleansheet %>% filter(Cleaner.lead %in% k) %>% select(all_of(colnames(old)))
    temp = rbind.fill(temp,old)
    write.xlsx(list("2025"=temp,"2024"=old2024),paste0(cleaning_loc,k,'.xlsx'),
               na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
  }
}