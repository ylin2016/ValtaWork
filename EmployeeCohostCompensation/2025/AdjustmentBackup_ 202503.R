## 3/26/2025 : make adjustment for Jan and Feb backup records
library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/")
startdate ='2025-03-01'
enddate = '2025-03-31'
filemonth = substr(enddate,1,7)
fileloc = './Working/Data/2025/'
cohost = read.xlsx('./Working/Data/Property_Cohost.xlsx')
employee = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Employee')
employee.rates = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Rates',rows=1:4)
employee = employee %>% select(-(Comment)) %>% join(employee.rates)
cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
  arrange(Listing)
load("./Working/reservations_for_backup.Rdata")

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

View(backup)
reservations = rbind(res01,res02) %>% filter(Status %in% "confirmed")
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

res_adj = rbind(reservations_backup,
                reservations %>% 
                  filter(Confirmation.Code %in% reservations_backup$Confirmation.Code) %>%
                  mutate(CohostPayOut = -CohostPayOut))
output.val = c('Month','Listing','Confirmation.Code','Status','GuestName','Guests','CheckIn',
               'CheckOut','Nights','Earnings','CohostPayOut','Backup','Comment')

sum_property_adj = res_adj %>% 
  group_by(Month,Cohost,Property) %>% 
  reframe(Reservations = sum(Status %in% c('confirmed','canceled')), 
          Nights = sum(Nights[Status %in% 'confirmed']),
          Confirmed = sum(Status %in% 'confirmed'),
          Canceled = sum(Status %in% 'canceled'),
          Trips = sum(Status %in% 'Trip'),
          Bimonthly= sum(Status %in% 'BimonInspection'),
          TrashMonthly = sum(Status %in% 'MonthlyTrashCollection'),
          LT_Lease = sum(Status %in% 'Long Term Leases'),
          Backup = sum(grepl('Backup for',Comment)),
          Earnings = round(sum(Earnings,na.rm=T),2),
          CohostPayOut = sum(as.numeric(CohostPayOut),na.rm=T)) 
 
sum_cohost_adj = sum_property_adj %>% group_by(Month,Cohost) %>% 
  reframe(Reservations = sum(Reservations), 
          Confirmed = sum(Confirmed),
          Canceled = sum(Canceled),
          Trips = sum(Trips),
          Bimonthly= sum(Bimonthly),
          TrashMonthly = sum(TrashMonthly),
          LT_Lease = sum(LT_Lease),
          Backup = sum(Backup),
          Earnings = round(sum(Earnings,na.rm=T),2),
          CohostPayOut = sum(CohostPayOut,na.rm=T)) 

View(sum_cohost)
View(sum_property)
save(res_adj,sum_cohost_adj,sum_property_adj,file="./Working/Backup_adjustment_202503.Rdata")
