###################################################################
## Create bimonthly inspection list ##
###################################################################
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/")
cohost = read.xlsx('./Working/Data/Property_Cohost.xlsx')
employee = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Employee')
employee.rates = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Rates')
employee = employee %>% select(-(Comment)) %>% join(employee.rates)
cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
  arrange(Listing)
#View(cohost)

fileloc = './Working/Data/'
filenames = c('Sept2023','Oct2023','Nov2023','Dec2023','2024-01','2024-02','2024-03','2024-04',"2024-05")
trips = inspections = NULL
for(filemonth in filenames)
{
  year = ifelse(grepl('2023',filemonth),'2023', 
                ifelse(grepl('2024',filemonth),'2024',filemonth))
  trip.tmp  = read.xlsx(paste0(fileloc,year,'/TripLog_',filemonth,'.xlsx'))
  trips = rbind.fill(trips,data.frame(yearmonth=filemonth,trip.tmp))
  if(!filemonth %in% c('Sept2023','Oct2023'))
  {
    inspections.tmp = read.xlsx(paste0(fileloc,year,'/Bi_monthly_Inspection_',filemonth,'.xlsx'))
    inspections = rbind.fill(inspections,data.frame(yearmonth=filemonth,inspections.tmp))
  }
}

colnames(trips)[1:6] = c('yearmonth','Timestamp','CohostName','visiting.Date','Property','Comment')
colnames(inspections)[1:5] = c('yearmonth','Timestamp','CohostName','Property','visiting.Date')

trips = trips %>% filter(!Property %in% 'Other') %>%
  mutate(Cohost = sapply(CohostName,function(x) unlist(strsplit(x,' '))[1]),
         LastVisit  = as.Date(visiting.Date,origin= '1899-12-30'),
         Status = ifelse(grepl('Bi-monthly inspection|Bio monthly inspection',Comment),'BimonInspection',
                         ifelse(Comment %in% 'Take care of trash','MonthlyTrashCollection','Trip'))) %>%
  mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
  mutate(Cohost = ifelse(CohostName %in% 'Jing Zhou','JingZhou',Cohost)) %>%
  join(employee %>% select(Nick.name,Trip.rate,`Bi-mo.inspection`,MonthlyTrashCollection) %>% 
         mutate(Cohost = Nick.name)) %>%
  mutate(CohostPayOut = ifelse(Status %in% 'BimonInspection',`Bi-mo.inspection`,
                               ifelse(Status %in% 'MonthlyTrashCollection',
                                      MonthlyTrashCollection,Trip.rate))) %>%
  filter(Status %in% 'BimonInspection')

inspections = inspections %>% filter(!Property %in% 'Other') %>%
  mutate(Cohost = sapply(CohostName,function(x) unlist(strsplit(x,' '))[1]),
         LastVisit  = as.Date(visiting.Date,origin= '1899-12-30'),
         Status = 'BimonInspection') %>% 
  mutate(Cohost = sub('Mengyu','Zoey',Cohost)) %>% 
  mutate(Cohost = ifelse(CohostName %in% 'Jing Zhou','JingZhou',Cohost)) %>%
  join(employee %>% select(Nick.name,`Bi-mo.inspection`) %>% 
         mutate(Cohost = Nick.name)) %>%
  mutate(CohostPayOut = `Bi-mo.inspection`) %>%
  arrange(Cohost,Property)

trip.var = c('Cohost','Property','LastVisit')
LastInspection = rbind.fill(inspections,trips)[,trip.var] %>% 
  filter(!duplicated(paste(Property,LastVisit))) %>% 
  arrange(Property,desc(LastVisit )) %>%
  mutate(NextDue = LastVisit + 60) %>%
  join(cohost %>% 
         mutate(Cohost = ifelse(Property %in% c('Seattle 115', "Seattle 1117","Seattle 1512","Seattle 710", "Seattle 710ADU"),"Yi",Cohost)) %>%
         select(Cohost,Property),type='right') %>%
  filter(!duplicated(Property))  
  
View(LastInspection)

sum_loc = "./Cohost's reservation sheets/"
Year2024_01= read.xlsx("./Cohost's reservation sheets/Bimonthly_Inspection_Status.xlsx",
                       sheet='Year2024_01') %>%
            mutate(LastVisit = as.Date(LastVisit,origin= '1899-12-30'),
                   NextDue= as.Date(NextDue,origin= '1899-12-30'))
Year2024_02= read.xlsx("./Cohost's reservation sheets/Bimonthly_Inspection_Status.xlsx",
                       sheet='Year2024_02') %>%
  mutate(LastVisit = as.Date(LastVisit,origin= '1899-12-30'),
         NextDue= as.Date(NextDue,origin= '1899-12-30'))                          
Year2024_03= read.xlsx("./Cohost's reservation sheets/Bimonthly_Inspection_Status.xlsx",
                       sheet='Year2024_03') %>%
  mutate(LastVisit = as.Date(LastVisit,origin= '1899-12-30'),
         NextDue= as.Date(NextDue,origin= '1899-12-30'))   

write.xlsx(LastInspection,
           "./Working/Bimonthly_Inspection_Status_current.xlsx",
           firstActiveRow = 2,withFilter = T)
