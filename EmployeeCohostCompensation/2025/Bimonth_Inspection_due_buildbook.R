###################################################################
## Create bimonthly inspection list ##
###################################################################
library(plyr)
library(dplyr)
library(openxlsx)
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/")
bimonth = read.xlsx('./Working/Data/2025/Bimonthly_Inspection_Status_20250914.xlsx') %>%
  mutate(DueDate=as.Date(DueDate,origin= '1899-12-30'))

dues08 = bimonth %>% filter(DueDate<'2025-09-01')
dues09 = bimonth %>% filter(DueDate<'2025-10-01')
output = list(Due.2509=dues09,
                Due.2508=dues08,
                Schedules = bimonth)
write.xlsx(output, "./Cohost's reservation sheets/Property_due_bimonthly_inspection.xlsx")

## check monthly: 

bimonth = read.xlsx("./Cohost's reservation sheets/Property_due_bimonthly_inspection.xlsx",
                  sheet="Schedules") %>%
  mutate(DueDate=as.Date(DueDate,origin= '1899-12-30'))

done08 = read.xlsx("./Cohost's reservation sheets/Property_due_bimonthly_inspection.xlsx",
                   sheet="Due.2508") %>%
  mutate(DueDate=as.Date(DueDate,origin= '1899-12-30'),
         Completion.Date=as.Date(Completion.Date,origin= '1899-12-30'))

done09 = read.xlsx("./Cohost's reservation sheets/Property_due_bimonthly_inspection.xlsx",
                   sheet="Due.2509") %>%
  mutate(DueDate=as.Date(DueDate,origin= '1899-12-30'),
         Completion.Date=as.Date(as.numeric(Completion.Date),origin= '1899-12-30'))

# 13 of 28 completed in Sept, only 5 with buildbook records

bimonth = merge(bimonth, 
                done09 %>% filter(!is.na(Completion.Date)) %>% 
                  select(Property,Completion.Date,`Buildbook.records?`),
                by="Property",all.x=T)

bimonth = bimonth %>%
          mutate(newdue = Completion.Date %m+% months(2),
                 DueDate = ifelse(!is.na(Completion.Date),as.character(newdue),
                                  as.character(DueDate)))
                 
dues = bimonth %>% mutate(DueDate = as.Date(DueDate)) %>% 
  filter(DueDate<'2025-11-01' & Status %in% 'Active') 
# 25 due in Oct with carry over from last month

output = list(Due = dues, Schedules = bimonth)
write.xlsx(output, "./Working/Property_due_bimonthly_inspection.xlsx")



                 
                 