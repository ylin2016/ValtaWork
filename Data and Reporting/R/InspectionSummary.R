library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/** Properties ** -- Valta/1_Maintenance and Cleaning inspections/1_Robert inspection/")
property = read.xlsx('/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Data and Reporting/01- Compensation Calculation/Working/Data/Property_Cohost.xlsx')

allrecords = data.frame(path=list.dirs())

all = allrecords %>% 
  mutate(Listing= sapply(path, function(x) unlist(strsplit(x,'/'))[2]),
          inspection = sapply(path, function(x) unlist(strsplit(x,'/'))[3])) %>%
  filter(!is.na(inspection)) %>% 
  mutate(time = sapply(inspection, function(x) unlist(strsplit(x,'2024'))[1]),
         InspectionType = sapply(inspection, function(x) unlist(strsplit(x,'2024'))[2]))

all = all %>% 
  mutate(Date = as.Date(paste0(2024,gsub(" ","",time)),format="%Y%m%d"))

all = merge(all %>% select(Listing,Date,InspectionType), 
            property %>% select(Listing,BEDROOMS),
            by='Listing',all.x=T)  
all = all %>% 
      mutate(Compensation = 125 + 25*(ifelse(BEDROOMS>5,5,BEDROOMS)-1))
write.xlsx(all,"Inspection_Robert.xlsx",firstActiveRow = 2,withFilter = T)

