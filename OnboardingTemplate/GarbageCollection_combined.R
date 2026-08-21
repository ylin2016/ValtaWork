library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-Valta AI/OnboardingTemplate/")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)

Listings = property$Listing[property$Type %in% "STR" & property$Status %in% "Active"]

garbage = NULL
for(k in Listings[-c(15,49,61)])
{
  tmp = read.xlsx(paste0("Output_AI/",k,'.xlsx'),sheet = "Property",startRow = 2)
  garbage = rbind.fill(garbage,
                       data.frame(Listing=k,
                                  tmp %>% 
                                  filter(Field %in% "Maintenance – Utilities – Garbage/Recycling Pickup days")))
}

write.xlsx(garbage,"Garbage Pickup days.xlsx")
