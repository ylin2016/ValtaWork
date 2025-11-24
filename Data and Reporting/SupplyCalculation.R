# 2023.12 Supply Calculation
# - Only properties in Seattle and EastSide are included, 710 excluded for that
# - For reservation >14 days, consider supply for 14 days only
# Last update: pull data 12/24/2023

library(readxl)
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost")
property = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')

#guesty = read.csv('./Data and Reporting/Data/Guesty_PastBooking_11082023.csv')
guesty = read.csv('./Data and Reporting/Data/Guesty_PastBooking_12312023.csv')
guesty$Earnings = ifelse(!is.na(guesty$TOTAL.PAID), guesty$TOTAL.PAID,
                         guesty$TOTAL.PAYOUT)
#vrbo = read.csv('./Data and Reporting/Input_PowerBI/VRBO_20200101-20231230.csv')
#vrbo = vrbo[grep('2023-',vrbo$CHECK.OUT),]
vrbo.fill = read.csv('./Data and Reporting/Data/VRBO_20200101-20231230_filled.csv')
supplies = read.xlsx('./Data and Reporting/Supplies Calculation/2023 supplies.xlsx',sheet='Sheet2')
colnames(supplies)[5] = 'UnitCost_Julie'
all = rbind(guesty,vrbo.fill)
all = all %>% 
     mutate(CheckOutYear = substr(CHECK.OUT,1,4),
            LISTING.S.NICKNAME = sub("Seattle 3617 Origin","Seattle 3617",LISTING.S.NICKNAME),
            CHECK.OUT = as.Date(CHECK.OUT),Quarter = quarters(CHECK.OUT),
            Counts = ifelse(NUMBER.OF.NIGHTS>14, 14,NUMBER.OF.NIGHTS)*NUMBER.OF.GUESTS) %>% 
     filter(CHECK.OUT<='2023-12-31' & CHECK.OUT>='2022-01-01') %>%
     filter(!LISTING.S.NICKNAME %in% c('Seattle 710','Seattle 710 ADU'))
all = merge(all, property[,c('Listing','Property','LISTING.S.CITY')],
            by.x='LISTING.S.NICKNAME',by.y='Listing',all.x=T)

sum_property = all %>% 
  filter(!LISTING.S.CITY %in% c('Hoodsport','Lilliwaup','Longbranch','Poulsbo','Shelton')) %>%
  filter(!(CheckOutYear %in% 2023 & Quarter %in% 'Q3' & 
             Property %in% c('Bellevue 514','Mercer 3627','Seattle 1623'))) %>%
  filter(!(CheckOutYear %in% 2023 & Quarter %in% 'Q4' & 
             Property %in% c('Bellevue 514','Mercer 3627','Seattle 1623','Seattle 3617','Ocean Spray 3'))) %>%
  filter(NUMBER.OF.NIGHTS<=14) %>%
  group_by(CheckOutYear,Quarter,Property,LISTING.S.CITY) %>% 
  reframe(Counts = sum(Counts),
          Nights = sum(NUMBER.OF.NIGHTS),
          AvgGuestPerNight = round(Counts/Nights))
          #NightsMissingGuestNumber = sum(NUMBER.OF.NIGHTS[is.na(NUMBER.OF.GUESTS)]))


supply_cal = sum_property %>% group_by(CheckOutYear,Quarter) %>% 
  reframe(TotalCounts = sum(Counts)) 
supply_cal = merge(supply_cal,supplies,by.x=c('CheckOutYear','Quarter'),
                   by.y=c('Year','Quarter'))
supply_cal = supply_cal %>% mutate(unitCost = round(SuppliesCost/TotalCounts,3),
                                   UnitCost_Julie = round(UnitCost_Julie,2))
View(supply_cal)
  
View(sum_property %>% filter(CheckOutYear==2023 & Quarter %in% 'Q1'))

sum_property = merge(sum_property,supply_cal[,c('CheckOutYear','Quarter','unitCost')],
                     by=c('CheckOutYear','Quarter')) %>% mutate(SupplyCost = round(Counts*unitCost,2))
                     
write.xlsx(list("QuarterSupplyCost"=sum_property,"Summary"=supply_cal),
           './Data and Reporting/Supplies Calculation/SupplyCosts.xlsx',
            na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)        
                     