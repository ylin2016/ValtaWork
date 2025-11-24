setwd("/Users/ylin/My Drive/Cohost/Data and Reporting/05-Cleaning/")
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(reshape2)
library(scales)
library(ggplot2)
library(ggbreak)
library(slider)

dispatch = read.xlsx("./Data/Maria Cleaning Dispatch Card.xlsx") %>%
        mutate(Cleaning.Date= as.Date(Cleaning.Date,origin='1899-12-30'))
colnames(dispatch)[-c(1:3)] = c("Team1",paste0("Car1P",1:5),"Car1Add",
                                "Team2",paste0("Car2P",1:5),"Car2Add",
                                "Team3",paste0("Car3P",1:4),"Car3Add",
                                "Team4",paste0("Car4P",1:4),"Car4Add")

dispatch[,-(1:3)] = apply(dispatch[,-(1:3)],2,function(x) ifelse(x %in% "Other specify:",NA,x))

for(i in 1: nrow(dispatch))
{
  nunit = 0
  for(j in 1:4)
  {
    units = dispatch[i,setdiff(grep(paste0("Car",j),colnames(dispatch)),
                               grep(paste0("Car",j,"Add"),colnames(dispatch)))]
    nunit = nunit + sum(!units %in% c(NA,"NA","Other specify:"))
    if(!is.na(dispatch[i,paste0("Car",j,"Add")])){
       units.add = unlist(strsplit(dispatch[i,paste0("Car",j,"Add")],";"))
        for(k in units.add)
        {
          
          num=as.integer(unlist(strsplit(k,"[.,]"))[1])
          unit.sel = trimws(unlist(strsplit(k,"[.,]"))[2])
          if(!is.na(num)) {
            dispatch[i,paste0("Car",j,"P",num)] = unit.sel
            nunit = nunit + 1
          }
        }
    }
  }
  dispatch[i,'nunit'] = nunit
}
## Manual correct: no 8/1 1717, 8/19 510,
dispatch[dispatch$Cleaning.Date %in% '2025-08-01','Car2P4'] = NA
dispatch[dispatch$Cleaning.Date %in% '2025-08-19',
         c("Car1P3","Car1P4","Car1P5")] = c("Seattle 1117","Redmond 7579",NA)

#  remove : 2025-05-28 Bellevue 188
#.          2025-07-07 Seattle 3511    
#           2025-07-26 Elektra 510
#           2025-07-31 Sammamish 23718
dispatch[dispatch$Cleaning.Date %in% '2025-05-28',paste0("Car1P",1:5)] =
  c(dispatch[dispatch$Cleaning.Date %in% '2025-05-28',paste0("Car1P",2:5)],NA)

dispatch[dispatch$Cleaning.Date %in% '2025-07-07',
         c("Car2P6","Car2P7","Car2P8")] = c("Kirkland 10219","Sammamish 2009",NA)

dispatch[dispatch$Cleaning.Date %in% '2025-07-26',
         paste0("Car1P",2:5)] =c("Elektra 1305","Elektra 809","Elektra 1115",NA)
dispatch[dispatch$Cleaning.Date %in% '2025-07-31',c("Car2P2","Car2P3")] =
  c("Mercer 3627 Main",NA)
# 5/25 dispatch card wrong, it was for 5/23
# 5/30 dispatch card have 3 cleaners, we paid 2 only


data = NULL
for(i in 1: nrow(dispatch))
 for(j in 1:4)
 {
  units = dispatch[i,setdiff(grep(paste0("Car",j),colnames(dispatch)),
                             grep(paste0("Car",j,"Add"),colnames(dispatch)))]
  names = names(units)[!is.na(units)]
  units = units[!is.na(units)]
  ppls = trimws(unlist(strsplit(dispatch[i,paste0("Team",j)],",")))
  if(length(units)>0) data = rbind(data,data.frame(Cleaning.Date = dispatch$Cleaning.Date[i],
                               cleaner = rep(ppls,each=length(units)),Listing=units,
                               Car.arrange=names))
 }

data$Listing = sub("Bothell 18005","Bothell 18006",data$Listing)
data$Listing = sub("Kirland 13805","Kirkland 13805",data$Listing)
data = data %>% arrange(Cleaning.Date,Car.arrange,cleaner) %>%
  mutate(cleaner = ifelse(cleaner %in% c("HIlda","Hilda"),"Hilda G",cleaner),
         Listing = sub("#|Island ","",Listing))
data$Listing[data$Listing %in% "Seattle 906 Lower"] = "Seattle 906"
#data$Listing[data$Listing %in% "Clyde hill 8830"] = "Clyde Hill 8830"
#data$Listing[data$Listing %in% "12520 Bellevue"] = "Bellevue 12520"

## check data...
tmp = dispatch %>% select(Cleaning.Date,nunit) %>% 
  join(data %>% group_by(Cleaning.Date) %>% 
         reframe(nunit1=length(unique(Listing))))
tmp %>% filter(nunit!=nunit1)
###

payCleaner = read.xlsx("./Data/Maria cleaning payment process_copied20250916.xlsx",
                       sheet = "Payment", startRow = 3) %>%
  filter(!grepl("Maria", Payment.date)) 

# Standardize payment dates
payCleaner = payCleaner %>%
  mutate(Week = as.Date(`Week.(Fri)`, origin = '1899-12-30'),
         Day = as.Date(Day, origin = '1899-12-30'),
         PayDay = as.Date(as.integer(Payment.date), origin = '1899-12-30'),
         Cleaner = ifelse(Cleaner %in% "Hilda G ","Hilda G",Cleaner))

## add the missing payment
added = payCleaner %>%  filter(Day %in% '2025-05-30' & Cleaner %in% "Gustavo")
added$Cleaner = "Ileana T"
payCleaner = rbind(payCleaner,added)
  
## check data..
daily_cleaners = payCleaner %>% group_by(Day) %>% reframe(ncleaner = n()) 
daily = data %>% group_by(Cleaning.Date) %>% 
  reframe(nunit=length(unique(Listing)),
          ncleaner= length(unique(cleaner))) 
daily = merge(daily,daily_cleaners,by.x ="Cleaning.Date",by.y="Day",all.x=T)
daily %>% filter(ncleaner.x!=ncleaner.y)
##-----------------

data = merge(data,payCleaner %>% select(Day,Cleaner,Rate),
             by.x=c("Cleaning.Date","cleaner"),by.y=c("Day","Cleaner"),all.x=T)

cleaning.rate = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx', 
                     sheet = 'Cleaning') #%>%
 #  mutate(Cleaning.fee = ifelse(is.na(New.cleaning.fee), 
 #                               Current.clg.fee, New.cleaning.fee))
property = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx')
cleaning.rate = merge(cleaning.rate,property %>% select(Listing,SqFt,BEDROOMS,BEDS,BATHROOMS),by="Listing",all.x=T)
cleaning.rate = cleaning.rate %>%
  mutate(base = ifelse(SqFt<=600,ifelse(BEDROOMS %in% 0,70,85),85),
         added = 20*(BEDS-1)+ 15*(BATHROOMS-1),
         laundry = 12*ifelse(BEDROOMS<=1,1,BEDROOMS),
         estimated.cleaning = base+added+laundry)

data = merge(data,cleaning.rate %>% 
              select(Listing,Cleaning.fee,Maria.pay,SqFt,BEDROOMS,BEDS,BATHROOMS,estimated.cleaning),
              by="Listing",all.x=T)

Residential.customers = read.xlsx("./Data/Valta Homes Residential Cleaning Schedule and Payment Record.xlsx",sheet="Data validation")
Residential.cleanings = read.xlsx("./Data/Valta Homes Residential Cleaning Schedule and Payment Record.xlsx") %>%
  select(Service.Date,Listing,residential.fee=`Maria.(.80.of.Column.K)`) %>%
  filter(!is.na(Service.Date)) %>%
  mutate(Service.Date = as.Date(as.integer(Service.Date), origin = '1899-12-30'),
         yearmonth = substr(Service.Date,1,7),
         Type ="Residential")  %>%
  filter(!is.na(Service.Date))
Residential.cleanings$order = 1:nrow(Residential.cleanings)

#Residential.cleanings = Residential.cleanings %>% arrange(order)
#write.csv(Residential.cleanings,"Residential_cleanings_reformat.csv",row.names=F,na='')

Residential = Residential.cleanings%>%
  filter(yearmonth %in% c('2025-05','2025-06','2025-07','2025-08',"2025-09"))

data = merge(data,Residential %>% 
               select(Service.Date,Listing,residential.fee,Type), 
             by.x=c('Cleaning.Date',"Listing"),
             by.y=c("Service.Date","Listing"),all.x=T) %>%
       mutate(CleaningIncome = ifelse(!is.na(Maria.pay),Maria.pay,residential.fee),
         estimated.cleaning = ifelse(!is.na(residential.fee),
                                     residential.fee,estimated.cleaning),
         Type = ifelse(is.na(Type),"STR",Type))

# check: dispatch has, neither in STR nor residential 
setdiff(setdiff(data$Listing,Residential$Listing),property$Listing)

# check missing residential cleaning, but on dispatch card
data %>% filter(is.na(estimated.cleaning)) %>% 
  select(Cleaning.Date,Listing) %>% distinct()

# treat Elektras as residential
idx = data$Listing %in% c("Elektra 609","Elektra 1514",
                          "Elektra 1314","Elektra 909","Elektra 510")
data$Type[idx] = "STR/Residential"
data$estimated.cleaning[idx] = data$Maria.pay[idx]

pay_cleaner = data %>% 
  group_by(Cleaning.Date,Listing,Cleaning.fee,CleaningIncome) %>% 
  reframe(ncleaner = n()) %>%
  mutate(cleaner_income = CleaningIncome/ncleaner)

pay_property = data %>% 
  group_by(Cleaning.Date,cleaner,Rate) %>% 
  reframe(nproperty = n(),
          wt.sum=sum(estimated.cleaning)) 

data = merge(data,pay_cleaner %>% select(Cleaning.Date,Listing,cleaner_income),
             by=c("Cleaning.Date","Listing"),all.x=T)
data = merge(data,pay_property %>% select(Cleaning.Date,cleaner,wt.sum),
             by=c("Cleaning.Date","cleaner"),all.x=T)
data = data %>% mutate(cleaner_paid = Rate*estimated.cleaning/wt.sum)
  
cleaner_paids = data %>% group_by(Cleaning.Date,cleaner,Rate) %>% 
      reframe(Units=n(),nSTR=length(unique(Listing[Type %in% "STR"])),
              income=sum(cleaner_income,na.rm=T),
              missing = sum(is.na(cleaner_income)))

property_costs = data %>% 
  group_by(Cleaning.Date,Listing,CleaningIncome,Cleaning.fee) %>% 
  reframe(ncleaner=n(),cleaningcost=sum(cleaner_paid,na.rm=T),
          missing = sum(is.na(cleaner_paid)))

# cleaner_paids %>%
#   ggplot(aes(Cleaning.Date,income)) +
#   geom_line()+
#   geom_point()+
#   geom_hline(aes(yintercept = Rate)) +
#   facet_wrap(~cleaner,ncol=2) +
#   labs(x="Cleaning Date",y='Income')

properties = sort(unique(property_costs$Listing))

# property_costs %>% 
#   ggplot(aes(Cleaning.Date,cleaningcost)) +
#   geom_line()+
#   geom_point()+
#   geom_hline(aes(yintercept = CleaningIncome)) +
#   facet_wrap(~Listing,ncol=2) +
#   labs(x="Cleaning Date",y='Cleaning Cost')

data = data %>% filter(!is.na(Rate)) # remove Lily's work as Maria pay her directly

save(property,data,cleaning.rate,Residential.customers,
     Residential.cleanings,file="./MariaCleaningCost/DispatchCardData.Rdata")

## get cood
# library(tidygeocoder)
# property_address = rbind(property %>% select(Property,address=listing.address.full,lat,long),
#                   Residential.customers %>% select(Property,address,lat,long)) %>% 
#                   filter(!duplicated(Property)& !is.na(address)) %>%  arrange(Property) 
# 
# idx = property_address$Property %in% "Seattle 1717"
# 
# property_address[idx,c('lat','long')] = 
#   geo(address = property_address$address[idx], method = "osm")[,2:3]
# 
# finded = property_address %>% filter(!is.na(long))
# 
# write.csv(property_address,'address.csv',row.names=F,na="")




  
