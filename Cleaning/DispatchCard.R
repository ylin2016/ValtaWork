setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/05-Cleaning/")
## source("/Users/ylin/ValtaWork/Cleaning/DispatchCard.R")

## !!! Cleaning fee use the ones on payout record to label each cleaning, 
## not the ones on property_cohost file

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

#--------------------------------------------------------------------------------
#        dispatch card
#--------------------------------------------------------------------------------
## Pay Cleaner records: payments and # clean per property
payfile2025 = "./Data/Maria cleaning payment process_2025.xlsx"
payfile2026 = "./Data/Maria cleaning payment process_copied20260117.xlsx"

dispatch = read.xlsx("./Data/Maria Cleaning Dispatch Card.xlsx") %>%
        mutate(Cleaning.Date= as.Date(Cleaning.Date,origin='1899-12-30'))
colnames(dispatch)[-c(1:3)] = c("Team1",paste0("Car1P",1:5),"Car1Add",
                                "Team2",paste0("Car2P",1:5),"Car2Add",
                                "Team3",paste0("Car3P",1:4),"Car3Add",
                                "Team4",paste0("Car4P",1:4),"Car4Add")

dispatch[,-(1:3)] = apply(dispatch[,-(1:3)],2,function(x) 
  ifelse(x %in% "Other specify:",NA,x))

for(i in 1: nrow(dispatch))
{
  nunit = 0
  for(j in 1:4)
  {
    units = dispatch[i,setdiff(grep(paste0("Car",j),colnames(dispatch)),
                               grep(paste0("Car",j,"Add"),colnames(dispatch)))]
    nunit = nunit + sum(!units %in% c(NA,"NA","Other specify:","Specify other:"))
    if(!is.na(dispatch[i,paste0("Car",j,"Add")])){
       if(dispatch$Cleaning.Date[i]>="2025-10-02" & 
          !grepl("[.]",dispatch[i,paste0("Car",j,"Add")])){
         units.add = unlist(strsplit(dispatch[i,paste0("Car",j,"Add")],","))
         units.add = trimws(units.add)
         nunit = nunit + length(units.add)
         car.sel = dispatch[i,grepl(paste0("Car",j,"P"),colnames(dispatch))]
         num = length(car.sel[!is.na(car.sel)])
         num = (num+1) : (num+length(units.add))
         dispatch[i,paste0("Car",j,"P",num)] = units.add
       }else{
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

# 1/7/26: confirm with Belle: on dispatch, not on residential,
# records were missed on dec. Manually added
#    2025-12-02 Sammamish 23718
#    2025-12-09   Issaquah 1938

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

data_dispatch = NULL
for(i in 1: nrow(dispatch))
 for(j in 1:4)
 {
  units = dispatch[i,setdiff(grep(paste0("Car",j),colnames(dispatch)),
                             grep(paste0("Car",j,"Add"),colnames(dispatch)))]
  names = names(units)[!is.na(units)]
  units = units[!is.na(units)]
  ppls = trimws(unlist(strsplit(dispatch[i,paste0("Team",j)],",")))
  if(length(units)>0) 
    data_dispatch = rbind(data_dispatch,
                          data.frame(Cleaning.Date = dispatch$Cleaning.Date[i],
                               cleaner = rep(ppls,each=length(units)),Listing=units,
                               Car.arrange=names))
 }

data_dispatch$Listing = sub("Bothell 18005","Bothell 18006",data_dispatch$Listing)
data_dispatch$Listing = sub("Kirland 13805","Kirkland 13805",data_dispatch$Listing)
data_dispatch = data_dispatch %>% arrange(Cleaning.Date,Car.arrange,cleaner) %>%
  mutate(cleaner = ifelse(cleaner %in% c("HIlda","Hilda"),"Hilda G",cleaner),
         Listing = sub("#|Island ","",Listing))
txt =c("Seattle 906","Clyde hill 8830", "Clyde Hill 8830 (Gustavo not included)","1203",
       "Bellevue 1638 Residential","Issaquah 2450 Residential Cleaning",
       "Seattle 10057 lower","Seattle 10057 upper","Seattle 710 adu",
       "Bellevue 4616 (Flavio","Issquah 1627","Seatac")
chg = c("Seattle 906 Lower","Clyde Hill 8830","Clyde Hill 8830","Elektra 1203",
        "Bellevue 1638","Issaquah 2450","Seattle 10057 Lower","Seattle 10057 Upper","Seattle 710 ADU",
        "Bellevue 4616","Issaquah 1627","Seatac 12834")

for(k in 1:length(txt))
  data_dispatch$Listing[data_dispatch$Listing %in% txt[k]] = chg[k]

#data_dispatch$Listing[data_dispatch$Listing %in% "12520 Bellevue"] = "Bellevue 12520"
#setdiff(data_dispatch$Listing,c(Residential$Listing,property$Listing))

## put Seatac 12834 to upper & lower to match pay records
add= data_dispatch %>% filter(Listing %in% "Seatac 12834")
add$Listing = "Seatac 12834 Lower"
data_dispatch = rbind(data_dispatch %>% 
                        mutate(Listing = ifelse(Listing %in% "Seatac 12834",
                        "Seatac 12834 Upper",Listing)),add)
##-----------------------------------------------------------------
## check data_dispatch : dispatch card vs. organized data_dispatch (find error in dispatch card)
##-----------------------------------------------------------------
tmp = dispatch %>% select(Cleaning.Date,nunit) %>% 
  join(data_dispatch %>% group_by(Cleaning.Date) %>% 
         reframe(nunit1=length(unique(Listing))))
tmp %>% filter(nunit!=nunit1) %>% arrange(Cleaning.Date)
#2025-10-15     8      7: it is ok as Clyde hill 8830 shown in car 1 &2 for joint work
#2025-10-11    12     11: it is ok as Kirkland 8017 shown in car 1 &2 for joint work
##-----------------------------------------------------------------

#--------------------------------------------------------------------------------
## ---  pay Cleaners ------
#--------------------------------------------------------------------------------
payCleaner = read.xlsx(payfile2026,sheet = "Payment", startRow = 3) %>%
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
payCleaner = rbind(payCleaner,added) %>% 
        mutate(yearmonth=substr(Day,1,7))

##-----------------------------------------------------------------
## check data_dispatch: daily dispatch #cleaners <> pay #cleaners
##-----------------------------------------------------------------
daily_cleaners = payCleaner %>% group_by(Day) %>% reframe(ncleaner = n()) 
daily = data_dispatch %>% group_by(Cleaning.Date) %>% 
  reframe(nunit=length(unique(Listing)),
          ncleaner= length(unique(cleaner))) 
daily = merge(daily,daily_cleaners,by.x ="Cleaning.Date",by.y="Day",all.x=T)
daily %>% filter(ncleaner.x!=ncleaner.y)
##-----------------------------------------------------------------

data = merge(data_dispatch,payCleaner %>% select(Day,Cleaner,Rate),
             by.x=c("Cleaning.Date","cleaner"),by.y=c("Day","Cleaner"),all=T)

## Remove "Lily" since Maria pay her 
data %>% filter(is.na(Rate))
data %>% filter(is.na(Listing))
data_all = data %>% filter(!is.na(Rate)) %>%
  mutate(yearmonth = substr(Cleaning.Date,1,7)) # remove Lily's work as Maria pay her directly

data_all %>% group_by(yearmonth) %>% 
  reframe(days = length(unique(Cleaning.Date)))

#--------------------------------------------------------------------------------
#        Income from STR and Residential
#--------------------------------------------------------------------------------
## cleaning per listing
payout = vector('list', 12)
# Read payout data for months 1-10
for(i in 1:12) {
  if(i==12){
    tmp = read.xlsx(payfile2026, sheet = paste0("2025-", i))
  }else{tmp = read.xlsx(payfile2025, sheet = paste0("2025-", i))}
  
  # Extract relevant payout information
  indv = tmp[c(1:(grep("Total payment", tmp[,1])[1]),
               grep("payout", tmp[,1]), grep("Due", tmp[,1])), 1:5]
  colnames(indv) = c("Listing", "Cleaning.fee", "Times", "Total", "PayDate")
  payout[[i]] = indv %>% filter(Total!=0)
}

# ----  STR Income Based on payment to get #clean, by month summary
IncomeSTR = NULL
for(k in 6:12)
{
  x=payout[[k]]
  IncomeSTR = rbind(IncomeSTR,
                        data.frame(yearmonth=ifelse(k<10,paste0("2025-0",k),paste0("2025-",k)),
                                   x[!is.na(x$Times) & !x$Listing %in% "Total payment",
                                     c("Listing","Times","Cleaning.fee","Total")]))
}

IncomeSTR$Listing = sub("#|Island ","",IncomeSTR$Listing) 
txt = c("Redmond Gull val 7(Redmond 7579)","Bellevue C19","Bellevue D303","Bellevue E205")
chg = c("Redmond 7579","Microsoft 14645-C19","Microsoft 14615-D303","Microsoft 14620-E205")
for(k in 1:length(txt)) 
  IncomeSTR$Listing[IncomeSTR$Listing %in% txt[k]]= chg[k]
IncomeSTR$CleaningIncome= IncomeSTR$Total
IncomeSTR$Type = "STR"

#################### temp fix ############################
IncomeSTR[IncomeSTR$yearmonth %in% '2025-09' & 
            IncomeSTR$Listing %in% "Mercer 3627 ADU",-1] =
  c("Mercer 3627 Main",2,130,260,260,"STR")
##########################################################

#----  Residential Cleaning Income:
IncomeResid = read.xlsx("./Data/Valta Homes Residential Cleaning Schedule and Payment Record.xlsx") %>%
  select(Service.Date,Listing,residential.fee=`Maria.(.80.of.Column.K)`) %>%
  filter(!is.na(Service.Date)) %>%
  mutate(Service.Date = as.Date(as.integer(Service.Date), origin = '1899-12-30'),
         yearmonth = substr(Service.Date,1,7),
         Type ="Residential",
         Listing = ifelse(Listing %in% "Seattle 10057", "Seattle 10057 Whole",Listing))  %>%
  filter(!is.na(Service.Date))

Incomes_monthly = rbind.fill(IncomeSTR, IncomeResid %>% 
                    mutate(yearmonth = substr(Service.Date,1,7)) %>%
  group_by(yearmonth,Listing,Type) %>% 
  reframe(Times=n(),CleaningIncome=sum(residential.fee)))
#-------------------------------------------------------------------------------

## get cleaning.fee from IncomeSTR for STR cleanings
data = merge(data_all %>% 
               filter(Cleaning.Date>="2025-06-01"), 
             IncomeSTR %>% select(yearmonth,Listing,Cleaning.fee),
             by=c("yearmonth","Listing"),all=T)

## check paid to Maria, but not in dispatch card/schedule
data %>% filter(is.na(Cleaning.Date))

#-------------------------------------------------------------------------------
# Cleaning rate for calculate laundry weights -----------------
#-------------------------------------------------------------------------------
cleaning.rate = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx', 
                     sheet = 'Cleaning') 
property = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx')
cleaning.rate = merge(cleaning.rate,property %>% 
                  select(Listing,SqFt,BEDROOMS,BEDS,BATHROOMS),
                  by="Listing",all.x=T)
cleaning.rate = cleaning.rate %>%
  mutate(base = ifelse(SqFt<=600,ifelse(BEDROOMS %in% 0,70,85),85),
         added = 20*(BEDS-1)+ 15*(BATHROOMS-1),
         laundry = 12*ifelse(BEDROOMS<=1,1,BEDROOMS),
         estimated.cleaning = base+added+laundry)

data = merge(data,cleaning.rate %>% 
              select(Listing,estimated.cleaning), by="Listing",all.x=T)
#-------------------------------------------------------------------------------

data = merge(data,IncomeResid %>% 
               select(Service.Date,Listing,residential.fee,Type), 
             by.x=c('Cleaning.Date',"Listing"),
             by.y=c("Service.Date","Listing"),all.x=T) %>%
       mutate(CleaningIncome = as.numeric(ifelse(!is.na(Cleaning.fee),Cleaning.fee,residential.fee)),
         estimated.cleaning = ifelse(!is.na(residential.fee),
                                     residential.fee,estimated.cleaning),
         Type = ifelse(is.na(Type),"STR",Type))

##--------------------------------------------------------------------------------
# check: dispatch has, neither in STR nor residential 
##--------------------------------------------------------------------------------
setdiff(setdiff(data$Listing,IncomeResid$Listing),property$Listing)
##--------------------------------------------------------------------------------
# check missing residential cleaning, but on dispatch card
##--------------------------------------------------------------------------------
data %>% filter(is.na(estimated.cleaning)) %>% 
  select(Cleaning.Date,Listing) %>% distinct()

data %>% filter(is.na(CleaningIncome)& Cleaning.Date<='2025-12-31')

data[data$yearmonth %in% '2025-08' & data$Listing %in% "Elektra 1314",
     c('residential.fee',"CleaningIncome") ] = 56

data[data$yearmonth %in% '2025-08' & data$Listing %in% "Elektra 1514",
     c('residential.fee',"CleaningIncome") ] = 60

data[data$yearmonth %in% '2025-08' & data$Listing %in% "Elektra 609",
     c('residential.fee',"CleaningIncome") ] = 72

##remove 7/2 beachwood 9 as 7/4 has one record

data = data %>% 
  filter(!(Cleaning.Date =='2025-07-02' & Listing %in% "Beachwood 9"))

## Check missing CleaningIncome, but on dispatch card:
data %>% filter(is.na(CleaningIncome)& Cleaning.Date<='2025-12-31')

dispatch_monthly = data %>% 
  mutate(yearmonth = substr(Cleaning.Date,1,7)) %>%
  filter(!duplicated(paste(Cleaning.Date,Listing))) %>%
  group_by(yearmonth,Listing) %>%
  reframe(Times = n())

k = "Elektra 1314" #Seattle 1512"#Redmond 16012" "Bellevue 1420"
IncomeSTR %>% filter(grepl(k,Listing))
IncomeResid %>% filter(grepl(k,Listing))
dispatch_monthly %>% filter(grepl(k,Listing))
data %>% filter(Cleaning.Date =='2025-11-14' & Listing %in% k)
##--------------------------------------------------------------------------------

# treat Elektras as residential
idx = data$Listing %in% c("Elektra 609","Elektra 1514",
                          "Elektra 1314","Elektra 909","Elektra 510")
data$Type[idx] = "STR/Residential"
data$estimated.cleaning[idx] = data$residential.fee[idx]

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

properties = sort(unique(property_costs$Listing))


data = data %>% filter(Cleaning.Date<="2025-12-31")
save(property,data,cleaning.rate,payCleaner,Incomes_monthly,
      file="./MariaCleaningCost/DispatchCardData-20260117.Rdata")

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




  
