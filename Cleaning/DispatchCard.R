setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/05-Cleaning/")
source("/Users/ylin/ValtaWork/Cleaning/DispatchCard_functions.R")
## !!! Cleaning fee use the ones on payout record to label each cleaning, 
## not the ones on property_cohost file
#--------------------------------------------------------------------------------
#        dispatch card
#--------------------------------------------------------------------------------
dispatch = read_dispatch()

## Manual correct: no 8/1 1717, 8/19 510,
dispatch[dispatch$Cleaning.Date %in% '2025-08-01','Car2P4'] = NA
dispatch[dispatch$Cleaning.Date %in% '2025-08-19',
         c("Car1P3","Car1P4","Car1P5")] = c("Seattle 1117","Redmond 7579",NA)

# remove : 2025-05-28 Bellevue 188
#          2025-07-07 Seattle 3511    
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
# 1/26/2026: No scheduled clean for Seattle 10057 Upper
dispatch[dispatch$Cleaning.Date %in% '2026-01-26',"Car1P4"] = NA
   
data_dispatch = format_dispatch(dispatch)   

##-----------------------------------------------------------------
## check data_dispatch : dispatch card vs. organized data_dispatch (find error in dispatch card)
##-----------------------------------------------------------------
tmp = dispatch %>% select(Cleaning.Date,nunit) %>% 
  join(data_dispatch %>% group_by(Cleaning.Date) %>% 
         reframe(nunit1=length(unique(Listing))))
tmp %>% filter(nunit!=nunit1) %>% arrange(Cleaning.Date)
#2025-10-15     8   7: it is ok as Clyde hill 8830 shown in car 1 &2 for joint work
#2025-10-11    12   11: it is ok as Kirkland 8017 shown in car 1 &2 for joint work
#2026-01-26    11   10:  No scheduled clean for Seattle 10057 Upper
#2026-02-15    13   14:  Seatac 12834 turn to upper & lower
##-----------------------------------------------------------------

#--------------------------------------------------------------------------------
## ---  pay Cleaners ------
#--------------------------------------------------------------------------------
payCleaner = pay_cleaner_record(payfile2026)

##-----------------------------------------------------------------
## check data_dispatch: daily dispatch #cleaners <> pay #cleaners
##-----------------------------------------------------------------
daily_cleaners = payCleaner %>% group_by(Day) %>% 
  reframe(ncleaner = length(unique(Cleaner))) 
daily = data_dispatch %>% group_by(Cleaning.Date) %>% 
  reframe(nunit=length(unique(Listing)),
          ncleaner= length(unique(cleaner))) 
daily = merge(daily,daily_cleaners,by.x ="Cleaning.Date",by.y="Day",all.x=T)
daily %>% filter(ncleaner.x!=ncleaner.y)
#5    2026-03-06     4          4          5  : loan deduction
#6    2026-03-13     5          5          6 : loan deduction
##-----------------------------------------------------------------

data = merge(data_dispatch,payCleaner %>% group_by(Day,Cleaner) %>%
               reframe(Rate=sum(Rate)),
             by.x=c("Cleaning.Date","cleaner"),by.y=c("Day","Cleaner"),all=T)

## Remove "Lily" since Maria pay her 
data %>% filter(is.na(Rate))
data %>% filter(is.na(Listing))
data_all = data %>% filter(!is.na(Rate)) %>%
  mutate(yearmonth = substr(Cleaning.Date,1,7)) # remove Lily's work as Maria pay her directly

data_all %>% group_by(yearmonth) %>% 
  reframe(days = length(unique(Cleaning.Date)))

dispatch_monthly = data_all %>% filter(Rate>0) %>%
  mutate(yearmonth = substr(Cleaning.Date,1,7)) %>%
  filter(!duplicated(paste(Cleaning.Date,Listing))) %>%
  group_by(yearmonth,Listing) %>%
  reframe(Times = n())

#--------------------------------------------------------------------------------
#        Income from STR and Residential
#--------------------------------------------------------------------------------
months = unique(substr(data$Cleaning.Date,1,7))
months = months#[-length(months)]

payout = read_payout(payfile2026,payfile2025,months)

# ----  STR Income Based on payment to get #clean, by month summary
IncomeSTR = str_income(payout,months)

#################### temp fix ############################
IncomeSTR[IncomeSTR$yearmonth %in% '2025-09' & 
            IncomeSTR$Listing %in% "Mercer 3627 ADU",-1] =
  c("Mercer 3627 Main",2,130,260,260,"STR")
##########################################################

#----  Residential Cleaning Income:
IncomeResid = residential_income()

Incomes_monthly = rbind.fill(IncomeSTR, IncomeResid %>% 
                    mutate(yearmonth = substr(Service.Date,1,7)) %>%
  group_by(yearmonth,Listing) %>% 
  reframe(Times=n(),CleaningIncome=sum(residential.fee,na.rm=T),
          Type="Residential")) %>%
  group_by(yearmonth,Listing) %>% 
  reframe(CleaningIncome = sum(as.numeric(CleaningIncome),na.rm=T),
         Times = sum(as.integer(Times),na.rm=T),
         Type = paste(Type,collapse = "/"))

# compare dispatch card and Incomes #time per listing per month
STR_monthly = IncomeSTR %>% group_by(yearmonth,Listing) %>% 
      reframe(Times=sum(as.integer(Times)))
Res_monthly = IncomeResid %>% group_by(yearmonth,Listing) %>% 
  reframe(Times.Res=n())

both = merge(dispatch_monthly,STR_monthly,by=c("yearmonth","Listing"),
             all=T,suffixes = c(".disp",'.str'))
both = merge(both, Res_monthly,by=c("yearmonth","Listing"), all=T) %>%
    mutate(Times.pay= coalesce(Times.str, 0) + coalesce(Times.Res, 0),
           diff = coalesce(Times.disp, 0)-coalesce(Times.pay, 0))

both %>% filter(yearmonth %in% '2026-03' & diff!=0)
## 2026-03
Kirkland 11321, 8017 on 3/21 and 3/22 twice
8415 on 3/22 and 3/23 twice

Bothell 18006    1 - NA -NA
Issaquah 1627    NA -NA - 1
Sammamish 1627

## 2025-11
Beachwood 1 scheduled + cleaned on 11/2 and 11/3, what reason?
Kirkland 8017 were cleaned 4 times in Nov, we only pay 3 times?
## 2025-10 

## 2026-01
#Beachwood 2 missing 1 residential cleaning. Good in Feb
#Kirkland 8017 missing 2 residential cleaning. Good in Feb
#Redmond 14707 missing 2 residential cleaning 

## 2026-02
# Bellevue 1420： scheduled 2, cleaned 1, owner stay, OK
# Bothell 3528        1 ->2   one is carpet
# Microsoft 14615-D303: re-cleaning once, OK
Bellevue 14507U1: 1 ->NA 
Elektra 1314        5 -> 6 
Elektra 1514        3 -> 2
Elektra 909         5 -> 4
Issaquah 917        1 -> 2
Kirkland 8017       2 -1- 2

month12 = both %>% filter(yearmonth %in% c('2026-03','2026-02','2026-01')) %>% 
  group_by(Listing) %>%
  reframe(across(where(is.numeric), sum,na.rm=T)) %>% 
  mutate(diff = Times.disp-Times.pay) 
month12 %>% filter(diff!=0)

#-------------------------------------------------------------------------------
Valtapay = data.frame(yearmonth = names(payout),
                      ValtaPaid = unlist(lapply(payout,function(x)
                        -sum(as.numeric(x[grep('payout',x$Listing),'Total']),na.rm=T))))
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
cleaning.rate = read.xlsx('../Data/Property_Cohost.xlsx', sheet = 'Cleaning') 
property = read.xlsx('../Data/Property_Cohost.xlsx')
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
       mutate(Cleaning.fee = as.numeric(Cleaning.fee),
              CleaningIncome = ifelse(!is.na(Cleaning.fee),Cleaning.fee,residential.fee),
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

enddate = '2026-01-31'
data %>% filter(is.na(estimated.cleaning) & Cleaning.Date <=enddate & Cleaning.Date>='2026-01-01') %>% 
  select(Listing) %>% distinct()

data %>% filter(is.na(CleaningIncome)& Cleaning.Date<=enddate)

data[data$yearmonth %in% '2025-08' & data$Listing %in% "Elektra 1314",
     c('residential.fee',"CleaningIncome") ] = 56

data[data$yearmonth %in% '2025-08' & data$Listing %in% "Elektra 1514",
     c('residential.fee',"CleaningIncome") ] = 60

data[data$yearmonth %in% '2025-08' & data$Listing %in% "Elektra 609",
     c('residential.fee',"CleaningIncome") ] = 72

##remove 7/2 beachwood 9 as 7/4 has one record

data = data %>% 
  filter(!(Cleaning.Date =='2025-07-02' & Listing %in% "Beachwood 9"))

# add cleaning fee for Mercer3925 cleaning
data[data$Cleaning.Date == "2026-01-03" & data$Listing %in% "Mercer 3925",
     c('Cleaning.fee',"CleaningIncome") ] = 50

## Check missing CleaningIncome, but on dispatch card:
data %>% filter(is.na(CleaningIncome)& Cleaning.Date<=enddate)


k = "Elektra 1314" #Seattle 1512"#Redmond 16012" "Bellevue 1420"
IncomeSTR %>% filter(grepl(k,Listing))
IncomeResid %>% filter(grepl(k,Listing))
dispatch_monthly %>% filter(grepl(k,Listing))
data %>% filter(Cleaning.Date =='2025-11-14' & Listing %in% k)
##--------------------------------------------------------------------------------

# treat Elektra as residential
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

data = data %>% filter(Cleaning.Date<=enddate)
save(property,data,cleaning.rate,payCleaner,
     Incomes_monthly,IncomeSTR,IncomeResid,Valtapay,
      file="./MariaCleaningCost/DispatchCardData-20260211.Rdata")

month.sel = '2026-01'
dispatch_m = data %>% filter(yearmonth %in% month.sel) %>%
  distinct(Cleaning.Date,Listing,CleaningIncome)%>% 
  group_by(Listing) %>% 
  reframe(Times=n(),CleaningIncome = sum(CleaningIncome))
Both = merge(dispatch_m,Incomes_monthly %>% 
               filter(yearmonth %in% month.sel),by="Listing",all=T)
Both %>% filter(Times.x!=Times.y)
k="Beachwood 1"
data_dispatch %>% mutate(yearmonth = substr(Cleaning.Date,1,7)) %>% 
  filter(yearmonth %in% month.sel & Listing %in% k) 

## dispatch card to check: on dispatch, not pay
# 2025-12-06 Redmond 14707 , manual removed
# 2025-06-29 Bellevue 1420
# 2025-07-29  Elektra 1314
# 2025-07-10 Redmond 16012
# 2025-11-14 Seattle 1512

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