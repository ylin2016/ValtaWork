## Format VRBO records to guesty records for input of PowerBi
## Format CH & VRBO records (from RevenueProjection 2022, 2023) to guesty records for input of PowerBi
## 10/18/2023: add Sept/Oct booking from guesty adj by airbnb
## 11/08/2023: Using all past booking data pulled today + airbnb_01_2023-12_2023.csv + airbnb_pending.csv
## 12/31/2023: Using all past booking data pulled today + airbnb_01_2023-12_2023.csv + airbnb_pending.csv

library(readxl)
library(dplyr)
library(plyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost")
property = read.xlsx('./Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
vrbo = read.csv('./Data and Reporting/Data/VRBO_PayoutSummaryReport_2020-01-01_2023-09-20.csv')

##===========Guesty record adjusted by airbnb payout ============================###
guesty = read.csv('./Data and Reporting/Data/Guesty_PastBooking_12312023.csv')
#guesty = read.csv('./Data and Reporting/Data/Guesty_PastBooking_11082023.csv')
# guesty = read.csv('./Data and Reporting/Data/Guesty_PastBooking_09232023.csv')
# guesty_9_10 = read.csv('./Data and Reporting/Data/Guesty_PastBooking_SeptOct_20231018.csv')
# guesty = rbind(data.frame(guesty,pulled='20230923'),
#                data.frame(guesty_9_10,pulled='20231018'))

## correct guesty record airbnb payout
#abpay = read.csv('./Data and Reporting/Data/2023.01.01 to 2023.12.31.csv')
abpay1 = read.csv('./Data and Reporting/Data/airbnb_01_2023-12_2023.csv')
abpay2 = read.csv('./Data and Reporting/Data/airbnb_pending.csv')
abpay2022 = read.csv('./Data and Reporting/Data/2020.01 to 2022.12 Airbnb transactions.csv')
abpay = rbind(abpay1,abpay2,abpay2022)
abpay = abpay[!abpay$Confirmation.Code %in% c(NA,''),]
dup = duplicated(abpay$Confirmation.Code)
dups = abpay[abpay$Confirmation.Code %in% abpay$Confirmation.Code[dup],]
dups = dups %>% arrange(Confirmation.Code)
ab.payout = abpay %>% group_by(Confirmation.Code,Start.Date,Nights,Guest,Listing) %>% 
  reframe(airbnb.payout = sum(Amount))
abpay2022 = ab.payout %>% filter(Confirmation.Code %in% abpay2022$Confirmation.Code)

guesty = merge(guesty,ab.payout[,c('Confirmation.Code','airbnb.payout')], 
               by.x = 'CONFIRMATION.CODE', by.y='Confirmation.Code',all.x=T)
guesty$Earnings = ifelse(!is.na(guesty$TOTAL.PAID), guesty$TOTAL.PAID,
                         guesty$TOTAL.PAYOUT)
diff = guesty %>% 
  filter(SOURCE %in% c('Airbnb') & (abs(Earnings -airbnb.payout) > 1e-6)) %>%
  select(CONFIRMATION.CODE,LISTING.S.NICKNAME,TOTAL.PAYOUT,Earnings,airbnb.payout)

idx =which(guesty$SOURCE %in% 'Airbnb' & 
             abs(guesty$Earnings -guesty$airbnb.payout) > 1e-6) 
#275 #262 (till 11/6/2023#257 #258 (add Sept+Oct)
guesty$Earnings[idx] = guesty$airbnb.payout[idx]
guesty[,c('inquiryId','airbnb.payout')] = NULL
guesty$LISTING.S.NICKNAME = sub("Seattle 3617 Origin","Seattle 3617",guesty$LISTING.S.NICKNAME)
write.csv(guesty,'Data and Reporting/Input_PowerBI/Guesty_PastBooking_airbnb_adj_12312023.csv',
          row.names=F,na='')

# write.csv(guesty[guesty$pulled %in% '20230923',] %>% select(-pulled),
#           'Data and Reporting/Input_PowerBI/Guesty_PastBooking_airbnb_adj_09232023.csv',
#           row.names=F,na='')
# 
# write.csv(guesty[guesty$pulled %in% '20231018',] %>% select(-pulled),
#           'Data and Reporting/Input_PowerBI/Guesty_PastBooking_airbnb_adj_10182023.csv',
#           row.names=F,na='')

###==========VRBO additional records ==============================================================##
vrbo = merge(vrbo,property[,c('Property.ID','Listing','LISTING.S.CITY','listing.address.full')],
             by='Property.ID',all.x=T)
vrbo$CHECK.IN = as.Date(vrbo$Check.in,format = "%B %d, %Y")
vrbo$CHECK.OUT = as.Date(vrbo$Check.out,format = "%B %d, %Y")
vrbo$CONFIRMATION.CODE = vrbo$Reservation.ID
vrbo$TOTAL.PAYOUT = vrbo$Payout
vrbo$NUMBER.OF.NIGHTS = vrbo$Nights
vrbo$LISTING.S.NICKNAME = vrbo$Listing
vrbo$SOURCE = 'VRBO'
vrbo$Status = vrbo$Booking.status

vrbo.output = vrbo[,c(intersect(colnames(vrbo),colnames(guesty)),'Status')]
vrbo.output = rbind.fill(guesty[1,],vrbo.output)[-1,]
vrbo.output = vrbo.output %>% filter(vrbo.output$Status %in% 'Reserve') %>%
              group_by(across(all_of(setdiff(colnames(vrbo.output),
                              c('TOTAL.PAYOUT','Earnings'))))) %>%
              reframe(TOTAL.PAYOUT = sum(TOTAL.PAYOUT,na.rm=T),
                      Earnings = TOTAL.PAYOUT) 
## remove duplicated records from VRBO records, 
##. confirmation codes are different, but checkin/out same
dup = vrbo.output %>% 
      select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE,Earnings) %>%
     join(guesty %>% 
          mutate(CONFIRMATION.CODE.guesty =CONFIRMATION.CODE) %>%
          select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE.guesty,Earnings),
           by=c('LISTING.S.NICKNAME','CHECK.IN','CHECK.OUT'),type='inner')
vrbo.output = vrbo.output[!vrbo.output$CONFIRMATION.CODE %in% dup$CONFIRMATION.CODE,]

dup2 = vrbo.output %>% 
  select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE,Earnings) %>%
  join(guesty %>% 
       mutate(CONFIRMATION.CODE.guesty =CONFIRMATION.CODE) %>%
       select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE.guesty,Earnings),
       by=c('LISTING.S.NICKNAME','CHECK.OUT'),type='inner') 

vrbo.output = vrbo.output[!vrbo.output$CONFIRMATION.CODE %in% dup2$CONFIRMATION.CODE,]

dup3 = vrbo.output %>% 
  select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE,Earnings) %>%
  join(guesty %>% 
         mutate(CONFIRMATION.CODE.guesty =CONFIRMATION.CODE) %>%
         select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE.guesty,Earnings),
       by=c('LISTING.S.NICKNAME','CHECK.IN'),type='inner') 
vrbo.output = vrbo.output %>% 
  filter(!CONFIRMATION.CODE %in% c(dup3$CONFIRMATION.CODE,"HA-12FK6L","HA-005LN6"))
#123
write.csv(vrbo.output[,colnames(guesty)],'Data and Reporting/Input_PowerBI/VRBO_20200101-20231230.csv',
          row.names=F,na='')

###==========Revenue additional records ===========================================================##
rev2022 = read.xlsx('Data and Reporting/RevenueProjection/Revenue Projection 2022.xlsx',
                    sheet='IGMS',startRow = 2)

rev2023 = read.xlsx('Data and Reporting/RevenueProjection/Revenue Projection 2023.xlsx',
                    sheet='CH data',startRow = 2)

rev2022 = rev2022 %>% 
    mutate(Mapping = sub('SF-Seattle 710 lower','SF-Seattle 710 ADU',Mapping)) %>%
    join(property %>% mutate(Mapping=Listings) %>%
           select(Mapping,Listing,listing.address.full,LISTING.S.CITY)) %>% 
    mutate(CHECK.IN = as.Date(From.Date,origin = '1899-12-30'),
           CHECK.OUT = as.Date(To.Date,origin = '1899-12-30'),
           CONFIRMATION.CODE = Code,
           TOTAL.PAYOUT = Total.Payout,
           Cleaning.fee = Cleaning.Fees,
           NUMBER.OF.NIGHTS = Nights,
           NUMBER.OF.GUESTS = Guests,
           LISTING.S.NICKNAME = Listing,
           SOURCE = Channel)

rev2022.uniq = rev2022 %>% arrange(desc(Download)) %>% 
      mutate(last_update = as.Date(Download,origin = '1899-12-30')) %>%
      select(all_of(c(intersect(colnames(rev2022),colnames(guesty)),'Status','last_update'))) %>%
      filter(!duplicated(paste0(LISTING.S.NICKNAME,CONFIRMATION.CODE))) 

# Airbnb     Booking.com Direct Bookings  VRBO 
#    831              12               7   102 

rev2022.output = rev2022.uniq[,c(intersect(colnames(rev2022.uniq),colnames(guesty)),'Status')]
rev2022.output = rbind.fill(guesty[1,],rev2022.output)[-1,] 
rev2022.output = rev2022.output[,colnames(guesty)]
setdiff(rev2022.output$LISTING.S.NICKNAME,property$Listing)
rev2022.direct = rev2022.output %>% 
  filter(SOURCE %in% c('Booking.com','Direct Bookings') & 
           !CONFIRMATION.CODE %in% c('152.0','989.0'))
dim(rev2022.direct) #17
write.csv(rev2022.direct, 'Data and Reporting/Input_PowerBI/Rev_Direct_Booking_2022.csv',
          row.names=F,na='')

air_guesty = setdiff(abpay2022$Confirmation.Code,
                     guesty$CONFIRMATION.CODE[guesty$CHECK.OUT<='2022-12-31' & 
                                                guesty$CHECK.OUT >='2022-01-01']) 
#234 #69 more from air2022
rev_guesty = setdiff(rev2022.output$CONFIRMATION.CODE[rev2022.output$SOURCE %in% 'Airbnb'],
                     guesty$CONFIRMATION.CODE) #88 #58 more from rev2022
air_rev_comm = intersect(air_guesty,rev_guesty) #68 # 38 in comm, unlist 31 and 7 use airbnb earnings

airbnb2022 = rev2022.output %>% filter(SOURCE %in% 'Airbnb')
airbnb2022 = merge(airbnb2022,guesty[,c('CONFIRMATION.CODE','Earnings')],
                   by='CONFIRMATION.CODE',suffix=c('.rev22',''),all.x=T)
airbnb2022 = airbnb2022 %>% filter(SOURCE %in% 'Airbnb' & is.na(Earnings)) %>% 
  mutate(Earings = TOTAL.PAYOUT)  #58

#unlisted are in airbnb records and revenue2022: 31
missed = setdiff(airbnb2022$LISTING.S.NICKNAME,guesty$LISTING.S.NICKNAME)
unlisted = airbnb2022[airbnb2022$LISTING.S.NICKNAME %in% missed,colnames(guesty)]
write.csv(unlisted,'Data and Reporting/Input_PowerBI/Rev_Airbnb_unlisted_2022.csv',
          row.names=F,na='') #63 #31

## airbnb 2022 and rev2022 overlaps, use airbnb earnings
setdiff(air_guesty,unlisted$CONFIRMATION.CODE) #171 #38 
code.air_rev2022 =intersect(airbnb2022$CONFIRMATION.CODE,setdiff(air_guesty,unlisted$CONFIRMATION.CODE)) #7
air_rev2022 = merge(airbnb2022[airbnb2022$CONFIRMATION.CODE %in% code.air_rev2022,],
            ab.payout[ab.payout$Confirmation.Code %in% code.air_rev2022,c('Confirmation.Code','airbnb.payout')],
            by.y='Confirmation.Code',by.x='CONFIRMATION.CODE')
air_rev2022$Earnings = air_rev2022$airbnb.payout
removed = c('HM99CRANAA','HM4ZZJ8HBC')
air_rev2022 = air_rev2022[!air_rev2022$CONFIRMATION.CODE %in% removed,]
write.csv(air_rev2022[,colnames(guesty)],
          'Data and Reporting/Input_PowerBI/Rev_Airbnb_rev_overlap_2022.csv',
          row.names=F,na='')

##=============================================================
## overlap record from guesty with different confirmation.code
air_guesty_comm = setdiff(air_guesty,air_rev_comm)
rev_guesty_comm = setdiff(rev_guesty,air_rev_comm)
rev_more = rev2022.output[rev2022.output$CONFIRMATION.CODE %in% rev_guesty_comm,c(12,1,2,3,5)] %>% 
    arrange(LISTING.S.NICKNAME,CHECK.IN)
guesty %>% filter(LISTING.S.NICKNAME %in% rev_more$LISTING.S.NICKNAME & SOURCE %in% 'Airbnb') %>% 
  filter(CHECK.OUT>='2022-06-20' & CHECK.OUT<='2022-06-30') %>% 
  select(LISTING.S.NICKNAME, CONFIRMATION.CODE,   CHECK.IN, CHECK.OUT,TOTAL.PAYOUT) %>% 
  arrange(LISTING.S.NICKNAME,CHECK.IN)
air_more = ab.payout %>% filter(Confirmation.Code %in% air_guesty_comm) %>% 
  mutate(CHECK.IN = as.Date(Start.Date,format="%m/%d/%Y")) %>%
  arrange(CHECK.IN) 

air_more = merge(air_more,guesty[!duplicated(guesty$LISTING.S.NICKNAME),
                                 c('LISTING.S.TITLE','LISTING.S.NICKNAME')],
                 by.x='Listing',by.y='LISTING.S.TITLE',all.x=T)
air_more$LISTING.S.NICKNAME[grep('Colorful & cozy',air_more$Listing)] = 'Elektra 703'
air_more$LISTING.S.NICKNAME[grep('UW-Bothell',air_more$Listing)] = 'Bothell 21833'
##=============================================================

## output vrbo missing 2022
vrbo2022 = rev2022.output %>% filter(SOURCE %in% 'VRBO')
vrbo2022.output = vrbo2022[vrbo2022$CONFIRMATION.CODE %in% 
                  setdiff(vrbo2022$CONFIRMATION.CODE,vrbo.output$CONFIRMATION.CODE),]
remove.code = c('HA-5XGPWF','HA-SLRVCG','HA-V4K78S','HA-QV4NKV','HA-005LN6')
vrbo2022.output = vrbo2022.output[!vrbo2022.output$CONFIRMATION.CODE %in% remove.code,]
write.csv(vrbo2022.output[,colnames(guesty)],
          'Data and Reporting/Input_PowerBI/Rev_vrbo_missing_2022.csv',
          row.names=F,na='')


rev2023 = rev2023 %>% 
  mutate(Property = sub("Kirkland 8252","Kirkland 8252-D201",Property)) %>%
  join(property %>% mutate(Property = Listing) %>%
         select(Property,listing.address.full,LISTING.S.CITY)) %>% 
  mutate(CHECK.IN = as.Date(From.date,origin = '1899-12-30'),
         CHECK.OUT = as.Date(To.date,origin = '1899-12-30'),
         CONFIRMATION.CODE = ifelse(is.na(Reservation),paste0('CH',From.date),Reservation),
         TOTAL.PAYOUT = `Rent/Payout.amount`,
         NUMBER.OF.NIGHTS = Booked.nights,
         LISTING.S.NICKNAME = Property,
         SOURCE = Platform,
         Status = 'confirmed')

rev2023.output = rev2023[,c(intersect(colnames(rev2023),colnames(guesty)),'Status')]
rev2023.output = rbind.fill(guesty[1,],rev2023.output)[-1,]
rev2023.output = rev2023.output  %>% 
  group_by(across(all_of(setdiff(colnames(rev2023.output),
                               c('TOTAL.PAYOUT','TOTAL.PAID','Earnings'))))) %>%
  reframe(TOTAL.PAYOUT = sum(TOTAL.PAYOUT,na.rm=T),
          TOTAL.PAID = sum(TOTAL.PAID,na.rm=T),Earnings = sum(Earnings,na.rm=T))

setdiff(rev2023.output$LISTING.S.NICKNAME,property$Listing)

dup = rev2023.output %>% 
  select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE,SOURCE,Earnings) %>%
  join(guesty %>% 
         select(LISTING.S.NICKNAME,CHECK.IN,CHECK.OUT,CONFIRMATION.CODE,Earnings) %>%
         mutate(CONFIRMATION.CODE.guesty = CONFIRMATION.CODE),
       by=c('LISTING.S.NICKNAME','CHECK.IN','CHECK.OUT'),type='inner')
CH = rev2023.output[,colnames(guesty)] %>% 
  filter(SOURCE %in% 'CH' &!CONFIRMATION.CODE %in% dup$CONFIRMATION.CODE)

write.csv(CH,'Data and Reporting/Input_PowerBI/Rev_CH_2023.csv',
          row.names=F,na='')
guesty$SOURCE[guesty$CONFIRMATION.CODE %in% dup$CONFIRMATION.CODE.guesty[dup$SOURCE %in%'CH']] ='CH'
write.csv(guesty,'Data and Reporting/Input_PowerBI/Guesty_PastBooking_airbnb_adj_12312023.csv',
                row.names=F,na='')
vrbo2023 = rev2023.output %>% filter(SOURCE %in% 'VRBO') #covered by vrbo records

##== Check double booking records ==========================
overall = rbind.fill(data.frame(filesource ='Guesty_PastBooking_airbnb_adj_11082023.csv',guesty),
                     data.frame(filesource ='VRBO_20200101-20231230.csv',vrbo.output[,colnames(guesty)]),
                     data.frame(filesource ='Rev_Direct_Booking_2022.csv',rev2022.direct[,colnames(guesty)]),
                     data.frame(filesource ='Rev_Airbnb_unlisted_2022.csv',unlisted[,colnames(guesty)]),
                     data.frame(filesource ='Rev_Airbnb_rev_overlap_2022.csv',air_rev2022[,colnames(guesty)]),
                     data.frame(filesource ='Rev_vrbo_missing_2022.csv',vrbo2022.output[,colnames(guesty)]),
                     data.frame(filesource ='Rev_CH_2023.csv',CH[,colnames(guesty)]))

overlaps = overall %>% select(filesource,LISTING.S.NICKNAME, CONFIRMATION.CODE,
                   CHECK.IN,CHECK.OUT,NUMBER.OF.NIGHTS,TOTAL.PAYOUT,TOTAL.PAID,Earnings,SOURCE) %>%
        filter((LISTING.S.NICKNAME %in% 'Elektra 703' & CHECK.IN %in% c('2023-04-01','2023-04-03'))| 
                (LISTING.S.NICKNAME %in% 'Elektra 1115' & CHECK.IN %in% c('2022-10-14')) |
                (LISTING.S.NICKNAME %in% 'Lilliwaup 28610' & CHECK.IN %in% c('2022-09-05','2022-09-06'))|
                 (LISTING.S.NICKNAME %in% 'Mercer 2449' & CHECK.IN %in% c('2022-07-28','2022-07-31'))) %>%
     arrange(LISTING.S.NICKNAME,CHECK.IN)
write.csv(overlaps,'Data and Reporting/double_bookings.csv',row.names=F,na='')






